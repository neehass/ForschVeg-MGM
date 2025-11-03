# help-functions
# Model run and Visual
library(ggplot2)
library(patchwork)

# ---- Temp profiles ------------------------------
T_profile <- function(z, T_epi, T_hypo, z0, k) {
  t <- T_hypo + (T_epi - T_hypo) / (1+exp((abs(z) - abs(z0)) / k))
  t[1] <- T_epi
  t[length(t)] <- T_hypo
  return(t)
}

# ---- Plot results -------------------------------
func_plot_macro <- function(macrodata, speciesID_nam, name, days, scenario) {
    
    # Plot
    p_macro <- ggplot(data = macrodata, aes(x = day, y = biomass, 
            group = interaction(speciesID, depth), 
            color = depth)) +
        geom_line() +
        theme_bw() +
        labs(
        title = paste("Biomass over time per species:",name,"\nScenario: ", scenario), x = "Day",
        y = "Biomass", color = "Depth"
        ) + facet_wrap(~ speciesID, labeller = labeller(speciesID = speciesID_nam))
    # p_macro

    # Save
    ggsave(paste0(name, "_macrophytes.png"), plot = p_macro, width = 10, height = 8)


}

func_plot_env <- function(envdata, name, days, scenario){
    # --- Env 
    p_temp <- ggplot(envdata, aes(x = day)) +
        geom_line(aes(y = tempEpi, colour = factor("tempEpi"))) +
        geom_line(aes(y = tempHypo, colour = factor("tempHypo"))) +
        scale_color_manual(values = c("tempEpi" = "red", "tempHypo" = "blue"),
            labels = c("Epilimnion Temp.", "Hypolimnion Temp."),
            name = "Parameter") +
        annotate("rect",
                xmin = min(days), xmax = max(days),
                ymin = -Inf, ymax = Inf,
                alpha = 0.2, fill = "grey") +
        labs(title = "Temperature",x = "Day",y = "Temp [°C]") +
        theme_bw()
    # p_temp

    p_mesodepth <- ggplot(envdata, aes(x = day)) +
        geom_line(aes(y = mesoDepth, colour = factor("mesoDepth"))) +
        geom_line(aes(y = waterlevel, colour = factor("waterlevel"))) +
        scale_color_manual(values = c("mesoDepth" = "magenta", "waterlevel" = "darkgreen"),
            labels = c("Mesolimnion Depth", "Waterlevel"), name = "") +
        annotate("rect",
                xmin = min(days), xmax = max(days),
                ymin = -Inf, ymax = Inf,
                alpha = 0.2, fill = "grey") +
        labs(title= "Waterlevel & Mesolimnion Depth", x = "Day",y = "Depth [m]") +
        theme_bw()
    # p_mesodepth

    p_light <- ggplot(envdata, aes(x = day)) +
        geom_line(aes(y = irradiance, colour = factor("irradiance"))) +
        annotate("text", x = 50, y = max(envdata$irradiance)-50, 
            label = paste("Light Attenuation \nCoefficient [1/m]:", unique(envdata$lightAttenuation)), 
        color = "red", size = 3) +
        scale_color_manual(values = c("irradiance" = "orange"),
            labels = c("Irradiance"), name = "") +
        annotate("rect",
                xmin = min(days), xmax = max(days),
                ymin = -Inf, ymax = Inf,
                alpha = 0.2, fill = "grey") +
        labs(title= "Light related", x = "Day",y = "[W/m2]") +
        theme_bw()
    # p_light

    #geom_line(aes(y = irradiance, colour = factor("irradiance"))) +

    p_env <- p_temp + p_mesodepth + p_light + plot_layout(ncol = 1, guides = "collect") &  # collect all legends
        theme(legend.position = "right")               # move legend to left

    p <- p_env + 
        plot_annotation(title = paste("Environmental Data: ",name,"\nScenario: ", scenario))
    # p
    ggsave(paste0(name, "_env.png"), plot = p, width = 8, height = 10)
}

# plot temp Profile ------------------------
func_plot_Tprofile <- function(envdata, depth, name, days, scenario, k){

  temp <- envdata[envdata$day %in% days,]

  max <- temp[temp$tempEpi == max(temp$tempEpi), ]
  dmax <- unique(max$day)

  min <- temp[temp$tempEpi == min(temp$tempEpi), ]
  dmin <- unique(min$day)

  meanTEpi <- mean(temp$tempEpi)
  meanTHypo <- mean(temp$tempHypo)
  meanZ0 <- mean(temp$mesoDepth) 

  prof_max <- T_profile(0:depth, unique(temp$tempEpi[temp$day == dmax[1]]), 
                      unique(temp$tempHypo[temp$day == dmax[1]]), 
                      z0= unique(temp$mesoDepth[temp$day == dmax[1]]), 
                      k)
  prof_min <- T_profile(0:depth, unique(temp$tempEpi[temp$day == dmin[1]]), 
                      unique(temp$tempHypo[temp$day == dmin[1]]), 
                      z0= unique(temp$mesoDepth[temp$day == dmin[1]]), 
                      k)
  prof_mid <- T_profile(0:depth, meanTEpi, meanTHypo, 
                      z0= meanZ0, 
                      k)
  df_profile <- data.frame(depth = 0:depth, prof_max = prof_max, 
                    prof_mid = prof_mid, prof_min = prof_min)

  p_profile <- ggplot(df_profile, aes(y = depth))+
            geom_line(aes(x = prof_max, color = "max")) + 
            geom_line(aes(x = prof_min, color = "min")) + 
            geom_line(aes(x = prof_mid, color = "mid")) + 
            scale_color_manual(values = c("max" = "red", "mid" =  "orange", "min" = "blue"),
              labels = c("Max Temp.", "mean Temp.", "Min. Temp"),
              name = "Parameter") + theme_bw() +
            labs(
              title = paste("Temperature Profile: ",name,
                "\nat the max, min and mean Temp. of timespan \nDays: ", dmax[1] , dmin[1], 
                "\nScenario: ", scenario), 
                x = "Temp [°C]", y = "Depth [m]") 

#   p_profile
  ggsave(paste0(name, "_Tprofile.png"), plot = p_profile, width = 8, height = 10)
}

