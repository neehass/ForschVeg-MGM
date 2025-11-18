# help-functions
# Model run and Visual
.libPaths(c("/home/ifgg1/R/x86_64-pc-linux-gnu-library/4.2", "/home/ifgg1/R/x86_64-pc-linux-gnu-library/4.5")) # packages Path NoMachine
# install.packages("gtable") # reinstall, to update ggplot
# install.packages("scales") # reinstall, to update ggplot
# install.packages("ggplot2")
# install.packages("patchwork")

library(ggplot2)
library(patchwork)
library(stringr)
set.seed(42)

# getAreaGroup ----------------------------
# same like in input.jl
# by Areakm2 --> AreaGroup is derived
    # ”very.small”: <1, ”small”: <2, ”medium”: 2km2, ”large”: 5km2 and ”very.large”: 20km2

    # Returns the area group for a given lake configuration file.
getAreaGroup <- function(areakm2) {
  if (area <= 1.0) {
    "very.small"
  } else if (area <= 2.0) {
    "small"
  } else if (area <= 5.0) {
    "medium"
  } else if (area <= 20.0) {
    "large"
  } else {
    "very.large"
  }
}


# Function to detect Julia (CHatGPT) ---------------------------------------
find_julia <- function() {
  # Common locations
  candidates <- c(
    Sys.which("julia"),             # julia in PATH
    "/usr/local/bin/julia",         # typical symlink
    list.files("/opt", pattern="^julia-", full.names=TRUE) # /opt/julia-*
  )
  
  # Filter only existing files/folders
  candidates <- Filter(file.exists, candidates)
  
  # Resolve symlinks if needed
  for (path in candidates) {
    if (file.info(path)$isdir) {
      julia_bin <- file.path(path, "bin")
      if (file.exists(file.path(julia_bin, "julia"))) {
        return(normalizePath(julia_bin))
      }
    } else if (basename(path) == "julia") {
      return(normalizePath(dirname(path)))
    }
  }
  
  stop("Julia not found on the system. Please install Julia.")
}



# --- select species ----------------------------------------
# 14xxx oligotroph
# 15xxx mesotroph
# 16xxx eutroph

# random selection of certain number (n) per species group

func_sel_spec <- function(n, species_path){
    files <- list.files(species_path)
    cleaned <- sub("\\.config\\.txt$", "", files)

    # select groups 
    oli <- cleaned[grepl("species_14\\d*", cleaned)]
    meso <- cleaned[grepl("species_15\\d*", cleaned)]
    eut <- cleaned[grepl("species_16\\d*", cleaned)]

    # select random 
    oli_n  <- sample(oli,  min(n, length(oli)))
    meso_n <- sample(meso, min(n, length(meso)))
    eut_n <- sample(eut,  min(n, length(eut)))

    selected_files <- c(oli_n, meso_n, eut_n)
    
    # length(selected_files)
    return(selected_files)
}
# --------------------------------------------

# ---- func_getSpecies_config ------------------------------
func_getSpecies_config <- function(path_configFile){
  file <- readLines(path_configFile)
  spec <- strsplit(file[grep("species", file)][[1]], " ")[[1]][-1]
  cleaned <- sub(".*(?=species_)", "", spec, perl = TRUE)
  species <- sub("\\.config\\.txt$", "", cleaned)
  # species_id <- as.numeric(sub(".*species_", "", species))

  return(species)
}

# --------------------------------------------


# ---- Temp profiles ------------------------------
T_profile <- function(z, T_epi, T_hypo, z0, k) {
  t <- T_hypo + (T_epi - T_hypo) / (1+exp((abs(z) - abs(z0)) / k))
  t[1] <- T_epi
  #t[length(t)] <- T_hypo
  return(t)
}

# ---- Plot results -------------------------------
func_plot_macroDay <- function(macrodata, speciesID_nam, name, days, scenario) {
    
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
    ggsave(paste0(name, "_macrophytesDay.png"), plot = p_macro, width = 10, height = 8)


}

func_plot_macroDepth <- function(macrodata, speciesID_nam, name, days, scenario) {
     macrodata$depth <- as.numeric(as.character(macrodata$depth))
    p_macro <- ggplot(data = macrodata, aes(x = depth, y = biomass, 
        color = as.factor(day), group = as.factor(day))) +
      geom_line() +
      theme_bw() +
        theme(legend.position = "none") +
      labs(title = paste("Biomass per Species per Depth:",name,
            "\n(grouped by day)",
            "\nScenario: ", scenario), 
      x = "Depth [m]", y = "Biomass") + 
      facet_wrap(~ speciesID, labeller = labeller(speciesID = speciesID_nam))+
      scale_x_reverse()
  # p_macro

    # Save
    ggsave(paste0(name, "_macrophytesDepth.png"), plot = p_macro, width = 10, height = 8)

}

# plot ENV -----------------------------------
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
            annotate("text", x = max(prof_max)-2, y = depth, 
                label = paste("steepness parameter \nk = ", k), 
                color = "black", size = 3) +
            labs(
              title = paste("Temperature Profile: ",name,
                "\nat the max, min and mean Temp. of timespan \nDays: ", dmax[1] , dmin[1], 
                "\nScenario: ", scenario), 
                x = "Temp [°C]", y = "Depth [m]") 

#   p_profile
  ggsave(paste0(name, "_Tprofile.png"), plot = p_profile, width = 8, height = 10)
}

