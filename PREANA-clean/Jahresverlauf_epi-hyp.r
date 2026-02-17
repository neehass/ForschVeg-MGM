# Jahresverlauf Epi & Hypo - mean per month by Area group
# 1) Epi & Hypo & Tdiff Jahresverlauf 
# 2) Fraktion between Hypo and epi
# 3) z_sprung (z0) Jahresverlauf - mean per month by Area group

library(dplyr)
library(tidyr)
library(cowplot)
library(ggplot2)

EpiHyp_dep10 <- read.csv("data/dep10_epi-hypo_zPatalas.csv")  
colnames(EpiHyp_dep10)
View(EpiHyp_dep10)
dep10_smooth410 <- read.csv("data/dep10_patalas_smooth_4-10.csv")
dep10_smooth_all <- read.csv("data/dep10_patalas_smooth_all.csv")
colnames(dep10_smooth_all)
# ---- 1) Epi & Hypo & Tdiff Jahresverlauf - mean per month by Area group ----------------------------
frac_mean <- EpiHyp_dep10 %>% group_by(Month, Area_group) %>%
  mutate(T_hypo = mean(T_hypo, na.rm = TRUE),
          T_epi =  mean(T_epi, na.rm = TRUE),
          Tdiff = mean(thermal_dif, na.rm = TRUE),
          Tgrad = mean(thermal_gradient, na.rm = TRUE)) %>%
  mutate(frac_hyp = (T_hypo/T_epi)*100)
overall_mean <- EpiHyp_dep10 %>% group_by(Month) %>%
  mutate(T_hypo = mean(T_hypo, na.rm = TRUE),
          T_epi =  mean(T_epi, na.rm = TRUE),
          Tdiff = mean(thermal_dif, na.rm = TRUE),
          Tgrad = mean(thermal_gradient, na.rm = TRUE)) %>%
  mutate(frac_hyp = (T_hypo/T_epi)*100)

# min and max, Middle Between min & max 
max_Tdiff <- min(overall_mean$Tdiff, na.rm = TRUE)
min_Tdiff <- max(overall_mean$Tdiff, na.rm = TRUE)
mid_Tdiff <- (max_Tdiff+min_Tdiff)/2

max_Thyp <- max(overall_mean$T_hypo, na.rm = TRUE)
min_Thyp <- min(overall_mean$T_hypo, na.rm = TRUE)
mid_Thyp <- (max_Thyp+min_Thyp)/2

max_Tepi <- max(overall_mean$T_epi, na.rm = TRUE)
min_Tepi <- min(overall_mean$T_epi, na.rm = TRUE)
mid_Tepi <- (max_Tepi+min_Tepi)/2


p_mean <- frac_mean %>% pivot_longer(cols = c(T_epi, T_hypo, Tdiff), names_to = "Layer", values_to = "Temp") %>%
    ggplot(aes(x = Month, y = Temp, color = Area_group, linetype = Layer,
    group = interaction(Area_group, Layer)), linewidth = 1) + 
    scale_x_continuous( breaks = 1:12, labels = month.name) + 
    geom_line() +
    labs(# title = "Mean Epi-& Hypo-Temperature by Area Group",
    x = "Month", y = "Temperature [°C]", color = "LSAG", linetype = "a) Temp.") +
    
    # mean Tepi, Thypo, Tdiff
    geom_line(data = overall_mean, aes(x = Month, y = T_epi, linetype  = "meanT_epi"), color = "black", inherit.aes = FALSE) +
    geom_line(data = overall_mean, aes(x = Month, y = T_hypo, linetype  = "meanT_hypo"), color = "black", inherit.aes = FALSE) +
    geom_line(data = overall_mean, aes(x = Month, y = Tdiff, linetype  = "meanTdiff"), color = "black", inherit.aes = FALSE) +
    scale_linetype_manual(values = c("T_epi" = "solid", "T_hypo" = "dashed", "Tdiff" = "dotdash",
    "meanT_epi" = "solid", "meanT_hypo" = "dashed", "meanTdiff" = "dotdash"), 
    labels = c("T_epi" = "Epilimnion", "T_hypo" = "Hypolimnion", "Tdiff"  = "Difference",
    "meanT_epi" = "mean Epilimnion", "meanT_hypo" = "mean Hypolimnion", "meanTdiff" = "mean Difference")) +
    
    # max, min, middle meanTepi
    geom_hline(yintercept = max_Tepi, linetype = "solid", color = "darkgrey") +
    annotate("text", x = 8, y = max_Tepi, label = paste("Max mean Tepi:", round(max_Tepi, 2)),
            hjust = 0, vjust = 1.5, size = 3.5, color = "darkgrey") +
    geom_hline(yintercept = min_Tepi, linetype = "solid", color = "darkgrey") +
    annotate("text", x = 8, y = min_Tepi, label = paste("Min mean Tepi:", 
              round(min_Tepi, 2)), hjust = 0, vjust = 1.5, size = 3.5, color = "darkgrey") +
    geom_hline(yintercept = mid_Tepi, linetype = "solid", color = "darkgrey") +
    annotate("text", x = 8, y = mid_Tepi, label = paste("mid mean Tepi:", 
              round(mid_Tepi, 2)), hjust = 0, vjust = 1.5, size = 3.5, color = "darkgrey") +
 

    # max, min, middle meanThyp
    geom_hline(yintercept = max_Thyp, linetype = "dashed", color = "darkgrey") +
    annotate("text", x = 6, y = max_Thyp, label = paste("Max mean Thypo:", round(max_Thyp, 2)),
            hjust = 0, vjust = -0.5, size = 3.5, color = "darkgrey") +
    geom_hline(yintercept = min_Thyp, linetype = "dashed", color = "darkgrey") +
    annotate("text", x = 6, y = min_Thyp, label = paste("Min mean Thypo:", 
              round(min_Thyp, 2)), hjust = 0, vjust = -0.5, size = 3.5, color = "darkgrey") +
    geom_hline(yintercept = mid_Thyp, linetype = "dashed", color = "darkgrey") +
    annotate("text", x = 6, y = mid_Thyp, label = paste("mid mean Thypo:", 
              round(mid_Thyp, 2)), hjust = 0, vjust = -0.5, size = 3.5, color = "darkgrey") +
 

    # max, min, middle meanTdiff
    geom_hline(yintercept = max_Tdiff, linetype = "dotdash", color = "black") +
    annotate("text", x = 1, y = max_Tdiff, label = paste("Max meanTdiff:", round(max_Tdiff, 2)),
            hjust = 0, vjust = -0.5, size = 3.5, color = "black") +
    geom_hline(yintercept = min_Tdiff, linetype = "dotdash", color = "black") +
    annotate("text", x = 1, y = min_Tdiff, label = paste("Min meanTdiff:", 
              round(min_Tdiff, 2)), hjust = 0, vjust = -0.5, size = 3.5, color = "black") +
    geom_hline(yintercept = mid_Tdiff, linetype = "dotdash", color = "black") +
    annotate("text", x = 1, y = mid_Tdiff, label = paste("mid meanTdiff:", 
              round(mid_Tdiff, 2)), hjust = 0, vjust = -0.5, size = 3.5, color = "black") +
 
    theme_bw()  + theme(legend.position = "none") + theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
p_mean
ggsave(file.path("plots/Jahresverlauf", "mean_epi_temp_by_area_group_minmax.png"), plot = p_mean, width = 4.5, height = 4.5, units = "in", dpi = 300, scale = 1)
#get legend 
legend <- p_mean + theme(legend.position = "bottom", legend.box = "vertical") + guides(linetype = guide_legend(nrow = 2)) 
legend <- get_legend(legend)
ggsave(file.path("plots/Jahresverlauf", "mean_epi_temp_by_area_group_minmax_LEGEND.png"), plot = legend, width = 6, height = 1, units = "in", dpi = 300, scale = 1)

# ---- 2) Fraktion between Hypo and epi -----------------
min_val_me <- min(frac_mean$frac_hyp[frac_mean$Area_group == "medium"], na.rm = TRUE)
min_val_vs <- min(frac_mean$frac_hyp[frac_mean$Area_group == "very.small"], na.rm = TRUE)
min_val_vl <- min(frac_mean$frac_hyp[frac_mean$Area_group == "very.large"], na.rm = TRUE)

max_val_vl <- max(frac_mean$frac_hyp[frac_mean$Area_group == "very.large"], na.rm = TRUE)
max_val_vs <- max(frac_mean$frac_hyp[frac_mean$Area_group == "very.small"], na.rm = TRUE)
max_val_me <- max(frac_mean$frac_hyp[frac_mean$Area_group == "medium"], na.rm = TRUE)

# mean min, max & mid between min, max
max_frac <- max(overall_mean$frac_hyp, na.rm = TRUE)
min_frac <- min(overall_mean$frac_hyp, na.rm = TRUE)
mid_frac <- (max_frac+min_frac)/2

p_frac <- ggplot(frac_mean, aes(x = Month, y = frac_hyp, color = factor(Area_group), group = Area_group), linewidth = 1) +
    geom_line() + scale_x_continuous( breaks = 1:12, labels = month.name) + 
    labs(# title = "Fraction = T_hyp/T_epi (mean by Area Group)", 
    y = "fraction (Thypo/Tepi) in [%]", color = "LSAG", linetype = "b) Fraction") + 

    # mean frac_hyp 
    geom_line(data = overall_mean, aes(x = Month, y = frac_hyp, linetype  = "mean-frac_hyp"), color = "black", inherit.aes = FALSE) +
    scale_linetype_manual(values = c("mean-frac_hyp" = "solid"), labels = c("mean-frac_hyp" = "mean hyp. fraction")) +
    
    # max, min, middle mean frac
    geom_hline(yintercept = max_frac, linetype = "solid", color = "black") +
    annotate("text", x = 8, y = max_frac, label = paste("Max mean frac:", round(max_frac, 2), "%"),
            hjust = 0, vjust = -0.5, size = 3.5, color = "black") +
    geom_hline(yintercept = min_frac, linetype = "solid", color = "black") +
    annotate("text", x = 8, y = min_frac, label = paste("Min mean frac:", 
              round(min_frac, 2), "%"), hjust = 0, vjust = -0.5, size = 3.5, color = "black") +
    geom_hline(yintercept = mid_frac, linetype = "solid", color = "black") +
    annotate("text", x = 6, y = mid_frac, label = paste("mid mean frac:", 
              round(mid_frac, 2), "%"), hjust = 0, vjust = -0.5, size = 3.5, color = "black") +

    # min medium, very.small, very.large
    geom_hline(yintercept = min_val_me, linetype = "dotdash", color = "darkgrey") +
    annotate("text", x = 1, y = min_val_me, label = paste("Min medium:", round(min_val_me, 2), "%"),
            hjust = 0, vjust = -0.5, size = 3.5, color = "darkgrey") +
    geom_hline(yintercept = min_val_vs, linetype = "dotted", color = "darkgrey") +
    annotate("text", x = 8, y = min_val_vs, label = paste("Min very.small:", 
              round(min_val_vs, 2), "%"), hjust = 0, vjust = -0.5, size = 3.5, color = "darkgrey") +
    geom_hline(yintercept = 50, linetype = "solid", color = "darkgrey") +
    annotate("text", x = 1, y = 50, label = paste("frac_hyp:", 
              50, "%"), hjust = 0, vjust = -0.5, size = 3.5, color = "darkgrey") +
    geom_hline(yintercept = min_val_vl, linetype = "dashed", color = "darkgrey") +
    annotate("text", x = 1, y = min_val_vl, label = paste("Min very.large:", round(min_val_vl, 2), "%"),
            hjust = 0, vjust = -0.5, size = 3.5, color = "darkgrey") +

    # max very.large, very.small
    geom_hline(yintercept = max_val_vl, linetype = "dashed", color = "darkgrey") +
    annotate("text", x = 1, y = max_val_vl, label = paste("Max very.large:", round(max_val_vl, 2), "%"),
            hjust = 0, vjust = -0.5, size = 3.5, color = "darkgrey") +
    geom_hline(yintercept = max_val_vs, linetype = "dotted", color = "darkgrey") +
    annotate("text", x = 1, y = max_val_vs, label = paste("Max very.small:", 
              round(max_val_vs, 2), "%"), hjust = 0, vjust = -0.5, size = 3.5, color = "darkgrey") +
    geom_hline(yintercept = max_val_me, linetype = "dotdash", color = "darkgrey") +
    annotate("text", x = 1, y = max_val_me, label = paste("Max medium:", round(max_val_me, 2), "%"),
            hjust = 0, vjust = -0.5, size = 3.5, color = "darkgrey") +
   
    theme_bw()  + theme(legend.position = "none") + theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
p_frac
ggsave(file.path("plots/Jahresverlauf", "mean_fraction_hyp_epi.png"), plot = p_frac, width = 4.5, height = 4.5, units = "in", dpi = 300, scale = 1)

#get legend 
legend <- p_frac + theme(legend.position = "bottom", legend.box = "vertical") + guides(linetype = guide_legend(nrow = 1)) 
legend <- get_legend(legend)
ggsave(file.path("plots/Jahresverlauf", "mean_fraction_hyp_epi_LEGEND.png"), plot = legend, width = 6, height = 1, units = "in", dpi = 300, scale = 1)

# collect legends 
legend <- p_mean+ p_frac + theme_bw() +plot_layout(guides = "collect")& theme(legend.position = "bottom", legend.box = "vertical")& guides(linetype = guide_legend(nrow = 1)) 
legend <- get_legend(legend)
ggsave(file.path("plots/Jahresverlauf", "meanfrac_MEAN_LEGEND.png"), plot = legend, width = 9, height = 2, units = "in", dpi = 300, scale = 1)


# months frac < 50% 
months <- unique(frac_mean$Month[frac_mean$frac_hyp < 50])

# save min max fraction -----------------------------------------
ag <- unique(c(frac_mean$Area_group, "mean"))
minmax_df <- data.frame(min = rep(NA,length(ag)), max = rep(NA,length(ag)))
rownames(minmax_df) <- ag

for(x in unique(frac_mean$Area_group)) {
        min <- min(frac_mean$frac_hyp[frac_mean$Area_group == x], na.rm = TRUE)
        max <- max(frac_mean$frac_hyp[frac_mean$Area_group == x], na.rm = TRUE)

        minmax_df[x, "min"] <- min /100
        minmax_df[x, "max"] <- max /100
}
minmax_df["mean", "min"] <- min_frac /100
minmax_df["mean", "max"] <- max_frac /100

write.csv(minmax_df, "data/minmax_frac_area_group.csv")

# ---- 3) z_sprung (z0) Jahresverlauf - mean per month by Area group -------------------------
Z_mean <- dep10_smooth_all %>% group_by(Month, Area_group) %>%
  mutate(z_epi_b = mean(z_epi_b, na.rm = TRUE),
          z_epi_p =  mean(z_epi_p, na.rm = TRUE),
          maxZ = mean(maxZ, na.rm = TRUE),
          Temp_smooth = mean(Temp_smooth, na.rm = TRUE), 
          Temp = mean(Temp, na.rm = TRUE), 
          Depth = mean(Depth), Area_km2 = mean(Area_km2)) %>%
    mutate(frac_area_maxZ = (Area_km2/maxZ), 
                frac_area_z_epi_b = abs(z_epi_b/Area_km2), 
                frac_area_z_epi_p = abs(z_epi_p/Area_km2),
                frac_area_maxZ = abs(maxZ/Area_km2),

                frac_depth_maxZ = (maxZ/Depth)*100,
                frac_depth_z_epi_b = (z_epi_b/Depth)*100,
                frac_depth_z_epi_p = (z_epi_p/Depth)*100,

                v_maxZ = maxZ / (Area_km2 * Depth), 
                v_z_epi_b = z_epi_b / (Area_km2 * Depth), v_z_epi_b = z_epi_p / (Area_km2 * Depth))


Z_overall_mean <- dep10_smooth_all %>% group_by(Month) %>%
  mutate(z_epi_b = mean(z_epi_b, na.rm = TRUE),
          z_epi_p =  mean(z_epi_p, na.rm = TRUE),
          maxZ = mean(maxZ, na.rm = TRUE),
          Temp_smooth = mean(Temp_smooth, na.rm = TRUE), 
          Temp = mean(Temp, na.rm = TRUE),
          Depth = mean(Depth), Area_km2 = mean(Area_km2)) %>%
   mutate(frac_area_maxZ = (Area_km2/maxZ), 
                frac_area_z_epi_b = abs(z_epi_b/Area_km2), 
                frac_area_z_epi_p = abs(z_epi_p/Area_km2),
                frac_area_maxZ = abs(maxZ/Area_km2),

                frac_depth_maxZ = (maxZ/Depth)*100,
                frac_depth_z_epi_b = (z_epi_b/Depth)*100,
                frac_depth_z_epi_p = (z_epi_p/Depth)*100,

                v_maxZ = maxZ / (Area_km2 * Depth), 
                v_z_epi_b = z_epi_b / (Area_km2 * Depth), v_z_epi_p = z_epi_p / (Area_km2 * Depth))

p <- ggplot(Z_mean, aes(x = Month, y = maxZ, color = factor(Area_group), group = Area_group)) +
    geom_line() + scale_x_continuous( breaks = 1:12, labels = month.name) + 
    labs(title = "mean z0 by Area group, derived from smoothed data & Patalas Formula", y = "Depths [m]") + 

    # mean maxz 
    geom_line(data = Z_overall_mean, aes(x = Month, y = maxZ, linetype  = "mean_z0"), color = "black", inherit.aes = FALSE) +
    
    # mean z_epi patalas 
    geom_line(data = Z_overall_mean, aes(x = Month, y = z_epi_b, linetype  = "z_patalas_b"), color = "black", inherit.aes = FALSE) +
    geom_line(data = Z_overall_mean, aes(x = Month, y = z_epi_p, linetype  = "z_patalas_p"), color = "black", inherit.aes = FALSE) +
  
    scale_linetype_manual(values = c("mean_z0" = "solid", "z_patalas_b" = "dashed", "z_patalas_p" = "dotdash")) + 
    geom_hline(yintercept = 0, linetype = "solid", color = "darkgrey") +
    annotate("text", x = 6, y = 0, label = paste("Depth:", 0, "m"),
            hjust = 0, vjust = -0.5, size = 3.5, color = "darkgrey") +
    theme_bw()
    
p
ggsave(file.path("plots/Jahresverlauf", "mean_z0_smoothed_patalas.png"), plot = p, width = 10, height = 8, units = "in", dpi = 300)

# fraction Area_km 
p <- ggplot(Z_mean, aes(x = Month, y = frac_area_maxZ, color = factor(Area_group), group = Area_group)) +
    geom_line() + scale_x_continuous( breaks = 1:12, labels = month.name) + 
    labs(title = "Fraction mean z0 by Area group, derived from smoothed data & Patalas Formula", 
    y = "Fraktion Area / z0 [km2/m]") + 

    # mean frac_hyp 
    geom_line(data = Z_overall_mean, aes(x = Month, y = frac_area_maxZ, linetype  = "frac_mean_z0"), color = "black", inherit.aes = FALSE) +
    
    # mean frac_hyp 
    geom_line(data = Z_overall_mean, aes(x = Month, y = frac_area_z_epi_b, linetype  = "frac_z_patalas_b"), color = "black", inherit.aes = FALSE) +
    geom_line(data = Z_overall_mean, aes(x = Month, y = frac_area_z_epi_p, linetype  = "frac_z_patalas_p"), color = "black", inherit.aes = FALSE) +
    scale_linetype_manual(values = c("frac_mean_z0" = "solid", "frac_z_patalas_b" = "dashed", "frac_z_patalas_p" = "dotdash")) + 
    #geom_hline(yintercept = 100, linetype = "solid", color = "darkgrey") +
#     annotate("text", x = 6, y = 100, label = paste(100, "%"),
#             hjust = 0, vjust = -0.5, size = 3.5, color = "darkgrey") +
    
    theme_bw()
p
ggsave(file.path("plots/Jahresverlauf", "frac_area_mean_z0_smoothed_patalas.png"), plot = p, width = 10, height = 8, units = "in", dpi = 300)

# fraction z0/depth 
p <- ggplot(Z_mean, aes(x = Month, y = frac_depth_maxZ, color = factor(Area_group), group = Area_group)) +
    geom_line() + scale_x_continuous( breaks = 1:12, labels = month.name) + 
    labs(title = "Fraction mean z0/ mean depth by Area group, derived from smoothed data & Patalas Formula", y = "Fraktion z0/Depth [%]") + 

    # mean frac_hyp 
    geom_line(data = Z_overall_mean, aes(x = Month, y = frac_depth_maxZ, linetype  = "frac_mean_z0"), color = "black", inherit.aes = FALSE) +
    
    # mean frac_hyp 
    geom_line(data = Z_overall_mean, aes(x = Month, y = frac_depth_z_epi_b, linetype  = "frac_z_patalas_b"), color = "black", inherit.aes = FALSE) +
    geom_line(data = Z_overall_mean, aes(x = Month, y = frac_depth_z_epi_p, linetype  = "frac_z_patalas_p"), color = "black", inherit.aes = FALSE) +
    scale_linetype_manual(values = c("frac_mean_z0" = "solid", "frac_z_patalas_b" = "dashed", "frac_z_patalas_p" = "dotdash")) + 
    geom_hline(yintercept = 100, linetype = "solid", color = "darkgrey") +
    annotate("text", x = 6, y = 100, label = paste(100, "%"),
            hjust = 0, vjust = -0.5, size = 3.5, color = "darkgrey") +
    
    theme_bw()
p
ggsave(file.path("plots/Jahresverlauf", "frac_depth_mean_z0_smoothed_patalas.png"), plot = p, width = 10, height = 8, units = "in", dpi = 300)

# save Fraktion depth: 
frac_depth_z0 <- Z_mean %>% 
        group_by(Month) %>% summarise(F_maxZ_mean = round(mean(frac_depth_maxZ)/100,2)) %>%
        mutate( F_zepi_b_mean = c(rep(1,3), rep(round(mean(Z_overall_mean$frac_depth_z_epi_b)/100,2), 8), 1), # annahme Januar - märz keine schichtung
                F_zepi_p_mean = c(rep(1,3), rep(round(mean(Z_overall_mean$frac_depth_z_epi_p)/100,2), 8), 1))
View(frac_depth_z0)

frac_depth_z0_area <- Z_mean %>% group_by(Month, Area_group) %>% summarise(F_maxZ = round(mean(frac_depth_maxZ)/100,2), .groups = "drop")
frac_depth_z0_area <- data.frame(
                F_maxZ_very.small = frac_depth_z0_area$F_maxZ[frac_depth_z0_area$Area_group == "very.small"],
                F_maxZ_small = frac_depth_z0_area$F_maxZ[frac_depth_z0_area$Area_group == "small"],
                F_maxZ_medium = frac_depth_z0_area$F_maxZ[frac_depth_z0_area$Area_group == "medium"],
                F_maxZ_large = frac_depth_z0_area$F_maxZ[frac_depth_z0_area$Area_group == "large"],
                F_maxZ_very.large = frac_depth_z0_area$F_maxZ[frac_depth_z0_area$Area_group == "very.large"])
View(frac_depth_z0_area)

frac_depth_z0_df <- cbind(frac_depth_z0, frac_depth_z0_area)
frac_depth_z0_df <- frac_depth_z0_df %>% select(-Month) %>%
  mutate(across(everything(), ~ ifelse(. >= 0.9, 1, .)))
frac_depth_z0_df$Month <- 1:12
View(frac_depth_z0_df)

write.csv(frac_depth_z0_df, "data/norm_z0_frac_depth_group.csv")

# long format: 
plot_df <- frac_depth_z0_df %>%
  pivot_longer(cols = -Month, names_to = "Depth_type", values_to = "Fraction")
mean_df <- plot_df %>% filter(Depth_type %in% c("F_maxZ_mean", "F_zepi_p_mean", "F_zepi_b_mean"))
mean_df$Source <- "Mean"

area_df <- plot_df %>% filter(!Depth_type %in% c("F_maxZ_mean", "F_zepi_p_mean", "F_zepi_b_mean"))
View(plot_df)
write.csv(plot_df, "data/norm_z0_frac_depth_group_long.csv")

# save min / month
summary_list <- list()
for(i in unique(plot_df$Depth_type)){
        dat <- plot_df[plot_df$Depth_type == i,]
        min <- min(dat$Fraction)
        min_month <- dat$Month[dat$Fraction %in% min]
        no_month <- unique(dat$Month[dat$Fraction ==1])
# Store in list
  summary_list[[i]] <- list(
    Min_Value = min,
    Min_Month = min_month,
    no_z0_month = no_month
  )

}
## Convert to data frame
summary_df <- tibble(
  Depth_type = names(summary_list),
  Min_Value = sapply(summary_list, function(x) x$Min_Value),
  Min_Month = sapply(summary_list, function(x) paste(x$Min_Month, collapse = ",")),
  no_z0_month = sapply(summary_list, function(x) paste(x$no_z0_month, collapse = ","))
)

write.csv(summary_df, "data/norm_z0_frac_depth_group_summary.csv")


# normalize fraktion z0/depth
p <- ggplot(area_df, aes(x = Month, y = Fraction*100, color = Depth_type, group = Depth_type)) +
  geom_line(linetype = "dashed") + scale_x_continuous( breaks = 1:12, labels = month.name) +
  geom_point(size = 1.5) +
  geom_line(data = mean_df, aes(x = Month, y = Fraction*100, 
        color = Depth_type, group = Depth_type, linetype = Source),)+
        scale_linetype_manual(values = c("Mean" = "solid")) + 
  labs(title = "Normalized z0 Fraction z0/Depth Ratios Over Time",
       x = "Month",
       y = "Fraction z0/Depth [%] (capped at 100)",
       color = "Depth Metric", linetype = "Curve Type") +
  theme_bw()
p
ggsave(file.path("plots/Jahresverlauf", "norm_frac_depth_mean_z0_smoothed_patalas.png"), plot = p, width = 10, height = 8, units = "in", dpi = 300)


# Faktor z0/ (Area_km2 * Depth) 
p <- ggplot(Z_mean, aes(x = Month, y = v_maxZ, color = factor(Area_group), group = Area_group)) +
    geom_line() + scale_x_continuous( breaks = 1:12, labels = month.name) + 
    labs(title = "z0/ (Area_km2 * Depth) grouped by Area group, derived from smoothed data & Patalas Formula", y = "z0/ (Area_km2 * Depth)") + 

    # mean frac_hyp 
    geom_line(data = Z_overall_mean, aes(x = Month, y = v_maxZ, linetype  = "frac_mean_z0"), color = "black", inherit.aes = FALSE) +
    
    # mean frac_hyp 
    geom_line(data = Z_overall_mean, aes(x = Month, y = v_z_epi_b, linetype  = "frac_z_patalas_b"), color = "black", inherit.aes = FALSE) +
    geom_line(data = Z_overall_mean, aes(x = Month, y = v_z_epi_p, linetype  = "frac_z_patalas_p"), color = "black", inherit.aes = FALSE) +
    scale_linetype_manual(values = c("frac_mean_z0" = "solid", "frac_z_patalas_b" = "dashed", "frac_z_patalas_p" = "dotdash")) + 
#     geom_hline(yintercept = 100, linetype = "solid", color = "darkgrey") +
#     annotate("text", x = 6, y = 100, label = paste(100, "%"),
#             hjust = 0, vjust = -0.5, size = 3.5, color = "darkgrey") +
    
    theme_bw()
p
ggsave(file.path("plots/Jahresverlauf", "area_depth_mean_z0_smoothed_patalas.png"), plot = p, width = 10, height = 8, units = "in", dpi = 300)

# save: Faktor z0 
# Z_epi 
Fak_z0_df <- Z_mean %>% 
        group_by(Month) %>% summarise(F_maxZ = round(mean(v_maxZ),2)) %>%
        mutate( F_zepi_b = c(rep(0,3), rep(round(mean(Z_overall_mean$z_epi_b),2), 8), 0), # annahme Januar - märz keine schichtung
                F_zepi_p = c(rep(0,3), rep(round(mean(Z_overall_mean$z_epi_p),2), 8), 0))
View(Fak_z0)

Fak_z0_area <- Z_mean %>% group_by(Month, Area_group) %>% summarise(F_maxZ = round(mean(v_maxZ),2), .groups = "drop")
Fak_z0_area_df <- data.frame(
                F_maxZ_very.small = Fak_z0_area$F_maxZ[Fak_z0_area$Area_group == "very.small"],
                F_maxZ_small = Fak_z0_area$F_maxZ[Fak_z0_area$Area_group == "small"],
                F_maxZ_medium = Fak_z0_area$F_maxZ[Fak_z0_area$Area_group == "medium"],
                F_maxZ_large = Fak_z0_area$F_maxZ[Fak_z0_area$Area_group == "large"],
                F_maxZ_very.large = Fak_z0_area$F_maxZ[Fak_z0_area$Area_group == "very.large"])
View(Fak_z0_area_df)

F_z0 <- cbind(Fak_z0, Fak_z0_area_df)
write.csv(F_z0, "data/factor_Z0_area_group.csv")

# ---- Zusammenhang Z0 und hypofraction ------------------------------------
Z_mean
Z_overall_mean

frac_mean
overall_mean


p <- ggplot() + scale_x_continuous( breaks = 1:12, labels = month.name) + 
        geom_line(data = mean_df, aes(x = Month, y = Fraction*100, color = Depth_type, group = Depth_type, linetype = Source))+
        
        geom_line(data = area_df, aes(x = Month, y = Fraction*100, group = Depth_type, color = Depth_type), , linetype = "dotdash") +
        geom_line(data = frac_mean, aes(x = Month, y = frac_hyp, group = Area_group, color = Area_group), linetype = "dashed") +
        geom_line(data = overall_mean, aes(x = Month, y = frac_hyp, linetype = "Mean"))+

        scale_linetype_manual(values = c("Mean" = "solid")) + 

        labs(title = "Fraction of z0/depth and Hypo/Epi grouped by Area group \n z0 derived from smoothed data & Patalas Formula", 
        y = "Fraktion [%]") + theme_bw()

p
ggsave(file.path("plots/Jahresverlauf", "zusammenhang_frac_hypoepi_z0.png"), plot = p, width = 10, height = 8, units = "in", dpi = 300)

