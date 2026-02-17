# ---------------------------------------------------
# Analysis of epilimnion thickness data from Patal et al. (2023)
# ---------------------------------------------------
# data --> test_epilimnion_thickness_Patalas

# Ich wollte aber nochmal genauer untersuchen, wie präzise damit die berechnete Sprungschicht ist.
# Außerdem wollte ich mir nochmal anschauen:
# -	Wie sehr die Fläche & Tiefe korreliert ist 
# -	Welche Monte einen signifikanten Temp-Unterschied zwischen Epi- und Hypolimnion Temp haben (ANOVA-Test)
# -	Ob es signifikante Unterschiede der Thermalen-Gradienten zwischen den Flächen/Tiefe gibt (ANOVA-Test)
# -	Und ob man die Hypolimnion Temp als Fraktion der Epi-Temp darstellen kann, oder dann über die Info der berechneten Sprungschicht Tiefe abschätzten kann – damit man nicht noch eine zusätzliche Input Variable hat

# 1) data Preparation - t_hyp, T_epi, thermal gradient, thermal difference
# 2) mean Temperatures for epi- and hypolimnion per month by Area group
# 3) Area & Depth Correlation 
# 4) Area & thermal gradient Correlation
# 5) calculate max slope of Tempprofiles by Area group

# ---------------------------------------------------
# Author: Neele 
# ---------------------------------------------------
# ---- packages ------------------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)
library(stats)
source("help-func-analysis.r")

# ---- load data ------------------------------------------------
# Load the data 
dep_10_Tjoined <- read.csv("data/dep10_epi-hypo_zPatalas.csv")  
View(dep_10_Tjoined)
unique(dep_10_Tjoined$Month)
dep_10_Tjoined$Depth[dep_10_Tjoined$Name == "AbtsdorferSee"]
dep_10_Tjoined$Depth[dep_10_Tjoined$Name == "Chiemsee"]
dep_10_Tjoined$Depth[dep_10_Tjoined$Name == "Eibsee"]

# Temp profiles & calculatet epilimnion thickness (Patalas et al.)
dep10_patalas <- read.csv("data/dep10_patalas.csv")[, -1]  # drop Row NR X
dep10_patalas_sum <- dep10_patalas %>%
  group_by(ID) %>%
  summarise(
    Depth = min(Depth, na.rm = TRUE),
    z_epi_b = mean(z_epi_b, na.rm = TRUE),
    z_epi_p = mean(z_epi_p, na.rm = TRUE),
    .groups = "drop"
  )

View(dep10_patalas)
# data for lakes with depth < -10 m
all_lake_dep10 <- read.csv("data/all_lake_dep10_short.csv")[, -1]

# lake area - (Openstreetmap, QGIS)
lake_area <- read.csv("data/lake_coords_area_qgis.csv")

# ---- 1) mean Temperatures for epi- and hypolimnion per month by Area group -----------------
area_dep <- dep_10_Tjoined %>% 
    select(ID, Month, Month_w, Area_km2, Area_group, Depth, 
    T_epi, T_hypo, thermal_gradient, thermal_dif) %>%
    distinct()
View(area_dep)

dep_mean_month_area <- area_dep %>% group_by(Month_w, Area_group) %>%
    mutate(Month_w = factor(Month_w, levels = month.abb)) %>%
    summarise(
        T_epi = mean(T_epi, na.rm = TRUE),
        T_hypo = mean(T_hypo, na.rm = TRUE),
        thermal_gradient = mean(thermal_gradient, na.rm = TRUE),
        thermal_dif = mean(thermal_dif, na.rm = TRUE),
        .groups = "drop"
    )

p_mean <- dep_mean_month_area %>% pivot_longer(cols = c(T_epi, T_hypo, thermal_dif), names_to = "Layer", values_to = "Temperature") %>%
    ggplot(aes(x = Month_w, y = Temperature, color = Area_group, linetype = Layer,
    group = interaction(Area_group, Layer))) +
    geom_line() +
    labs(title = "Mean Epi-& Hypo-Temperature by Area Group", x = "Month", y = "Temperature (°C)") +
    scale_linetype_manual(values = c("T_epi" = "solid", "T_hypo" = "dashed", "thermal_dif" = "dotted", "thermal_gradient" = "dashed")) +
    geom_hline(aes(yintercept = -5), color = "black") +
    theme_bw()
p_mean
ggsave(file.path("plots", "mean_epi_temp_by_area_group.png"), plot = p_mean, width = 10, height = 8, units = "in", dpi = 300)

# p_tdiff<- dep_mean_month_area %>% pivot_longer(cols = thermal_dif, values_to = "T_diff") %>%
#     ggplot(aes(x = Month_w, y = T_diff, color = Area_group,
#     group = Area_group)) +
#     geom_line() +
#     labs(title = "Mean TempDiff between Epi-& Hypo-Temperature by Area Group", x = "Month", y = "Temperature Diff (°C)") +
#     theme_bw()
# p_tdiff
# ggsave(file.path("plots", "mean_temp_diff_by_area_group.png"), plot = p_tdiff, width = 10, height = 6, units = "in", dpi = 300)

## ---- 1.1)  < -5° diff of mean epi- and hypolimnion temp in wich month -----------------
# Filter for months with a temperature difference of less than -5°C
temp_diff_filtered <- dep_mean_month_area %>%
  filter(thermal_dif > 10)  
unique(temp_diff_filtered$Month_w)

# ----- 2) Area & Depth Correlation ------------------------------------
p_ad <- ggplot(area_dep, aes(x = Area_km2, y = Depth, color = Area_group)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Area vs Depth of Lakes", x = "Area (km²)", y = "Depth (m)") +
  theme_minimal()
p_ad

# ---- 3) Area & thermal gradient Correlation -----------------------------
area_dep_th <- area_dep %>% group_by(ID, Area_group, Month_w) %>%
  summarise(thermal_gradient = mean(thermal_gradient, na.rm = TRUE), .groups = "drop")
View(area_dep_th)

p_athg <- ggplot(area_dep_th, aes(x = Month_w, y = thermal_gradient, color = Month_w)) +
  geom_boxplot() +
  facet_wrap(~Area_group, scales = "free_y") +
  labs(title = "mean Thermal Gradient vs Month by Area Group ", x = "Area (km²)", y = "thermal_gradient") +
  scale_fill_brewer(palette = "Set1")+
  theme_bw()
p_athg

ggsave(file.path("plots", "boxplot_area_thermal_gradient.png"), plot = p_athg, width = 20, height = 10, units = "in", dpi = 300)

# mean thermal gradient by area group and month
p_athg_mean <- ggplot(dep_mean_month_area, aes(x = Month_w, y = thermal_gradient, color = Area_group, group = Area_group)) +
  geom_line() +
  labs(title = "mean Thermal Gradient vs Month by Area Group ", x = "Area (km²)", y = "thermal_gradient") +
  theme_bw()
p_athg_mean
ggsave(file.path("plots", "mean_thermal_gradient_by_area_group.png"), plot = p_athg_mean, width = 10, height = 6, units = "in", dpi = 300)

# ---- 4) calculate max slope of Tempprofiles ------------------------
# slope between points of Tempprofiles 
dep10_patalas_months <- dep10_patalas %>%
    group_by(ID, Name, Depth, Month) %>%
    summarise(
      Temp = mean(Temp, na.rm = TRUE),
      z_epi_b = mean(z_epi_b, na.rm = TRUE),
      z_epi_p = mean(z_epi_p, na.rm = TRUE),
      Area_km2 = mean(Area_km2, na.rm = TRUE), 
      .groups = "drop") %>%
    mutate(Area_group = cut(
      Area_km2,
      breaks = quantile(Area_km2, probs = seq(0, 1, length.out = 6), na.rm = TRUE),
      labels = c("very.small", "small", "medium", "large", "very.large"),
      include.lowest = TRUE
    ))
View(dep10_patalas_months)

# function finde z with min slope: slope = dT/dz
unique(dep10_patalas_months$ID)
span = 0.5 # 0.5 works best!!
for(i in unique(dep10_patalas_months$ID)) {
  # select one lake for example
  ex <- dep10_patalas_months %>% filter(ID == i) # , Month %in% 4:10
  ex_smooth <- ex %>% group_by(Month) %>%
            mutate(
              Temp_smooth = func_smooth(Temp, Depth, span = span),
              maxZ = func_max_slope(Temp, Depth, span = span)$maxZ,
              maxT = func_max_slope(Temp, Depth, span = span)$maxT,
              max_slope = func_max_slope(Temp, Depth, span = 1)$max_slope, 
              z_epi_b = mean(z_epi_b, na.rm = TRUE),
              z_epi_p = mean(z_epi_p, na.rm = TRUE),
              wpT = func_wendepunkt(Temp, Depth, span = span)$inflectionT,
              wpZ = func_wendepunkt(Temp, Depth, span = span)$inflectionZ
            ) 

  # plot the temperature profiles with the calculated epilimnion thickness
  # plot smooth and estimated max slope 
  p_ex <- ggplot(ex, aes(x = Temp, y = Depth, color = factor(Month), group = Month)) +
    geom_line() +
    geom_point(data = ex_smooth, aes(x = Temp_smooth, y = Depth), 
              color = "red") +

    # # WEndepunkt
    # geom_vline(data = ex_smooth, aes(xintercept = wpT), 
    #           linetype = "dashed", color = "magenta") +
    # geom_hline(data = ex_smooth, aes(yintercept = wpZ), 
    #           linetype = "dashed", color = "cyan") +

    # max slope
    geom_vline(data = ex_smooth, aes(xintercept = maxT), 
              linetype = "dashed", color = "red") +
    geom_hline(data = ex_smooth, aes(yintercept = maxZ), 
              linetype = "dashed", color = "blue") +
    
    # Patalas et al. 2023
    geom_hline(data = ex_smooth, aes(yintercept = z_epi_b), 
              linetype = "dotted", color = "black") +
    geom_hline(data = ex_smooth, aes(yintercept = z_epi_p), 
              linetype = "dotted", color = "darkgreen") +
    facet_wrap(~Month, scales = "free_y") +
    labs(title = paste("Temperature Profiles for Lake", unique(ex$Name)),
         x = "Temperature (°C)", y = "Depth (m)") +
    theme_bw()
  ggsave(file.path("plots/temp-smooth-all", paste0(unique(ex$Name),"_temp_profile_", i, "_", span, ".png")), plot = p_ex, width = 10, height = 8, units = "in", dpi = 300)
}

# save the data with calculated max slope and Wendepunkt --------------------
span = 0.5 # 0.5 works best!!
dep10_smooth <- dep10_patalas_months %>% filter(!Name %in% c("Rottachsee", "KleinerBrombachsee", "GrosserBrombachsee")) %>%
              #filter(Month %in% 4:10) %>%
              group_by(ID, Name, Month) %>% 
          
          # calculate the smoothed temperature profile and the max slope
          mutate(
            Temp_smooth = func_smooth(Temp, Depth, span = span),
            maxZ = func_max_slope(Temp, Depth, span = span)$maxZ,
            maxT = func_max_slope(Temp, Depth, span = span)$maxT,
            max_slope = func_max_slope(Temp, Depth, span = 1)$max_slope, 

            z_epi_b = mean(z_epi_b, na.rm = TRUE),
            z_epi_p = mean(z_epi_p, na.rm = TRUE),

            dif_z_epi_b = abs(mean(z_epi_b, na.rm = TRUE) - func_max_slope(Temp, Depth, span = span)$maxZ),
            dif_z_epi_p = abs(mean(z_epi_p, na.rm = TRUE) - func_max_slope(Temp, Depth, span = span)$maxZ),

            wpT = func_wendepunkt(Temp, Depth, span = span)$inflectionT,
            wpZ = func_wendepunkt(Temp, Depth, span = span)$inflectionZ
          ) # ignor warning 
write.csv(dep10_smooth, file = "data/dep10_patalas_smooth_all.csv", row.names = FALSE)

View(dep10_smooth)
max(dep10_smooth$dif_z_epi_b)
max(dep10_smooth$dif_z_epi_p)

min(dep10_smooth$dif_z_epi_b)
min(dep10_smooth$dif_z_epi_p)

mean(dep10_smooth$dif_z_epi_b)
mean(dep10_smooth$dif_z_epi_p)

