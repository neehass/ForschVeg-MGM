# comparion between tempdata 
# just env data from model_top is needed because env deep == env top 

# packages & functions
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(vegan) # wie in numerische Ökologie VL2
library(corrplot)
library(patchwork)

# home
dir <- "C:/Users/maiim/Documents/25-25SS/Forschungsprojekt_Vegetationskunde/scripte-data-MGM/master-MGM"
lewSpec_dir <- "C:/Users/maiim/Documents/25-25SS/Forschungsprojekt_Vegetationskunde/scripte-data-MGM/LewerentzEtAl2023_ModelledMacrophyteSpeciesRichness-1.0"

# Workstation
dir <- "C:/Users/student/Documents/Neele-ForschVeg-2025/ForschVeg-MGM/master-MGM"
lewSpec_dir <- "C:/Users/student/Documents/Neele-ForschVeg-2025/ForschVeg-MGM/LewerentzEtAl2023_ModelledMacrophyteSpeciesRichness-1.0"

getwd()
setwd(dir)

# packages & functions

source("./experiment/help-func.r")
source("./output-analysis/func_data_prep.R")

# dir setup ---------------------------------------------------------------------------------
# Folder output of MGM experiment and Analysis results folder
output_top_TP <- "output-analysis/dep10_300spec_base_Tprofil_20years"
out_TP <- "output/dep10_300spec_base_Tprofil_20years"

output_top_TS <- "output-analysis/dep10_300spec_base_Tsteady_20years"

out_ana_path_top <- "output-analysis/dep10_300spec_base_Tprofil_20years"
out_ana_path_deep <- "output-analysis/dep10_300spec_base_Tprofile_20years_deep"

save_comparison <- "output-analysis/combo_comparison_final_20years/Env_compare"

dir.create(save_comparison)

# input files
lake_path <- "input/lakes"

# load data -----------------------
# environment
sort_envTP <- readRDS(file.path(output_top_TP, "sortENV.rds")) # env_sort # sorted environmental data (grouped by llakeClass, lakeGroup_Area, day)
sort_envTS <- readRDS(file.path(output_top_TS, "sortENV.rds")) # env_sort # sorted environmental data (grouped by llakeClass, lakeGroup_Area, day)

gen.conf <- readLines(file.path(out_TP,"general.config.txt"))
k <- as.numeric(strsplit(gen.conf[8], " ")[[1]][2])

# results 
sort_res_top <- readRDS(file.path(out_ana_path_top, "sortRES.rds")) # sort_res # biomass > 0 sorted macrophyte data (grouped by lakeClass, speciesGroup, lakeGroup_Area, depth, day)
sort_res_deep <- readRDS(file.path(out_ana_path_deep, "sortRES.rds"))

# --- combine results ----------------------------------------
# sort results biomass > 0
unique(sort_res_top$depth)
unique(sort_res_deep$depth)

sortRES_combo <- rbind(sort_res_top, sort_res_deep)
unique(sortRES_combo$depth)
sortRES_combo <- sortRES_combo %>%
  mutate(
    lakeClass = factor(
      lakeClass,
      levels = c("clear", "medium", "turb"),
      labels = c("clear lakes", "intermediate lakes", "turbid lakes")
    )
  )

# for env 
depth_bio <- c(0, sort(unique(sortRES_combo$depth), decreasing = TRUE))

maxDay <- max(unique(sortRES_combo$day))
minDay <- min(unique(sortRES_combo$day))

# rename lakes ----------------------
sort_envTP <- sort_envTP %>%
  mutate(
    lakeClass = factor(
      lakeClass,
      levels = c("clear", "medium", "turb"),
      labels = c("clear lakes", "intermediate lakes", "turbid lakes")
    )
  )
sort_envTS <- sort_envTS %>%
  mutate(
    lakeClass = factor(
      lakeClass,
      levels = c("clear", "medium", "turb"),
      labels = c("clear lakes", "intermediate lakes", "turbid lakes")
    )
  )

# Tprofiel --------------------------------
sort_env_Tprof_DAY <- sort_envTP %>% # sort_env[sort_env$day %in% unique(sort_res$day), ] 
  group_by(lakeClass, AreaGroup, day) %>%
  # mutate(day_bin = floor((day - 1) / 30) * 30 + 1) %>%  # days 1–7 → 1, 8–14 → 8, etc.
  mutate(month_bin = floor((day - 1) / 31) + 1) %>%  # month 1, 2, 3...
  group_by(lakeClass, AreaGroup, day, month_bin) %>%
  summarise(
    tempEpi_av = mean(tempEpi_mean, na.rm = TRUE),
    tempHypo_av = mean(tempHypo_mean, na.rm = TRUE),
    metaDepth_av = mean(metaDepth_mean, na.rm = TRUE),
    lakeDepth_av = round(mean(lakeDepth_mean, na.rm = TRUE)), 
    T_prof = list(T_profile(z = sort(seq(lakeDepth_av, 0, 0.5), decreasing = TRUE), 
                            T_epi = tempEpi_av, T_hypo = tempHypo_av,
                            z0 = metaDepth_av, k = k))) %>% 
  ungroup()
# View(sort_env_Tprof_DAY)
length(sort_env_Tprof_DAY$month_bin %>% unique())

# View(sort_env_Tprof_DAY)
sort_env_Tprof_DAY_long <- sort_env_Tprof_DAY %>%
  group_by(lakeClass, AreaGroup, day, month_bin) %>%
  mutate(depth = list(sort(seq(lakeDepth_av, 0, 0.5), decreasing = TRUE))) %>%  # depth for each T_prof
  unnest(c(T_prof, depth)) 
# View(sort_env_Tprof_DAY_long)

mean_profiles <- sort_env_Tprof_DAY_long %>%
  filter(day >= minDay) %>%
  group_by(lakeClass, AreaGroup,depth) %>%     # depth must stay in grouping
  summarise(
    T_prof_mean = mean(T_prof, na.rm = TRUE),   # mean temperature at each depth
    .groups = "drop"
  )
mean <- mean_profiles %>% group_by(lakeClass, depth) %>%
  summarise(
    mean = mean(T_prof_mean, na.rm = TRUE),   # mean temperature at each depth
    .groups = "drop"
  )
mean$AreaGroup <- "mean"

p_Tprofile <- ggplot(mean_profiles, aes(x = depth, y = T_prof_mean, color = AreaGroup, 
                                        group = AreaGroup)) +
  geom_line() +
  geom_line(data = mean, aes(x = depth, y = mean), color = "black", linetype = "dashed") +
  # geom_point(data = test, aes(x = -0.5, y = T_profile(z = -0.5, T_epi = test$tempEpi_av, T_hypo = test$tempHypo_av, z0 = test$metaDepth_av, k = 5)), color = "red") +
  scale_x_reverse(breaks = depth_bio, limits = c(0,-9),) + # 
  facet_wrap(~ lakeClass, nrow = 1) +
  theme_bw() +
  labs(title = paste("mean Temperature Profiles, days", minDay, "to", maxDay),
       y =  "mean Temperature [°C]",
       x = "Depth [m]",
       color = "Lake \nArea-Group") + 
  scale_color_brewer(palette = "Set2") +
  theme(legend.position = "none") + theme(legend.position = "bottom") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

p_Tprofile 
ggsave(file.path(save_comparison, paste0("TP_temp_depth.png")), p_Tprofile, height = 10, width = 12)

# Tsteady --------------------------------

env_TS_DAY <- sort_envTS %>% # sort_env[sort_env$day %in% unique(sort_res$day), ] 
  group_by(lakeClass, AreaGroup, day) %>%
  # mutate(day_bin = floor((day - 1) / 30) * 30 + 1) %>%  # days 1–7 → 1, 8–14 → 8, etc.
  mutate(month_bin = floor((day - 1) / 31) + 1) %>%  # month 1, 2, 3...
  group_by(lakeClass, AreaGroup, day, month_bin) %>%
  summarise(
    tempEpi_av = mean(tempEpi_mean, na.rm = TRUE),
    tempHypo_av = mean(tempHypo_mean, na.rm = TRUE),
    metaDepth_av = mean(metaDepth_mean, na.rm = TRUE),
    lakeDepth_av = round(mean(lakeDepth_mean, na.rm = TRUE)), 
    T_prof = list(rep(tempEpi_av, abs(lakeDepth_av*2)+1))) %>% # Tsteady >> Tepi at all depths
  ungroup()

# View(sort_env_Tprof_DAY)
length(env_TS_DAY$month_bin %>% unique())

# View(sort_env_Tprof_DAY)
sort_env_TS_DAY_long <- env_TS_DAY %>%
  group_by(lakeClass, AreaGroup, day, month_bin) %>%
  mutate(depth = list(sort(seq(lakeDepth_av, 0, 0.5), decreasing = TRUE))) %>%  # depth for each T_prof
  unnest(c(T_prof, depth)) 
# View(sort_env_Tprof_DAY_long)

mean_profiles_TS <- sort_env_TS_DAY_long %>%
  filter(day >= minDay) %>%
  group_by(lakeClass, AreaGroup,depth) %>%     # depth must stay in grouping
  summarise(
    T_prof_mean = mean(T_prof, na.rm = TRUE),   # mean temperature at each depth
    .groups = "drop"
  )
mean_TS <- mean_profiles_TS %>% group_by(lakeClass, depth) %>%
  summarise(
    mean = mean(T_prof_mean, na.rm = TRUE),   # mean temperature at each depth
    .groups = "drop"
  )
mean_TS$AreaGroup <- "mean"

p_TS <- ggplot(mean_profiles_TS, aes(x = depth, y = T_prof_mean, color = AreaGroup, 
                                        group = AreaGroup)) +
  geom_line() +
  geom_line(data = mean_TS, aes(x = depth, y = mean), color = "black", linetype = "dashed") +
  # geom_point(data = test, aes(x = -0.5, y = T_profile(z = -0.5, T_epi = test$tempEpi_av, T_hypo = test$tempHypo_av, z0 = test$metaDepth_av, k = 5)), color = "red") +
  scale_x_reverse(breaks = depth_bio, limits = c(0,-9),) + # 
  facet_wrap(~ lakeClass, nrow = 1) +
  theme_bw() +
  labs(title = paste("mean Temperature Profiles, days", minDay, "to", maxDay),
       y =  "mean Temperature [°C]",
       x = "Depth [m]",
       color = "Lake \nArea-Group") + 
  scale_color_brewer(palette = "Set2") +
  theme(legend.position = "none") + theme(legend.position = "bottom") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

p_TS 
ggsave(file.path(save_comparison, paste0("TS_temp_depth.png")), p_TS, height = 10, width = 12)

# ------------------------------------------------------------------------
# ---Compariosn ----------------------------------------------------------

mean_env_profile_combo <- mean_profiles %>%
  left_join(mean_profiles_TS, by = c("lakeClass", "AreaGroup", "depth"), suffix = c("", "_TS")) %>%
  mutate(diff_temp = T_prof_mean - T_prof_mean_TS
  )

mean_env_combo <- mean %>% 
  left_join(mean_TS, by = c("lakeClass", "AreaGroup", "depth"), suffix = c("", "_TS")) %>%
  mutate(mean_diff_temp = mean - mean_TS
  )
  

p_comp <- ggplot(mean_env_profile_combo, aes(x = depth, y = diff_temp, color = AreaGroup, 
                                     group = AreaGroup)) +
  geom_line() +
  geom_line(data = mean_env_combo, aes(x = depth, y = mean_diff_temp), color = "black", linetype = "dashed") +
  # geom_point(data = test, aes(x = -0.5, y = T_profile(z = -0.5, T_epi = test$tempEpi_av, T_hypo = test$tempHypo_av, z0 = test$metaDepth_av, k = 5)), color = "red") +
  scale_x_reverse(breaks = depth_bio, limits = c(0,-9),) + # 
  facet_wrap(~ lakeClass, nrow = 1) +
  geom_hline(yintercept = 0, col ="grey") +
  theme_bw() +
  labs(title =" ",# paste("mean Temperature Profiles, days", minDay, "to", maxDay),
       y =  "mean Temperature [°C]",
       x = "Depth [m]",
       color = "Lake \nArea-Group") + 
  scale_color_brewer(palette = "Set2") +
  theme(legend.position = "bottom") + guides(color = guide_legend(nrow = 2)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

p_comp 
ggsave(file.path(save_comparison, paste0("diff_temp_depth.png")), p_comp, height = 4.5, width = 4.5)
