# -------------------------------------------------------------------------------------------
# Visualise Results
# -------------------------------------------------------------------------------------------
# projekt: dep10_lakes_100spec_base_Tprofile

# packages & functions
library(ggplot2)
library(patchwork)
library(dplyr)
library(tidyr)
library(stringr)
library(RColorBrewer)
library(viridis)
library(ggrepel)
library(ggpmisc)
library(ggpubr)
# library(tidyverse)

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

# ---------------------------------------------------------------------------------
# Folder output of MGM experiment and Analysis results folder
output <- "output/dep10_lakes_100spec_base_Tprofile"

save_figures <- "output-analysis/dep10_lakes_100spec_base_Tprofile"

dir.create(save_figures)

# input files
lake_path <- "input/lakes"

# ---------------------------------------------------------------------------------------------------------
# load data (Data preparation should be run before data_prep.R) ------------------------------------------------
load(file.path(save_figures, "res_dep10_Tprofile.RData"))   # res # all data
load(file.path(save_figures, "sortRES_dep10_Tprofile.RData")) # sort_res # biomass > 0 sorted macrophyte data (grouped by lakeClass, speciesGroup, lakeGroup_Area, depth, day)

load(file.path(save_figures, "env_dep10_Tprofile.RData"))  # env # all environmental data
load(file.path(save_figures, "sortENV_dep10_Tprofile.RData")) # env_sort # sorted environmental data (grouped by llakeClass, lakeGroup_Area, day)

gen.conf <- readLines(file.path(output,"general.config.txt"))

load(file.path(lewSpec_dir, "data-raw/observed/Morphology.rda"))
load(file.path(lewSpec_dir, "data/data_lakes_env_class.rda"))
head(Morphology)
head(data_lakes_env_class) # Turbidity classes 

# ---------------------------------------------------------------------------------------------------------
# ---- plot ------------------------------------------------------------------------------------------
sort_res_mean <- sort_res %>% group_by(speciesGroup, lakeClass, AreaGroup, day) %>%
  summarise(biomass_mean = mean(biomass_mean, rm.na = TRUE)) %>% ungroup()

p_macro <- ggplot(sort_res_mean, aes(x = day, y = biomass_mean,
                                         color = AreaGroup)) +
  geom_line() +
  facet_grid(speciesGroup ~  lakeClass) +
  theme_bw() +
  labs(title = "Biomass over Active Days (mean over all dephts)",
       y = "Mean Biomass", x = "Days", color = "Lake Area-Group") +
  scale_color_brewer(palette = "Set2")
p_macro
ggsave(file.path(save_figures, "biomass_day.png"), p_macro, height = 20, width = 15)

# ---- boxplot per dephts ------------------------------------------------------------------------------------------
maxDay <- max(unique(sort_res$day))
minDay <- min(unique(sort_res$day))

p_box <- ggplot(sort_res, aes(x = factor(depth, levels = rev(sort(unique(depth)))), 
                              y = biomass_mean, fill = lakeClass)) +
  geom_boxplot() +
  facet_grid(speciesGroup ~  lakeGroup_Area) +
  theme_bw() +
  labs(title = paste("Biomass over Active Days, days", minDay, "to", maxDay),
       y = "Mean Biomass", x = "Depths [m]", fill = "Turbidity")+
  scale_fill_brewer(palette = "Accent") 
p_box
ggsave(file.path(save_figures, "BOX_biomass_day.png"), p_box, height = 20, width = 15)

# ---------------------------------------------------------------------------------------------------------

# ---- Environmental variables ----------------------------------------------------------------------
# ---- plot ------------------
sort_env <- sort_env %>%
  rename(AreaGroup = lakeGroup_Area)

scenario <- "base-100spec_Tprofile"
func_sortENV_plot(sort_env, save_figures, scenario) # defined in help-func.R
# lightAttenuation_mean missing 

# --- plot TempProfiles for all Groups ------
# !!! # mean Depths per lakeClass and lakeGroup_Area needed !!!!
head(res)
depth_bio <- c(0, sort(unique(sort_res$depth), decreasing = TRUE))
k <- as.numeric(strsplit(gen.conf[8], " ")[[1]][2])

sort_env_Tprof <- sort_env[sort_env$day %in% unique(sort_res$day), ] %>%
  group_by(lakeClass, AreaGroup) %>%
  summarise(
    tempEpi_av = mean(tempEpi_mean, na.rm = TRUE),
    tempHypo_av = mean(tempHypo_mean, na.rm = TRUE),
    metaDepth_av = mean(metaDepth_mean, na.rm = TRUE),
    lakeDepth_mean = round(mean(lakeDepth_mean, na.rm = TRUE)), 
    T_prof = list(T_profile(z = c(depth_bio), T_epi = tempEpi_av, T_hypo = tempHypo_av,
                            z0 = metaDepth_av,k = k))) %>% 
  ungroup()

# head(sort_env_Tprof)
sort_env_Tprof_long <- sort_env_Tprof %>%
  group_by(lakeClass, AreaGroup) %>%
  mutate(depth = list(c(depth_bio))) %>%  # depth for each T_prof
  unnest(c(T_prof, depth))
# sort_env_Tprof_long <- sort_env_Tprof_long[sort_env_Tprof_long$depth >= min(depth_bio),]
# View(sort_env_Tprof_long)
# unique(sort_env_Tprof_long$depth)
# test <- sort_env_Tprof_long[1, ]

maxDay <- max(unique(sort_res$day))
minDay <- min(unique(sort_res$day))

p_Tprofile <- ggplot(sort_env_Tprof_long, aes(x = depth, y = T_prof, color = AreaGroup)) +
  geom_line(linewidth = 1) +
  # geom_point(data = test, aes(x = -0.5, y = T_profile(z = -0.5, T_epi = test$tempEpi_av, T_hypo = test$tempHypo_av, z0 = test$metaDepth_av, k = 5)), color = "red") +
  scale_x_reverse(breaks = depth_bio) + # limits = c(0, -5),
  facet_wrap(~ lakeClass, nrow = 1) +
  theme_bw() +
  labs(title = paste("mean Temperature Profiles, days", minDay, "to", maxDay),
       y =  "mean Temperature [°C]",
       x = "Depth [m]",
       color = "Turbidity") +
  scale_color_brewer(palette = "Accent") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

p_combo <-  p_box + p_Tprofile + plot_layout(ncol = 1, heights = c(3, 1))
p_combo
ggsave(file.path(save_figures, "BOX-Tprof_biomass_day.png"), p_combo, height = 25, width = 15)

# --- TProfile Development over Days ------
# mean Depths per lakeClass and lakeGroup_Area
sort_env_Tprof_DAY <- sort_env %>% # sort_env[sort_env$day %in% unique(sort_res$day), ] 
  group_by(lakeClass, lakeGroup_Area, day) %>%
  # mutate(day_bin = floor((day - 1) / 30) * 30 + 1) %>%  # days 1–7 → 1, 8–14 → 8, etc.
  mutate(month_bin = floor((day - 1) / 30) + 1) %>%  # month 1, 2, 3...
  group_by(lakeClass, lakeGroup_Area, month_bin) %>%
  summarise(
    tempEpi_av = mean(tempEpi_mean, na.rm = TRUE),
    tempHypo_av = mean(tempHypo_mean, na.rm = TRUE),
    metaDepth_av = mean(metaDepth_mean, na.rm = TRUE),
    lakeDepth_mean = round(mean(lakeDepth_mean, na.rm = TRUE)), 
    T_prof = list(T_profile(z = sort(seq(lakeDepth_mean, 0, 0.5), decreasing = TRUE), T_epi = tempEpi_av, T_hypo = tempHypo_av,
                            z0 = metaDepth_av,k = k))) %>% 
  ungroup()

# View(sort_env_Tprof_DAY)
sort_env_Tprof_DAY_long <- sort_env_Tprof_DAY %>%
  group_by(lakeClass, lakeGroup_Area, month_bin) %>%
  mutate(depth = list(sort(seq(lakeDepth_mean, 0, 0.5), decreasing = TRUE))) %>%  # depth for each T_prof
  unnest(c(T_prof, depth)) 
# View(sort_env_Tprof_DAY_long)

all_days <- sort(unique(sort_env_Tprof_DAY_long$month_bin))
mid_idx <- round(quantile(1:length(all_days), probs = c(0.25, 0.5, 0.75)))
q25 <- c(mid_idx[1]-1, mid_idx[1], mid_idx[1]+1)
mid <- c(mid_idx[2]-1, mid_idx[2], mid_idx[2]+1)
q75 <-  c(mid_idx[3]-1, mid_idx[3], mid_idx[3]+1)
show_days <- as.character(c(head(all_days, 3), all_days[q25], 
                            all_days[mid], all_days[q75], tail(all_days, 3)))

p_Tprofile_DAY <- ggplot(sort_env_Tprof_DAY_long, aes(x = T_prof, y = depth, 
                                                      color = factor(month_bin))) +
  geom_line(linewidth = 1) +
  # scale_x_reverse( breaks = c(-5.0, -3.0, -1.5, -0.5),  limits = c(0, -5)) + # limits = c(0, -5),
  facet_wrap(lakeGroup_Area ~lakeClass, ncol = 3) +
  theme_bw() +
  labs(title = paste("monthly mean Temperature Profiles \n(Days 1-365 summairsed in 30 day steps)" ),
       x =  "mean Temperature [°C]",
       y = "Depth [m]",
       color = "approx. Months")  +
  scale_color_discrete(breaks = show_days) +   # << show only selected days
  theme(legend.position = "right")

# theme(legend.position = "none") # + 
# scale_color_viridis(option = "inferno", discrete = is.factor(sort_env_Tprof_DAY_long$day))

p_Tprofile_DAY
ggsave(file.path(save_figures, "Tprof_perAproxMonth.png"), p_Tprofile_DAY, height = 20, width = 15)

# --- plot nutrients / lake parameters per day ---
# .....
# -----------
----------------------------------------------------------------------------------------------
  # Gradient potential species richness vs observed species richness  --------------------------------
# in data&visual_specPot-Observed. R