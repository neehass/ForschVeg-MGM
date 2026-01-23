# -------------------------------------------------------------------------------------------
# Visualise Results
# -------------------------------------------------------------------------------------------
# projekt: test_oli_plus5
# test just oligotrophic species under +5 sceanario

# packages & functions
library(ggplot2)
library(patchwork)
library(dplyr)
library(tidyr)
library(stringr)
library(RColorBrewer)
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
output <- "output/test_oli_plus5"

out_ana_path <- "output-analysis/test_oli_plus5"

save_figures <- "output-analysis/test_oli_plus5/figures"

dir.create(save_figures)
name <- "test_oli_plus5"

# input files
lake_path <- "input/lakes"

# ---------------------------------------------------------------------------------------------------------
# load data (Data preparation should be run before data_prep.R) ------------------------------------------------

# results 
# res <- readRDS(file.path(out_ana_path, "all_res.rds"))   # res # all data
sort_res <- readRDS(file.path(out_ana_path, "sortRES.rds")) # sort_res # biomass > 0 sorted macrophyte data (grouped by lakeClass, speciesGroup, lakeGroup_Area, depth, day)

# environment
# env <- readRDS(file.path(out_ana_path, "all_env.rds"))  # env # all environmental data
sort_env <- readRDS(file.path(out_ana_path, "sortENV.rds")) # env_sort # sorted environmental data (grouped by llakeClass, lakeGroup_Area, day)

gen.conf <- readLines(file.path(output,"general.config.txt"))
k <- as.numeric(strsplit(gen.conf[8], " ")[[1]][2])

load(file.path(lewSpec_dir, "data-raw/observed/Morphology.rda"))
load(file.path(lewSpec_dir, "data/data_lakes_env_class.rda"))
# head(Morphology)
# head(data_lakes_env_class) # Turbidity classes 

# ---------------------------------------------------------------------------------------------------------
# ---- Environmental variables ----------------------------------------------------------------------
# ---- plot ------------------
scenario <- "+5 sceanrio T-Profile"
sort_env_Tprof <- func_sortENV_plot(sort_env, save_figures, scenario, k) # defined in help-func.R
# lightAttenuation_mean missing 

# ---- Results -------------------------------------------
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
  scale_color_brewer(palette = "Set2") + theme(legend.position = "bottom") +
  guides(color = guide_legend(nrow = 1))
p_macro
ggsave(file.path(save_figures, paste0(name,"_biomass_day.png")), p_macro, height = 10, width = 12)

# ---- boxplot per dephts ------------------------------------------------------------------------------------------
maxDay <- max(unique(sort_res$day))
minDay <- min(unique(sort_res$day))

p_box <- ggplot(sort_res, aes(x = factor(depth, levels = rev(sort(unique(depth)))), 
                              y = biomass_mean, fill = AreaGroup)) +
  geom_boxplot() +
  facet_grid(speciesGroup ~  lakeClass) +
  theme_bw() +
  labs(title = paste("Biomass over Active Days, days", minDay, "to", maxDay),
       y = "Mean Biomass", x = "Depths [m]", fill = "Lake Area-Group")+
  scale_fill_brewer(palette = "Set2")  + theme(legend.position = "bottom") +
  guides(color = guide_legend(nrow = 1))
p_box
ggsave(file.path(save_figures, paste0(name,"_BOX_biomass_day.png")), p_box, height = 10, width = 12)

# ---------------------------------------------------------------------------------------------------------

# ---- Environmental variables ----------------------------------------------------------------------
# --- TProfile Development over Days ------
# mean Depths per lakeClass and lakeGroup_Area
# head(sort_env)

sort_env_Tprof_DAY <- sort_env %>% # sort_env[sort_env$day %in% unique(sort_res$day), ] 
  group_by(lakeClass, AreaGroup, day) %>%
  # mutate(day_bin = floor((day - 1) / 30) * 30 + 1) %>%  # days 1–7 → 1, 8–14 → 8, etc.
  mutate(month_bin = floor((day - 1) / 31) + 1) %>%  # month 1, 2, 3...
  group_by(lakeClass, AreaGroup, month_bin) %>%
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
  group_by(lakeClass, AreaGroup, month_bin) %>%
  mutate(depth = list(sort(seq(lakeDepth_av, 0, 0.5), decreasing = TRUE))) %>%  # depth for each T_prof
  unnest(c(T_prof, depth)) 
# View(sort_env_Tprof_DAY_long)

# plotting 
all_days <- sort(unique(sort_env_Tprof_DAY_long$month_bin))
lake_classes <- unique(sort_env_Tprof_DAY_long$lakeClass)
max_rows <- sort_env_Tprof_DAY_long %>%
  group_by(lakeClass) %>%
  summarise(nRows = ceiling(n_distinct(AreaGroup)/5)) %>%
  pull(nRows) %>%
  max()
p_Tprofile_DAY <- vector("list", 3)
for(i in 1:3){
  message(lake_classes[i])
  dat <- sort_env_Tprof_DAY_long %>% filter(lakeClass == lake_classes[i])
  h <- length(unique(dat$AreaGroup))
  p_Tprofile_DAY[[i]] <- ggplot(dat, aes(x = T_prof, y = depth, 
                                         color = factor(month_bin))) +
    geom_line(linewidth = 1) +
    
    # scale_x_reverse( breaks = c(-5.0, -3.0, -1.5, -0.5),  limits = c(0, -5)) + # limits = c(0, -5),
    facet_wrap( ~AreaGroup, ncol = 5, nrow = max_rows) +
    theme_bw() + labs(title = lake_classes[i], color = "Approx. Months", 
                      x = "Mean Temperature [°C]", y = "Depth [m]") +
    theme(legend.position = "none") + guides(color = guide_legend(nrow = 1)) 
  
  ggsave(file.path(save_figures, paste(lake_classes[i],"_Tprof_perAproxMonth.png")),
         p_Tprofile_DAY[[i]], height = 4, width = 2*h)
  
}

p_Tprofile_DAY_all <- (p_Tprofile_DAY[[1]] / 
                         p_Tprofile_DAY[[2]] / p_Tprofile_DAY[[3]]) +
  plot_layout(guides = "collect" ) &
  theme(legend.position = "bottom",
        axis.title.x = element_text(),  # show x-axis title
        axis.text.x = element_text()    # show x-axis labels
  ) 

p_Tprofile_DAY_all
ggsave(file.path(save_figures, paste0(name,"_Tprof_perAproxMonth.png")), p_Tprofile_DAY_all, height = 10, width = 10)


p_Tprofile_DAY <- ggplot(sort_env_Tprof_DAY_long, aes(x = T_prof, y = depth, 
                                                      color = factor(month_bin))) +
  geom_line(linewidth = 1) +
  # scale_x_reverse( breaks = c(-5.0, -3.0, -1.5, -0.5),  limits = c(0, -5)) + # limits = c(0, -5),
  facet_wrap(lakeClass  ~AreaGroup, ncol = 3) +
  theme_bw() +
  labs(title = paste("monthly mean Temperature Profiles \n(Days 1-365 summairsed in 30 day steps)" ),
       x =  "mean Temperature [°C]",
       y = "Depth [m]",
       color = "approx. Months")  +
  theme(legend.position = "bottom") + guides(color = guide_legend(nrow = 1))

p_Tprofile_DAY
ggsave(file.path(save_figures, paste0(name,"_Tprof_perAproxMonth2.png")), p_Tprofile_DAY, height = 10, width = 10)

# Biomass plot + temp 

# ----------------------------------------------------------------------------
# --- plot TempProfiles for all Groups ------
# !!! # mean Depths per lakeClass and lakeGroup_Area needed !!!!

depth_bio <- c(0, sort(unique(sort_res$depth), decreasing = TRUE))
k <- as.numeric(strsplit(gen.conf[8], " ")[[1]][2])

maxDay <- max(unique(sort_res$day))
minDay <- min(unique(sort_res$day))

sort_env_Tprof_DAY <- sort_env %>% # sort_env[sort_env$day %in% unique(sort_res$day), ] 
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
  scale_x_reverse(breaks = depth_bio, limits = c(0, -5),) + # 
  facet_wrap(~ lakeClass, nrow = 1) +
  theme_bw() +
  labs(title = paste("mean Temperature Profiles, days", minDay, "to", maxDay),
       y =  "mean Temperature [°C]",
       x = "Depth [m]",
       color = "Lake Area-Group") + 
  scale_color_brewer(palette = "Set2") +
  theme(legend.position = "none") + theme(legend.position = "bottom") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

p_Tprofile 
ggsave(file.path(save_figures, paste0(name, "_Tprofile_depth.png")), p_Tprofile, height = 10, width = 12)

p_combo <-  p_box + p_Tprofile + plot_layout(guides = "collect") +
  plot_layout(ncol = 1, heights = c(3, 1)) &
  theme(legend.position = "bottom")
p_combo
ggsave(file.path(save_figures,  paste0(name,"_BOX-Tprof_biomass_day.png")), p_combo, height = 10, width = 12)

p_combo <-  p_macro + p_Tprofile + plot_layout(guides = "collect") + 
  plot_layout(ncol = 1, heights = c(3, 1)) &
  theme(legend.position = "bottom")
p_combo
ggsave(file.path(save_figures, paste0(name,"_Tprof_biomass_day.png")), p_combo, height = 10, width = 12)

