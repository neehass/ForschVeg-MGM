# -------------------------------------------------------------------------------------------
# Visualise Results combo 0-5 & 5-9m
# -------------------------------------------------------------------------------------------
#  projekt: dep10_300spec_base_Tprofile_20years & 
# dep10_300spec_base_Tprofile_20years_deep

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
output_top <- "output/dep10_300spec_base_Tprofil_20years"
out_ana_path_top <- "output-analysis/dep10_300spec_base_Tprofil_20years"

output_deep <- "output/dep10_300spec_base_Tprofile_20years_deep"
out_ana_path_deep <- "output-analysis/dep10_300spec_base_Tprofile_20years_deep"

# combo 
save_figures <- "output-analysis/combo_baseTprofile_20years/figures"
save_out <- "output-analysis/combo_baseTprofile_20years"

scenario <- "base Tprofile"
name <- "Tprofile_20years_combo"

dir.create(save_out)
dir.create(save_figures)

# input files
lake_path <- "input/lakes"

# ---------------------------------------------------------------------------------------------------------
# load data (Data preparation should be run before data_prep.R) ------------------------------------------------

# results 
sort_res_top <- readRDS(file.path(out_ana_path_top, "sortRES.rds")) # sort_res # biomass > 0 sorted macrophyte data (grouped by lakeClass, speciesGroup, lakeGroup_Area, depth, day)
sort_res_deep <- readRDS(file.path(out_ana_path_deep, "sortRES.rds"))

# environment
sort_env_top <- readRDS(file.path(out_ana_path_top, "sortENV.rds")) # env_sort # sorted environmental data (grouped by llakeClass, lakeGroup_Area, day)
sort_env_deep <- readRDS(file.path(out_ana_path_deep, "sortENV.rds"))

gen.conf_top <- readLines(file.path(output_top,"general.config.txt"))
gen.conf_deep <- readLines(file.path(output_top,"general.config.txt"))
if(as.numeric(strsplit(gen.conf_top[8], " ")[[1]][2]) == as.numeric(strsplit(gen.conf_deep[8], " ")[[1]][2])){
  print("same k")
  k <- as.numeric(strsplit(gen.conf_top[8], " ")[[1]][2])
}

load(file.path(lewSpec_dir, "data-raw/observed/Morphology.rda"))
load(file.path(lewSpec_dir, "data/data_lakes_env_class.rda"))
# head(Morphology)
# head(data_lakes_env_class) # Turbidity classes 

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
saveRDS(sortRES_combo, file = file.path(save_out, paste0("sortRES_",name,".rds")))

# environmetnal data is the same
any(sort_env_top != sort_env_deep)
sort_env_top <- sort_env_top %>%
  mutate(
    lakeClass = factor(
      lakeClass,
      levels = c("clear", "medium", "turb"),
      labels = c("clear lakes", "intermediate lakes", "turbid lakes")
    )
  )
# ---- Results -------------------------------------------
# ---- plot ------------------------------------------------------------------------------------------
sort_res_mean <- sortRES_combo %>% group_by(speciesGroup, lakeClass, AreaGroup, day) %>%
  summarise(biomass_mean = mean(biomass_mean, rm.na = TRUE)) %>% ungroup()

p_macro <- ggplot(sort_res_mean, aes(x = day, y = biomass_mean,
                                     color = AreaGroup)) +
  geom_line() +
  facet_grid(speciesGroup ~  lakeClass) +
  theme_bw() +
  labs(title = "Biomass over Active Days (mean over all dephts)",
       y = "Mean Biomass [g]", x = "Days", color = "Lake \nArea-Group") +
  scale_color_brewer(palette = "Set2") + theme(legend.position = "bottom") +
  guides(color = guide_legend(nrow = 1))
p_macro
ggsave(file.path(save_figures, paste("Biomass_day_",name,".png")), p_macro, 
       width = 6.5, height=8, dpi="print", bg="white", scale=1.2)

# ---- boxplot per dephts ------------------------------------------------------------------------------------------
maxDay <- max(unique(sortRES_combo$day))
minDay <- min(unique(sortRES_combo$day))

p_box <- ggplot(sortRES_combo, aes(x = factor(depth, levels = rev(sort(unique(depth)))), 
                              y = biomass_mean, col=AreaGroup, group=interaction(AreaGroup,Lake))) +# fill = AreaGroup)) ++
  #geom_path(alpha=0.5)+
  geom_boxplot(aes(group=interaction(depth,AreaGroup), fill=AreaGroup)) +

  facet_grid(speciesGroup ~  lakeClass) +
  theme_bw() +
  labs(title = paste("Biomass over Active Days, days", minDay, "to", maxDay),
       y = "Mean Biomass [g]", x = "Depths [m]", fill = "Lake \nArea-Group",  col = "Lake \nArea-Group")+
     scale_fill_brewer(palette = "Set2") + scale_color_brewer(palette = "Set2")  +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 1)) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
p_box
ggsave(file.path(save_figures, paste("BOX", name, "_biomass_day.png")), p_box, 
       width = 6.5, height=8, dpi="print", bg="white", scale=1.2)

# -----------------------------------------------------------------------------------------
# ---- plot nur nach species group und depth------------------------------------------------------------------------------------------
sort_res_mean <- sortRES_combo %>% group_by(speciesGroup, depth, lakeClass, day) %>%
  summarise(biomass_mean = mean(biomass_mean, rm.na = TRUE)) %>% ungroup()
sort_res_mean$depth <- factor(sort_res_mean$depth, levels = rev(sort(unique(sort_res_mean$depth))))

TrophiePalette <- c("#56B4E9", "#E69F00", "#009E73")

p_macro <- ggplot(sortRES_combo, aes(x = day, y = biomass_mean,
                                      color = speciesGroup)) +
  geom_line(alpha = 0.2) +
  geom_line(data = sort_res_mean, aes(x = day, y = biomass_mean,
                                    color = speciesGroup), linewidth = 0.8) +
  scale_color_manual(values =  c(rev(TrophiePalette))) +
  facet_grid( depth ~  lakeClass, scales = "free_y") +
  theme_bw() + theme(legend.position = "bottom") +
  labs(title = scenario,
       y = "Mean Biomass [g]", x = "Days", color = "Spec. Group") 

p_macro
ggsave(file.path(save_figures, paste("Biomass_depth_day_",name,".png")), p_macro, 
       width = 6.5, height=8, dpi="print", bg="white", scale=1.2)

# ---------------------------------------------------------------------------------------------------------

# ---- Environmental variables ----------------------------------------------------------------------
# --- TProfile Development over Days ------
# mean Depths per lakeClass and lakeGroup_Area
# head(sort_env)

sort_env_Tprof_DAY <- sort_env_top %>% # sort_env[sort_env$day %in% unique(sort_res$day), ] 
  group_by(lakeClass, AreaGroup, day) %>%
  # mutate(day_bin = floor((day - 1) / 30) * 30 + 1) %>%  # days 1–7 → 1, 8–14 → 8, etc.
  mutate(month_bin = floor((day - 1) / 31) + 1) %>%  # month 1, 2, 3...
  group_by(lakeClass, AreaGroup, month_bin) %>%
  summarise(
    tempEpi_av = mean(tempEpi_mean, na.rm = TRUE),
    tempHypo_av = mean(tempHypo_mean, na.rm = TRUE),
    metaDepth_av = mean(metaDepth_mean, na.rm = TRUE),
    lakeDepth_av = round(mean(lakeDepth_mean, na.rm = TRUE)), 
    T_prof =  list(T_profile(z = sort(seq(lakeDepth_av, 0, 0.5), decreasing = TRUE), 
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
ggsave(file.path(save_figures, paste0(name,"_perAproxMonth.png")), p_Tprofile_DAY_all, height = 4.5, width = 4.5, scale = 1.5)


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
ggsave(file.path(save_figures, paste0(name,"_perAproxMonth2.png")), p_Tprofile_DAY, height = 4.5, width = 4.5, scale = 1.5)


# ----------------------------------------------------------------------------
# # Biomass plot + temp  -------------------------------------
# --- plot TempProfiles for all Groups ------
# !!! # mean Depths per lakeClass and lakeGroup_Area needed !!!!

depth_bio <- c(0, sort(unique(sortRES_combo$depth), decreasing = TRUE))

maxDay <- max(unique(sortRES_combo$day))
minDay <- min(unique(sortRES_combo$day))

sort_env_Tprof_DAY <- sort_env_top %>% # sort_env[sort_env$day %in% unique(sort_res$day), ] 
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
ggsave(file.path(save_figures, paste0(name,"_depth.png")), p_Tprofile, height = 10, width = 12)

# combo temp mit boxplot
p_combo <-  p_box + p_Tprofile + plot_layout(guides = "collect") +
  plot_layout(ncol = 1, heights = c(3, 1)) &
  theme(legend.position = "bottom",  legend.box = "vertical")&
  guides(fill = guide_legend(nrow = 1),
         colour = guide_legend(nrow = 1))
p_combo
ggsave(file.path(save_figures, paste0("BOX-temp",name,"_biomass_day.png")), p_combo, 
       width = 5, height=7, dpi="print", bg="white", scale=1.2)

p_combo <-  p_macro + p_Tprofile + plot_layout(guides = "collect") + 
  plot_layout(ncol = 1, heights = c(3, 1)) &
  theme(legend.position = "bottom", legend.direction = "horizontal" , 
        legend.box = "vertical")
p_combo
ggsave(file.path(save_figures, paste0("Biomass_day_temp",name,".png")), p_combo, 
       width = 4.5, height=7, dpi="print", bg="white", scale=1.2)

