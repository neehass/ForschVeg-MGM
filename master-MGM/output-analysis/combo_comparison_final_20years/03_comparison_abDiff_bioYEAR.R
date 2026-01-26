# Comparison Biomass Jahresverlauf

# load data from data_prep_comparison.R

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
dir_Tprofile <- "output-analysis/dep10_300spec_base_Tprofil_20years"
dir_Tsteady <- "output-analysis/dep10_300spec_base_Tsteady_20years"

save_comparison <- "output-analysis/comparison_final_20years/bio_TprofVSTsteady/adDiff"

dir.create(save_comparison)

# Folder output of MGM experiment and Analysis results folder
save_comparison <- "output-analysis/comparison/biomassYear"
dir.create(save_comparison)

# input files
lake_path <- "input/lakes"

# load data --------------------------------------------------------------------------------
# load data prep in data_prep_comparison.R
# ---------------------------------------------------------------------------------------------------------
# Tprofile --------------------------------------------------------------------------------
sortRES_baseTP <- readRDS(file.path(dir_Tprofile, "sortRES.rds")) # sort_res # biomass > 0 sorted macrophyte data (grouped by lakeClass, speciesGroup, lakeGroup_Area, depth, day)
sortENV_baseTP <- readRDS(file.path(dir_Tprofile, "sortENV.rds")) # env_sort # sorted environmental data (grouped by llakeClass, lakeGroup_Area, day)

sortRES_baseTP$depth <- factor(sortRES_baseTP$depth,
                                levels = sort(unique(sortRES_baseTP$depth), decreasing = TRUE))

mean_res_TP <- sortRES_baseTP %>%
  group_by(day, lakeClass, depth, speciesGroup) %>%
  summarise(biomass_mean = mean(biomass_mean), .groups = "drop")
head(mean_res_TP)

# ---------------------------------------------------------------------------------------------------------
# Tsteady --------------------------------------------------------------------------------
sortRES_baseTSteady <- readRDS(file.path(dir_Tsteady, "sortRES.rds")) # sort_res # biomass > 0 sorted macrophyte data (grouped by lakeClass, speciesGroup, lakeGroup_Area, depth, day)
sortENV_baseTSteady <- readRDS(file.path(dir_Tsteady, "sortENV.rds")) # env_sort # sorted environmental data (grouped by llakeClass, lakeGroup_Area, day)

sortRES_baseTSteady$depth <- factor(sortRES_baseTSteady$depth,
                                levels = sort(unique(sortRES_baseTSteady$depth), decreasing = TRUE))

mean_res_TS <- sortRES_baseTSteady %>%
  group_by(day, lakeClass, depth, speciesGroup) %>%
  summarise(biomass_mean = mean(biomass_mean), .groups = "drop")
head(mean_res_TS)

# ----------------------------------
# ---- plot ------------------------------------------------------------------------------------------
TrophiePalette <- c("#56B4E9", "#E69F00", "#009E73") # old: TrophiePalette <- c("cornflowerblue","aquamarine4","coral4")
modeltypePalette <- brewer.pal(n = 3, name = "Set2")
abDiffPalette <- brewer.pal(n = 3, name = "Pastel2")
depthPalette <- brewer.pal(n = 4, name = "Paired")

# Tprofile
p_macro_TP <- ggplot(sortRES_baseTP, aes(x = day, y = biomass_mean,
                                          color = speciesGroup)) +
  geom_line(alpha = 0.2) +
  geom_line(data = mean_res_TP, aes(x = day, y = biomass_mean,
                                    color = speciesGroup), linewidth = 0.8) +
  scale_color_manual(values = TrophiePalette) +
  facet_grid( depth ~  lakeClass) +
  theme_bw() + theme(legend.position = "bottom") +
  labs(title = "base Tprofile",
       y = "Mean Biomass [g]", x = "Days", color = "Spec. Group") 
p_macro_TP
ggsave(file.path(save_comparison, "biomass_day_baseTprofile.png"), p_macro_TP, 
       height = 4.5, width = 4.5, dpi = "print", scale =1.2)

# Tsteady
p_macro_TS <- ggplot(sortRES_baseTSteady, aes(x = day, y = biomass_mean,
                                          color = speciesGroup)) +
  geom_line(alpha = 0.2) +
  geom_line(data = mean_res_TS, aes(x = day, y = biomass_mean,
                                    color = speciesGroup), linewidth = 0.8) +
  scale_color_manual(values = TrophiePalette) +
  facet_grid( depth ~  lakeClass) +
  theme_bw() + theme(legend.position = "bottom") +
  labs(title = "base Tsteady",
       y = "Mean Biomass [g]", x = "Days", color = "Spec. Group") 
p_macro_TS
ggsave(file.path(save_comparison, "biomass_day_baseTsteady.png"), p_macro_TS, 
       height = 4.5, width = 4.5, dpi = "print", scale =1.2)

# ------------------------------------------------------------------------------
# Comparison 
colnames(sortRES_baseTP) == colnames(sortRES_baseTSteady)
colnames(sortRES_baseTP)

res_combo <- sortRES_baseTP %>% 
  left_join(
    sortRES_baseTSteady,
    by = c("lakeClass", "speciesGroup", "AreaGroup", "depth", "day", "lakeDepth_mean"),
    suffix = c("_Tprofile", "_Tsteady")
  ) 
head(res_combo)

mean_res_combo <- res_combo %>%
  group_by(day, lakeClass, depth, speciesGroup) %>%
  summarise(bio_mean_Tprofile = mean(biomass_mean_Tprofile, na.rm = TRUE), 
            bio_mean_Tsteady = mean(biomass_mean_Tsteady, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(across(where(is.numeric), ~ replace_na(., 0)),
         diff = bio_mean_Tprofile - bio_mean_Tsteady,
         proDiff = (bio_mean_Tprofile - bio_mean_Tsteady)/ bio_mean_Tsteady * 100, # prozentual diff
         log_proDiff = log(bio_mean_Tprofile/bio_mean_Tsteady)*100) 

# absolut diff
p_compar_bio <- ggplot(data = mean_res_combo, aes(x = day, y = diff,
                                                  color = speciesGroup)) +
  geom_line(linewidth = 0.8) +
  scale_color_manual(values = TrophiePalette) +
  facet_grid( depth ~  lakeClass, scales = "free_y") +
  theme_bw() + theme(legend.position = "bottom") +
  labs(title = "Tprofile - Tsteady",
       y = "absolut Diff. of Mean Biomass [g]", x = "Days", color = "Spec. Group") 
p_compar_bio
ggsave(file.path(save_comparison, "adDiff_biomass_day_compar.png"), p_compar_bio, 
       height = 4.5, width = 4.5, dpi = "print", scale =1.2)

# proz diff
p_compar_bio_pro <- ggplot(data = mean_res_combo, aes(x = day, y = proDiff, # log_proDiff
                                                  color = speciesGroup)) +
  geom_line(linewidth = 0.8) +
  scale_color_manual(values = TrophiePalette) +
  facet_grid( depth ~  lakeClass, scales = "free_y") +
  theme_bw() + theme(legend.position = "bottom") +
  labs(title = "(Tprofile - Tsteady)/ Tsteady * 100",
       y = "percentage Diff. of Mean Biomass [%]", x = "Days", color = "Spec. Group") 
p_compar_bio_pro
ggsave(file.path(save_comparison, "proDiff_biomass_day_compar.png"), p_compar_bio_pro, 
       height = 4.5, width = 4.5, dpi = "print", scale =1.2)

# added plots
# layout <- "
# A C
# B C
# "
p_TP <- p_macro_TP + theme(legend.position = "none") + guides(fill = "none", linetype = "none", color = "none", linewidth = "none", alpha = "none")
p_TS <- p_macro_TS + theme(legend.position = "none") +  guides(fill = "none", linetype = "none", color = "none", linewidth = "none", alpha = "none")
p_cb <- p_compar_bio + theme(legend.position = "none") +  guides(fill = "none", linetype = "none")
p__pdiff <- p_compar_bio_pro + theme(legend.position = "none") +  guides(fill = "none", linetype = "none")

p_combo_bio1 <- ((p_TP/p_TS) | p_cb) +
  # plot_layout(design = layout) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = 'a',
                  tag_prefix = '(',
                  tag_suffix = ')')& 
  theme(plot.tag = element_text(size = 12))&  
  theme(legend.position = "bottom")&
  guides(colour = guide_legend(override.aes = list(size=3)))

p_combo_bio1  
ggsave(file.path(save_comparison, "01_biomass_day_compar.png"), p_combo_bio1,
       height = 6, width = 6, dpi = "print", scale =1.2)

# added plots
# layout <- "
# A C
# B D
# "
p_combo_bio2 <- ((p_TP/p_TS) | (p_cb/p__pdiff)) +
  # plot_layout(design = layout) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = 'a',
                  tag_prefix = '(',
                  tag_suffix = ')')& 
  theme(plot.tag = element_text(size = 12))&  
  theme(legend.position = "bottom")&
  guides(colour = guide_legend(override.aes = list(size=3)))

p_combo_bio2  
ggsave(file.path(save_comparison, "02_biomass_day_compar.png"), p_combo_bio2,
       height = 6, width = 6, dpi = "print", scale =1.2)

