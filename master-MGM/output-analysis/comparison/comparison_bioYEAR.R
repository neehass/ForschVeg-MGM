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
base_Tprofile <- "output/dep10_lakes_300spec_base_Tprofile"
save_base_Tprofile <- "output-analysis/dep10_lakes_300spec_base_Tprofile"

base_Tsteady <- "output/dep10_lakes_300spec_base_Tsteady"
save_base_Tsteady <- "output-analysis/dep10_lakes_300spec_base_Tsteady"

# Folder output of MGM experiment and Analysis results folder
save_comparison <- "output-analysis/comparison/biomassYear"
dir.create(save_comparison)

# input files
lake_path <- "input/lakes"

# load data --------------------------------------------------------------------------------
# load data prep in data_prep_comparison.R
# ---------------------------------------------------------------------------------------------------------
# Tprofile --------------------------------------------------------------------------------
load(file.path(save_base_Tprofile, "res_dep10.RData"))   # res # all data
load(file.path(save_base_Tprofile, "sortRES_dep10.RData")) # sort_res # biomass > 0 sorted macrophyte data (grouped by lakeClass, speciesGroup, lakeGroup_Area, depth, day)
res_baseTP <- res
sort_res_baseTP <- sort_res

load(file.path(save_base_Tprofile, "env_dep10.RData"))  # env # all environmental data
load(file.path(save_base_Tprofile, "sortENV_dep10.RData")) # env_sort # sorted environmental data (grouped by llakeClass, lakeGroup_Area, day)
env_baseTP <- res
sort_env_baseTP <- sort_res
rm(res, env, sort_res, sort_env)  # remove the generic

head(res_baseTP)
class(res_baseTP)

sort_res_baseTP <- sort_res_baseTP %>% 
  mutate(Group=ifelse(speciesGroup=="eutroph","eutraphentic",
                      ifelse(speciesGroup=="mesotroph","mesotraphentic",
                             ifelse(speciesGroup=="oligotroph","oligotraphentic","NA"))))
sort_res_baseTP$depth <- factor(sort_res_baseTP$depth,
                                levels = sort(unique(sort_res_baseTP$depth), decreasing = TRUE))

mean_res_TP <- sort_res_baseTP %>%
  group_by(day, lakeClass, depth, speciesGroup) %>%
  summarise(biomass_mean = mean(biomass_mean), .groups = "drop")

# ---------------------------------------------------------------------------------------------------------
# Tsteady --------------------------------------------------------------------------------
load(file.path(save_base_Tsteady, "res_dep10.RData"))   # res # all data
load(file.path(save_base_Tsteady, "sortRES_dep10.RData")) # sort_res # biomass > 0 sorted macrophyte data (grouped by lakeClass, speciesGroup, lakeGroup_Area, depth, day)
res_baseTS <- res
sort_res_baseTS <- sort_res

load(file.path(save_base_Tsteady, "env_dep10.RData"))  # env # all environmental data
load(file.path(save_base_Tsteady, "sortENV_dep10.RData")) # env_sort # sorted environmental data (grouped by llakeClass, lakeGroup_Area, day)
env_baseTS <- res
sort_env_baseTS <- sort_res
rm(res, env, sort_res, sort_env)  # remove the generic

sort_res_baseTS <- sort_res_baseTS %>% 
  mutate(Group=ifelse(speciesGroup=="eutroph","eutraphentic",
                      ifelse(speciesGroup=="mesotroph","mesotraphentic",
                             ifelse(speciesGroup=="oligotroph","oligotraphentic","NA"))))
sort_res_baseTS$depth <- factor(sort_res_baseTS$depth,
                                levels = sort(unique(sort_res_baseTS$depth), decreasing = TRUE))

mean_res_TS <- sort_res_baseTS %>%
  group_by(day, lakeClass, depth, speciesGroup) %>%
  summarise(biomass_mean = mean(biomass_mean), .groups = "drop")

# ----------------------------------
# ---- plot ------------------------------------------------------------------------------------------
WinnerLoserPalette<- c(carto_pal(2,"PinkYl")[c(2)],carto_pal(2,"TealGrn")[c(1)])
TrophiePalette <- c("cornflowerblue","aquamarine4","coral4")
modeltypePalette <- brewer.pal(n = 3, name = "Set2")
abDiffPalette <- brewer.pal(n = 3, name = "Pastel2")
depthPalette <- brewer.pal(n = 4, name = "Paired")

# Tprofile
p_macro_TP <- ggplot(sort_res_baseTP, aes(x = day, y = biomass_mean,
                                       color = speciesGroup)) +
  geom_line(alpha = 0.3) +
  geom_line(data = mean_res_TP, aes(x = day, y = biomass_mean,
                                    color = speciesGroup), linewidth = 0.8) +
  scale_color_manual(values = TrophiePalette) +
  facet_grid( depth ~  lakeClass) +
  theme_bw() +
  labs(title = "Tprofile",
    y = "Mean Biomass", x = "Days", color = "Spec. Group") 
p_macro_TP
ggsave(file.path(save_comparison, "biomass_day_baseTprofile.png"), p_macro_TP, height = 6, width = 8.5)

# Tsteady
p_macro_TS <- ggplot(sort_res_baseTS, aes(x = day, y = biomass_mean,
                                          color = speciesGroup)) +
  geom_line(alpha = 0.3) +
  geom_line(data = mean_res_TS, aes(x = day, y = biomass_mean,
                                    color = speciesGroup), linewidth = 0.8) +
  scale_color_manual(values = TrophiePalette) +
  facet_grid( depth ~  lakeClass) +
  theme_bw() +
  labs(title = "Tsteady",
    y = "Mean Biomass", x = "Days", color = "Spec. Group") 
p_macro_TS
ggsave(file.path(save_comparison, "biomass_day_baseTsteady.png"), p_macro_TS, height = 6, width = 8.5)

# ------------------------------------------------------------------------------
# Comparison 
colnames(sort_res_baseTP)
colnames(sort_res_baseTS)

res_combo <- sort_res_baseTP %>% 
  left_join(
    sort_res_baseTS,
    by = c("lakeClass", "speciesGroup", "AreaGroup", "depth", "day", "lakeDepth_mean", "Group"),
    suffix = c("_Tprofile", "_Tsteady")
  ) 
head(res_combo)

mean_res_combo <- res_combo %>%
  group_by(day, lakeClass, depth, speciesGroup) %>%
  summarise(bio_mean_Tprofile = mean(biomass_mean_Tprofile, na.rm = TRUE), 
            bio_mean_Tsteady = mean(biomass_mean_Tsteady, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(across(where(is.numeric), ~ replace_na(., 0)),
         diff = bio_mean_Tprofile - bio_mean_Tsteady) 
head(mean_res_combo)

# diff
p_compar_bio <- ggplot(data = mean_res_combo, aes(x = day, y = diff,
                                          color = speciesGroup)) +
  geom_line(linewidth = 0.8) +
  scale_color_manual(values = TrophiePalette) +
  facet_grid( depth ~  lakeClass) +
  theme_bw() +
  labs(title = "Tprofile - Tsteady",
    y = "Difference of Mean Biomass", x = "Days", color = "Spec. Group") 
p_compar_bio
ggsave(file.path(save_comparison, "biomass_day_compar.png"), p_compar_bio, height = 6, width = 8.5)

# added plots
# layout <- "
# A B
# C B
# "
p_TP <- p_macro_TP + theme(legend.position = "none") + guides(fill = "none", linetype = "none", color = "none", linewidth = "none", alpha = "none")
p_TS <- p_macro_TS + theme(legend.position = "none") +  guides(fill = "none", linetype = "none", color = "none", linewidth = "none", alpha = "none")
p_cb <- p_compar_bio + theme(legend.position = "none") +  guides(fill = "none", linetype = "none")

p_combo_bio <- ((p_TP/p_TS) | p_cb) +
  # plot_layout(design = layout) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = 'a',
                  tag_prefix = '(',
                  tag_suffix = ')')& 
  theme(plot.tag = element_text(size = 12))&  
  theme(legend.position = "bottom")&
  guides(colour = guide_legend(override.aes = list(size=3)))

p_combo_bio  
ggsave(file.path(save_comparison, "biomass_day_compar_all.png"), p_combo_bio, height = 7, width = 10)

