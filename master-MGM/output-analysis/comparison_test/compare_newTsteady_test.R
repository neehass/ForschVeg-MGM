# compare environmetn data of Tprofile and Tstead
# Tepi von Tsteady should be same as TEpi von Trpofile (if applied in Trpofile)


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
output <- "output/dep10_300spec_base_Tprofile_final"

out_ana_path <- "output-analysis/dep10_300spec_base_Tprofile_final"

save_figures <- "output-analysis/dep10_300spec_base_Tprofile_final/figures"

dir.create(save_figures)

# test Tsteady
output_TS <- "output/dep10_5spec_base_Tsteady_test"

out_ana_path_TS <- "output-analysis/dep10_5spec_base_Tsteady_test"

save_figures_TS <- "output-analysis/dep10_5spec_base_Tsteady_test/figures"

dir.create(save_figures_TS)

# input files
lake_path <- "input/lakes"

# ---------------------------------------------------------------------------------------------------------
# load data (Data preparation should be run before data_prep.R) ------------------------------------------------

# results 
sort_res <- readRDS(file.path(out_ana_path, "sortRES.rds")) # sort_res # biomass > 0 sorted macrophyte data (grouped by lakeClass, speciesGroup, lakeGroup_Area, depth, day)

# environment
sort_env <- readRDS(file.path(out_ana_path, "sortENV.rds")) # env_sort # sorted environmental data (grouped by llakeClass, lakeGroup_Area, day)

gen.conf <- readLines(file.path(output,"general.config.txt"))
k <- as.numeric(strsplit(gen.conf[8], " ")[[1]][2])

# ------------
# test tsteady
# results 
sort_res_TS <- readRDS(file.path(out_ana_path_TS, "sortRES.rds")) # sort_res # biomass > 0 sorted macrophyte data (grouped by lakeClass, speciesGroup, lakeGroup_Area, depth, day)

# environment
sort_env_TS <- readRDS(file.path(out_ana_path_TS, "sortENV.rds")) # env_sort # sorted environmental data (grouped by llakeClass, lakeGroup_Area, day)

gen.conf_TS <- readLines(file.path(output_TS,"general.config.txt"))

load(file.path(lewSpec_dir, "data-raw/observed/Morphology.rda"))
load(file.path(lewSpec_dir, "data/data_lakes_env_class.rda"))
# head(Morphology)
# head(data_lakes_env_class) # Turbidity classes 
# ------------------------------------------------------------------------------
# Tprofile:
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
    T_prof = T_profile(z = sort(0, decreasing = TRUE), 
                            T_epi = tempEpi_av, T_hypo = tempHypo_av,
                            z0 = metaDepth_av, k = k))  %>%
  ungroup()

# Tsteady: --------------------------------------------
sort_env_Tsteady_DAY <- sort_env_TS %>% # sort_env[sort_env$day %in% unique(sort_res$day), ] 
  group_by(lakeClass, AreaGroup, day) %>%
  # mutate(day_bin = floor((day - 1) / 30) * 30 + 1) %>%  # days 1–7 → 1, 8–14 → 8, etc.
  mutate(month_bin = floor((day - 1) / 31) + 1) %>%  # month 1, 2, 3...
  group_by(lakeClass, AreaGroup, month_bin) %>% 
  summarise(
    tempEpi_av = mean(tempEpi_mean, na.rm = TRUE),
    tempHypo_av = mean(tempHypo_mean, na.rm = TRUE),
    metaDepth_av = mean(metaDepth_mean, na.rm = TRUE),
    lakeDepth_av = round(mean(lakeDepth_mean, na.rm = TRUE)), 
    T_prof = T_profile(z = sort(0, decreasing = TRUE), 
                            T_epi = tempEpi_av, T_hypo = tempHypo_av,
                            z0 = metaDepth_av, k = k))  %>%
  ungroup()
# View(sort_env_Tprof_DAY)
length(sort_env_Tsteady_DAY$month_bin %>% unique())

# --------------------------------
# ------------------------------------------------------------
# compare Tsteady with Tprofile 
View(sort_env_Tprof_DAY)
View(sort_env_Tsteady_DAY)

any(sort_env_Tprof_DAY$T_prof == sort_env_Tsteady_DAY$T_prof)

#plot 
plot(sort_env_Tprof_DAY$month_bin, sort_env_Tprof_DAY$T_prof)
points(sort_env_Tsteady_DAY$month_bin, sort_env_Tsteady_DAY$T_prof, col = "red", pch = 4)
