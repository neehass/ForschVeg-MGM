# DATA PREP for Comparison analysis between different MGM experiment scenarios

# packages & functions
library(dplyr)
library(tidyr)
library(stringr)

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
out_Tprofile <- "output/test_oli_plus5_notparallel"
save_Tprofile <- "output-analysis/test_oli_plus5_notparallel"

dir.create(save_Tprofile)

save_comparison <- "output-analysis/test_oli_plus5_notparallel/DDG"
dir.create(save_comparison)

name1 <- "test_oli_plus5_notparallel"

# input files
lake_path <- "input/lakes"

# ---------------------------------------------------------------------------------------------------------
# load data --------------------------------------------------------------------------------
load(file.path(lewSpec_dir, "data/data_lakes_env_class.rda")) # lake info

# ---------------------------------------------------------------------------------------------------------
# Tprofile --------------------------------------------------------------------------------
# results 
res <- readRDS(file.path(out_Tprofile, "added_all_res.rds")) 

gen.conf <- readLines(file.path(out_Tprofile,"general.config.txt"))
k <- as.numeric(strsplit(gen.conf[8], " ")[[1]][2])

# ---------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------
# DATA PREP Comparision between T_profile vs without T_profile ------------------
# ...............
load(file.path(lewSpec_dir, "data/all_diff_presabs_tobase.rda"))
head(all_diff_presabs_tobase)
# bring data in this format

# ------------------------------------------------------------------------------------------
# prepare data for DDG
res1_prep <- res %>%
  group_by(lakeClass, speciesGroup, depth, lakeID, speciesID) %>%
  summarise(biomass = sum(biomass)) %>%  ungroup() %>%
  mutate(biomass_orig = biomass) %>%
  mutate(depth_label = paste0("depth_", dense_rank(abs(depth)))) %>% # abs wichitg hier sonst werden depths falsch herum zugeordnet!
  pivot_wider(
    names_from = depth_label,
    values_from = biomass
  ) %>% relocate(biomass_orig) %>%
  
  # Replace NA in all pivoted columns with 0
  replace_na(list(
    depth_1 = 0, # -0.5
    depth_2 = 0,
    depth_3 = 0,
    depth_4 = 0
  )) %>% 
  mutate(Biomass_cat = if_else(biomass_orig > 0, 1, 0, missing = 0))# %>% filter(biomass_orig!= 0)

res1_prep$Group <- as.factor(res1_prep$speciesGroup)
res1_prep$Species <- res1_prep$speciesID
res1_prep$Lake <- paste0("lake_", res1_prep$lakeID)
res1_prep$scenario <- name1

saveRDS(res1_prep, file = file.path(save_comparison, paste0("DDG_reshape_",name1,".rds")))


