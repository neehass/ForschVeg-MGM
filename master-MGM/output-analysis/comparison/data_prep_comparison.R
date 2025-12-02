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
base_Tprofile <- "output/dep10_lakes_300spec_base_Tprofile"
save_base_Tprofile <- "output-analysis/dep10_lakes_300spec_base_Tprofile"

base_Tsteady <- "output/dep10_lakes_300spec_base_Tsteady"
save_base_Tsteady <- "output-analysis/dep10_lakes_300spec_base_Tsteady"

# test <- "output/test_spec_14xxx"
# save_test <- "output-analysis/test_spec_14xxx"

dir.create(save_base_Tprofile)
dir.create(save_base_Tsteady)

save_comparison <- "output-analysis/comparison"
dir.create(save_comparison)

# input files
lake_path <- "input/lakes"

# ---------------------------------------------------------------------------------------------------------
# load data --------------------------------------------------------------------------------
load(file.path(lewSpec_dir, "data/data_lakes_env_class.rda")) # lake info

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

head(res_baseTS)

any(res_baseTS$biomass != res_baseTP$biomass)

# ---------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------
# DATA PREP Comparision between T_profile vs without T_profile ------------------
# ...............
load(file.path(lewSpec_dir, "data/all_diff_presabs_tobase.rda"))
head(all_diff_presabs_tobase)
# bring data in this format

# ------------------------------------------------------------------------------------------
# prepare base T_profile data
res_baseTprofile_prep <- res_baseTP %>%
  mutate(scenario = "base_Tprofile") %>%
  group_by(lakeID, speciesID, depth, speciesGroup, lakeClass) %>%
  summarise(
    Biomass_cat = if_else(mean(biomass, na.rm = TRUE) > 0, 1, 0, missing = 0))
head(res_baseTprofile_prep)
save(res_baseTprofile_prep, file = file.path(save_comparison, "res_baseTprofile_prep.rda"))

# prepare T_steady data
res_baseTSteady_prep <- res_baseTS %>%
  mutate(scenario = "base_Tsteady") %>%
  group_by(lakeID, speciesID, depth, speciesGroup, lakeClass) %>%
  summarise(
    Biomass_cat = if_else(mean(biomass, na.rm = TRUE) > 0, 1, 0, missing = 0))
head(res_baseTSteady_prep)
save(res_baseTSteady_prep, file = file.path(save_comparison, "res_baseTSteady_prep.rda"))


# combine both datasets
res_combined <- res_baseTprofile_prep %>%
  left_join(res_baseTSteady_prep, by = c("lakeID", "depth", "speciesID", "speciesGroup", "lakeClass"), 
            suffix = c("_baseTP", "_baseTS")) %>%
  mutate(
    Biomass_cat_baseTP = replace_na(Biomass_cat_baseTP, 0),
    Biomass_cat_baseTS   = replace_na(Biomass_cat_baseTS, 0)
  ) %>%
  rename(
    baseTP = Biomass_cat_baseTP,
    baseTS = Biomass_cat_baseTS
  )
head(res_combined)
any(is.na(res_combined))
unique(res_combined$baseTP)
unique(res_combined$baseTS)

any(res_combined$baseTP != res_combined$baseTS) # TRUE  -> mindestens ein Wert ist unterschiedlich
save(res_combined, file = file.path(save_comparison, "res_combined_base.rda"))

# ---------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------
# PERMANOVA DATA PREP BIOMASS Comparision between T_profile vs without T_profile ------------------

# ------------------------------------------------------------------------------------------
# prepare base T_profile data
res_baseTprofile_bio <- res_baseTP %>%
  mutate(scenario = "base_Tprofile") %>%
  group_by(lakeID, speciesID, depth, speciesGroup, lakeClass) %>%
  summarise(
    biomass = mean(biomass, na.rm = TRUE))
head(res_baseTprofile_bio)
save(res_baseTprofile_bio, file = file.path(save_comparison, "res_baseTprofile_bio.rda"))

# prepare T_steady data
res_baseTSteady_bio <- res_baseTS %>%
  mutate(scenario = "base_Tsteady") %>%
  group_by(lakeID, speciesID, depth, speciesGroup, lakeClass) %>%
  summarise(
    biomass = mean(biomass, na.rm = TRUE))
head(res_baseTSteady_prep)
save(res_baseTSteady_bio, file = file.path(save_comparison, "res_baseTSteady_bio.rda"))


# combine both datasets
res_combined_BIO <- res_baseTprofile_bio %>%
  left_join(res_baseTSteady_bio, by = c("lakeID", "depth", "speciesID", "speciesGroup", "lakeClass"), 
            suffix = c("_baseTP", "_baseTS")) %>%
  mutate(
    biomass_baseTP = replace_na(biomass_baseTP, 0),
    biomass_baseTS   = replace_na(biomass_baseTS, 0)
  ) %>%
  rename(
    baseTP = biomass_baseTP,
    baseTS = biomass_baseTS
  )
head(res_combined_BIO)
any(is.na(res_combined_BIO))
unique(res_combined_BIO$baseTP)
unique(res_combined_BIO$baseTS)

any(res_combined_BIO$baseTP != res_combined_BIO$baseTS) # TRUE  -> mindestens ein Wert ist unterschiedlich
save(res_combined_BIO, file = file.path(save_comparison, "res_combined_base_BIO.rda"))

# ---------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------
# DDG DATA PREP BIOMASS Comparision between T_profile vs without T_profile ------------------
# Depth diversity gradient of potential and observed species richness (%):

# prepare base T_profile data
res_reshape_Tprofile <- res_baseTP %>%
  group_by(lakeClass, speciesGroup, depth, lakeID, speciesID) %>%
  summarise(biomass = sum(biomass)) %>%  ungroup() %>%
  mutate(biomass_orig = biomass) %>%
  mutate(depth_label = paste0("depth_", dense_rank(depth))) %>%
  pivot_wider(
    names_from = depth_label,
    values_from = biomass
  ) %>%  relocate(biomass_orig) %>%
  
  # Replace NA in all pivoted columns with 0
  replace_na(list(
    depth_1 = 0,
    depth_2 = 0,
    depth_3 = 0,
    depth_4 = 0
  )) %>% 
  mutate(Biomass_cat = if_else(biomass_orig > 0, 1, 0, missing = 0))# %>% filter(biomass_orig!= 0)

res_reshape_Tprofile$Group <- as.factor(res_reshape_Tprofile$speciesGroup)
res_reshape_Tprofile$Species <- res_reshape_Tprofile$speciesID
res_reshape_Tprofile$Lake <- paste0("lake_", res_reshape_Tprofile$lakeID)
res_reshape_Tprofile$scenario <- "base_Tprofile"

head(res_reshape_Tprofile)
colnames(res_reshape_Tprofile)
save(res_reshape_Tprofile, file = file.path("DDG_res_reshape_Tprofile.rda"))

# prepare T_steady data
res_reshape_Tsteady <- res_baseTS %>%
  group_by(lakeClass, speciesGroup, depth, lakeID, speciesID) %>%
  summarise(biomass = sum(biomass)) %>%  ungroup() %>%
  mutate(biomass_orig = biomass) %>%
  mutate(depth_label = paste0("depth_", dense_rank(depth))) %>%
  pivot_wider(
    names_from = depth_label,
    values_from = biomass
  ) %>%  relocate(biomass_orig) %>%
  
  # Replace NA in all pivoted columns with 0
  replace_na(list(
    depth_1 = 0,
    depth_2 = 0,
    depth_3 = 0,
    depth_4 = 0
  )) %>% 
  mutate(Biomass_cat = if_else(biomass_orig > 0, 1, 0, missing = 0))# %>% filter(biomass_orig!= 0)

res_reshape_Tsteady$Group <- as.factor(res_reshape_Tsteady$speciesGroup)
res_reshape_Tsteady$Species <- res_reshape_Tsteady$speciesID
res_reshape_Tsteady$Lake <- paste0("lake_", res_reshape_Tsteady$lakeID)
res_reshape_Tsteady$scenario <- "base_Tsteady"

head(res_reshape_Tsteady)
colnames(res_reshape_Tsteady)
save(res_reshape_Tsteady, file = file.path("DDG_res_reshape_Tsteady.rda"))
