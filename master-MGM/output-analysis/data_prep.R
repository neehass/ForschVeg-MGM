# -------------------------------------------------------------------------------------------
# Data Preparation  Results
# -------------------------------------------------------------------------------------------
# projekt: all_lakes_100spec_base_Tprofile

# packages & functions
library(dplyr)
library(tidyr)
library(stringr)

source("C:/Users/maiim/Documents/25-25SS/Forschungsprojekt_Vegetationskunde/scripte-data-MGM/master-MGM/experiment/help-func.r")
source("C:/Users/maiim/Documents/25-25SS/Forschungsprojekt_Vegetationskunde/scripte-data-MGM/master-MGM/output-analysis/func_data_prep.R")

getwd()
dir <- "C:/Users/maiim/Documents/25-25SS/Forschungsprojekt_Vegetationskunde/scripte-data-MGM/master-MGM"
setwd(dir)

# -----------------------------------------------------------------------------------------
# T-Profile + base scenario, 100 species, all lakes < -10 m depth -------------------------
# Folder output of MGM experiment and Analysis results folder

base_Tprofile <- "output/dep10_lakes_100spec_base_Tprofile"
save_base_Tprofile <- "output-analysis/dep10_lakes_100spec_base_Tprofile"

dir.create(save_base_Tprofile)

# input files
lake_path <- "input/lakes"
lewSpec_dir <- "C:/Users/maiim/Documents/25-25SS/Forschungsprojekt_Vegetationskunde/scripte-data-MGM/LewerentzEtAl2023_ModelledMacrophyteSpeciesRichness-1.0"

func_prep_data(output = base_Tprofile, save_figures = save_base_Tprofile, lake_path, lewSpec_dir)

# ---------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------

# -----------------------------------------------------------------------------------------
# T-steady + base scenario, 100 species, all lakes < -10 m depth -------------------------
# Folder output of MGM experiment and Analysis results folder

base_Tsteady <- "output/dep10_lakes_100spec_base_Tsteady"
save_base_Tsteady <- "output-analysis/dep10_lakes_100spec_base_Tsteady"

dir.create(save_base_Tsteady)

# input files
lake_path <- "input/lakes"
lewSpec_dir <- "C:/Users/maiim/Documents/25-25SS/Forschungsprojekt_Vegetationskunde/scripte-data-MGM/LewerentzEtAl2023_ModelledMacrophyteSpeciesRichness-1.0"

func_prep_data(output = base_Tsteady, save_figures = save_base_Tsteady, lake_path, lewSpec_dir)

# ---------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------

test_Chiem_Abts_Eib_newSpecies
# -----------------------------------------------------------------------------------------
# Test -------------------------
# Folder output of MGM experiment and Analysis results folder

test <- "output/test_spec_14xxx"
save_test <- "output-analysis/test_spec_14xxx"

dir.create(save_test)

# input files
lake_path <- "input/lakes"
lewSpec_dir <- "C:/Users/maiim/Documents/25-25SS/Forschungsprojekt_Vegetationskunde/scripte-data-MGM/LewerentzEtAl2023_ModelledMacrophyteSpeciesRichness-1.0"

func_prep_data(output = test, save_figures = save_test, lake_path, lewSpec_dir)

# ---------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------





# Gradient potential species richness vs observed species richness  --------------------------------
# in data&visual_specPot-Observed. R