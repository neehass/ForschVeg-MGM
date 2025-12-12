# -------------------------------------------------------------------------------------------
# Data Preparation  Results
# -------------------------------------------------------------------------------------------
# Gradient potential species richness vs observed species richness in data&visual_specPot-Observed. R

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

# final projekt ---------------------------
# -----------------------------------------------------------------------------------------
# T-Profile + base scenario, 100 species, all lakes < -10 m depth -------------------------
# Folder output of MGM experiment and Analysis results folder

base_Tprofile <- "output/dep10_lakes_300spec_base_Tprofile"
save_base_Tprofile <- "output-analysis/dep10_lakes_300spec_base_Tprofile"

dir.create(save_base_Tprofile)

# input files
lake_path <- "input/lakes"

func_prep_data_dt_parallel(output = base_Tprofile, save_figures = save_base_Tprofile, lake_path, lewSpec_dir, ncores = NULL)
# func_prep_data_fast(output = base_Tprofile, save_figures = save_base_Tprofile, lake_path, lewSpec_dir)
# func_prep_data(output = base_Tprofile, save_figures = save_base_Tprofile, lake_path, lewSpec_dir)

# ---------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------------
# T-steady + base scenario, 100 species, all lakes < -10 m depth -------------------------
# Folder output of MGM experiment and Analysis results folder

base_Tsteady <- "output/dep10_lakes_300spec_base_Tsteady"
save_base_Tsteady <- "output-analysis/dep10_lakes_300spec_base_Tsteady"

dir.create(save_base_Tsteady)

# input files
lake_path <- "input/lakes"

func_prep_data_dt_parallel(output = base_Tsteady, save_figures = save_base_Tsteady, lake_path, lewSpec_dir, ncores = NULL)
# func_prep_data_fast(output = base_Tsteady, save_figures = save_base_Tsteady, lake_path, lewSpec_dir)
# func_prep_data(output = base_Tsteady, save_figures = save_base_Tsteady, lake_path, lewSpec_dir)
# ---------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------



# Test -------------------
# -----------------------------------------------------------------------------------------
# T-Profile + base scenario, 100 species, all lakes < -10 m depth -------------------------
# Folder output of MGM experiment and Analysis results folder

base_Tprofile <- "output/dep10_lakes_100spec_base_Tprofile"
save_base_Tprofile <- "output-analysis/dep10_lakes_100spec_base_Tprofile"

dir.create(save_base_Tprofile)

# input files
lake_path <- "input/lakes"

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

func_prep_data(output = base_Tsteady, save_figures = save_base_Tsteady, lake_path, lewSpec_dir)

# ---------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------
# Test -------------------------
# Folder output of MGM experiment and Analysis results folder

test <- "output/test_spec_14xxx"
save_test <- "output-analysis/test_spec_14xxx"

dir.create(save_test)

# input files
lake_path <- "input/lakes"

func_prep_data(output = test, save_figures = save_test, lake_path, lewSpec_dir)

# ---------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------

# Test Tprofile parallel final -------------------
# -----------------------------------------------------------------------------------------
# T-Profile + base scenario, 5 species, all lakes < -10 m depth -------------------------
# Folder output of MGM experiment and Analysis results folder

base_Tprofile_par <- "output/dep10_lakes_5spec_base_Tprofile_parallelNAME"
save_base_Tprofile_par <- "output-analysis/dep10_lakes_5spec_base_Tprofile_parallelNAME"

dir.create(save_base_Tprofile_par)

# input files
lake_path <- "input/lakes"

func_prep_data_dt_parallel(output = base_Tprofile_par, save_figures = save_base_Tprofile_par, lake_path, lewSpec_dir, ncores = NULL)

load(file.path(save_base_Tprofile_par, "sortENV_dep10.RData"))  
sort_env5 <- sort_env
rm(sort_env)
View(sort_env5)
scenario <- "base-5spec_Tprofile"
func_sortENV_plot(sort_env5, save_base_Tprofile_par, scenario) # defined in help-func.R

# ---------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------
