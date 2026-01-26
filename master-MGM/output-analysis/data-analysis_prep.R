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
# T-Profile + base scenario, 0.5-5m, 900species, all lakes < -10 m depth (27) -------------------------
# Folder output of MGM experiment and Analysis results folder

base_Tprofile <- "output/dep10_300spec_base_Tprofil_20years" # dep10_300spec_base_Tprofile_final2.0
save_base_Tprofile <- "output-analysis/dep10_300spec_base_Tprofil_20years"
nameTP <- "Tprofile_20years"

dir.create(save_base_Tprofile)

# input files
lake_path <- "input/lakes"

func_prep_data(output = base_Tprofile, save_figures = save_base_Tprofile, lake_path, lewSpec_dir)

func_prep_DDG(output = base_Tprofile, lewSpec_dir, save_out = paste0(save_base_Tprofile, "/DDG"), name1 = nameTP)

# slower beacause data is loaded to each core
#func_prep_data_dt_parallel(output = base_Tprofile, save_figures = save_base_Tprofile, lake_path, lewSpec_dir, ncores = NULL)
# func_prep_data_fast(output = base_Tprofile, save_figures = save_base_Tprofile, lake_path, lewSpec_dir)

# deep [5.5-9m] -------------------------------------------------------------------
baseTP_deep <- "output/dep10_300spec_base_Tprofile_20years_deep"
save_baseTP_deep <- "output-analysis/dep10_300spec_base_Tprofile_20years_deep"
nameTP <- "Tprofile_20years_deep"

dir.create(save_baseTP_deep)

# input files
lake_path <- "input/lakes"

func_prep_data(output = baseTP_deep, save_figures = save_baseTP_deep, lake_path, lewSpec_dir)

func_prep_DDG(output = baseTP_deep, lewSpec_dir, save_out = paste0(save_baseTP_deep, "/DDG"), name1 = nameTP)

# --------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------
# T-steady + base scenario, 0.5-5m, 900species, all lakes < -10 m depth (27) -------------------------
# Folder output of MGM experiment and Analysis results folder

base_Tsteady <- "output/dep10_300spec_base_Tsteady_20years" # dep10_300spec_base_Tsteady_final2.0 # 
save_base_Tsteady <- "output-analysis/dep10_300spec_base_Tsteady_20years"
nameTS <- "Tsteady_20years"

dir.create(save_base_Tsteady)

# input files
lake_path <- "input/lakes"

func_prep_data(output = base_Tsteady, save_figures = save_base_Tsteady, lake_path, lewSpec_dir)

func_prep_DDG(output = base_Tsteady, lewSpec_dir, save_out = paste0(save_base_Tsteady, "/DDG"), name1 = nameTS)
  
# func_prep_data_dt_parallel(output = base_Tsteady, save_figures = save_base_Tsteady, lake_path, lewSpec_dir, ncores = NULL)
# func_prep_data_fast(output = base_Tsteady, save_figures = save_base_Tsteady, lake_path, lewSpec_dir)

# deep [5.5-9m] -------------------------------------------------------------------
baseTS_deep <- "output/dep10_300spec_base_Tsteady_20years_deep"
save_baseTS_deep <- "output-analysis/dep10_300spec_base_Tsteady_20years_deep"
nameTS <- "Tsteady_20years_deep"
dir.create(save_baseTS_deep)

# input files
lake_path <- "input/lakes"

func_prep_data(output = baseTS_deep, save_figures = save_baseTS_deep, lake_path, lewSpec_dir)

func_prep_DDG(output = baseTS_deep, lewSpec_dir, save_out = paste0(save_baseTS_deep, "/DDG"), name1 = nameTS)

# ---------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------
# 20 years tprofile plus 5 degree ----------------------------------------
# 20 YEARS T-Profile + base scenario, 100 species, all lakes < -10 m depth  20 YEARS-------------------------
# Folder output of MGM experiment and Analysis results folder

base_Tprofile20_plus5 <- "output/dep10_300spec_base_Tprofil_20years_plus5"
save_base_Tprofile20_plus5 <- "output-analysis/dep10_300spec_base_Tprofil_20years_plus5"

dir.create(save_base_Tprofile20_plus5)

# input files
lake_path <- "input/lakes"

func_prep_data(output = base_Tprofile20_plus5, save_figures = save_base_Tprofile20_plus5, lake_path, lewSpec_dir)
# slower beacause data is loaded to each core
#func_prep_data_dt_parallel(output = base_Tprofile, save_figures = save_base_Tprofile, lake_path, lewSpec_dir, ncores = NULL)
# func_prep_data_fast(output = base_Tprofile, save_figures = save_base_Tprofile, lake_path, lewSpec_dir)

func_prep_DDG(output = base_Tprofile20_plus5, lewSpec_dir, save_out = paste0(save_base_Tprofile20_plus5, "/DDG"), name1 = "Tprofile_20years_plus5")

# ---------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------
# Test -------------------
# -----------------------------------------------------------------------------------------
# test oligotroph species T-Profile + 5 scenario, 100 species, all lakes < -10 m depth -------------------------
# Folder output of MGM experiment and Analysis results folder
base_Tprofile <- "output/test_oli_plus5"
save_base_Tprofile <- "output-analysis/test_oli_plus5"

dir.create(save_base_Tprofile)

# input files
lake_path <- "input/lakes"

func_prep_data(output = base_Tprofile, save_figures = save_base_Tprofile, lake_path, lewSpec_dir)

# Test -------------------
# -----------------------------------------------------------------------------------------
# test oligotroph species T-Profile + 5 scenario, 100 species, all lakes < -10 m depth -------------------------
# Folder output of MGM experiment and Analysis results folder
base_Tprofile <- "output/test_oli_plus5_notparallel"
save_base_Tprofile <- "output-analysis/test_oli_plus5_notparallel"

dir.create(save_base_Tprofile)

# input files
lake_path <- "input/lakes"

func_prep_data(output = base_Tprofile, save_figures = save_base_Tprofile, lake_path, lewSpec_dir)

# TEst -----------------------------
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
# test Tsteady after adjusting Tfunction for surface 
# trying to have the sam esurface temp wie in Tprofile

base_Tsteady <- "output/dep10_5spec_base_Tsteady_test"
save_base_Tsteady <- "output-analysis/dep10_5spec_base_Tsteady_test"

dir.create(save_base_Tsteady)

# input files
lake_path <- "input/lakes"

func_prep_data(output = base_Tsteady, save_figures = save_base_Tsteady, lake_path, lewSpec_dir)

