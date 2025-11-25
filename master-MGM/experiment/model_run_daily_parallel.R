# ---------------------------------------------------------------------------------------------------
# Master script to run model for multiple species in multiple lakes
# ---------------------------------------------------------------------------------------------------
# CHARISMA_biomass_N_weight_hight_env()
# ---------------------------------------------------------------------------------------------------
# sel.general.config: Lakes: chiemsee (large) = 1, AbtsdorfSee (very.small) = 2, Eibsee (medium) = 3
# general.config: 
# ---------------------------------------------------------------------------------------------------

getwd() # "C:/Users/maiim/Documents/25-25SS/Forschungsprojekt_Vegetationskunde/MGM-scripte-data"
dir <- "C:/Users/student/Documents/Neele-ForschVeg-2025"
setwd(dir)
# Sys.getenv("PATH")
library(parallel)

# ---------------------------------------------------------------------------------------------------
## General configurations ----
# ---------------------------------------------------------------------------------------------------
machine <- "NoMachine" # "home" # NoMachine !! doesnt work yet !!
n <- 100 # number of species per group (oligotroph, mesotroph, eutroph)
# n <- 10

setting <- "parallel" # "HPC" # HPC, # parallel
modelrun <- "dep10_lakes_100spec_base_Tsteady" # dep10_lakes_100spec_base_Tprofile #"test_spec_14xxx" #  # "test_NoMachine" #Name of experiment
years <- 10 #Number of years to get simulated [n]
depths <- c(-0.5, -1.5, -3, -5) # -3.0, -5.0, # only 4 depths possible here
yearsoutput <- 2
tempProfile <- "false" # "false" true
k <- 5

# (full path needed!)
if(machine == "home") {
  print("home")
  setwd("C:/Users/maiim/Documents/25-25SS/Forschungsprojekt_Vegetationskunde/scripte-data-MGM/master-MGM/experiment")
  source("C:/Users/maiim/Documents/25-25SS/Forschungsprojekt_Vegetationskunde/scripte-data-MGM/master-MGM/experiment/help-func.r")
  library(stringr)
  
  HypoFrac_dir <- "C:/Users/maiim/Documents/25-25SS/Forschungsprojekt_Vegetationskunde/scripte-data-MGM/master-MGM/input/lakeFractionParameters/HypoTemp_fraction.config.txt"
  MetaFrac_dir <- "C:/Users/maiim/Documents/25-25SS/Forschungsprojekt_Vegetationskunde/scripte-data-MGM/master-MGM/input/lakeFractionParameters/MetaDepth_fraction.config.txt"
  
  species_path <- "C:/Users/maiim/Documents/25-25SS/Forschungsprojekt_Vegetationskunde/scripte-data-MGM/master-MGM/input/species"
  
  juliaDIR <- "C:/Users/maiim/AppData/Local/Programs/Julia-1.11.5/bin"
  # juliaHPC
  
} else if (machine == "NoMachine") {
  print("NoMachine")
  
  setwd("C:/Users/student/Documents/Neele-ForschVeg-2025/ForschVeg-MGM/master-MGM/experiment") # NoMachine
  source("C:/Users/student/Documents/Neele-ForschVeg-2025/ForschVeg-MGM/master-MGM/experiment/help-func.r")
  
  
  HypoFrac_dir <- "./input/lakeFractionParameters/HypoTemp_fraction.config.txt"
  MetaFrac_dir <- "./input/lakeFractionParameters/MetaDepth_fraction.config.txt"
  
  species_path <- "C:/Users/student/Documents/Neele-ForschVeg-2025/ForschVeg-MGM/master-MGM/master-MGM/input/species"
  
} else {print("define dir")}

# species selection -----------------------
# first run, select species randomly
# species <- func_sel_spec(n, species_path) 

# second run/ want to take same species as path_configFile
path_configFile <- "C:/Users/student/Documents/Neele-ForschVeg-2025/ForschVeg-MGM/master-MGM/output/dep10_lakes_100spec_base_Tprofile/general.config.txt"
species <- func_getSpecies_config(path_configFile)
ex <- paste0("species_", c(14249, 14264, 14121, 14233, 14251,
                           15040, 15044, 15174, 15191,
                           16043, 16231, 16233, 16299, 16300)) # error check
species <- species[!species %in% ex]

# species ID
species_id <- unlist(str_extract_all(species, "\\d+"))
species_id <- as.numeric(species_id)
if(length(species) != 300){stop("stop species ERORR")}

# test species
# ERROR: reproDay < germinationDay + seedsEndAge
# ./input/species/species_14249.config.txt
# ./input/species/species_14264.config.txt
# ./input/species/species_14121.config.txt
# ./input/species/species_14233.config.txt
# ./input/species/species_14251.config.txt

# ./input/species/species_15040.config.txt
# ./input/species/species_15044.config.txt
# ./input/species/species_15174.config.txt
# ./input/species/species_15191.config.txt

# ./input/species/species_16043.config.txt
# ./input/species/species_16231.config.txt
# ./input/species/species_16233.config.txt
# ./input/species/species_16299.config.txt
# ./input/species/species_16300.config.txt

# species_id <- c(16001:16300)
# species_id <- species_id[species_id > 16299]
# species_id <- 14121
# species <- paste0("species_", species_id)

# lakes -----------------------
lakes <- c(1:31) # c(6,1,7) 
exclude <- c(11, 12, 30, 4) # # ID (11, 12, 30, 4) > -10 Depth (excluding)
lakes <- lakes[!lakes %in% exclude]
#nthreads = 6 #Set number of of kernels to be used in julia; max nlakes*ndepths
detectable=1
lakestemplate = "reallakes_simplifiedVersion"

# ---------------------------------------------------------------------------------------------------
## Julia and R setup ----
# ---------------------------------------------------------------------------------------------------
# CORES
# if (setting =="HPC") Sys.setenv(JULIA_NUM_THREADS = nthreads) #Sets number of threads
# if(setting =="HPC") Sys.setenv(JULIA_NUM_THREADS = "8") # set cores for parallelizing
# if (setting =="local") Sys.setenv(JULIA_NUM_THREADS = "1")
# Sys.getenv("JULIA_NUM_THREADS")
# Sys.getenv()

# Setup integration of julia
# install.packages("JuliaCall")
library(JuliaCall)

#if (setting =="HPC") julia_setup(JULIA_HOME = "/home/anl85ck/.julia/bin",installJulia = F) #on HPC
# if ("julia" %in% ls()) {
#   julia_exit()  # terminates the running Julia session
# }

if (setting == "local") {
  julia_setup(
    JULIA_HOME = juliaDIR,
    installJulia = FALSE,
    verbose = TRUE
  )
} else if (setting == "NoMAchine") { # still doeasnt work!!
  julia_setup()
}

# julia_command("using Base.Threads")
julia_command("Threads.nthreads()") #check N threads

# Load julia packages
# Installiere externe Pakete (nur einmal)
# julia_command("using Pkg")
# julia_command('Pkg.add("CSV")')
# julia_command('Pkg.add("DataFrames")')
# julia_command('Pkg.add("Distributions")')
# julia_command('Pkg.add("StatsBase")')

#julia_library("HCubature")
julia_library("DelimitedFiles")
julia_library("Dates")
julia_library("Distributions")
julia_library("Random")
julia_library("CSV")
julia_library("DataFrames")
julia_library("StatsBase")
julia_library("Base.Threads") # parallel

# R Packages
# install.packages(c("tidyverse", "DEoptim", "data.table", "here"))
library(tidyverse)
library(DEoptim)
library(data.table)
library(here)

# -----------------------------------------------------------------------------------------------
#  Settings for Model ----
# -----------------------------------------------------------------------------------------------
#Scenario
scenarios <- data.table(
  #para=c("maxTemp","maxNutrient", "maxKd"),
  base=c(0.0, 0.0, 0.0) #,  
  #BLIZ_2.6_Biodiv=c(0.5, -0.25, -0.25),
  #BLIZ_2.6_Mit=c(0.5, 0.25, 0.25),
  #BLIZ_2.6_Adap=c(0.5, 0.0, 0.0),
  #BLIZ_8.6_Biodiv=c(3.0, 0.0, 0.0),
  #BLIZ_8.6_Mit=c(3.0, 0.5, 0.5),
  #BLIZ_8.6_Adap=c(3.0, 0.25, 0.25)
)

# Set working directories 
if ((setting == "local") | (setting == "parallel")){ # Local Machine{
  setwd('../')
  wd<-getwd()
  if (str_sub(wd,-3,-1) != "MGM") { #moves you to project folder containing MGM
    print("Wrong path!")
    quit(save="no")
  }
  print(wd)
}

if (setting == "HPC"){ # High Performance Computing — a server or cluster environment
  wd<-here::here()
  setwd(wd)
  print(wd)
  #if (str_sub(wd, start= -9) != "2_Macroph") print("Wrong path!")
}

# Import julia functions
julia_source("model/CHARISMA_function.jl")
julia_source("model/structs.jl")
julia_source("model/defaults.jl")
julia_source("model/input.jl")
julia_source("model/functions.jl")
julia_source("model/run_simulation.jl")
julia_source("model/output.jl")

# ---------------------------------------------------------------------------
##  Model run ----
# Returns: Daily Biomass, Number of Individuals, indWeight, Height, 
# for all lakes, species, and multiple depths in the last year of started simulation
# ---------------------------------------------------------------------------

for (S in 1:length(scenarios)){
  
  # lake scenario change
  change<-as.array(scenarios[[S]])
  scenario_name<-colnames(scenarios)[S]
  
  # adapt lake config files
  # add depth & Areakm2 in template 
  for (N in 1:31){
    # Import template for lakes
    lak <- read.table(paste0(wd,"/input/template/lakes/lake_",N,".config.txt"), 
                      header = F, comment.char="#")
    
    lak[lak$V1=="maxTemp",]$V2 <- sprintf("%.1f",
                                          round((as.numeric(lak[lak$V1=="maxTemp",]$V2)+
                                                   change[1]),1))
    lak[lak$V1=="maxNutrient",]$V2 <- as.numeric(lak[lak$V1=="maxNutrient",]$V2)+
      (as.numeric(lak[lak$V1=="maxNutrient",]$V2) *
         change[2])
    lak[lak$V1=="maxKd",]$V2 <- as.numeric(lak[lak$V1=="maxKd",]$V2)+
      (as.numeric(lak[lak$V1=="maxKd",]$V2) *
         change[3])
    lak[lak$V1=="minKd",]$V2 <- lak[lak$V1=="maxKd",]$V2
    
    # Write adapted lake config file
    data.table::fwrite(lak, 
                       file=paste0(wd,"/input/lakes/lake_",N,".config.txt"), 
                       col.names=F, sep = " ")
  } # adapt lake - scenario
  
  
  # --- print general config file 
  L1 <- c()
  S1 <- c()
  for (l in 1:length(lakes)) {
    lake = paste("./input/lakes/lake_", lakes[l], ".config.txt", sep = "")
    L1 <- cbind(L1, lake)
  }
  for (l in 1:length(species)) {
    spec <- paste("./input/species/", species[l], ".config.txt", sep = "")
    S1 <- cbind(S1, spec)
  }
  
  to_print <- c(
    paste0("modelrun ", paste0(modelrun, collapse = " ")),
    paste0("years ", paste0(years, collapse = " ")),
    paste0("depths ", paste0(depths, collapse = " ")),
    paste0("yearsoutput ", paste0(yearsoutput, collapse = " ")),
    paste0("lakes ", paste0(L1, collapse = " ")),
    paste0("species ", paste0(S1, collapse = " ")),
    paste0("tempProfile ",  paste0(tempProfile, collapse = " ")),
    paste0("k ",  paste0(k, collapse = " ")),
    paste0("HypoFrac_dir ",  paste0(HypoFrac_dir, collapse = " ")),
    paste0("MetaFrac_dir ",  paste0(MetaFrac_dir, collapse = " "))
  )
  
  # save general.config-file
  writeLines(text = to_print)
  writeLines(text = to_print,
             con = paste0(wd, "/input/general.config.txt"))
  
  if(!dir.exists("output")){dir.create("output")}
  if(!dir.exists(paste0("output/",modelrun))){dir.create(paste0("output/",modelrun))}
  writeLines(text = to_print,
             con =paste0(wd,"/output/",modelrun,"/general.config.txt"))
  
  # ---------------------------------------------------------------------------
  # Model run -------------
  print("start model")
  start_time <- Sys.time()
  if(setting == "parallel"){
    model2 <- julia_eval("CHARISMA_biomass_N_weight_hight_env_parallel()")
  } else if (setting == "local") {
    model2 <- julia_eval("CHARISMA_biomass_N_weight_hight_env()")
  }
  
  comb_l_s <- seq(1, length(model2), by = 2)
  
  end_time <- Sys.time()
  time <- end_time - start_time
  
  print(paste("modle run", modelrun, time))
  
  # ---------------------------------------------------------------------------
  # save macrophyte data for all lakes, species, depths
  print("data saving")
  
  # ---------------------------------------------------------------------------
  # Set number of cores
  ncores <- detectCores() - 1
  cl <- makeCluster(ncores)
  clusterExport(cl, varlist = c("model2", "species_id", "lakes", "depths",
                                "scenario_name", "comb_l_s", "comb_env"))
  
  # ---------------------- Macrophyte Data -------------------------------------
  print("saving macrophyte data")
  
  start_time_dat <- Sys.time()
  
  # Function to process a single combination
  process_macrophyte <- function(i){
    idx <- ((i - 1) / 2) + 1
    l_ix <- ceiling(idx / length(species_id))
    s_ix <- idx - (l_ix - 1) * length(species_id)
    
    res <- model2[[i]]
    df_list <- vector("list", 4)
    
    for(d in 1:4){
      df <- as.data.frame(res[[d]])
      colnames(df) <- c("biomass", "numberInd", "indWeight", "height")
      df$depth <- depths[d]
      df$speciesID <- species_id[s_ix]
      df$lakeID <- lakes[l_ix]
      df$day <- 1:365
      df$scenario <- scenario_name
      df_list[[d]] <- df
    }
    
    do.call(rbind, df_list)
  }
  
  # Parallel processing
  all_df_list <- parLapply(cl, comb_l_s, process_macrophyte)
  all_df <- do.call(rbind, all_df_list)
  
  # Save
  write.table(all_df, file = paste0(wd,"/output/",modelrun,"/all_res_biomass_number_weight_height_daily.txt"),
              col.names = TRUE, row.names = FALSE)
  
  print("macrophyte data saved")
  
  # ---------------------- Environment Data -----------------------------------
  print("saving environment data")
  
  # Function to process a single environment combination
  process_env <- function(pos){
    i <- comb_l_s[pos]
    idx <- ((i - 1) / 2) + 1
    l_ix <- ceiling(idx / length(species_id))
    s_ix <- idx - (l_ix - 1) * length(species_id)
    
    res <- model2[[comb_env[pos]]]
    df <- as.data.frame(matrix(nrow = 365, ncol = 6))
    env_par <- c("tempEpi", "tempHypo", "metaDepth", "irradiance", "waterlevel", "lightAttenuation")
    
    for(d in 1:6){
      df[, d] <- as.vector(res[[d]])
    }
    colnames(df) <- env_par
    df$speciesID <- species_id[s_ix]
    df$lakeID <- lakes[l_ix]
    df$day <- 1:365
    df$scenario <- scenario_name
    df
  }
  
  # Parallel processing
  all_env_list <- parLapply(cl, seq_along(comb_env), process_env)
  all_env <- do.call(rbind, all_env_list)
  
  # Save
  write.table(all_env, file = paste0(wd,"/output/",modelrun,"/env.txt"),
              col.names = TRUE, row.names = FALSE)
  
  print("environment data saved")
  
  # Stop cluster
  stopCluster(cl)
  
  end_time_dat <- Sys.time()
  timedat <- end_time_dat - start_time_dat
  
  print(paste("data saving time:", timedat))
  print(paste("modle run", modelrun, time))
  print(paste("totale time", modelrun, time + timedat))
} # Scenario loop

