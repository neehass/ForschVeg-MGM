# ---------------------------------------------------------------------------------------------------
# Master script to run model for multiple species in multiple lakes
# ---------------------------------------------------------------------------------------------------
# CHARISMA_biomass_N_weight_hight_env()
# ---------------------------------------------------------------------------------------------------
# sel.general.config: Lakes: chiemsee (large) = 1, AbtsdorfSee (very.small) = 2, Eibsee (medium) = 3
# general.config: 
# ---------------------------------------------------------------------------------------------------

library(parallel) 
library(future.apply)

# ---------------------------------------------------------------------------------------------------
## General configurations ----
# ---------------------------------------------------------------------------------------------------
machine <- "NoMachine" # "home" # NoMachine !! doesnt work yet !!
n <- 300 # number of species per group (oligotroph, mesotroph, eutroph)
# n <- 10

setting <- "parallel" # "HPC" # local # parallel
modelrun <- "dep10_300spec_base_Tprofile_final" # "test_data_paral" # "dep10_lakes_100spec_base_Tsteady" # dep10_lakes_100spec_base_Tprofile #"test_spec_14xxx" #  # "test_NoMachine" #Name of experiment
years <- 10 #Number of years to get simulated [n]
depths <- c(-0.5, -1.5, -3, -5) # only 4 depths possible here
yearsoutput <- 2
tempProfile <- "true" # "false" true
k <- 5

# set dir ------------------------------------------------------------------------------------------
if(machine == "home") {
  print("home")
  
  setwd("C:/Users/maiim/Documents/25-25SS/Forschungsprojekt_Vegetationskunde/scripte-data-MGM/master-MGM/experiment")
  source("C:/Users/maiim/Documents/25-25SS/Forschungsprojekt_Vegetationskunde/scripte-data-MGM/master-MGM/experiment/help-func.r")
  
  HypoFrac_dir <- "./input/lakeFractionParameters/HypoTemp_fraction.config.txt"
  MetaFrac_dir <- "./input/lakeFractionParameters/MetaDepth_fraction.config.txt"
  
  species_path <- "C:/Users/maiim/Documents/25-25SS/Forschungsprojekt_Vegetationskunde/scripte-data-MGM/master-MGM/input/species"
  
  juliaDIR <- "C:/Users/maiim/AppData/Local/Programs/Julia-1.11.5/bin"
  
} else if (machine == "NoMachine") {
  print("NoMachine")
  
  setwd("C:/Users/student/Documents/Neele-ForschVeg-2025/ForschVeg-MGM/master-MGM/experiment") # NoMachine
  source("C:/Users/student/Documents/Neele-ForschVeg-2025/ForschVeg-MGM/master-MGM/experiment/help-func.r")
  
  HypoFrac_dir <- "./input/lakeFractionParameters/HypoTemp_fraction.config.txt"
  MetaFrac_dir <- "./input/lakeFractionParameters/MetaDepth_fraction.config.txt"
  
  species_path <- "C:/Users/student/Documents/Neele-ForschVeg-2025/ForschVeg-MGM/master-MGM/input/species"
  
  setting <- "parallel"
  
} else {print("define dir")}

# species selection -----------------------
# first run, select species randomly
# species <- func_sel_spec(n, species_path)

# second run/ want to take same species as path_configFile
path_configFile <- "C:/Users/student/Documents/Neele-ForschVeg-2025/ForschVeg-MGM/master-MGM/output/dep10_lakes_100spec_base_Tprofile/general.config.txt" # 100 spec
path_configFile <- "C:/Users/student/Documents/Neele-ForschVeg-2025/ForschVeg-MGM/master-MGM/output/dep10_lakes_300spec_base_Tprofile/general.config.txt" # 300 spec

path_configFile <- "C:/Users/student/Documents/Neele-ForschVeg-2025/ForschVeg-MGM/master-MGM/output/dep10_lakes_5spec_base_Tprofile_parallel/general.config.txt"
species <- func_getSpecies_config(path_configFile)

# exlclude species
# ex <- paste0("species_", c(14249, 14264, 14121, 14233, 14251,
#                            15040, 15044, 15174, 15191,
#                            16043, 16231, 16233, 16299, 16300)) # error check # ERROR: reproDay < germinationDay + seedsEndAge
# species <- species[!species %in% ex]

# species ID
species_id <- unlist(str_extract_all(species, "\\d+"))
species_id <- as.numeric(species_id)
if(length(species) != 300){stop(paste("stop species ERORR", length(species)))}
NSpec <- length(species)

# test species --
# species_id <- c(16001:16300)
# species_id <- species_id[species_id > 16299]
# species_id <- 14121
# species <- paste0("species_", species_id)

# lakes -----------------------
lakes <- c(1:31) # c(1:31) # c(6,1,7) 
exclude <- c(11, 12, 30, 4) # # ID (11, 12, 30, 4) > -10 Depth (excluding)
lakes <- lakes[!lakes %in% exclude]
lakes_id <- unlist(str_extract_all(lakes, "\\d+"))

#nthreads = 6 #Set number of of kernels to be used in julia; max nlakes*ndepths
detectable=1
lakestemplate = "reallakes_simplifiedVersion"
Nlak <- length(lakes_id)

# ---------------------------------------------------------------------------------------------------
## Julia and R setup ----
# ---------------------------------------------------------------------------------------------------
# CORES
# should be set in CDM see Parallel_Eingabe.png before starting Rstudio/ VsCode !!

# Setup integration of julia
# install.packages("JuliaCall")
library(JuliaCall)

if (setting == "local") {
  julia_setup(
    JULIA_HOME = juliaDIR,
    installJulia = FALSE,
    verbose = TRUE
  )
} else if (setting == "NoMAchine") { 
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
    model2 <- julia_eval("CHARISMA_biomass_N_weight_hight_env_parallel_name()")
  } else if (setting == "local") {
    model2 <- julia_eval("CHARISMA_biomass_N_weight_hight_env()")
  }
  
  saveRDS(model2,  file = file.path(wd, "output", modelrun, "model.rds"))
  
  end_time <- Sys.time()
  time <- end_time-start_time
  secs <- as.numeric(time, units = "secs")
  
  hours   <- floor(secs / 3600)
  minutes <- floor((secs %% 3600) / 60)
  seconds <- round(secs %% 60)
  
  cat(sprintf("model run %s: %02d:%02d:%02d\n",
              modelrun, hours, minutes, seconds))
  
  # ---------------------------------------------------------------------------
  # save modeloutput -------------
  print("start saving modeloutput")
  start_time_s <- Sys.time()
  plan(multisession, workers = parallel::detectCores() - 1)
  
  mod_out <- func_PREPmodeloutput_final_para(model2, Nlak, NSpec, depths, species_id, lake_id, scenario_name)
  
  final_res <- do.call(rbind, mod_out$df_allRES)
  
  final_env <- do.call(rbind, mod_out$df_allENV)
  
  saveRDS(final_res, file = file.path(wd, "output", modelrun, "all_res.rds"))
  rm(final_res) # delete variable
  
  saveRDS(final_env, file = file.path(wd, "output", modelrun, "all_env.rds"))
  rm(final_env) # delete variable
  
  # time
  end_time_s <- Sys.time()
  time_s <- end_time_s-start_time_s
  secs_s <- as.numeric(time_s, units = "secs")
  
  hours_s   <- floor(secs_s / 3600)
  minutes_s <- floor((secs_s %% 3600) / 60)
  seconds_s <- round(secs_s %% 60)
  
  cat(sprintf("saving %s: %02d:%02d:%02d\n",
              modelrun, hours_s, minutes_s, seconds_s))
  
  # total time 
  end <- Sys.time()
  t <- end -start_time
  s <- as.numeric(t, units = "secs")
  
  h   <- floor(s / 3600)
  m <- floor((s %% 3600) / 60)
  s <- round(s %% 60)
  cat(sprintf("total time %s: %02d:%02d:%02d\n",
              modelrun, h, m, s))
  
} # Scenario loop
# ---------------------------------------------------------------------------------------------------------


# ---------------------------------------------------------------------------------------------------------
# inspect data structur ------------------
# ---------------------------------------------------------------------------------------------------------
View(model2)

mod_out <- func_PREPmodeloutput(model2, Nlak, NSpec, depths, species_id, lake_id, scenario_name) # not parallel

mod_out_par <- func_PREPmodeloutput_final_para(model2, Nlak, NSpec, depths, species_id, lake_id, scenario_name)

identical(do.call(rbind, mod_out_par$df_allRES), do.call(rbind, mod_out$df_allRES))  # TRUE if exactly the same



# load
model2 <- readRDS(file.path(wd, "output", modelrun, "model.rds"))

iCOMBO <- 1:(NSpec * Nlak)

## WICHTIG: model2 vorher aufteilen
model2_sub <- model2[iCOMBO]

plan()
length(model2_sub)
object.size(model2_sub)

chunk_size <- 200

chunks <- split(
  model2_sub,
  ceiling(seq_along(model2_sub) / chunk_size)
)

length(chunks)
# ~122 chunks
View(chunks)

worker_process_chunk <- function(chunk, scenario_name) {
  
  lapply(chunk, function(mod){ # wie foor loop chunk[[i]]
    
    nameLAK  <- mod$lake
    nameSPEC <- mod$species
    Lid <- unlist(stringr::str_extract_all(nameLAK, "\\d+"))
    Sid <- unlist(stringr::str_extract_all(nameSPEC, "\\d+"))
    
    res <- mod$results
    env <- mod$environment
    
    # -------------------------------
    # Depth results
    # -------------------------------
    df_depth <- vector("list", length(res))
    
    for(d in seq_along(res)){
      depth <- res[[d]]$depth
      
      data <- data.table::as.data.table(res[[d]]$data)
      colnames(data) <- c("biomass", "numberInd", "indWeight", "height")
      
      data[, depth := depth]
      data[, speciesID := Sid]
      data[, lakeID := Lid]
      data[, day := 1:365]
      data[, scenario := scenario_name]
      
      df_depth[[d]] <- data
    }
    
    depth_bind <- data.table::rbindlist(df_depth)
    
    # -------------------------------
    # ENV data
    # -------------------------------
    env_bind <- data.table::as.data.table(do.call(cbind, env))
    colnames(env_bind) <-
      c("tempEpi", "tempHypo", "metaDepth",
        "irradiance", "waterlevel", "lightAttenuation")
    
    env_bind[, speciesID := Sid]
    env_bind[, lakeID := Lid]
    env_bind[, day := 1:365]
    env_bind[, scenario := scenario_name]
    
    list(df_res = depth_bind, df_env = env_bind)
  })
}

func_PREPmodeloutput_final_para <- function(model2, Nlak, NSpec, depths,
                                            species_id, lake_id, scenario_name){
  
  iCOMBO <- 1:(NSpec * Nlak)
  model2_sub <- model2[iCOMBO]
  
  chunk_size <- 200
  chunks <- split( # split in smaller list with length 122
    model2_sub,
    ceiling(seq_along(model2_sub) / chunk_size)
  )
  
  results_parallel <- future_lapply(
    chunks,
    worker_process_chunk, # defined before
    scenario_name = scenario_name,
    future.globals = FALSE
  )
  
  results_parallel <- do.call(c, results_parallel)
  
  df_allRES <- lapply(results_parallel, `[[`, "df_res")
  df_allENV <- lapply(results_parallel, `[[`, "df_env")
  
  list(df_allRES = df_allRES, df_allENV = df_allENV)
}





