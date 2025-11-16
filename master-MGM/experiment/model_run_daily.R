# ---------------------------------------------------------------------------------------------------
# Master script to run model for multiple species in multiple lakes
# ---------------------------------------------------------------------------------------------------
# CHARISMA_biomass_N_weight_hight_env()
# ---------------------------------------------------------------------------------------------------
# sel.general.config: Lakes: chiemsee (large) = 1, AbtsdorfSee (very.small) = 2, Eibsee (medium) = 3
# general.config: Lakes: chiemsee (large) = 6, AbtsdorfSee (very.small) = 1, Eibsee (medium) = 7
# ---------------------------------------------------------------------------------------------------

getwd() # "C:/Users/maiim/Documents/25-25SS/Forschungsprojekt_Vegetationskunde/MGM-scripte-data"
Sys.getenv("PATH")
# ---------------------------------------------------------------------------------------------------
## General configurations ----
# ---------------------------------------------------------------------------------------------------
machine <- "home" # NoMachine !! doesnt work yet !!
n <- 50 # number of species per group (oligotroph, mesotroph, eutroph)
n <- 100

setting <- "local" # "HPC" # HPC = parallel
modelrun <- "all_lakes_100spec_base_Tprofile" #"test_spec_14xxx" #  # "test_NoMachine" #Name of experiment
years <- 10 #Number of years to get simulated [n]
depths <- c(-0.5, -1.5, -3, -5) # -3.0, -5.0, # only 4 depths possible here
yearsoutput <- 2
tempProfile <- "true" # "false"
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

  setwd("/media/ifgg1/E: data (1 TB)/Neele/Forsch_Veg/ForschVeg-MGM/master-MGM/experiment") # NoMachine
  source("/media/ifgg1/E: data (1 TB)/Neele/Forsch_Veg/ForschVeg-MGM/master-MGM/experiment/help-func.r")
  .libPaths(c("/home/ifgg1/R/x86_64-pc-linux-gnu-library/4.2", "/home/ifgg1/R/x86_64-pc-linux-gnu-library/4.5")) # packages Path NoMachine
  # install.packages("Rcpp") # fix install.packages bug
  library(stringr)
  Sys.setenv(PATH = paste("/opt/julia-1.12.1/bin", Sys.getenv("PATH"), sep=":"))

  HypoFrac_dir <- "/media/ifgg1/E: data (1 TB)/Neele/Forsch_Veg/ForschVeg-MGM/master-MGM/input/lakeFractionParameters/HypoTemp_fraction.config.txt"
  MetaFrac_dir <- "/media/ifgg1/E: data (1 TB)/Neele/Forsch_Veg/ForschVeg-MGM/master-MGM/input/lakeFractionParameters/MetaDepth_fraction.config.txt"

  species_path <- "/media/ifgg1/E: data (1 TB)/Neele/Forsch_Veg/ForschVeg-MGM/master-MGM/input/species"

  juliaDIR <- "/opt/julia-1.12.1/bin"
  juliaHPC <- "/opt/julia-1.12.1/bin"

  # Detect Julia path
  juliaDIR <- find_julia()
  message("Using Julia at: ", juliaDIR)
  Sys.setenv(JULIA_HOME = juliaDIR)

} else {print("define dir")}

# species selection -----------------------
species <- func_sel_spec(n, species_path)
ex <- paste0("species_", c(14249, 14264, 14121, 14233, 14251,
                          15040, 15044, 15174, 15191,
                          16043, 16231, 16233, 16299, 16300)) # error check
species <- species[!species %in% ex]
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
if (setting =="HPC") Sys.setenv(JULIA_NUM_THREADS = nthreads) #Sets number of threads
if(setting =="HPC") Sys.setenv(JULIA_NUM_THREADS = "8") # set cores for parallelizing
if (setting =="local") Sys.setenv(JULIA_NUM_THREADS = "1")
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
  Sys.setenv(JULIA_HOME = juliaDIR)
  julia_setup(installJulia = FALSE)

}

# julia_command("using Base.Threads")
julia_command("Threads.nthreads()") #check N threads

# Load julia packages
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
scenarios <-data.table(
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
if (setting =="local") {
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
  start_time <- Sys.time()
  if(setting == "HPC"){
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
  all_df <- data.frame()
  for(i in comb_l_s){
  # figure out which lake/species this corresponds to
    idx <- ((i - 1) / 2) + 1  # combination index
    l_ix <- ceiling(idx / length(species))
    s_ix <- idx - (l_ix - 1) * length(species)
    #print(paste(i, l_ix, s_ix))

    # print(paste("res",i, "lake", lakes[l_ix], "species", species_id[s_ix]))

    # data
    res <- model2[[i]]

    # depth dataframe
    for(d in 1:4){
      # print(paste("depth",depths[d]))
      df <- as.data.frame(res[[d]])
      colnames(df) <- c("biomass", "numberInd", "indWeight", "height")
      df$depth <- depths[d]
      df$speciesID <- species_id[s_ix]
      df$lakeID <- lakes[l_ix]
      df$day <- 1:365
      df$scenario <- scenario_name

      all_df <- rbind(all_df, df)
    }
  } # save macrophyt

  # unique(all_df$speciesID)
  # View(all_df)
  write.table(all_df, file = paste0(wd,"/output/",modelrun,"/all_res_biomass_number_weight_height_daily.txt"), 
              col.names = T, row.names = F) 
  print("macrophyt data saved")
  # save environment data ------------------------------------------------------
  # tempEpi, tempHypo, metaDepth, irradiance, waterlevel, lightAttenuation
  comb_env <- seq(2, length(model2), by = 2)
  all_env <- data.frame()
  env_par <-  c("tempEpi", "tempHypo", "metaDepth", "irradiance", "waterlevel", "lightAttenuation")

  print("saving env data")
  for(pos in 1:length(comb_l_s)){

  # figure out which lake/species this corresponds to
    i <- comb_l_s[pos]
    idx <- ((i - 1) / 2) + 1  # combination index
    l_ix <- ceiling(idx / length(species))
    s_ix <- idx - (l_ix - 1) * length(species)
    #print(paste(comb_env[pos], l_ix, s_ix))

    #print(paste("res",i, "lake", lakes[l_ix], "species", species_id[s_ix]))

    p <- comb_env[pos]
    # data
    res <- model2[[p]]

    # env dataframe
    df <- data.frame(matrix(nrow = 365, ncol = 6))

    for(d in 1:6){
      # print(paste("env_par", env_par[d]))
      df[, d] <- as.vector(res[[d]])
    }

    # set column names and extra info
    colnames(df) <- env_par
    df$speciesID <- species_id[s_ix]
    df$lakeID <- lakes[l_ix]
    df$day <- 1:365
    df$scenario <- scenario_name

    # append to master dataframe
    all_env <- rbind(all_env, df)

  } # svae env
  # View(all_env)
  write.table(all_env, file = paste0(wd,"/output/",modelrun,"/env.txt"), 
              col.names = T, row.names = F) 
  print("env data saved")
} # Scenario loop


# --------------------------
# # Species loop  
# all_res <- list()

# for (s in 1:length(species)) {
#   # Write species-specific config file
#   S1 <- paste("./input/species/",
#               species[s],
#               ".config.txt",
#               sep = "")
  
#   # Create temporary general config for each lake
#   for (l in 1:length(lakes)) {
#     L1 <- paste("./input/lakes/lake_", lakes[l], ".config.txt", sep = "")
    
#     to_print <- c(
#       paste0("modelrun ", paste0(modelrun, collapse = " ")),
#       paste0("years ", paste0(years, collapse = " ")),
#       paste0("depths ", paste0(depths, collapse = " ")),
#       paste0("yearsoutput ", paste0(yearsoutput, collapse = " ")),
#       paste0("lakes ", L1),
#       paste0("species ", S1),
#       paste0("tempProfile ", tempProfile),
#       paste0("HypoFrac_dir ", HypoFrac_dir),
#       paste0("MetaFrac_dir ", MetaFrac_dir)
#     )
    
#     # save general.config-file
#     writeLines(
#       text = to_print,
#       con = paste0(wd, "/input/general.config.txt")
#     )
    
#     print(paste("start model for", species[s], "& lake", lakes[l]))
    
#     ###################
#     # Run Model in Julia
#     model2 <- julia_eval("CHARISMA_biomass_N_weight_hight_env()")  # 4 depths defined in general.config.file
    
#     # output speichern 
#     res_list <- list()
#     res_list[["macrophyt"]] <- model2[[1]]
#     names(res_list[["macrophyt"]]) <- paste0("depth", depths)
    
#     # Save results
#     all_res[[paste0("species_", species_id[s], "_lake_", lakes[l])]] <- res_list
    
#   } # end lake
# } # end species

# names(all_res)
# # --- save output
# if(!dir.exists("output")){dir.create("output")}
# if(!dir.exists(paste0("output/",modelrun))){dir.create(paste0("output/",modelrun))}
# saveRDS(all_res, file = file.path(wd,"output",modelrun,"all_res_biomass_number_weight_height.RData"))

