# ---------------------------------------------------------------------------------------------------
# Master script to run model for multiple species in multiple lakes
# ---------------------------------------------------------------------------------------------------
# Charisma_parrallel()
## Run model for multiple species in multiple lakes
## Parameter combinations have to be produced first with an extra script and added to the input folder
## Script runs all species in one lake per loop and writes ouput per lake
## Output mean summer biomass after a given number of years in given depths
## Output is written in folder output in a subfolder called after the name of the modelrun
# ---------------------------------------------------------------------------------------------------
# sel.general.config: Lakes: chiemsee (large) = 1, AbtsdorfSee (very.small) = 2, Eibsee (medium) = 3
# general.config: Lakes: chiemsee (large) = 6, AbtsdorfSee (very.small) = 1, Eibsee (medium) = 7
# ---------------------------------------------------------------------------------------------------

getwd() # "C:/Users/maiim/Documents/25-25SS/Forschungsprojekt_Vegetationskunde/MGM-scripte-data"
setwd("master-MGM/experiment")

# ---------------------------------------------------------------------------------------------------
## General configurations ----
# ---------------------------------------------------------------------------------------------------
setting <- "local" # "HPC"
modelrun <- "chiem_test_R_parallel" #Name of experiment
years <- 10 #Number of years to get simulated [n]
depths <- c(-0.5, -1.5, -3, -5) # -3.0, -5.0, # only 4 depths possible here
yearsoutput <- 2
tempProfile <- "true" # "false"
k <- 5 # steepness factor for tempprofile

# (full path needed!)
HypoFrac_dir <- "C:/Users/maiim/Documents/25-25SS/Forschungsprojekt_Vegetationskunde/scripte-data-MGM/master-MGM/input/lakeFractionParameters/HypoTemp_fraction.config.txt"
MetaFrac_dir <- "C:/Users/maiim/Documents/25-25SS/Forschungsprojekt_Vegetationskunde/scripte-data-MGM/master-MGM/input/lakeFractionParameters/MetaDepth_fraction.config.txt"

species_id <- 1:2 # c(1:5)
species <- paste0("species_", species_id)
lakes <- 6:7 # c(1:31)
#nthreads = 6 #Set number of of kernels to be used in julia; max nlakes*ndepths
detectable=1
lakestemplate = "reallakes_simplifiedVersion"

# ---------------------------------------------------------------------------------------------------
## Julia and R setup ----
# ---------------------------------------------------------------------------------------------------

# CORES
if (setting =="HPC") Sys.setenv(JULIA_NUM_THREADS = nthreads) #Sets number of threads
if (setting =="local") Sys.setenv(JULIA_NUM_THREADS = "1")
Sys.getenv("JULIA_NUM_THREADS")
Sys.getenv()


# Setup integration of julia
# install.packages("JuliaCall")
library(JuliaCall)
#if (setting =="HPC") julia_setup(JULIA_HOME = "/home/anl85ck/.julia/bin",installJulia = F) #on HPC

if (setting =="local") julia_setup(JULIA_HOME = "C:/Users/maiim/AppData/Local/Programs/Julia-1.11.5/bin", installJulia = F)
julia <- julia_setup(verbose = T, install = T)
julia_eval("Threads.nthreads()") #check N threads
#julia_command("Threads.nthreads()")


# Load julia packages
#julia_library("HCubature")
julia_library("DelimitedFiles")
julia_library("Dates")
julia_library("Distributions")
julia_library("Random")
julia_library("CSV")
julia_library("DataFrames")
julia_library("StatsBase")


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
scenarios<-data.table(
  #para=c("maxTemp","maxNutrient", "maxKd"),
  #base=c(0.0, 0.0, 0.0),  
  #BLIZ_2.6_Biodiv=c(0.5, -0.25, -0.25),
  #BLIZ_2.6_Mit=c(0.5, 0.25, 0.25),
  #BLIZ_2.6_Adap=c(0.5, 0.0, 0.0),
  #BLIZ_8.6_Biodiv=c(3.0, 0.0, 0.0),
  #BLIZ_8.6_Mit=c(3.0, 0.5, 0.5),
  BLIZ_8.6_Adap=c(3.0, 0.25, 0.25)
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

# ---------------------------------------------------------------------------------------------------
## Model run 
# Returns: Mean summer biomass for all lakes, species, and multiple depths ----
# ---------------------------------------------------------------------------------------------------

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
  }
  
  
  # Species loop  
  for (I in 1:length(species_id)){
    # Write general.config-file
    S1 <- paste("./input/species/",
                species[I],
                ".config.txt",
                sep = "")
    
    L1 <- c()
    for (n in 1:length(lakes)) {
      LAK = paste("./input/lakes/lake_", lakes[n], ".config.txt", sep = "")
      L1 <- cbind(L1, LAK)
    }

    to_print <- c(
      paste0("modelrun ", paste0(modelrun, collapse = " ")),
      paste0("years ", paste0(years, collapse = " ")),
      paste0("depths ", paste0(depths, collapse = " ")),
      paste0("yearsoutput ", paste0(yearsoutput, collapse = " ")),
      paste0("lakes ", paste0(L1, collapse = " ")),
      paste0("species ", paste0(S1, collapse = " ")),
      paste0("tempProfile ",  paste0(tempProfile, collapse = " ")),
      paste0("HypoFrac_dir ",  paste0(HypoFrac_dir, collapse = " ")),
      paste0("MetaFrac_dir ",  paste0(MetaFrac_dir, collapse = " "))
    )
    
    # save general.config-file
    writeLines(text = to_print)
    writeLines(text = to_print,
               con = paste0(wd, "/input/general.config.txt"))
    
    
    ###################
    # Run Model in julia
    model<-julia_eval("CHARISMA_biomass_parallel()") #? 4 depths defined in general.config.file?
    model<-as.data.table(model)
    
    setnames(model, c("V1","V2","V3","V4","V5","V6"), c(depths,"speciesID","lakeID")) # only 4 depths possible here
    

    for (d in 1:4){
      for (l in 1:dim(model)[1]){
        if(model[l,d, with=F]<1) { #if Biomass too small - not found
          model[l,d]=0
        }
      }
    }
    
    
    modelout <- melt(model, id=5:6)
    modelout$scenario<-scenario_name
    
    
    # data.table::fwrite(modelout, 
    #                   file=paste0(wd,"/scenarios/220608_BLIZSCENARIOS_2/results/",
    #                              species[I],"_",scenario_name,"_scenario.txt"), 
    #                 col.names=T, sep = " ")

    
    
    if(!dir.exists("output")){dir.create("output")}
    if(!dir.exists(paste0("output/",modelrun))){dir.create(paste0("output/",modelrun))}
    write.table(modelout, file = paste0(wd,"/output/",modelrun,"/result_species_",species[I],"_",scenario_name,".txt"), 
                col.names = T, row.names = F)
    # Returns: Mean summer biomass for all lakes, species, and multiple depths
    
  }
} # total scenario loop end

