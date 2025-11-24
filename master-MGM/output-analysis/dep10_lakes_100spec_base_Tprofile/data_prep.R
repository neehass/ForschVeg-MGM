# -------------------------------------------------------------------------------------------
# Data Preparation  Results
# -------------------------------------------------------------------------------------------
# projekt: all_lakes_100spec_base_Tprofile

# packages & functions
library(dplyr)
library(stringr)

# library(tidyverse)
source("C:/Users/maiim/Documents/25-25SS/Forschungsprojekt_Vegetationskunde/scripte-data-MGM/master-MGM/experiment/help-func.r")

getwd()
dir <- "C:/Users/maiim/Documents/25-25SS/Forschungsprojekt_Vegetationskunde/scripte-data-MGM/master-MGM"
setwd(dir)

# Folder output of MGM experiment and Analysis results folder
output <- "output/dep10_lakes_100spec_base_Tprofile"

save_figures <- "output-analysis/dep10_lakes_100spec_base_Tprofile"

dir.create(save_figures)

# input files
lake_path <- "input/lakes"
lewSpec_dir <- "C:/Users/maiim/Documents/25-25SS/Forschungsprojekt_Vegetationskunde/scripte-data-MGM/LewerentzEtAl2023_ModelledMacrophyteSpeciesRichness-1.0"

# ---------------------------------------------------------------------------------------------------------
# load data --------------------------------------------------------------------------------
res <- read.table(file.path(output, "all_res_biomass_number_weight_height_daily.txt"), header =TRUE)
env <- read.table(file.path(output,"env.txt"), header =TRUE)
gen.conf <- readLines(file.path(output,"general.config.txt"))
# head(res)
# head(res[res$biomass >0,])
load(file.path(lewSpec_dir, "data-raw/observed/Morphology.rda"))
load(file.path(lewSpec_dir, "data/data_lakes_env_class.rda"))
# ---------------------------------------------------------------------------------------------------------
# --- add species group -------------------------------------------------------------------------------------------
# scenario
scenario <- unique(res$scenario)

# get species group name
species_raw <- strsplit(gen.conf[[6]][[1]], " ")[[1]] 
species_path <- species_raw[-1]
cleand <- sub("\\.config\\.txt$", "", species_path)
species <- sub(".*(?=species_)", "", cleand, perl = TRUE)
species_id <- as.numeric(unlist(str_extract_all(species, "\\d+")))

group <- c()
for(i in 1:length(species_path)){
    n <- read.table(species_path[i])
    group[i] <- n$V2[n$V1 == "Group"]
}
unique(group) 

# add group
res$speciesGroup <- NA
env$speciesGroup <- NA

res$speciesGroup[( res$speciesID > 14000) & (res$speciesID < 14301)] <- "oligotroph"
res$speciesGroup[( res$speciesID > 15000) & (res$speciesID < 15301)] <- "mesotroph"
res$speciesGroup[( res$speciesID > 16000) & (res$speciesID < 16301)] <- "eutroph"
res$speciesGroup <- as.factor(res$speciesGroup)

res$speciesID[is.na(res$speciesGroup)]

env$speciesGroup[( env$speciesID > 14000) & (env$speciesID < 14301)] <- "oligotroph"
env$speciesGroup[( env$speciesID > 15000) & (env$speciesID < 15301)] <- "mesotroph"
env$speciesGroup[( env$speciesID > 16000) & (env$speciesID < 16301)] <- "eutroph"
env$speciesGroup <- as.factor(env$speciesGroup)

env$speciesID[is.na(env$speciesGroup)]

# ---------------------------------------------------------------------------------------------------------
# --- add lake groups -------------------------------------------------------------------------------------------
# Turbidity: clear, intermediate, Turbid 
# size: very.small, small, medium, large, very.large

# --- Turbidity:  maximal summer temperature, nutrient content, and turbidity. 
# Based on these four parameters we classified the lakes into 
# clear, medium, and turbid lakes  
# performing a hierarchical clustering using Euclidean distance and the Ward linkage method on normalized environmental data of the lakes. 
head(Morphology)
head(data_lakes_env_class) # Turbidity classes 

lake_id <- data_lakes_env_class$Lake 
lake_class <- data_lakes_env_class$class

res$lakeClass <- NA
env$lakeClass <- NA

res$lakeClass <- lake_class[ match(res$lakeID, lake_id)]
res$lakeClass <- as.factor(res$lakeClass)
unique(res$lakeClass)

env$lakeClass <- lake_class[ match(env$lakeID, lake_id)]
env$lakeClass <- as.factor(env$lakeClass)
unique(env$lakeClass)

# --- size: Areagroup 
# get Areakm2
lake_area <- func_getAreaKm2(lake_path)

lake_Agroup <- sapply(lake_area$areakm2, func_getAreaGroup)

res$lakeGroup_Area <- NA
env$lakeGroup_Area <- NA

res$lakeGroup_Area <- lake_Agroup[match(res$lakeID, lake_area$id)]
env$lakeGroup_Area <- lake_Agroup[match(env$lakeID, lake_area$id)]

# --- lake Depth
lake_depth <- func_getLakeDepth(lake_path)
res$lakeDepth <- NA
env$lakeDepth <- NA

res$lakeDepth <- lake_depth$lakeDepth[match(res$lakeID, lake_depth$id)]
env$lakeDepth <- lake_depth$lakeDepth[match(env$lakeID, lake_depth$id)]
# head(res)
save(env, file = file.path(save_figures, "env_dep10_Tprofile.RData"))
save(res, file = file.path(save_figures, "res_dep10_Tprofile.RData"))

# ---------------------------------------------------------------------------------------------------------
# ---- sort data Macrophyts ----------------------------------------------------------------------

valid_res <- res[res$biomass > 0, ]

sort_res <- valid_res %>%
    group_by(lakeClass, speciesGroup, lakeGroup_Area, depth, day) %>%
    summarise(
        biomass_mean = mean(biomass), 
        numberInd_mean = mean(numberInd),
        indWeight_mean = mean(indWeight),
        height_mean = mean(height),
        lakeDepth_mean = mean(lakeDepth)
    ) %>% ungroup()  %>%
    mutate(lakeGroup_Area = factor(lakeGroup_Area,
                                 levels = c("very.small", "small", "medium", "large", "very.large")))
head(sort_res)
nrow(sort_res)

unique(sort_res$day)
save(sort_res, file = file.path(save_figures, "sortRES_dep10_Tprofile.RData"))

# ---------------------------------------------------------------------------------------------------------
# ---- sort data Environment ----------------------------------------------------------------------

sort_env <- env %>%
    group_by(lakeClass, lakeGroup_Area, day) %>%
    summarise(
        tempEpi_mean = mean(tempEpi), 
        tempHypo_mean = mean(tempHypo),
        metaDepth_mean = mean(metaDepth),
        irradiance_mean = mean(irradiance),
        waterlevel_mean = mean(waterlevel),
        lightAttenuation_mean = mean(lightAttenuation), 
        lakeDepth_mean = mean(lakeDepth)
    ) %>% ungroup()  %>%
    mutate(lakeGroup_Area = factor(lakeGroup_Area,
                                 levels = c("very.small", "small", "medium", "large", "very.large")))
head(sort_env)
nrow(sort_env)

unique(sort_env$day)
save(sort_env, file = file.path(save_figures, "sortENV_dep10_Tprofile.RData"))

# ---------------------------------------------------------------------------------------------------------
# Gradient potential species richness vs observed species richness  --------------------------------
# in data&visual_specPot-Observed. R