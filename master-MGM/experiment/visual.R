# -------------------------------------------------------------------------------------------
# Visualise Results
# -------------------------------------------------------------------------------------------
# beginne nur mit Chiemsee = lake 6
# general.config: Lakes: chiemsee (large) = 6, AbtsdorfSee (very.small) = 1, Eibsee (medium) = 7
# projekt: final_Chiem_Abts_Eib_1-5

# packages & functions
library(ggplot2)
library(patchwork)
library(dplyr)
# library(tidyverse)
source("C:/Users/maiim/Documents/25-25SS/Forschungsprojekt_Vegetationskunde/scripte-data-MGM/master-MGM/experiment/help-func.r")

# working directory
getwd()
dir <- "master-MGM/output/final_Chiem_Abts_Eib_1-5"
setwd(dir)

# load data
res <- read.table("all_res_biomass_number_weight_height_daily.txt", header =TRUE)
env <- read.table("env.txt", header =TRUE)

# View(res) # > 0 from day 140-240
# View(env)

# -------------------------------------------------------------------------------------------
# Chiemsee
# -------------------------------------------------------------------------------------------
class(res$lakeID)

res$speciesID <- as.factor(res$speciesID)
res$depth <- as.factor(res$depth)
res$lakeID <- as.factor(res$lakeID)
levels(res$lakeID)

speciesID_nam <- c("1" = "Callitriche cophocarpa",
                    "2" = "Ceratophyllum demersum",
                    "3" = "Chara aspera",
                    "4" = "Chara aspera var. curta",
                    "5" = "Chara contraria")
# -----------------------------------------------------
# --- Macrophyt data ------------------------
chiem <- res[res$lakeID == "6",]
chiem <- chiem[chiem$biomass > 0,] # select biomass > 0

days <- unique(chiem$day)
scenario <- unique(chiem$scenario)
# unique(chiem$lakeID) 
# class(chiem$lakeID)
# View(chiem)

# --- Env Data ------------------------
env_chiem <- env[env$lakeID == 6,]
depth <- -60

# colnames(env_chiem)
# irradiance [??] W/m2

# plot functions
func_plot_macro(macrodata = chiem, speciesID_nam, name = "Chiemsee", days, scenario)
func_plot_env(envdata = env_chiem, name = "Chiemsee", days, scenario)
func_plot_Tprofile(envdata = env_chiem, depth, name = "Chiemsee", days, scenario, k = 1)
# -----------------------------------------------------

