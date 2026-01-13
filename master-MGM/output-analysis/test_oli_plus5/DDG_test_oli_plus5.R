# Comparison of DDG 
# Depth diversity gradient of potential and observed species richness
# as in analysis.Rmd

# load data from data-analysis-prep.R
# test_oli_plus5

# packages & functions
library(dplyr)
library(tidyr)
library(stringr)

library(vegan) # wie in numerische Ökologie VL2
library(corrplot)

library(car)
library(emmeans)

library(ggplot2)
library(ggrepel)
library(ggpmisc)
library(ggpubr)
library(rcartocolor)
library(patchwork)
library(RColorBrewer)
par(mar=c(3,4,2,2))
display.brewer.all()
dev.off()

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

save_comparison <- "output-analysis/test_oli_plus5/DDG"
name1 <- "test_oli_plus5"

# input files
lake_path <- "input/lakes"

# load data --------------------------------------------------------------------------------
# load data after prep in data-analysisi_prep.R

# DDG res_reshape
res_reshape_Tprofile <- readRDS(file.path(save_comparison, paste0("DDG_reshape_",name1,".rds"))) # res_reshape_Tprofile
res_reshape_Tprofile <- res_reshape_Tprofile[, !names(res_reshape_Tprofile) %in% "depth_NA"]
unique(res_reshape_Tprofile$scenario)

# print nrow where biomass > 0
specgroup <- unique(res_reshape_Tprofile$speciesGroup)
for(i in 1:3){
  oli <- res_reshape_Tprofile %>% filter(speciesGroup == specgroup[i])  %>% filter(Biomass_cat > 0)
  message(specgroup[i], ":", sum(oli$Biomass_cat))
}

# ---------------------------------------------------------------------------------------
# T profile ----------------------------
scenario <- res_reshape_Tprofile$scenario %>% unique()

NSPEC <- func_DDG(res_reshape = res_reshape_Tprofile , 
                  lewSpec_dir, scenario, save_figures = save_comparison) # in func_data_prep.R
lakesDDG_Tprofile <- NSPEC$lakesDDG
NSPEC$NSPECbase
#View(lakesDDG_Tprofile)
# load(file.path(save_comparison, paste0("lakesDDG_dep10", scenario, ".RData"))

# Plot Depth diversity gradient -------------
p_DDG_TP <- func_plot_DDG(lewSpec_dir, lakesDDG_Tprofile, scenario) # in help-func.R
p_DDG_TP
ggsave(file.path(save_comparison, paste0("DDG_dep10_",scenario,".png")), p_DDG_TP, 
       width = 6.5, height=8, dpi="print", bg="white", scale=1.2)


