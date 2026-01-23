# Comparison of DDG 
# Depth diversity gradient of potential and observed species richness
# as in analysis.Rmd

# combo 0-5 & 5-9m
# -------------------------------------------------------------------------------------------
#  projekt: dep10_300spec_base_Tprofile_20years & 
# dep10_300spec_base_Tprofile_20years_deep

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

# ---------------------------------------------------------------------------------
# dir setup ---------------------------------------------------------------------------------
out_ana_path_top <- "output-analysis/dep10_300spec_base_Tsteady_20years/DDG"
nametop <- "Tsteady_20years"
out_ana_path_deep <- "output-analysis/dep10_300spec_base_Tsteady_20years_deep/DDG"
namedeep <- "Tsteady_20years_deep"

# combo 
save_comparison <- "output-analysis/combo_baseTsteady_20years/DDG"
save_out <- "output-analysis/combo_baseTsteady_20years"

scenario <- "base Tsteady"
name1 <- "Tsteady_20years_combo"

dir.create(save_out)
dir.create(save_comparison)

# input files
lake_path <- "input/lakes"

# load data --------------------------------------------------------------------------------
# load data after prep in data-analysisi_prep.R

# DDG res_reshape 0-5 m
res_reshape_Tprofile_top <- readRDS(file.path(out_ana_path_top, paste0("DDG_reshape_",nametop,".rds"))) # res_reshape_Tprofile
res_reshape_Tprofile_top <- res_reshape_Tprofile_top[, !names(res_reshape_Tprofile_top) %in% "depth_NA"]
unique(res_reshape_Tprofile_top$scenario)
res_reshape_Tprofile_top$depth_5 <- 0
res_reshape_Tprofile_top$depth_6 <- 0
res_reshape_Tprofile_top$depth_7 <- 0
res_reshape_Tprofile_top$depth_8 <- 0

# DDG res_reshape 5-9 m
res_reshape_Tprofile_deep <- readRDS(file.path(out_ana_path_deep, paste0("DDG_reshape_",namedeep,".rds"))) # res_reshape_Tprofile
res_reshape_Tprofile_deep <- res_reshape_Tprofile_deep[, !names(res_reshape_Tprofile_deep) %in% "depth_NA"]
res_reshape_Tprofile_deep <- res_reshape_Tprofile_deep %>% rename(depth_5 = depth_1, 
                                                                  depth_6 = depth_2,
                                                                  depth_7 = depth_3,
                                                                  depth_8 = depth_4)
res_reshape_Tprofile_deep$depth_1 <- 0
res_reshape_Tprofile_deep$depth_2 <- 0
res_reshape_Tprofile_deep$depth_3 <- 0
res_reshape_Tprofile_deep$depth_4 <- 0

unique(res_reshape_Tprofile_deep$scenario)
res_reshape_Tprofile_deep$scenario <- unique(res_reshape_Tprofile_top$scenario)

setdiff(names(res_reshape_Tprofile_top), names(res_reshape_Tprofile_deep))

res_reshape_Tprofile <- bind_rows(res_reshape_Tprofile_top, res_reshape_Tprofile_deep)
head(res_reshape_Tprofile)
unique(res_reshape_Tprofile$depth)
saveRDS(res_reshape_Tprofile, file = file.path(save_out, paste0("DDG_reshape_",name1,".rds")))


# print nrow where biomass > 0
specgroup <- unique(res_reshape_Tprofile$speciesGroup)
for(i in 1:3){
  oli <- res_reshape_Tprofile %>% filter(speciesGroup == specgroup[i])  %>% filter(Biomass_cat > 0)
  message(specgroup[i], ":", sum(oli$Biomass_cat))
}

# ---------------------------------------------------------------------------------------
# DDG plot for 0-9m 

NSPEC <- func_DDG_deep(res_reshape = res_reshape_Tprofile , 
                  lewSpec_dir, scenario, save_figures = save_comparison) # in func_data_prep.R

lakesDDG_Tprofile <- NSPEC$lakesDDG
NSPEC$NSPECbase

head(lakesDDG_Tprofile)
unique(lakesDDG_Tprofile$depth)

# Plot Depth diversity gradient -------------
p_DDG_TP <- func_plot_DDG_deep(lewSpec_dir, lakesDDG_Tprofile, scenario) # in help-func.R
p_DDG_TP

ggsave(file.path(save_comparison, paste0("DDG_dep10_deep",scenario,".png")), p_DDG_TP, 
       width = 6.5, height=8, dpi="print", bg="white", scale=1.2)

# ---------------------------------------------------------------------------------------
# comparison with observation data just 0-5m ----------------------------
scenario <- res_reshape_Tprofile$scenario %>% unique()

NSPEC <- func_DDG(res_reshape = res_reshape_Tprofile , 
                  lewSpec_dir, scenario, save_figures = save_comparison) # in func_data_prep.R
lakesDDG_Tprofile <- NSPEC$lakesDDG
NSPEC$NSPECbase
head(lakesDDG_Tprofile)
unique(lakesDDG_Tprofile$depth)
#View(lakesDDG_Tprofile)
# load(file.path(save_comparison, paste0("lakesDDG_dep10", scenario, ".RData"))

# Plot Depth diversity gradient -------------
p_DDG_TP <- func_plot_DDG(lewSpec_dir, lakesDDG_Tprofile, scenario) # in help-func.R
p_DDG_TP
ggsave(file.path(save_comparison, paste0("DDG_dep10_observ_data",scenario,".png")), p_DDG_TP, 
       width = 6.5, height=8, dpi="print", bg="white", scale=1.2)

