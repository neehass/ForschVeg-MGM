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
res_reshape_Tsteady_top <- readRDS(file.path(out_ana_path_top, paste0("DDG_reshape_",nametop,".rds"))) # res_reshape_Tsteady
res_reshape_Tsteady_top <- res_reshape_Tsteady_top[, !names(res_reshape_Tsteady_top) %in% "depth_NA"]
unique(res_reshape_Tsteady_top$scenario)
res_reshape_Tsteady_top$depth_5 <- 0
res_reshape_Tsteady_top$depth_6 <- 0
res_reshape_Tsteady_top$depth_7 <- 0
res_reshape_Tsteady_top$depth_8 <- 0

# DDG res_reshape 5-9 m
res_reshape_Tsteady_deep <- readRDS(file.path(out_ana_path_deep, paste0("DDG_reshape_",namedeep,".rds"))) # res_reshape_Tsteady
res_reshape_Tsteady_deep <- res_reshape_Tsteady_deep[, !names(res_reshape_Tsteady_deep) %in% "depth_NA"]
res_reshape_Tsteady_deep <- res_reshape_Tsteady_deep %>% rename(depth_5 = depth_1, 
                                                                  depth_6 = depth_2,
                                                                  depth_7 = depth_3,
                                                                  depth_8 = depth_4)
res_reshape_Tsteady_deep$depth_1 <- 0
res_reshape_Tsteady_deep$depth_2 <- 0
res_reshape_Tsteady_deep$depth_3 <- 0
res_reshape_Tsteady_deep$depth_4 <- 0

unique(res_reshape_Tsteady_deep$scenario)
res_reshape_Tsteady_deep$scenario <- unique(res_reshape_Tsteady_top$scenario)

setdiff(names(res_reshape_Tsteady_top), names(res_reshape_Tsteady_deep))

res_reshape_Tsteady <- bind_rows(res_reshape_Tsteady_top, res_reshape_Tsteady_deep)
head(res_reshape_Tsteady)
unique(res_reshape_Tsteady$depth)
saveRDS(res_reshape_Tsteady, file = file.path(save_out, paste0("DDG_reshape_",name1,".rds")))


# print nrow where biomass > 0
specgroup <- unique(res_reshape_Tsteady$speciesGroup)
for(i in 1:3){
  oli <- res_reshape_Tsteady %>% filter(speciesGroup == specgroup[i])  %>% filter(Biomass_cat > 0)
  message(specgroup[i], ":", sum(oli$Biomass_cat))
}

# ---------------------------------------------------------------------------------------
# DDG plot for 0-9m 

NSPEC <- func_DDG_deep(res_reshape = res_reshape_Tsteady , 
                  lewSpec_dir, scenario, save_figures = save_comparison) # in func_data_prep.R

lakesDDG_Tsteady <- NSPEC$lakesDDG
NSPEC$NSPECbase

head(lakesDDG_Tsteady)
lakesDDG_Tsteady$dataset <- "model_base_Tsteady"
unique(lakesDDG_Tsteady$dataset)

saveRDS(lakesDDG_Tsteady, file = file.path(save_out, paste0("lakesDDG_",name1,".rds")))

# Plot Depth diversity gradient -------------
p_DDG_TP <- func_plot_DDG_deep(lewSpec_dir, lakesDDG_Tsteady, scenario) # in help-func.R
p_DDG_TP

ggsave(file.path(save_comparison, paste0("DDG_dep10_deep",scenario,".png")), p_DDG_TP, 
       width = 6.5, height=8, dpi="print", bg="white", scale=1.2)

# ---------------------------------------------------------------------------------------
# comparison with observation data just 0-5m ----------------------------
scenario <- res_reshape_Tsteady$scenario %>% unique()

NSPEC <- func_DDG(res_reshape = res_reshape_Tsteady , 
                  lewSpec_dir, scenario, save_figures = save_comparison) # in func_data_prep.R
lakesDDG_Tsteady <- NSPEC$lakesDDG
NSPEC$NSPECbase
head(lakesDDG_Tsteady)
unique(lakesDDG_Tsteady$depth)
#View(lakesDDG_Tsteady)
# load(file.path(save_comparison, paste0("lakesDDG_dep10", scenario, ".RData"))

# Plot Depth diversity gradient -------------
p_DDG_TP <- func_plot_DDG(lewSpec_dir, lakesDDG_Tsteady, scenario) # in help-func.R
p_DDG_TP
ggsave(file.path(save_comparison, paste0("DDG_dep10_observ_data",scenario,".png")), p_DDG_TP, 
       width = 6.5, height=8, dpi="print", bg="white", scale=1.2)

