# Comparison of DDG 
# Depth diversity gradient of potential and observed species richness
# as in analysis.Rmd

# load data from data_prep_comparison.R

# packages & functions
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(vegan) # wie in numerische Ökologie VL2
library(corrplot)
library(ggrepel)
library(ggpmisc)
library(ggpubr)
library(rcartocolor)
library(patchwork)

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
save_comparison <- "output-analysis/comparison"
dir.create(save_comparison)

# input files
lake_path <- "input/lakes"

# load data --------------------------------------------------------------------------------
# load data prep in data_prep_comparison.R

# DDG res_reshape
load(file.path("DDG_res_reshape_Tprofile.rda")) # res_reshape_Tprofile
load(file.path("DDG_res_reshape_Tsteady.rda")) # res_reshape_Tsteady

# ---------------------------------------------------------------------------------------
# T profile ----------------------------
scenario <- res_reshape_Tprofile$scenario %>% unique()

NSPEC <- func_DDG(res_reshape = res_reshape_Tprofile , 
                  lewSpec_dir, scenario, save_figures = save_comparison) # in func_data_prep.R
lakesDDG_Tprofile <- NSPEC$lakesDDG
NSPEC$NSPECbase
# load(file.path(save_comparison, paste0("lakesDDG_dep10", scenario, ".RData"))

# Plot Depth diversity gradient -------------
p_DDG_TP <- func_plot_DDG(lewSpec_dir, lakesDDG_Tprofile, scenario) # in help-func.R
p_DDG_TP
ggsave(file.path(save_comparison, paste0("DDG_dep10_",scenario,".png")), p_DDG_TP, 
       width = 6.5, height=8, dpi="print", bg="white", scale=1.2)

# ---------------------------------------------------------------------------------------
# T steady ----------------------------
scenario <- res_reshape_Tsteady$scenario %>% unique()

NSPEC_TS <- func_DDG(res_reshape = res_reshape_Tsteady , 
                  lewSpec_dir, scenario, save_figures = save_comparison)
lakesDDG_Tsteady <- NSPEC_TS$lakesDDG
NSPEC_TS$NSPECbase
# load(file.path(save_comparison, paste0("lakesDDG_dep10", scenario, ".RData"))

# Plot Depth diversity gradient -------------
p_DDG_TS <- func_plot_DDG(lewSpec_dir, lakesDDG_Tsteady, scenario) # in help-func.R
p_DDG_TS
ggsave(file.path(save_comparison, paste0("DDG_dep10_",scenario,".png")), p_DDG_TS, 
       width = 6.5, height=8, dpi="print", bg="white", scale=1.2)

# ---------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------
# Comparison Plot Depth diversity gradient -------------

load(file.path(lewSpec_dir, "data/data_lakes_env_class.rda"))

WinnerLoserPalette<- c(carto_pal(2,"PinkYl")[c(2)],carto_pal(2,"TealGrn")[c(1)])
TrophiePalette <- c("cornflowerblue","aquamarine4","coral4")

# plot Gradient -----------------------------------------------
my.formula <- y ~ x 

lakeclasses <- c("clear lakes","intermediate lakes", "turbid lakes")
names(lakeclasses)<-c("clear","medium","turb")

lakesDDG_combo <- rbind(lakesDDG_Tprofile, lakesDDG_Tsteady)
head(lakesDDG_combo)

A3_combo_data <- lakesDDG_combo %>%
  left_join(data_lakes_env_class %>% 
              mutate(Lake = paste0("lake_", Lake)) %>% 
              select(-LakeName),
            by = "Lake") %>%
  group_by(Lake, type, Group, class, depth ) %>%
  summarise(NSpecP = mean(NSpecP, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = type, values_from = NSpecP) %>%
  # log-transform (add small constant to avoid log(0))
  filter(!is.na(model_base_Tprofile), !is.na(model_base_Tsteady),
         model_base_Tprofile >= 0, model_base_Tsteady >= 0) %>%
  mutate(log_Tprofile = log10(model_base_Tprofile),
         log_Tsteady  = log10(model_base_Tsteady))
head(A3_combo_data)

A3_combo_data %>%
  group_by(Group) %>%
  summarise(r = cor(model_base_Tprofile, model_base_Tsteady, use = "complete.obs"))

corr_labels <- A3_combo_data %>%
  group_by(Group, Lake, depth, class) %>%
  summarise(cor_val = cor(log_Tprofile, log_Tsteady, method = "pearson"),
            .groups = "drop") %>%
  mutate(label = paste0("r = ", round(cor_val, 2)))


A3_combo <-A3_combo_data %>% 
  ggplot(aes(x= model_base_Tprofile ,y=model_base_Tsteady, col=Group))+ 
  geom_point(show.legend = FALSE)+ 
  facet_grid(~class, labeller = labeller(class = lakeclasses))+ 
  stat_correlation(vstep = 0.1,label.x = "centre", show.legend = FALSE)+ 
  scale_colour_manual(values = c(rev(TrophiePalette)))+ 
  geom_abline(intercept = 0, slope = 1)+ 
  xlab("Tprofile \nPotential spec. richness (%)")+ 
  ylab("Tsteady \nPotential spec. richness (%)")+
  theme(legend.position = "none") +
  theme_minimal() +
  ylim(0,40)+xlim(0,40) 
A3_combo

A3_combo_log <- A3_combo_data %>%
  ggplot(aes(x = log_Tprofile, y = log_Tsteady, col = Group)) +
  geom_point() +
  facet_grid(~class, labeller = labeller(class = lakeclasses)) +
  stat_correlation(vstep = 0.1,label.x = "centre")+ 
  scale_colour_manual(values = rev(TrophiePalette)) +
  geom_abline(intercept = 0, slope = 1) +
  xlab("Tprofile \n(log10 Potential species richness)") +
  ylab("Tsteady \n(log10 Potential species richness)") +
  theme_minimal()
A3_combo_log

p_DDG_comp <- (A3_combo / A3_combo_log) +
  #plot_layout(guides = "collect") +
  theme(legend.position = "bottom")+ 
  labs(col="Spec. group") + 
  plot_annotation(tag_levels = 'a',
                  tag_prefix = '(',
                  tag_suffix = ')')& 
  theme(plot.tag = element_text(size = 12))& 
  guides(colour = guide_legend(override.aes = list(size=3)))
p_DDG_comp

ggsave(file.path(save_comparison, paste0("DDG_dep10_COMP_base.png")), p_DDG_comp, 
       width = 6.5, height=8, dpi="print", bg="white", scale=1.2)
