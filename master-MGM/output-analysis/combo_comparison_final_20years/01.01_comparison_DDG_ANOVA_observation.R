# Comparison of DDG 
# Depth diversity gradient of potential and observed species richness
# as in analysis.Rmd

# load data from data-analysis-prep.R

# data used:
# dep10_300spec_base_Tprofil_20years
# dep10_300spec_base_Tsteady_20years

# dep10_300spec_base_Tprofil_20years_deep
# dep10_300spec_base_Tsteady_20years_deep


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

dir_Tprofile <- "output-analysis/combo_baseTprofile_20years"
dir_Tsteady <- "output-analysis/combo_baseTsteady_20years"

save_comparison <- "output-analysis/combo_comparison_final_20years/DDG_TprofileVSTsteady/observation"

dir.create(save_comparison)

# input files
lake_path <- "input/lakes"

# load data --------------------------------------------------------------------------------
# load data after prep in data-analysisi_prep.R/ or output-analysis folders of models

# DDG res_reshape
res_reshape_Tprofile <- readRDS(file.path(dir_Tprofile, "DDG_reshape_Tprofile_20years_combo.rds")) # res_reshape_Tprofile
res_reshape_Tprofile <- res_reshape_Tprofile[, !names(res_reshape_Tprofile) %in% "depth_NA"]
unique(res_reshape_Tprofile$depth)

res_reshape_Tsteady <- readRDS(file.path(dir_Tsteady,"DDG_reshape_Tsteady_20years_combo.rds")) # res_reshape_Tsteady
res_reshape_Tsteady <- res_reshape_Tsteady[, !names(res_reshape_Tsteady) %in% "depth_NA"]

# print nrow where biomass > 0
specgroup <- unique(res_reshape_Tprofile$speciesGroup)
for(i in 1:3){
  oli <- res_reshape_Tprofile %>% filter(speciesGroup == specgroup[i])  %>% filter(Biomass_cat > 0)
  message(specgroup[i], ":", sum(oli$Biomass_cat))
}

# ---------------------------------------------------------------------------------------
# only depth 0-5m da observation data auch nur <5m
# T profile ----------------------------
scenario <- res_reshape_Tprofile$scenario %>% unique()

NSPEC <- func_DDG(res_reshape = res_reshape_Tprofile , 
                  lewSpec_dir, scenario, save_figures = save_comparison) # in func_data_prep.R
lakesDDG_Tprofile <- NSPEC$lakesDDG
NSPEC$NSPECbase
unique(lakesDDG_Tprofile$depth)

lakesDDG_Tprofile$dataset[lakesDDG_Tprofile$dataset != "mapped"] <- "model_base_Tprofile"
# View(lakesDDG_Tprofile)
# load(file.path(save_comparison, paste0("lakesDDG_dep10", scenario, ".RData"))
# ---------------------------------------------------------------------------------------
# T steady ----------------------------
scenario <- res_reshape_Tsteady$scenario %>% unique()

NSPEC_TS <- func_DDG(res_reshape = res_reshape_Tsteady , 
                     lewSpec_dir, scenario, save_figures = save_comparison)
lakesDDG_Tsteady <- NSPEC_TS$lakesDDG
NSPEC_TS$NSPECbase
lakesDDG_Tsteady$dataset[lakesDDG_Tsteady$dataset != "mapped"] <- "model_base_Tsteady"
unique(lakesDDG_Tsteady$dataset)
# load(file.path(save_comparison, paste0("lakesDDG_dep10", scenario, ".RData"))

# ---------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------
# Comparison Plot Depth diversity gradient -------------
load(file.path(lewSpec_dir, "data/data_lakes_env_class.rda"))
TrophiePalette <- c("#56B4E9", "#E69F00", "#009E73")
modeldatasetPalette <- brewer.pal(n = 3, name = "Set2")
abDiffPalette <- brewer.pal(n = 3, name = "Pastel2")
depthPalette <- brewer.pal(n = 4, name = "Paired")

# combine data 
lakesDDG_combo <- rbind(lakesDDG_Tprofile[lakesDDG_Tprofile$dataset == "model_base_Tprofile",], lakesDDG_Tsteady) %>%
  left_join(data_lakes_env_class %>% 
              mutate(Lake = paste0("lake_", Lake)) %>% 
              select(-LakeName), by = "Lake") %>% ungroup()
lakesDDG_combo_sel <- lakesDDG_combo %>% select(Group, depth, NSpecP, dataset, class)
lakesDDG_combo <- lakesDDG_combo %>%
  mutate(
    class = factor(
      class,
      levels = c("clear", "medium", "turb"),
      labels = c("clear lakes", "intermediate lakes", "turbid lakes")
    )
  )

head(lakesDDG_combo_sel)
# summary(lakesDDG_combo_sel)
# unique(lakesDDG_combo$Group)
# --------------------------------------------------------------------------------------
# Absolut differences
head(lakesDDG_combo_sel) 
nrow(lakesDDG_combo_sel)

lakesDDG_combo_abDiff <- lakesDDG_combo %>% 
  group_by(Lake, Group, class, depth) %>% 
  # summarise(NSpecP = list(NSpecP), dataset = list(dataset), .groups = "drop") %>%
  # unnest(c(NSpecP, dataset)) %>%
  pivot_wider(
    names_from = dataset,
    values_from = NSpecP
  ) %>%
  mutate(
    diff_mapped_Tprofile = mapped - model_base_Tprofile,
    diff_mapped_Tsteady = mapped - model_base_Tsteady,
  ) %>%
  pivot_longer(
    cols = c(diff_mapped_Tprofile, diff_mapped_Tsteady),
    names_to = "comparison",
    values_to = "diff",
  )

# head(lakesDDG_combo_abDiff)

comparison_labels <- c(
  diff_mapped_Tprofile = "mapped - \nmodel base Tprofile",
  diff_mapped_Tsteady = "mapped - \nmodel base Tsteady"
  
)

# Boxplot
p_box_NSpecP_abDiff <- ggplot(lakesDDG_combo_abDiff, aes(x = comparison, y = diff, fill = comparison)) +
  geom_boxplot() +
  scale_x_discrete(labels = comparison_labels) +
  scale_fill_manual(values = abDiffPalette) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.8)+
  labs(
    x = "",
    y = "Absolute diff. Spec. richness (%)"
  ) +
  theme_bw() +
  theme(legend.position = "none")  # falls keine Legende nötig

# p_box_NSpecP_abDiff

p_box_NSpecP_funcdataset_abDiff <- ggplot(lakesDDG_combo_abDiff,
                                       aes(x = comparison, y = diff, fill = Group)) +
  geom_boxplot(position = position_dodge(width = 0.75)) +
  scale_x_discrete(labels = comparison_labels) +
  scale_fill_manual(values =  c(rev(TrophiePalette))) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.8)+
  labs(
    x = "",
    y = "Absolute diff. \nSpec. richness (%)"
  ) +
  theme_bw() +
  labs(fill = "Spec.Group")
p_box_NSpecP_funcdataset_abDiff

# diff per depth
p_box_NSpecP_depth_abDiff_TP <- ggplot(lakesDDG_combo_abDiff[lakesDDG_combo_abDiff$comparison == "diff_mapped_Tprofile", ],
                                    aes(x = depth, y = diff, fill = Group)) +
  geom_boxplot(position = position_dodge(width = 0.75)) +
  scale_x_discrete(labels = comparison_labels) +
  scale_fill_manual(values =  c(rev(TrophiePalette))) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.8)+
  labs(
    x = "",
    y = "Absolute diff. \nSpec. richness (%)"
  ) +
  theme_bw() + 
  facet_wrap(~class, ncol = 3) +
  labs(title = "mapped - \nmodel base Tprofile", fill = "Spec.Group")
p_box_NSpecP_depth_abDiff_TP

p_box_NSpecP_depth_abDiff_TS <- ggplot(lakesDDG_combo_abDiff[lakesDDG_combo_abDiff$comparison == "diff_mapped_Tsteady", ],
                                       aes(x = depth, y = diff, fill = Group)) +
  geom_boxplot(position = position_dodge(width = 0.75)) +
  scale_x_discrete(labels = comparison_labels) +
  scale_fill_manual(values =  c(rev(TrophiePalette))) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.8)+
  labs(
    x = "",
    y = "Absolute diff. \nSpec. richness (%)"
  ) +
  theme_bw() + 
  facet_wrap(~class, ncol = 3) +
  labs(title = "mapped - \nmodel base Tsteady", fill = "Spec.Group")
p_box_NSpecP_depth_abDiff_TP

p_box_combo_abDiff <- ((p_box_NSpecP_funcdataset_abDiff / 
  p_box_NSpecP_depth_abDiff_TP)/ p_box_NSpecP_depth_abDiff_TS) + 
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom") &
  plot_annotation(tag_levels = 'a',
                  tag_prefix = '(',
                  tag_suffix = ')')& 
  theme(plot.tag = element_text(size = 12))& 
  guides(colour = guide_legend(override.aes = list(size=3))) 
p_box_combo_abDiff
ggsave(file.path(save_comparison, "DDG_abDiff_box_modledataset_observ.png"), p_box_combo_abDiff, 
       height = 6, width = 4.5, dpi = "print", scale =1.2)

# ---------------------------------------------------------------------------------------
# ANOVA -----------------------------------
lakesDDG_combo_sel$depth <- factor(lakesDDG_combo_sel$depth)
lakesDDG_combo_sel$Group <- factor(lakesDDG_combo_sel$Group)
lakesDDG_combo_sel$dataset <- factor(lakesDDG_combo_sel$dataset)
lakesDDG_combo_sel$class <- factor(lakesDDG_combo_sel$class)

# Is NSpecP significantly different among the model and mapped datasets? ----------------------- 
# ANOVA 
anova1 <- aov(NSpecP ~ dataset, data = lakesDDG_combo_sel)
summary(anova1) 
# effect of dataset on NSpecP is highly significant. p 1.18e-08 und F= 18.6 means difference is strong

# Post-hoc pairwise tests (ANOVA signifcant)
TukeyHSD(anova1) 
# Mapped has significantly lower NSpecP than both model datasets.
# Tprofile +3.3 einheiten höher als mapped
# Tsteady 3.9 einheiten höher als mapped
# kein sign unterschied zwischen Tprofiel and Tsteady, kleiner unterschied


res <- residuals(anova1)
shapiro.test(res) # H0= residuals are normal dis. --> p<4.231e-16 not perfectly nomral distribution 

leveneTest(NSpecP ~ dataset, data = lakesDDG_combo_sel) # H0 = Variances are equal --> p=0.9839 varainces are homogen

# conclusion Hapriro shows not perfectly dist. residuals, but with large n of smaples 'ANOVA is robust
## due to homogenous Variance: 
# ANOVA is robust to unequal sample sizes as long as variance is homogeneous — which your Levene’s test confirmed.
# ---------------------------------
# ANOVA  -------------------------------------------------------------------
anova31 <- aov(NSpecP ~ dataset + dataset* Group, data = lakesDDG_combo_sel)
summary(anova31)
emmeans(anova31, pairwise ~ dataset * Group)

cont2_df <- as.data.frame(emmeans(anova31, pairwise ~ dataset)[[2]])
cont2_df$sign <- sapply(cont2_df$p.value, get_signif)
cont2_df$p.value <- sapply(cont2_df$p.value, getformat_p)
cont2_df[,c(1,6,7)]

# boxplot 
p_box_NSpecP <- ggplot(lakesDDG_combo_sel,
                       aes(x = dataset, y = NSpecP, fill = dataset)) +
  geom_boxplot(position = position_dodge(width = 0.75)) +
  scale_fill_manual(values = modeldatasetPalette) +
  labs(#title = "NSpecP by dataset",
    x = "Modeldataset",
    y = "Spec. richness (%)") +
  theme_bw() 
# p_box_NSpecP

spacing <- 2
y_pos <- max(lakesDDG_combo_sel$NSpecP) + seq(spacing, spacing * 3, by = spacing)
p_box_NSpecP_text <- p_box_NSpecP +
  geom_text(
    data = cont2_df,
    aes(x = 0.7,  
        y = y_pos,
        label = paste(contrast, ": p.value", p.value, sign)),
    inherit.aes = FALSE,
    hjust = 0 , # linksbündig
    size = 3
  )
# p_box_NSpecP_text

p_box_NSpecP_funcdataset <- ggplot(lakesDDG_combo_sel,
                                aes(x = dataset, y = NSpecP, fill = Group)) +
  geom_boxplot(position = position_dodge(width = 0.75)) +
  scale_fill_manual(values =  c(rev(TrophiePalette))) +
  labs(#title = "NSpecP by dataset and Functional Species Group",
    x = "Modeldataset",
    y = "Spec. richness (%)", 
    fill = "Spec. Group") +
  theme_bw() 
# theme(axis.text.x = element_text(angle = 45, hjust = 1))
# p_box_NSpecP_funcdataset

p_box_combo <- p_box_NSpecP_text / p_box_NSpecP_funcdataset +
  plot_annotation(tag_levels = 'a',
                  tag_prefix = '(',
                  tag_suffix = ')')& 
  theme(plot.tag = element_text(size = 12))& 
  guides(colour = guide_legend(override.aes = list(size=3)))
p_box_combo
ggsave(file.path(save_comparison, "DDG_anova_box_modledataset.png"), p_box_combo, height = 8, width = 6, dpi = "print", scale =1.2)

# ANOVA possible two-way interactions ------------------------------------------------------------
# !! mapped included !!!
anova_inter <- aov(NSpecP ~ dataset * Group + dataset * depth + dataset * class, data = lakesDDG_combo_sel)
summary(anova_inter) # NSpecP differences among datasets are not constant across groups, depths, or classes.

sum_inter <- summary(anova_inter)[[1]]
p_values_inter <- sapply(sum_inter$`Pr(>F)`, getformat_p)
sign_inter <- sapply(p_values_inter, get_signif)

p_values_inter_df <- data.frame(x = rownames(sum_inter), p_values = p_values_inter , sign = sign_inter, stringsAsFactors = FALSE)
p_values_inter_df$x <- as.character(p_values_inter_df$x)
p_values_inter_df

# Post-hoc pairwise tests (ANOVA signifcant)
emmeans(anova_inter,  ~ dataset*Group)

# plot 
p_group <- ggplot(lakesDDG_combo_sel, aes(x = dataset, y = NSpecP, color = Group)) +
  stat_summary(fun = function(x) mean(x, na.rm = TRUE), geom = "point", size = 3) +
  stat_summary(fun = function(x) mean(x, na.rm = TRUE), geom = "line", aes(group = Group)) +
  annotate("text", x = 1, y = max(lakesDDG_combo_sel$NSpecP) * 0.95, 
           label = paste("p-value", p_values_inter_df$p_values[5], p_values_inter_df$sign[5]), 
           color = "black", size = 3) +
  scale_color_manual(values =  c(rev(TrophiePalette))) +
  theme_bw() +
  labs(# title = "Interaction: NSpecP ~ dataset * Group",
    x = "Modeldataset", y = "Spec. richness (%)")
p_depth <- ggplot(lakesDDG_combo_sel, aes(x = dataset, y = NSpecP, color = depth)) +
  stat_summary(fun = function(x) mean(x, na.rm = TRUE), geom = "point", size = 3) +
  stat_summary(fun = function(x) mean(x, na.rm = TRUE), geom = "line", aes(group = depth)) +
  scale_color_manual(values = depthPalette) +
  annotate("text", x = 1, y = max(lakesDDG_combo_sel$NSpecP) * 0.95, 
           label = paste("p-value", p_values_inter_df$p_values[6], p_values_inter_df$sign[6]), 
           color = "black", size = 3) +
  theme_bw() +
  labs(# title = "Interaction: NSpecP ~ dataset * depth",
    x = "Modeldataset", y = "Spec. richness (%)")
p_class <- ggplot(lakesDDG_combo_sel, aes(x = dataset, y = NSpecP, color = class)) +
  stat_summary(fun = function(x) mean(x, na.rm = TRUE), geom = "point", size = 3) +
  stat_summary(fun = function(x) mean(x, na.rm = TRUE), geom = "line", aes(group = class)) +
  theme_bw() +
  annotate("text", x = 1, y = max(lakesDDG_combo_sel$NSpecP) * 0.95, 
           label = paste("p-value", p_values_inter_df$p_values[7], p_values_inter_df$sign[7]), 
           color = "black", size = 3) +
  labs(#title = "Interaction: NSpecP ~ dataset * class",
    x = "Modeldataset", y = "Spec. richness (%)")

p_inter_combo <- ((p_group / p_depth)/ p_class) +
  plot_annotation(tag_levels = 'a',
                  tag_prefix = '(',
                  tag_suffix = ')')& 
  theme(plot.tag = element_text(size = 12))& 
  guides(colour = guide_legend(override.aes = list(size=3)))
p_inter_combo
ggsave(file.path(save_comparison, "DDG_anova_modeldataset_inter.png"), p_inter_combo, height = 8, width = 6, dpi = "print", scale =1.2)


