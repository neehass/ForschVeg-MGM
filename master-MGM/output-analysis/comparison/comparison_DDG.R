# Comparison of DDG 
# Depth diversity gradient of potential and observed species richness
# as in analysis.Rmd

# load data from data_prep_comparison.R

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
modeltypePalette <- brewer.pal(n = 3, name = "Set2")
depthPalette <- brewer.pal(n = 4, name = "Paired")

lakesDDG_combo <- rbind(lakesDDG_Tprofile, lakesDDG_Tsteady) %>%
  left_join(data_lakes_env_class %>% 
              mutate(Lake = paste0("lake_", Lake)) %>% 
              select(-LakeName), by = "Lake") %>% ungroup()
lakesDDG_combo_sel <- lakesDDG_combo %>% select(Group, depth, NSpecP, type, class)

head(lakesDDG_combo_sel)
summary(lakesDDG_combo_sel)



# ANOVA -----------------------------------
lakesDDG_combo_sel$depth <- factor(lakesDDG_combo_sel$depth)
lakesDDG_combo_sel$Group <- factor(lakesDDG_combo_sel$Group)
lakesDDG_combo_sel$type <- factor(lakesDDG_combo_sel$type)
lakesDDG_combo_sel$class <- factor(lakesDDG_combo_sel$class)

# 1. Is NSpecP significantly different among the 3 types? ----------------------- 

# ANOVA 
anova1 <- aov(NSpecP ~ type, data = lakesDDG_combo_sel)
summary(anova1) 
  # effect of type on NSpecP is highly significant. p <2e-16 und F= 120 means difference is strong

# Post-hoc pairwise tests (ANOVA signifcant)
TukeyHSD(anova1) 
  # Mapped has significantly lower NSpecP than both model_base types.
  # model_base_Tprofile and model_base_Tsteady do not differ significantly from each other.


# ANOVA Assumptions
res <- residuals(anova1)
shapiro.test(res) # H0= residuals are normal dis. --> p<2.2e-16 not perfectly nomral distribution 

leveneTest(NSpecP ~ type, data = lakesDDG_combo_sel) # H0 = Variances are equal --> p=0.4393 varainces are homogen

# conclusion Hapriro shows not perfectly dist. residuals, but with large n of smaples 'ANOVA is robust
## due to homogenous Variance: 
# ANOVA is robust to unequal sample sizes as long as variance is homogeneous — which your Levene’s test confirmed.

# ANOVA type* Group
anova2 <- aov(NSpecP ~ type + type* Group, data = lakesDDG_combo_sel)
summary(anova2)
emmeans(anova2, pairwise ~ type)
emmeans(anova2, pairwise ~ type * Group)
# type & groupe sign differences
# ffekt von type auf NSpecP hängt von der Gruppe ab
# mapped Werte zeigen größere Unterschiede zwischen Gruppen als die Modelltypen. (nur hier sign)
# Zwischen den Modelltypen selbst (Tprofile vs. Tsteady) gibt es in den meisten Gruppen keine sign Unterschiede

cont2_df <- as.data.frame(emmeans(anova2, pairwise ~ type)[[2]])
cont2_df$sign <- sapply(cont2_df$p.value, get_signif)
cont2_df$p.value <- sapply(cont2_df$p.value, getformat_p)
cont2_df[,c(1,6,7)]

# ANOVA exclude mapped 
lakesDDG_combo_sel2 <- lakesDDG_combo_sel %>% filter(type != "mapped")
anova3 <- aov(NSpecP ~ type + type* Group, data = lakesDDG_combo_sel2)
summary(anova3)
emmeans(anova3, pairwise ~ type * Group)

# boxplot 
p_box_NSpecP <- ggplot(lakesDDG_combo_sel,
                       aes(x = type, y = NSpecP, fill = type)) +
  geom_boxplot(position = position_dodge(width = 0.75)) +
  scale_fill_manual(values = modeltypePalette) +
  labs(#title = "NSpecP by Type",
    x = "Modeltype",
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

p_box_NSpecP_functype <- ggplot(lakesDDG_combo_sel,
                                aes(x = type, y = NSpecP, fill = Group)) +
  geom_boxplot(position = position_dodge(width = 0.75)) +
  scale_fill_manual(values = TrophiePalette) +
  labs(#title = "NSpecP by Type and Functional Species Group",
    x = "Modeltype",
    y = "Spec. richness (%)", 
    fill = "Spec. Group") +
  theme_bw() 
# theme(axis.text.x = element_text(angle = 45, hjust = 1))
# p_box_NSpecP_functype

p_box_combo <- p_box_NSpecP_text / p_box_NSpecP_functype +
  plot_annotation(tag_levels = 'a',
                  tag_prefix = '(',
                  tag_suffix = ')')& 
  theme(plot.tag = element_text(size = 12))& 
  guides(colour = guide_legend(override.aes = list(size=3)))
p_box_combo
ggsave(file.path(save_comparison, "DDG_anova_box_modletype.png"), p_box_combo, height = 8, width = 6, dpi = "print", scale =1.2)


# 2. NSpecP multi-factor types, group, depth, class -----------------------------------------
anova_multi <- aov(NSpecP ~ type + Group + depth + class, data = lakesDDG_combo_sel)
summary(anova_multi)

# Post-hoc pairwise tests (ANOVA signifcant)
emmeans(anova_multi, pairwise ~ type) # Mapped type is lower in NSpecP, while the two model_base types are simila
emmeans(anova_multi, pairwise ~ Group) # There are clear differences in NSpecP among groups
emmeans(anova_multi, pairwise ~ class) # Class has no significant effect on NSpecP
emmeans(anova_multi, pairwise ~ depth) # NSpecP is lowest at -5, highest around -3 to -1.5, and slightly lower at -0.5, Differences are statistically significant except between -3 and -1.5 (similar NSpecP).


# 3. possible two-way interactions ------------------------------------------------------------
anova_inter <- aov(NSpecP ~ type * Group + type * depth + type * class, data = lakesDDG_combo_sel)
summary(anova_inter) # NSpecP differences among types are not constant across groups, depths, or classes.

sum_inter <- summary(anova_inter)[[1]]
p_values_inter <- sapply(sum_inter$`Pr(>F)`, getformat_p)
sign_inter <- sapply(p_values_inter, get_signif)

p_values_inter_df <- data.frame(x = rownames(sum_inter), p_values = p_values_inter , sign = sign_inter, stringsAsFactors = FALSE)
p_values_inter_df$x <- as.character(p_values_inter_df$x)
p_values_inter_df

# Post-hoc pairwise tests (ANOVA signifcant)
emmeans(anova_inter,  ~ type*Group)

# plot 
p_group <- ggplot(lakesDDG_combo_sel, aes(x = type, y = NSpecP, color = Group)) +
  stat_summary(fun = mean, geom = "point", size = 3) +
  stat_summary(fun = mean, geom = "line", aes(group = Group)) +
  annotate("text", x = 0.7, y = max(lakesDDG_combo_sel$NSpecP) * 0.95, 
           label = paste("p-value", p_values_inter_df$p_values[5], p_values_inter_df$sign[5]), 
           color = "black", size = 2) +
  scale_color_manual(values = TrophiePalette) +
  theme_bw() +
  labs(# title = "Interaction: NSpecP ~ type * Group",
       x = "Modeltype", y = "Spec. richness (%)")
p_depth <- ggplot(lakesDDG_combo_sel, aes(x = type, y = NSpecP, color = depth)) +
  stat_summary(fun = mean, geom = "point", size = 3) +
  stat_summary(fun = mean, geom = "line", aes(group = depth)) +
  scale_color_manual(values = depthPalette) +
  annotate("text", x = 0.7, y = max(lakesDDG_combo_sel$NSpecP) * 0.95, 
           label = paste("p-value", p_values_inter_df$p_values[6], p_values_inter_df$sign[6]), 
           color = "black", size = 2) +
  theme_bw() +
  labs(# title = "Interaction: NSpecP ~ type * depth",
       x = "Modeltype", y = "Spec. richness (%)")
p_class <- ggplot(lakesDDG_combo_sel, aes(x = type, y = NSpecP, color = class)) +
  stat_summary(fun = mean, geom = "point", size = 3) +
  stat_summary(fun = mean, geom = "line", aes(group = class)) +
  theme_bw() +
  annotate("text", x = 0.7, y = max(lakesDDG_combo_sel$NSpecP) * 0.95, 
           label = paste("p-value", p_values_inter_df$p_values[7], p_values_inter_df$sign[7]), 
           color = "black", size = 2) +
  labs(#title = "Interaction: NSpecP ~ type * class",
       x = "Modeltype", y = "Spec. richness (%)")

p_inter_combo <- ((p_group / p_depth)/ p_class) +
  plot_annotation(tag_levels = 'a',
                  tag_prefix = '(',
                  tag_suffix = ')')& 
  theme(plot.tag = element_text(size = 12))& 
  guides(colour = guide_legend(override.aes = list(size=3)))
p_inter_combo
ggsave(file.path(save_comparison, "DDG_anova_modeltype_inter.png"), p_inter_combo, height = 8, width = 6, dpi = "print", scale =1.2)

# Artenreichtum (% NSpecP) unterscheidet sich deutlich zwischen den Modelltypen.
# nterschiedliche Gruppen zeigen unterschiedliche NSpecP-Werte.
# Tiefe beeinflusst NSpecP stark.
# Klassifizierung alleine hat keinen signifikanten Einfluss auf NSpecP.
# interaktion
# Der Effekt von type auf NSpecP hängt davon ab, welche Group betrachtet wird, Unterschiede zwischen Modelltypen sind nicht konstant über Gruppen
# Der Effekt von type auf NSpecP variiert mit der Tiefe
# Der Effekt von type auf NSpecP variiert mit der Klassifizierung

# 4. Interaction class, group, depth ----------------------------
anova_group_depth_class <- aov(NSpecP ~ Group * class * depth, data = lakesDDG_combo_sel)
summary(anova_group_depth_class)
sum_gdc <- summary(anova_group_depth_class)[[1]]
p_values_gdc <- sum_gdc$`Pr(>F)`
sign_gdc <- sapply(p_values_gdc, get_signif)

p_values_gdc_df <- data.frame(x = rownames(sum_gdc), p_values =  p_values_gdc, sign = sign_gdc)
p_values_gdc_df$x <- as.character(p_values_gdc_df$x)
p_values_gdc_df$p_values <- as.character(round(p_values_gdc_df$p_values,3))

# Two-way interaction example
emmeans(anova_group_depth_class, pairwise ~  Group* class)

# Three-way interaction example
emmeans(anova_class_group_depth, pairwise ~ Group* class * depth)

# plot 
# plot Group *Class interaction
p_g_c <- ggplot(lakesDDG_combo_sel, aes(x = Group, y = NSpecP, color = class)) +
  stat_summary(fun = mean, geom = "point", size = 3) +
  stat_summary(fun = mean, geom = "line", aes(group = class)) +
  annotate("text", x = 0.7, y = max(lakesDDG_combo_sel$NSpecP) * 0.95, 
           label = paste("p-value", p_values_gdc_df$p_values[4], p_values_gdc_df$sign[4]), 
           color = "black", size = 2) +
  theme_bw() +
  labs(#title = "Interaction: NSpecP ~ Group *class"
       color = "Turbidity", x = "Spec. Group", y = "Spec. richness (%)")
# p_g_c
# group x depth interaction
p_g_d <- ggplot(lakesDDG_combo_sel, aes(x = depth, y = NSpecP, color = Group)) +
  stat_summary(fun = mean, geom = "point", size = 3) +
  stat_summary(fun = mean, geom = "line", aes(group = Group)) +
  scale_color_manual(values = TrophiePalette) +
  theme_bw() +
  # annotate("text", x = 0.7, y = max(lakesDDG_combo_sel$NSpecP) * 0.95, 
  #          label = paste("p-value:", p_values_gdc_df$p_values[5], p_values_gdc_df$sign[5]), 
  #          color = "black", size = 5) +
  labs(#title = "Interaction: NSpecP ~ Group * depth"
  color = "Spec. Group", x = "Depth", y = "Spec. richness (%)")
# p_g_d

p_c_d <- ggplot(lakesDDG_combo_sel, aes(x = depth, y = NSpecP, color = class)) +
  stat_summary(fun = mean, geom = "point", size = 3) +
  stat_summary(fun = mean, geom = "line", aes(group = class)) +
  annotate("text", x = 0.7, y = max(lakesDDG_combo_sel$NSpecP) * 0.95, 
           label = paste("p-value", p_values_gdc_df$p_values[6], p_values_gdc_df$sign[6]), 
           color = "black", size = 2) +
  theme_bw() +
  labs(#title = "Interaction: NSpecP ~ Group * depth"
    color = "Turbidity", x = "Depth", y = "Spec. richness (%)")
# p_c_d

p_class_group_depth <- ggplot(lakesDDG_combo_sel, aes(x = depth, y = NSpecP, color = Group, linetype = class)) +
  stat_summary(fun = mean, geom = "point") +
  stat_summary(fun = mean, geom = "line", aes(group = interaction(Group, class))) +
  scale_color_manual(values = TrophiePalette  ) + # depthPalette
  labs(color = "Spec. Group", x = "Depth", y = "Spec. richness (%)", linetype = "Turbidity") +
  theme_bw()

# p_class_group_depth

p_gdc <- ((p_g_c / p_g_d)/ p_c_d /p_class_group_depth) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = 'a',
                  tag_prefix = '(',
                  tag_suffix = ')')& 
  theme(plot.tag = element_text(size = 12))& 
  guides(colour = guide_legend(override.aes = list(size=3)))
p_gdc
ggsave(file.path(save_comparison, "DDG_anova_GroupDepthClass_inter.png"), p_gdc, height = 8, width = 6, dpi = "print", scale =1.2)

# Groups and depths sign differences
# The effect of class depends on Group. sign
# Depth effect is similar across Groups. not sign
# Depth effect differs slightly among classes. sign class:depth → depth effect varies by class. sign
# Depth and Group main effects

# Depth modifies the effect of Group and class
# Shallow depths (-3, -1.5) often have higher NSpecP than deep (-5), but the increase varies by Group and class.
# Group differences depend on class and depth
# Mesotraphentic lakes can have higher NSpecP than eutraphentic, but mostly at medium clarity or shallower depths.
# Class differences are conditional
# “Clear” vs “medium” vs “turbid” only shows consistent differences in certain Groups and depths.
# Only a few contrasts are significant after adjustment
# Most differences are small or not statistically reliable when correcting for multiple comparisons.

# ---------------------------------------------------------------------------------------
# plot Gradient -----------------------------------------------


my.formula <- y ~ x 

lakeclasses <- c("clear lakes","intermediate lakes", "turbid lakes")
names(lakeclasses)<-c("clear","medium","turb")


A3_combo_data <- lakesDDG_combo %>%
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
