# Comparison of DDG 
# Depth diversity gradient of potential and observed species richness
# as in analysis.Rmd

# load data from data-analysis-prep.R

# data used:
# dep10_300spec_base_Tprofil_20years
# dep10_300spec_base_Tsteady_20years

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

dir_Tprofile <- "output-analysis/dep10_300spec_base_Tprofil_20years"
dir_Tsteady <- "output-analysis/dep10_300spec_base_Tsteady_20years"

save_comparison <- "output-analysis/comparison_final_20years/DDG_TprofileVSTsteady"

dir.create(save_comparison)

# input files
lake_path <- "input/lakes"

# load data --------------------------------------------------------------------------------
# load data after prep in data-analysisi_prep.R/ or output-analysis folders of models

# DDG res_reshape
res_reshape_Tprofile <- readRDS(file.path(dir_Tprofile, "DDG", "DDG_reshape_Tprofile_20years.rds")) # res_reshape_Tprofile
res_reshape_Tprofile <- res_reshape_Tprofile[, !names(res_reshape_Tprofile) %in% "depth_NA"]

res_reshape_Tsteady <- readRDS(file.path(dir_Tsteady, "DDG","DDG_reshape_Tsteady_20years.rds")) # res_reshape_Tsteady
res_reshape_Tsteady <- res_reshape_Tsteady[, !names(res_reshape_Tsteady) %in% "depth_NA"]

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
    diff_Tprofile_Tsteady = model_base_Tprofile - model_base_Tsteady 
  ) %>%
  pivot_longer(
    cols = c(diff_mapped_Tprofile, diff_mapped_Tsteady, diff_Tprofile_Tsteady),
    names_to = "comparison",
    values_to = "diff",
  )

# head(lakesDDG_combo_abDiff)

comparison_labels <- c(
  diff_mapped_Tprofile = "mapped - \nmodel base Tprofile",
  diff_mapped_Tsteady = "mapped - \nmodel base Tsteady",
  diff_Tprofile_Tsteady = "base Tprofile - \nbase Tsteady"
)

# Boxplot
p_box_NSpecP_abDiff <- ggplot(lakesDDG_combo_abDiff, aes(x = comparison, y = diff, fill = comparison)) +
  geom_boxplot() +
  scale_x_discrete(labels = comparison_labels) +
  scale_fill_manual(values = abDiffPalette) +
  geom_hline(yintercept = 0, linedataset = "dashed", color = "black", linewidth = 0.8)+
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
  scale_fill_manual(values = TrophiePalette) +
  geom_hline(yintercept = 0, linedataset = "dashed", color = "black", linewidth = 0.8)+
  labs(
    x = "",
    y = "Absolute diff. \nSpec. richness (%)"
  ) +
  theme_bw() +
  labs(fill = "Spec.Group")
#p_box_NSpecP_funcdataset_abDiff

# diff per depth
p_box_NSpecP_depth_abDiff <- ggplot(lakesDDG_combo_abDiff[lakesDDG_combo_abDiff$comparison == "diff_Tprofile_Tsteady", ],
                                    aes(x = depth, y = diff, fill = Group)) +
  geom_boxplot(position = position_dodge(width = 0.75)) +
  scale_x_discrete(labels = comparison_labels) +
  scale_fill_manual(values = TrophiePalette) +
  geom_hline(yintercept = 0, linedataset = "dashed", color = "black", linewidth = 0.8)+
  labs(
    x = "",
    y = "Absolute diff. \nSpec. richness (%)"
  ) +
  theme_bw() + 
  facet_wrap(~class, ncol = 3) +
  labs(title = "model base \n Tprofile - Tsteady", fill = "Spec.Group")
# p_box_NSpecP_depth_abDiff

p_box_combo_abDiff <- ((#p_box_NSpecP_abDiff / 
  p_box_NSpecP_funcdataset_abDiff)/ p_box_NSpecP_depth_abDiff) + 
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom") &
  plot_annotation(tag_levels = 'a',
                  tag_prefix = '(',
                  tag_suffix = ')')& 
  theme(plot.tag = element_text(size = 12))& 
  guides(colour = guide_legend(override.aes = list(size=3))) 
p_box_combo_abDiff
ggsave(file.path(save_comparison, "DDG_abDiff_box_modledataset.png"), p_box_combo_abDiff, 
       height = 6, width = 4.5, dpi = "print", scale =1.2)

# ---------------------------------------------------------------------------------------
# ANOVA -----------------------------------
lakesDDG_combo_sel$depth <- factor(lakesDDG_combo_sel$depth)
lakesDDG_combo_sel$Group <- factor(lakesDDG_combo_sel$Group)
lakesDDG_combo_sel$dataset <- factor(lakesDDG_combo_sel$dataset)
lakesDDG_combo_sel$class <- factor(lakesDDG_combo_sel$class)

lakesDDG_combo_sel2 <- lakesDDG_combo_sel %>% filter(dataset != "mapped") # only Tsteady & Tprofile 

# Is NSpecP significantly different among the 3 datasets? ----------------------- 
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

# 1. is Nspec sign. differnt between Trpofile base & Tsteady ?? ------------------------------------
# !!! without mapped !!!
# ANOVA Assumptions
anova2 <- aov(NSpecP ~ dataset, data = lakesDDG_combo_sel2)
summary(anova2) # not significant different !

res <- residuals(anova2)
shapiro.test(res) # H0= residuals are normal dis. --> p<4.231e-16 not perfectly nomral distribution 

leveneTest(NSpecP ~ dataset, data = lakesDDG_combo_sel2) # H0 = Variances are equal --> p=0.9839 varainces are homogen

# conclusion Hapriro shows not perfectly dist. residuals, but with large n of smaples 'ANOVA is robust
## due to homogenous Variance: 
# ANOVA is robust to unequal sample sizes as long as variance is homogeneous — which your Levene’s test confirmed.

# 2. ANOVA dataset* Group -----------------------------------------------
anova3 <- aov(NSpecP ~ dataset + dataset *Group, data = lakesDDG_combo_sel2)
summary(anova3)
emmeans(anova3, pairwise ~ dataset * Group)
# species Groups unterscheiden sich signifikant, innerhalb der datasets * Group nicht 

# 3. NSpecP multi-factor datasets, group, depth, class -----------------------------------------
anova_multi <- aov(NSpecP ~ dataset + Group + depth + class, data = lakesDDG_combo_sel2)
summary(anova_multi)

# Post-hoc pairwise tests (ANOVA signifcant)
emmeans(anova_multi, pairwise ~ Group) # There are clear differences in NSpecP among groups, va eutrophic ~ meso & eu ~ oli
emmeans(anova_multi, pairwise ~ class) # Class significant effect on NSpecP, va clear ~ medium \& clear ~ turb
emmeans(anova_multi, pairwise ~ depth) # all depth combos sign differeces only -05 ~ -3 not sign

# 4. ANOVA Interaction class, group, depth ----------------------------
anova_group_depth_class <- aov(NSpecP ~ Group * class * depth, data = lakesDDG_combo_sel2)
summary(anova_group_depth_class)
sum_gdc <- summary(anova_group_depth_class)[[1]]
p_values_gdc <- sum_gdc$`Pr(>F)`
sign_gdc <- sapply(p_values_gdc, get_signif)
p_values_gdc_2 <- p_values_gdc
p_values_gdc_2[p_values_gdc_2 < 2.2e-16] <- "< 2.2e-16"

p_values_gdc_df <- data.frame(x = rownames(sum_gdc), p_values =  p_values_gdc_2, sign = sign_gdc)
p_values_gdc_df$x <- as.character(p_values_gdc_df$x)
p_values_gdc_df$p_values <- ifelse(
  suppressWarnings(!is.na(as.numeric(p_values_gdc_df$p_values))),
  formatC(
    as.numeric(p_values_gdc_df$p_values),
    format = "e",
    digits = 3
  ),
  p_values_gdc_df$p_values
)
p_values_gdc_df$p_values <- as.character(p_values_gdc_df$p_values)

# plot 
# plot Group *Class interaction
p_g_c <- ggplot(lakesDDG_combo_sel2, aes(x = Group, y = NSpecP, color = class)) +
  stat_summary(fun = function(x) mean(x, na.rm = TRUE), geom = "point", size = 3) +
  stat_summary(fun = function(x) mean(x, na.rm = TRUE), geom = "line", aes(group = class)) +
  annotate("text", x = 1, y = max(lakesDDG_combo_sel$NSpecP) * 0.95, 
           label = paste("p-value", p_values_gdc_df$p_values[4], p_values_gdc_df$sign[4]), 
           color = "black", size = 3) +
  theme_bw() +
  labs(#title = "Interaction: NSpecP ~ Group *class"
    color = "Turbidity", x = "Spec. Group", y = "Spec. richness (%)")
# p_g_c
# group x depth interaction
p_g_d <- ggplot(lakesDDG_combo_sel2, aes(x = depth, y = NSpecP, color = Group)) +
  stat_summary(fun = function(x) mean(x, na.rm = TRUE), geom = "point", size = 3) +
  stat_summary(fun = function(x) mean(x, na.rm = TRUE), geom = "line", aes(group = Group)) +
  scale_color_manual(values = TrophiePalette) +
  theme_bw() +
  # annotate("text", x = 0.7, y = max(lakesDDG_combo_sel$NSpecP) * 0.95, 
  #          label = paste("p-value:", p_values_gdc_df$p_values[5], p_values_gdc_df$sign[5]), 
  #          color = "black", size = 5) +
  labs(#title = "Interaction: NSpecP ~ Group * depth"
    color = "Spec. Group", x = "Depth", y = "Spec. richness (%)")
# p_g_d

p_c_d <- ggplot(lakesDDG_combo_sel2, aes(x = depth, y = NSpecP, color = class)) +
  stat_summary(fun = function(x) mean(x, na.rm = TRUE), geom = "point", size = 3) +
  stat_summary(fun = function(x) mean(x, na.rm = TRUE), geom = "line", aes(group = class)) +
  annotate("text", x = 1, y = max(lakesDDG_combo_sel$NSpecP) * 0.95, 
           label = paste("p-value", p_values_gdc_df$p_values[6], p_values_gdc_df$sign[6]), 
           color = "black", size = 3) +
  theme_bw() +
  labs(#title = "Interaction: NSpecP ~ Group * depth"
    color = "Turbidity", x = "Depth", y = "Spec. richness (%)")
# p_c_d

p_class_group_depth <- ggplot(lakesDDG_combo_sel2, aes(x = depth, y = NSpecP, color = Group, linedataset = class)) +
  stat_summary(fun = function(x) mean(x, na.rm = TRUE), geom = "point") +
  stat_summary(fun = function(x) mean(x, na.rm = TRUE), geom = "line", aes(group = interaction(Group, class))) +
  scale_color_manual(values = TrophiePalette  ) + # depthPalette
  labs(color = "Spec. Group", x = "Depth", y = "Spec. richness (%)", linedataset = "Turbidity") +
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
ggsave(file.path(save_comparison, "DDG_anova_GroupDepthClass_inter.png"), p_gdc,
       height = 6, width = 4.5, dpi = "print", scale =1.2)

summary(anova_group_depth_class)
# Two-way interaction example
emmeans(anova_group_depth_class, pairwise ~  Group* class)

# Three-way interaction example
emmeans(anova_group_depth_class, pairwise ~ Group* class * depth)

# Group, class, depth separated sign differnce
# Group:class (a) \& class:depth sign difference (c)
# group:depth not sign 

# Group × Class × Depth interaction was not significant 
# >> no evidence that depth-dependent group effects vary consistently across classes.

# 5. ANOVA possible two-way interactions ------------------------------------------------------------
anova_inter <- aov(NSpecP ~ dataset * Group + dataset * depth + dataset * class, data = lakesDDG_combo_sel2)
summary(anova_inter) # NSpecP differences among datasets are not constant across groups, depths, or classes.
# dataset no sign unterschied | group, depth, class haben in sich signifikante unterschiede

sum_inter <- summary(anova_inter)[[1]]
p_values_inter <- sapply(sum_inter$`Pr(>F)`, getformat_p)
sign_inter <- sapply(p_values_inter, get_signif)
sign_inter[is.na(sign_inter)] <- ""
p_values_inter[is.na(p_values_inter)] <- ""

p_values_inter_df <- data.frame(x = trimws(rownames(sum_inter)), p_values = p_values_inter, sign =  unname(sign_inter), stringsAsFactors = FALSE)
p_values_inter_df$x <- as.character(p_values_inter_df$x)
p_values_inter_df


# plot emmeans(anova_inter,  pairwise~ dataset*depth)
emm_df <- as.data.frame(emmeans(anova_inter,  pairwise~ dataset*depth + dataset*Group)[[1]])

# Optional: depth als Faktor sortieren
emm_df$depth <- factor(emm_df$depth)

emm_contrasts <- as.data.frame(emmeans(anova_inter, pairwise~ dataset*depth + dataset*Group)[[2]])
emm_contrasts_sig <- emm_contrasts[emm_contrasts$p.value < 0.1,c("contrast", "p.value")]
emm_contrasts_sig$sign <- sapply(emm_contrasts_sig$p.value, get_signif)

emm_contrasts_sig$depths <- str_extract_all(emm_contrasts_sig$contrast, "depth-[-0-9.]+")
emm_contrasts_sig <- emm_contrasts_sig %>%
  mutate(
    depth1 = sapply(depths, `[`, 1),
    depth2 = sapply(depths, `[`, 2)
  ) %>%
  select(-depths) %>% # die Liste entfernen
  mutate(
    depth1 = as.factor(sub("depth-", "-", depth1)),
    depth2 = as.factor(sub("depth-", "-", depth2))
  ) %>%
  mutate(groups = str_extract_all(contrast, "eutraphentic|mesotraphentic|oligotraphentic")) %>%
  # Nehme die erste Gruppe (von der linken Seite)
  mutate(group1 = sapply(groups, `[`, 1),
         group2 = sapply(groups, `[`, 2)) %>%
  select(-groups) %>% # die Liste entfernen
  mutate(model = str_extract_all(contrast, "model_base_Tprofile|model_base_Tsteady")) %>%
  # Nehme die erste Gruppe (von der linken Seite)
  mutate(model1 = sapply(model, `[`, 1),
         model2 = sapply(model, `[`, 2)) %>%
  select(-model)

# signifikant depth difference within the same group and model
sign_D_gm <- emm_contrasts_sig %>%
  filter(model1 == model2 & group1 == group2 & depth1 != depth2) %>% select(-contrast)


sign_D_gm <- sign_D_gm %>%
  mutate(
    # y-Position = max CI der beiden Tiefen für das Modell und die Gruppe + kleiner offset
    y_pos = mapply(function(d1, d2, grp, mod) {
      max(
        emm_df$upper.CL[emm_df$depth == d1 & emm_df$Group == grp & emm_df$dataset == mod],
        emm_df$upper.CL[emm_df$depth == d2 & emm_df$Group == grp & emm_df$dataset == mod]
      ) + 0.5  # kleiner Abstand nach oben
    }, depth1, depth2, group1, model1)
  )


p_enm <- ggplot(emm_df, aes(x = depth, y = emmean, color = Group, group = Group)) +
  geom_point(position = position_dodge(width = 0.3), size = 3) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                position = position_dodge(width = 0.3), width = 0.2) +
  scale_color_manual(values = TrophiePalette) +
  labs(
    x = "Depth (m)",
    y = "Estimated mean NSpecP",
    color = "Spec. Group"
  ) + facet_wrap(~dataset) +
  theme_bw()
p_enm # no sign difference between depths
ggsave(file.path(save_comparison, "DDG_sign_diff_depth.png"), p_enm, 
       height = 6, width = 4.5, dpi = "print", scale =1.2)



# sign diff between model depth
sign_M <- emm_contrasts_sig %>%
  filter(model1 != model2 & group1 == group2 & depth1 == depth2) %>% select(-contrast)
sign_M
# da zero >> keine signifikanten unterscheide zwischen den modeldatasetn - nur absoluter Unterschied zusehen

# ---------------------------------------------------------------------------------------
# 6. plot Gradient -----------------------------------------------

my.formula <- y ~ x 

lakeclasses <- c("clear lakes","intermediate lakes", "turbid lakes")
names(lakeclasses)<-c("clear","medium","turb")


A3_combo_data <- lakesDDG_combo %>%
  group_by(Lake, dataset, Group, class, depth ) %>%
  summarise(NSpecP = mean(NSpecP, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = dataset, values_from = NSpecP) %>%
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
  ggplot(aes(y= model_base_Tsteady ,x= model_base_Tprofile, col=Group))+ 
  geom_point(show.legend = FALSE)+ 
  facet_grid(~class, labeller = labeller(class = lakeclasses))+ 
  stat_correlation(vstep = 0.1,label.x = "centre", show.legend = FALSE)+ 
  scale_colour_manual(values = c(rev(TrophiePalette)))+ 
  geom_abline(intercept = 0, slope = 1)+ 
  ylab("Tsteady \nPotential spec. richness (%)")+ 
  xlab("Tprofile \nPotential spec. richness (%)")+
  theme(legend.position = "none") +
  theme_minimal() +
  ylim(0,40)+xlim(0,40) 
A3_combo

A3_combo_log <- A3_combo_data %>%
  ggplot(aes(y = log_Tsteady, x = log_Tprofile, col = Group)) +
  geom_point() +
  facet_grid(~class, labeller = labeller(class = lakeclasses)) +
  stat_correlation(vstep = 0.1,label.x = "centre")+ 
  scale_colour_manual(values = rev(TrophiePalette)) +
  geom_abline(intercept = 0, slope = 1) +
  ylab("Tsteady \n(log10 Potential species richness)") +
  xlab("Tprofile \n(log10 Potential species richness)") +
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
       height = 6, width = 4.5, dpi = "print", scale =1.2)

# ---------------------------------
# ANOVA including  mapped  -------------------------------------------------------------------
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
  scale_fill_manual(values = TrophiePalette) +
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
  scale_color_manual(values = TrophiePalette) +
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

# Artenreichtum (% NSpecP) unterscheidet sich deutlich zwischen den Modelldatasetn.
# nterschiedliche Gruppen zeigen unterschiedliche NSpecP-Werte.
# Tiefe beeinflusst NSpecP stark.
# Klassifizierung alleine hat keinen signifikanten Einfluss auf NSpecP.
# interaktion
# Der Effekt von dataset auf NSpecP hängt davon ab, welche Group betrachtet wird, Unterschiede zwischen Modelldatasetn sind nicht konstant über Gruppen
# Der Effekt von dataset auf NSpecP variiert mit der Tiefe
# Der Effekt von dataset auf NSpecP variiert mit der Klassifizierung

