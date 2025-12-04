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
abDiffPalette <- brewer.pal(n = 3, name = "Pastel2")
depthPalette <- brewer.pal(n = 4, name = "Paired")

# combine data 
lakesDDG_combo <- rbind(lakesDDG_Tprofile[lakesDDG_Tprofile$type == "model_base_Tprofile",], lakesDDG_Tsteady) %>%
  left_join(data_lakes_env_class %>% 
              mutate(Lake = paste0("lake_", Lake)) %>% 
              select(-LakeName), by = "Lake") %>% ungroup()
lakesDDG_combo_sel <- lakesDDG_combo %>% select(Group, depth, NSpecP, type, class)

head(lakesDDG_combo_sel)
summary(lakesDDG_combo_sel)
# --------------------------------------------------------------------------------------
# Absolut differences
head(lakesDDG_combo_sel) 
nrow(lakesDDG_combo_sel)

lakesDDG_combo_abDiff <- lakesDDG_combo %>% 
  group_by(Lake, Group, class, depth) %>% 
  # summarise(NSpecP = list(NSpecP), type = list(type), .groups = "drop") %>%
  # unnest(c(NSpecP, type)) %>%
  pivot_wider(
    names_from = type,
    values_from = NSpecP,
    values_fn = mean
  ) %>%
  mutate(
    diff_mapped_Tprofile = mapped - model_base_Tprofile,
    diff_mapped_Tsteady = mapped - model_base_Tsteady,
    diff_Tsteady_Tprofile = model_base_Tsteady - model_base_Tprofile
  ) %>%
  pivot_longer(
    cols = c(diff_mapped_Tprofile, diff_mapped_Tsteady, diff_Tsteady_Tprofile),
    names_to = "comparison",
    values_to = "diff"
  )

head(lakesDDG_combo_abDiff)

comparison_labels <- c(
  diff_mapped_Tprofile = "mapped - \nmodel base Tprofile",
  diff_mapped_Tsteady = "mapped - \nmodel base Tsteady",
  diff_Tsteady_Tprofile = "Tsteady - \nTprofile"
)

# Boxplot
p_box_NSpecP_abDiff <- ggplot(lakesDDG_combo_abDiff, aes(x = comparison, y = diff, fill = comparison)) +
  geom_boxplot() +
  scale_x_discrete(labels = comparison_labels) +
  scale_fill_manual(values = abDiffPalette) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.8)+
  labs(
    x = "",
    y = "Absolute diff. Spec. richness (%)"
  ) +
  theme_bw() +
  theme(legend.position = "none")  # falls keine Legende nötig

# p_box_NSpecP_abDiff

p_box_NSpecP_functype_abDiff <- ggplot(lakesDDG_combo_abDiff,
                                       aes(x = comparison, y = diff, fill = Group)) +
  geom_boxplot(position = position_dodge(width = 0.75)) +
  scale_x_discrete(labels = comparison_labels) +
  scale_fill_manual(values = TrophiePalette) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.8)+
  labs(
    x = "",
    y = "Absolute diff. Spec. richness (%)"
  ) +
  theme_bw() +
  labs(fill = "Spec.Group")
# p_box_NSpecP_functype_abDiff

# diff per depth
p_box_NSpecP_depth_abDiff <- ggplot(lakesDDG_combo_abDiff[lakesDDG_combo_abDiff$comparison == "diff_Tsteady_Tprofile", ],
                                    aes(x = depth, y = diff, fill = Group)) +
  geom_boxplot(position = position_dodge(width = 0.75)) +
  scale_x_discrete(labels = comparison_labels) +
  scale_fill_manual(values = TrophiePalette) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.8)+
  labs(
    x = "",
    y = "Absolute diff. Spec. richness (%)"
  ) +
  theme_bw() + 
  facet_wrap(~class, ncol = 3) +
  labs(title = "model Tsteady - model Tprofile", fill = "Spec.Group")
# p_box_NSpecP_depth_abDiff

p_box_combo_abDiff <- ((#p_box_NSpecP_abDiff / 
  p_box_NSpecP_functype_abDiff)/ p_box_NSpecP_depth_abDiff) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = 'a',
                  tag_prefix = '(',
                  tag_suffix = ')')& 
  theme(plot.tag = element_text(size = 12))& 
  guides(colour = guide_legend(override.aes = list(size=3)))
p_box_combo_abDiff
ggsave(file.path(save_comparison, "DDG_abDiff_box_modletype.png"), p_box_combo_abDiff, height = 8, width = 6, dpi = "print", scale =1.2)

# ---------------------------------------------------------------------------------------
# ANOVA -----------------------------------
lakesDDG_combo_sel$depth <- factor(lakesDDG_combo_sel$depth)
lakesDDG_combo_sel$Group <- factor(lakesDDG_combo_sel$Group)
lakesDDG_combo_sel$type <- factor(lakesDDG_combo_sel$type)
lakesDDG_combo_sel$class <- factor(lakesDDG_combo_sel$class)

lakesDDG_combo_sel2 <- lakesDDG_combo_sel %>% filter(type != "mapped")

# 1. Is NSpecP significantly different among the 3 types? ----------------------- 
# ANOVA 
anova1 <- aov(NSpecP ~ type, data = lakesDDG_combo_sel2)
summary(anova1) 
# effect of type on NSpecP is highly significant. p <2e-16 und F= 120 means difference is strong

# Post-hoc pairwise tests (ANOVA signifcant)
TukeyHSD(anova1) 
# Mapped has significantly lower NSpecP than both model_base types.
# model_base_Tprofile and model_base_Tsteady do not differ significantly from each other.


# ANOVA Assumptions
res <- residuals(anova1)
shapiro.test(res) # H0= residuals are normal dis. --> p<2.2e-16 not perfectly nomral distribution 

leveneTest(NSpecP ~ type, data = lakesDDG_combo_sel2) # H0 = Variances are equal --> p=0.4393 varainces are homogen

# conclusion Hapriro shows not perfectly dist. residuals, but with large n of smaples 'ANOVA is robust
## due to homogenous Variance: 
# ANOVA is robust to unequal sample sizes as long as variance is homogeneous — which your Levene’s test confirmed.

# ANOVA type* Group
anova2 <- aov(NSpecP ~ type + type* Group, data = lakesDDG_combo_sel2)
summary(anova2)
emmeans(anova2, pairwise ~ type)
emmeans(anova2, pairwise ~ type * Group)
# kein signifikanter unterschied in Gruppen und tpye!!

cont2_df <- as.data.frame(emmeans(anova2, pairwise ~ type)[[2]])
cont2_df$sign <- sapply(cont2_df$p.value, get_signif)
cont2_df$p.value <- sapply(cont2_df$p.value, getformat_p)
cont2_df[,c(1,6,7)]


# boxplot 
p_box_NSpecP <- ggplot(lakesDDG_combo_sel2,
                       aes(x = type, y = NSpecP, fill = type)) +
  geom_boxplot(position = position_dodge(width = 0.75)) +
  scale_fill_manual(values = modeltypePalette) +
  labs(#title = "NSpecP by Type",
    x = "Modeltype",
    y = "Spec. richness (%)") +
  theme_bw() 
# p_box_NSpecP

spacing <- 2
# y_pos <- max(lakesDDG_combo_sel2$NSpecP) + seq(spacing, spacing * 3, by = spacing)
p_box_NSpecP_text <- p_box_NSpecP +
  geom_text(
    data = cont2_df,
    aes(x = 0.7,  
        y = max(lakesDDG_combo_sel2$NSpecP),
        label = paste(contrast, ": p.value", p.value, sign)),
    inherit.aes = FALSE,
    hjust = 0 , # linksbündig
    size = 3
  )
# p_box_NSpecP_text

p_box_NSpecP_functype <- ggplot(lakesDDG_combo_sel2,
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
ggsave(file.path(save_comparison, "DDG_anova_box_modletype2.png"), p_box_combo, height = 8, width = 6, dpi = "print", scale =1.2)


# 2. NSpecP multi-factor types, group, depth, class -----------------------------------------
anova_multi <- aov(NSpecP ~ type + Group + depth + class, data = lakesDDG_combo_sel2)
summary(anova_multi)

# Post-hoc pairwise tests (ANOVA signifcant)
emmeans(anova_multi, pairwise ~ type) # Mapped type is lower in NSpecP, while the two model_base types are simila
emmeans(anova_multi, pairwise ~ Group) # There are clear differences in NSpecP among groups
emmeans(anova_multi, pairwise ~ class) # Class has no significant effect on NSpecP
emmeans(anova_multi, pairwise ~ depth) # NSpecP is lowest at -5, highest around -3 to -1.5, and slightly lower at -0.5, Differences are statistically significant except between -3 and -1.5 (similar NSpecP).


# 3. ANOVA possible two-way interactions ------------------------------------------------------------
anova_inter <- aov(NSpecP ~ type * Group + type * depth + type * class, data = lakesDDG_combo_sel2)
summary(anova_inter) # NSpecP differences among types are not constant across groups, depths, or classes.
# type wnieg sign unterschied, depth und class haben in sich signifikante unterschiede

sum_inter <- summary(anova_inter)[[1]]
p_values_inter <- sapply(sum_inter$`Pr(>F)`, getformat_p)
sign_inter <- sapply(p_values_inter, get_signif)
sign_inter[is.na(sign_inter)] <- ""
p_values_inter[is.na(p_values_inter)] <- ""

p_values_inter_df <- data.frame(x = trimws(rownames(sum_inter)), p_values = p_values_inter, sign =  unname(sign_inter), stringsAsFactors = FALSE)
p_values_inter_df$x <- as.character(p_values_inter_df$x)
p_values_inter_df

# Post-hoc pairwise tests (ANOVA signifcant)
emmeans(anova_inter,  pairwise~ type*Group)
emmeans(anova_inter,  pairwise~ type*depth)
emmeans(anova_inter,  pairwise~ type*class)
emmeans(anova_inter,  pairwise~ type*depth + type*Group)


# plot emmeans(anova_inter,  pairwise~ type*depth)
emm_df <- as.data.frame(emmeans(anova_inter,  pairwise~ type*depth + type*Group)[[1]])

# Optional: depth als Faktor sortieren
emm_df$depth <- factor(emm_df$depth)

emm_contrasts <- as.data.frame(emmeans(anova_inter, pairwise~ type*depth + type*Group)[[2]])
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
        emm_df$upper.CL[emm_df$depth == d1 & emm_df$Group == grp & emm_df$type == mod],
        emm_df$upper.CL[emm_df$depth == d2 & emm_df$Group == grp & emm_df$type == mod]
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
  ) + facet_wrap(~type) +
  theme_bw()
p_enm
ggsave(file.path(save_comparison, "DDG_sign_diff_depth.png"), p_enm, height = 6, width = 6, dpi = "print", scale =1.2)


# sign diff between model depth
sign_M <- emm_contrasts_sig %>%
  filter(model1 != model2 & group1 == group2 & depth1 == depth2) %>% select(-contrast)

# da zero >> keine signifikanten unterscheide zwischen den modeltypen - nur absoluter Unterschied zusehen 

