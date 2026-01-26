# Comparison analysis between different MGM experiment scenarios
# load data from  data-analysis_prep.R / function func_prepBIO_compare

# packages & functions
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(vegan) # wie in numerische Ökologie VL2
library(corrplot)
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
dir_Tprofile <- "output-analysis/combo_baseTprofile_20years"
dir_Tsteady <- "output-analysis/combo_baseTsteady_20years"

output_top_TP <- "output/dep10_300spec_base_Tprofil_20years"
output_deep_TP <- "output/dep10_300spec_base_Tprofile_20years_deep"

output_top_TS <- "output/dep10_300spec_base_Tsteady_20years"
output_deep_TS <- "output/dep10_300spec_base_Tsteady_20years_deep"

save_comparison <- "output-analysis/combo_comparison_final_20years/bio_TprofVSTsteady/Permanova"

dir.create(save_comparison)

# input files
lake_path <- "input/lakes"

# load data --------------------------------------------------------------------------------
# load data prep in data_prep_comparison.R
# ---------------------------------------------------------------------------------------------------------
# biomass data 
sortRES_baseTP <- readRDS(file.path(dir_Tprofile, "sortRES_Tprofile_20years_combo.rds")) # sort_res # biomass > 0 sorted macrophyte data (grouped by lakeClass, speciesGroup, lakeGroup_Area, depth, day)

sortRES_baseTP$depth <- factor(sortRES_baseTP$depth,
                               levels = sort(unique(sortRES_baseTP$depth), decreasing = TRUE))

RES_top_TP <- readRDS(file.path(output_top_TP, "added_all_res.rds")) # 
RES_deep_TP <- readRDS(file.path(output_deep_TP, "added_all_res.rds"))

RES_top_TS <- readRDS(file.path(output_top_TS, "added_all_res.rds")) # 
RES_deep_TS <- readRDS(file.path(output_deep_TS, "added_all_res.rds"))

# combo
RES_top_TP <- as.data.table(RES_top_TP)
RES_deep_TP <- as.data.table(RES_deep_TP)

RES_top_TS <- as.data.table(RES_top_TS)
RES_deep_TS <- as.data.table(RES_deep_TS)

res_baseTP <- rbindlist(list(RES_top_TP, RES_deep_TP), use.names = TRUE)
res_baseTS <- rbindlist(list(RES_top_TS, RES_deep_TS), use.names = TRUE)

# COMPARISON -----------------------------------------
# tprofile
res_baseTprofile_bio <- res_baseTP %>%
  mutate(scenario = "base_Tprofile") %>%
  group_by(lakeID, speciesID, depth, speciesGroup, lakeClass) %>%
  summarise(
    biomass = mean(biomass, na.rm = TRUE))
head(res_baseTprofile_bio)

# tsteady
res_baseTSteady_bio <- res_baseTS %>%
  mutate(scenario = "base_Tsteady") %>%
  group_by(lakeID, speciesID, depth, speciesGroup, lakeClass) %>%
  summarise(
    biomass = mean(biomass, na.rm = TRUE))

# combine
res_combined_BIO <- res_baseTprofile_bio %>%
  left_join(res_baseTSteady_bio, by = c("lakeID", "depth", "speciesID", "speciesGroup", "lakeClass"), 
            suffix = c("_baseTP", "_baseTS")) %>%
  mutate(
    biomass_baseTP = replace_na(biomass_baseTP, 0),
    biomass_baseTS   = replace_na(biomass_baseTS, 0),
    diff = biomass_baseTP - biomass_baseTS
  ) %>%
  rename(
    baseTP = biomass_baseTP,
    baseTS = biomass_baseTS
  )
head(res_combined_BIO)

# ------------------------------------------------------------------------------------------
# Biomasse ---------------------------------------
head(res_combined_BIO)

baseTprofile_wide_bio <- pivot_wider(res_baseTprofile_bio, names_from = speciesID, 
                                     values_from = biomass, values_fill = 0)
head(baseTprofile_wide_bio)

baseTprofile_mat_bio <- baseTprofile_wide_bio[, 5:ncol(baseTprofile_wide_bio)]
meta_Tprofile_bio <- baseTprofile_wide_bio[, 1:4]  # Beispiel: lakeID, depth, lake_group, dataset
meta_Tprofile_bio$dataset <- "Tprofile"

baseTSteady_wide_bio <- pivot_wider(res_baseTSteady_bio, names_from = speciesID, 
                                    values_from = biomass, values_fill = 0)
head(baseTSteady_wide_bio)

baseTSteady_mat_bio  <- baseTSteady_wide_bio[, 5:ncol(baseTSteady_wide_bio)]
meta_TSteady_bio     <- baseTSteady_wide_bio[, 1:4]
meta_TSteady_bio$dataset <- "Tsteady"

biomass_all <- rbind(baseTprofile_mat_bio, baseTSteady_mat_bio)
colnames(biomass_all)

meta_all_bio    <- rbind(meta_Tprofile_bio, meta_TSteady_bio)
meta_all_bio$dataset <- factor(meta_all_bio$dataset)
colnames(meta_all_bio)

# remove empty rows
biomass_all_clean <- biomass_all[rowSums(biomass_all) > 0, ] 
biomass_all_clean <- biomass_all_clean[complete.cases(biomass_all_clean), ]

meta_all_bio <- meta_all_bio[rowSums(biomass_all) > 0, ] 
meta_all_bio <- meta_all_bio[complete.cases(meta_all_bio[, c("dataset", "lakeClass", "depth", "speciesGroup")]), ]

# -----------------------------------------------------
# bray crutis-Distanzmatrix berechnen --------------------------------------------------------------------------------------------
dist_bray <- vegan::vegdist(biomass_all_clean,method = "bray")

# PERMANOVA --------------------------------------------------------------------------------------------
permanova_all_bio <- vegan::adonis2(
  dist_bray ~ dataset + lakeClass + depth + speciesGroup, 
  data = meta_all_bio, by = "margin",
  permutations = 999
)

print(permanova_all_bio) # main result

permanova_all_bio <- vegan::adonis2(
  dist_bray ~   dataset*lakeClass + dataset*depth + dataset*speciesGroup, 
  data = meta_all_bio, by = "margin",
  permutations = 999
)

print(permanova_all_bio) # main result

# Beta-dispersion (test if PERMANOVA valid) --------------------------------------------------------------------------------------------
# test if Jaccard-Distanzen has homogeneous dispersion within groups (Tsteady/ Tprofile)
disp_bio <- betadisper(dist_bray, meta_all_bio$dataset)
anova(disp_bio) # F = 4.4, p = 0.036 → signifikant auf dem 5%-Niveau

permutest(disp_bio, permutations = 999)              # Test der Homogenität der Dispersionen
plot(disp_bio)                                       # PCoA-Plot mit Entfernungen zum Zentroid
boxplot(disp_bio, main="Distances to group centroid")

# dispersion per group
# disp$distances  # distance of each sample to groupmiddle
tapply(disp_bio$distances, meta_all_bio$dataset, mean) # mean of distance
tapply(disp_bio$distances, meta_all_bio$dataset, sd)  # sd

# Daten für ggplot vorbereiten
disp_df_bio <- data.frame(
  distance = disp_bio$distances,
  dataset = meta_all_bio$dataset
)

# Boxplot
p_beta_bio <- ggplot(disp_df_bio, aes(x = dataset, y = distance, fill = dataset)) +
  geom_boxplot(alpha = 0.7) +
  # geom_jitter(width = 0.2, alpha = 0.5) +
  labs(
    title = "Beta-Dispersion",
    x = "Dataset",
    y = "Distance from Groupcenter"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
p_beta_bio
ggsave(file.path(save_comparison, "beat_comp_Varib_BIO.png"), p_beta_bio, bg = "white", height = 5, width = 5)

# >>>>>>>>>>>>>>
# permanova per SpeciesGRoup -----------------------------------------------------------------------------------
permanova_func_type_BIO <- list()
permanova_func_type_BIO2 <- list()

types <- meta_all_bio$speciesGroup %>% unique() %>% na.omit()

for(i in 1:3){
  print(types[i])
  meta <- meta_all_bio[meta_all_bio$speciesGroup == types[i],]
  bio <- biomass_all_clean[meta_all_bio$speciesGroup == types[i],]
  
  # Jaccard-Distanzmatrix
  dist_bray_x <- vegan::vegdist(bio, method = "bray")
  
  # Permanova
  permanova_x <- vegan::adonis2(
    dist_bray_x ~ dataset + lakeClass + depth, 
    data = meta, by = "margin",
    permutations = 999
  )
  
  permanova_func_type_BIO[[i]] <- permanova_x
  
  # Permanova
  permanova_x2 <- vegan::adonis2(
    dist_bray_x ~ dataset*lakeClass, 
    data = meta, by = "margin",
    permutations = 999
  )
  
  permanova_func_type_BIO2[[i]] <- permanova_x2
}
names(permanova_func_type_BIO) <- types
print(permanova_func_type_BIO)

names(permanova_func_type_BIO2) <- types
print(permanova_func_type_BIO2)
# in allen species grouppen hat dataset einfluss auf die unterschiede zwischen den lakeclasses 

# >>>>>>>>>>>>>>
# permanova per SpeciesGRoup & lakeclass -----------------------------------------------------------------------------------

types <- meta_all$speciesGroup %>% unique()  %>% na.omit()
lakeCL <- meta_all$lakeClass %>% unique()  %>% na.omit()

permanova_func_type_lake_BIO <- setNames(vector("list", length(types)), types)
permanova_func_type_lake_BIO2 <- setNames(vector("list", length(types)), types)

for(i in 1:3){
  print(types[i])
  
  for(j in 1:3){
    idx <- meta_all_bio$speciesGroup == types[i] & meta_all_bio$lakeClass == lakeCL[j]
    # select SpeciesGRoup & lakeclass
    meta <- meta_all_bio[idx, ]
    bio <- biomass_all_clean[idx, ]
    
    # Jaccard-Distanzmatrix
    dist_bray_x <- vegan::vegdist(bio, method = "bray")
    
    # Permanova
    permanova_x <- vegan::adonis2(
      dist_bray_x ~ dataset + depth, 
      data = meta, by = "margin",
      permutations = 999
    )
    
    permanova_func_type_lake_BIO[[i]][[lakeCL[j]]] <- permanova_x
    
    # Permanova
    permanova_x2 <- vegan::adonis2(
      dist_bray_x ~ dataset * depth, 
      data = meta, by = "margin",
      permutations = 999
    )
    
    permanova_func_type_lake_BIO2[[i]][[lakeCL[j]]] <- permanova_x2
  }
  
  
}
print(permanova_func_type_lake_BIO)
# bei trub lakeClass --> dataset etwas höher er R2 5.2%, 5.8% & 6.3% (signifikant)
# Fwert ~ 1-2 >> Der Unterschied zwischen Datensätzen ist moderat.
# dataset und depth unterscheiden sich signifikant

print(permanova_func_type_lake_BIO2)
# keine Einfluss von dataset auf depth unteschiede

# PCoA (Jaccard) pro Spceis Group--------------------------------------------------------------------------------------------
PCoA_func_type <- list()
PCoA_var <- list()
PCoA_plot <- list()

types <- meta_all_bio$speciesGroup %>% unique()
for(i in 1:3){
  print(types[i])
  meta <- meta_all_bio[meta_all_bio$speciesGroup == types[i],]
  bio <- biomass_all_clean[meta_all_bio$speciesGroup == types[i],]
  
  # bray curtis-Distanzmatrix & PCoA
  dist_bray_x <- vegan::vegdist(bio, method = "bray")
  pcoa_res <- cmdscale(dist_bray_x, eig = TRUE, k = 2)
  
  # Scores extrahieren
  pcoa_scores <- as.data.frame(pcoa_res$points)
  colnames(pcoa_scores) <- c("PCoA1", "PCoA2")
  
  # Meta-Daten anhängen
  pcoa_scores$dataset <- meta$dataset
  pcoa_scores$lakeClass <- meta$lakeClass
  pcoa_scores$speciesGroup <- meta$speciesGroup
  pcoa_scores$depth <- meta$depth
  
  # Varianzerklärung
  variance <- pcoa_res$eig / sum(pcoa_res$eig)
  
  PCoA_func_type[[i]] <- pcoa_scores
  PCoA_var[[i]] <- variance
  
  # plot
  p_PCoA <- ggplot(pcoa_scores, aes(PCoA1, PCoA2, color = dataset, shape = lakeClass, size = factor(depth))) +
    geom_point(alpha = 0.5) +
    labs(
      title = types[i],
      x = paste0("PCoA1 (", round(variance[1]*100, 1), "%)"),
      y = paste0("PCoA2 (", round(variance[2]*100, 1), "%)")
    ) +
    #scale_size_continuous(range = c(1, 6)) +  # minimal und maximal Punktgröße
    theme_minimal()
  
  PCoA_plot[[i]] <- p_PCoA
  
}
names(PCoA_func_type) <- types
names(PCoA_var) <- types
names(PCoA_plot) <- types

p_com_PCoA <- (PCoA_plot[[1]] | PCoA_plot[[2]] | PCoA_plot[[3]]) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "PCoA (Bray Curtis)",
    tag_levels = 'a',
    tag_prefix = '(',
    tag_suffix = ')'
  ) &
  theme(legend.position = "right")   # shared legend position
p_com_PCoA
ggsave(file.path(save_comparison, "PCoA_functype_BIO.png"), p_com_PCoA, bg = "white", height = 8, width = 20)

# ---end---------------------------------------------------------------------------------------
