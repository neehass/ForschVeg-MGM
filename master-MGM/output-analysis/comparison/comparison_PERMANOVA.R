# Comparison analysis between different MGM experiment scenarios
# load data from data_prep_comparison.R

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
save_comparison <- "output-analysis/comparison"
dir.create(save_comparison)

# input files
lake_path <- "input/lakes"

# load data --------------------------------------------------------------------------------
# load data prep in data_prep_comparison.R

# Presence/Absence data 
load(file.path(save_comparison, "res_combined_base.rda")) # res_combined base scenrario Tsteady and Tprofile 

# seperate data Tprofile and Tsteady base
load(file.path(save_comparison, "res_baseTprofile_prep.rda")) # res_baseTprofile_prep
load(file.path(save_comparison, "res_baseTSteady_prep.rda")) # res_baseTSteady_prep

# biomass data 
load(file.path(save_comparison, "res_combined_base_BIO.rda")) # reas_combined_BIO

load(file.path(save_comparison, "res_baseTprofile_bio.rda"))
load(file.path(save_comparison, "res_baseTSteady_bio.rda"))

# ------------------------------------------------------------------------------------------
# Comparison T_profile vs Tsteady ------------------
# Species Presence/Absence 

# long to wide --------------------------------------------------------------------------------------------
baseTprofile_wide <- pivot_wider(res_baseTprofile_prep, names_from = speciesID, values_from = Biomass_cat, values_fill = 0)
head(baseTprofile_wide)

baseTprofile_mat <- baseTprofile_wide[, 5:ncol(baseTprofile_wide)]
meta_Tprofile <- baseTprofile_wide[, 1:4]  # Beispiel: lakeID, depth, lake_group, dataset
meta_Tprofile$dataset <- "Tprofile"

baseTSteady_wide <- pivot_wider(res_baseTSteady_prep, names_from = speciesID, values_from = Biomass_cat, values_fill = 0)
head(baseTSteady_wide)

baseTSteady_mat  <- baseTSteady_wide[, 5:ncol(baseTSteady_wide)]
meta_TSteady     <- baseTSteady_wide[, 1:4]
meta_TSteady$dataset <- "Tsteady"

species_all <- rbind(baseTprofile_mat, baseTSteady_mat)
colnames(species_all)

meta_all    <- rbind(meta_Tprofile, meta_TSteady)
meta_all$dataset <- factor(meta_all$dataset)
colnames(meta_all)

# Jaccard-Distanzmatrix berechnen --------------------------------------------------------------------------------------------
dist_jac <- vegan::vegdist(species_all, method = "jaccard", binary = TRUE)

# PERMANOVA --------------------------------------------------------------------------------------------
# „Ist die Variation der Artenzusammensetzung zwischen Dataset A und B größer als innerhalb der Datensets?“
permanova_dataset <- vegan::adonis2(
  dist_jac ~ dataset, 
  data = meta_all,
  permutations = 999
)

print(permanova_dataset) # first restult

permanova_all <- vegan::adonis2(
  dist_jac ~ dataset + lakeClass + depth + speciesGroup, 
  data = meta_all, by = "margin",
  permutations = 999
)

print(permanova_all) # main result

# Beta-dispersion (test if PERMANOVA valid) --------------------------------------------------------------------------------------------
# test if Jaccard-Distanzen has homogeneous dispersion within groups (Tsteady/ Tprofile)
disp <- betadisper(dist_jac, meta_all$dataset)
anova(disp) # F = 4.4, p = 0.036 → signifikant auf dem 5%-Niveau

permutest(disp, permutations = 999)              # Test der Homogenität der Dispersionen
plot(disp)                                       # PCoA-Plot mit Entfernungen zum Zentroid
boxplot(disp, main="Distances to group centroid")

# dispersion per group
# disp$distances  # distance of each sample to groupmiddle
tapply(disp$distances, meta_all$dataset, mean) # mean of distance
tapply(disp$distances, meta_all$dataset, sd)  # sd

# Daten für ggplot vorbereiten
disp_df <- data.frame(
  distance = disp$distances,
  dataset = meta_all$dataset
)

# Boxplot
p_beta <- ggplot(disp_df, aes(x = dataset, y = distance, fill = dataset)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(
    title = "Beta-Dispersion nach Dataset - base",
    x = "Dataset",
    y = "Abstand zum Gruppenzentrum"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
p_beta
ggsave(file.path(save_comparison, "beat_comp_Varib.png"), p_beta, bg = "white", height = 5, width = 5)

# >>>>>>>>>>>>>>
# permanova per SpeciesGRoup -----------------------------------------------------------------------------------
permanova_func_type <- list()

types <- meta_all$speciesGroup %>% unique()

for(i in 1:3){
  print(types[i])
  meta <- meta_all[meta_all$speciesGroup == types[i],]
  spec <- species_all[meta_all$speciesGroup == types[i],]
  
  # Jaccard-Distanzmatrix
  dist_jac_x <- vegan::vegdist(spec, method = "jaccard", binary = TRUE)
  
  # Permanova
  permanova_x <- vegan::adonis2(
    dist_jac_x ~ dataset + lakeClass + depth, 
    data = meta, by = "margin",
    permutations = 999
  )
  
  permanova_func_type[[i]] <- permanova_x
}
names(permanova_func_type) <- types
print(permanova_func_type)

# >>>>>>>>>>>>>>
# permanova per SpeciesGRoup & lakeclass -----------------------------------------------------------------------------------

types <- meta_all$speciesGroup %>% unique()
lakeCL <- meta_all$lakeClass %>% unique()

permanova_func_type_lake <- setNames(vector("list", length(types)), types)

for(i in 1:3){
  print(types[i])
  
  for(j in 1:3){
    idx <- meta_all$speciesGroup == types[i] & meta_all$lakeClass == lakeCL[j]
    # select SpeciesGRoup & lakeclass
    meta <- meta_all[idx, ]
    spec <- species_all[idx, ]
    
    # Jaccard-Distanzmatrix
    dist_jac_x <- vegan::vegdist(spec, method = "jaccard", binary = TRUE)
    
    # Permanova
    permanova_x <- vegan::adonis2(
      dist_jac_x ~ dataset + depth, 
      data = meta, by = "margin",
      permutations = 999
    )
    
    permanova_func_type_lake[[i]][[lakeCL[j]]] <- permanova_x
  }
  
  
}
print(permanova_func_type_lake)
# bei trub lakeClass --> dataset etwas höher er R2 7.4%, 6.7% & 8.7% (signifikant)
# Fwert ~ 2.4 >> Der Unterschied zwischen Datensätzen ist moderat.

# PCoA (Jaccard) pro Spceis Group--------------------------------------------------------------------------------------------
PCoA_func_type <- list()
PCoA_var <- list()
PCoA_plot <- list()

types <- meta_all$speciesGroup %>% unique()

for(i in 1:3){
  print(types[i])
  meta <- meta_all[meta_all$speciesGroup == types[i],]
  spec <- species_all[meta_all$speciesGroup == types[i],]
  
  # Jaccard-Distanzmatrix & PCoA
  dist_jac_x <- vegan::vegdist(spec, method = "jaccard", binary = TRUE)
  pcoa_res <- cmdscale(dist_jac_x, eig = TRUE, k = 2)
  
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
    title = "PCoA (Jaccard)",
    tag_levels = "A"   # optional panel labels A,B,C
  ) &
  theme(legend.position = "right")   # shared legend position
p_com_PCoA
ggsave(file.path(save_comparison, "PCoA_functype.png"), p_com_PCoA, bg = "white", height = 8, width = 20)

# ------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------
# Biomasse ---------------------------------------
# .............
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
colnames(meta_all)

# bray crutis-Distanzmatrix berechnen --------------------------------------------------------------------------------------------
dist_bray <- vegan::vegdist(biomass_all,method = "bray")

# PERMANOVA --------------------------------------------------------------------------------------------
permanova_all_bio <- vegan::adonis2(
  dist_bray ~ dataset + lakeClass + depth + speciesGroup, 
  data = meta_all, by = "margin",
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
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(
    title = "Beta-Dispersion nach Dataset - base",
    x = "Dataset",
    y = "Abstand zum Gruppenzentrum"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
p_beta_bio
ggsave(file.path(save_comparison, "beat_comp_Varib_BIO.png"), p_beta_bio, bg = "white", height = 5, width = 5)

# >>>>>>>>>>>>>>
# permanova per SpeciesGRoup -----------------------------------------------------------------------------------
permanova_func_type_BIO <- list()

types <- meta_all$speciesGroup %>% unique()

for(i in 1:3){
  print(types[i])
  meta <- meta_all_bio[meta_all_bio$speciesGroup == types[i],]
  bio <- biomass_all[meta_all_bio$speciesGroup == types[i],]
  
  # Jaccard-Distanzmatrix
  dist_bray_x <- vegan::vegdist(bio, method = "bray")
  
  # Permanova
  permanova_x <- vegan::adonis2(
    dist_bray_x ~ dataset + lakeClass + depth, 
    data = meta, by = "margin",
    permutations = 999
  )
  
  permanova_func_type_BIO[[i]] <- permanova_x
}
names(permanova_func_type_BIO) <- types
print(permanova_func_type_BIO)

# >>>>>>>>>>>>>>
# permanova per SpeciesGRoup & lakeclass -----------------------------------------------------------------------------------

types <- meta_all$speciesGroup %>% unique()
lakeCL <- meta_all$lakeClass %>% unique()

permanova_func_type_lake_BIO <- setNames(vector("list", length(types)), types)

for(i in 1:3){
  print(types[i])
  
  for(j in 1:3){
    idx <- meta_all_bio$speciesGroup == types[i] & meta_all_bio$lakeClass == lakeCL[j]
    # select SpeciesGRoup & lakeclass
    meta <- meta_all_bio[idx, ]
    bio <- biomass_all[idx, ]
    
    # Jaccard-Distanzmatrix
    dist_bray_x <- vegan::vegdist(bio, method = "bray")
    
    # Permanova
    permanova_x <- vegan::adonis2(
      dist_bray_x ~ dataset + depth, 
      data = meta, by = "margin",
      permutations = 999
    )
    
    permanova_func_type_lake_BIO[[i]][[lakeCL[j]]] <- permanova_x
  }
  
  
}
print(permanova_func_type_lake_BIO)
# bei trub lakeClass --> dataset etwas höher er R2 5.2%, 5.8% & 6.3% (signifikant)
# Fwert ~ 1-2 >> Der Unterschied zwischen Datensätzen ist moderat.
# dataset und depth unterscheiden sich signifikant

# PCoA (Jaccard) pro Spceis Group--------------------------------------------------------------------------------------------
PCoA_func_type <- list()
PCoA_var <- list()
PCoA_plot <- list()

types <- meta_all$speciesGroup %>% unique()

for(i in 1:3){
  print(types[i])
  meta <- meta_all_bio[meta_all_bio$speciesGroup == types[i],]
  bio <- biomass_all[meta_all_bio$speciesGroup == types[i],]
  
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
    tag_levels = "A"   # optional panel labels A,B,C
  ) &
  theme(legend.position = "right")   # shared legend position
p_com_PCoA
ggsave(file.path(save_comparison, "PCoA_functype_BIO.png"), p_com_PCoA, bg = "white", height = 8, width = 20)

# ------------------------------------------------------------------------------------------
# Comparison between scenarios with T_profile ------------------
# ...........

# ------------------------------------------------------------------------------------------
# Traits Winner / Loser  ------------------
# ...........