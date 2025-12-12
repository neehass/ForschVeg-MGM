library(dplyr)
library(tidyr)
library(stringr)
# multicore_data_table_prep.R
library(data.table)
library(parallel)

# ---------------------------------------------------------------------------------------------------------
# Function to prepare data --------------------------------------------------------------------------------

func_prep_data <- function(output, save_figures, lake_path, lewSpec_dir){
    # ---------------------------------------------------------------------------------------------------------
    # load data --------------------------------------------------------------------------------
    res <- read.table(file.path(output, "all_res_biomass_number_weight_height_daily.txt"), header =TRUE)
    env <- read.table(file.path(output,"env.txt"), header =TRUE)
    gen.conf <- readLines(file.path(output,"general.config.txt"))
    
    # ---------- 1. FAST IO ----------
    message("Reading data with data.table::fread() ...")
    # res <- fread(file.path(output, "all_res_biomass_number_weight_height_daily.txt"))
    # env <- fread(file.path(output, "env.txt"))
    # 
    # res <- all_df <- readRDS(file.path(output, "all_df.rds"))
    # res <- as.data.table(res)
    # env <- all_df <- readRDS(file.path(output, "all_env.rds"))
    # env <- as.data.table(env)
    
    # head(res)
    # head(res[res$biomass >0,])
    load(file.path(lewSpec_dir, "data-raw/observed/Morphology.rda"))
    load(file.path(lewSpec_dir, "data/data_lakes_env_class.rda"))

    # ---------------------------------------------------------------------------------------------------------
    # --- add species group -------------------------------------------------------------------------------------------
    # scenario
    scenario <- unique(res$scenario)

    # get species group name
    species_raw <- strsplit(gen.conf[[6]][[1]], " ")[[1]] 
    species_path <- species_raw[-1]
    cleand <- sub("\\.config\\.txt$", "", species_path)
    species <- sub(".*(?=species_)", "", cleand, perl = TRUE)
    species_id <- as.numeric(unlist(str_extract_all(species, "\\d+")))

    group <- c()
    for(i in 1:length(species_path)){
        n <- read.table(species_path[i])
        group[i] <- n$V2[n$V1 == "Group"]
    }
    unique(group) 

    # add group
    res$speciesGroup <- NA
    env$speciesGroup <- NA

    res$speciesGroup[( res$speciesID > 14000) & (res$speciesID < 14301)] <- "oligotroph"
    res$speciesGroup[( res$speciesID > 15000) & (res$speciesID < 15301)] <- "mesotroph"
    res$speciesGroup[( res$speciesID > 16000) & (res$speciesID < 16301)] <- "eutroph"
    res$speciesGroup <- as.factor(res$speciesGroup)

    res$speciesID[is.na(res$speciesGroup)]

    env$speciesGroup[( env$speciesID > 14000) & (env$speciesID < 14301)] <- "oligotroph"
    env$speciesGroup[( env$speciesID > 15000) & (env$speciesID < 15301)] <- "mesotroph"
    env$speciesGroup[( env$speciesID > 16000) & (env$speciesID < 16301)] <- "eutroph"
    env$speciesGroup <- as.factor(env$speciesGroup)

    env$speciesID[is.na(env$speciesGroup)]

    # ---------------------------------------------------------------------------------------------------------
    # --- add lake groups -------------------------------------------------------------------------------------------
    # Turbidity: clear, intermediate, Turbid 
    # size: very.small, small, medium, large, very.large

    # --- Turbidity:  maximal summer temperature, nutrient content, and turbidity. 
    # Based on these four parameters we classified the lakes into 
    # clear, medium, and turbid lakes  
    # performing a hierarchical clustering using Euclidean distance and the Ward linkage method on normalized environmental data of the lakes. 
    head(Morphology)
    head(data_lakes_env_class) # Turbidity classes 

    lake_id <- data_lakes_env_class$Lake 
    lake_class <- data_lakes_env_class$class

    res$lakeClass <- NA
    env$lakeClass <- NA

    res$lakeClass <- lake_class[ match(res$lakeID, lake_id)]
    res$lakeClass <- as.factor(res$lakeClass)
    unique(res$lakeClass)

    env$lakeClass <- lake_class[ match(env$lakeID, lake_id)]
    env$lakeClass <- as.factor(env$lakeClass)
    unique(env$lakeClass)

    # --- size: Areagroup 
    # get Areakm2
    lake_area <- func_getAreaKm2(lake_path)

    lake_Agroup <- sapply(lake_area$areakm2, func_getAreaGroup)

    res$AreaGroup <- NA
    env$AreaGroup <- NA

    res$AreaGroup <- lake_Agroup[match(res$lakeID, lake_area$id)]
    env$AreaGroup <- lake_Agroup[match(env$lakeID, lake_area$id)]

    # --- lake Depth
    lake_depth <- func_getLakeDepth(lake_path)
    res$lakeDepth <- NA
    env$lakeDepth <- NA

    res$lakeDepth <- lake_depth$lakeDepth[match(res$lakeID, lake_depth$id)]
    env$lakeDepth <- lake_depth$lakeDepth[match(env$lakeID, lake_depth$id)]
    # head(res)
    save(env, file = file.path(save_figures, "env_dep10_Tprofile.RData"))
    save(res, file = file.path(save_figures, "res_dep10_Tprofile.RData"))
    print("Data loaded and lake groups added.")

    # ---------------------------------------------------------------------------------------------------------
    # ---- sort data Macrophyts ----------------------------------------------------------------------

    valid_res <- res[res$biomass > 0, ]

    sort_res <- valid_res %>%
        group_by(lakeClass, speciesGroup, AreaGroup, depth, day) %>%
        summarise(
            biomass_mean = mean(biomass), 
            numberInd_mean = mean(numberInd),
            indWeight_mean = mean(indWeight),
            height_mean = mean(height),
            lakeDepth_mean = mean(lakeDepth)
        ) %>% ungroup()  %>%
        mutate(AreaGroup = factor(AreaGroup,
                                    levels = c("very.small", "small", "medium", "large", "very.large")))
    head(sort_res)
    nrow(sort_res)

    unique(sort_res$day)
    save(sort_res, file = file.path(save_figures, "sortRES_dep10_Tprofile.RData"))
    print("Data sorted for Macrophytes.")
    # ---------------------------------------------------------------------------------------------------------
    # ---- sort data Environment ----------------------------------------------------------------------

    sort_env <- env %>%
        group_by(lakeClass, AreaGroup, day) %>%
        summarise(
            tempEpi_mean = mean(tempEpi), 
            tempHypo_mean = mean(tempHypo),
            metaDepth_mean = mean(metaDepth),
            irradiance_mean = mean(irradiance),
            waterlevel_mean = mean(waterlevel),
            lightAttenuation_mean = mean(lightAttenuation), 
            lakeDepth_mean = mean(lakeDepth)
        ) %>% ungroup()  %>%
        mutate(AreaGroup = factor(AreaGroup,
                                    levels = c("very.small", "small", "medium", "large", "very.large")))
    head(sort_env)
    nrow(sort_env)

    unique(sort_env$day)
    save(sort_env, file = file.path(save_figures, "sortENV_dep10_Tprofile.RData"))
    print("Data sorted for Environment.")
    print("Data preparation done.")
}

# --------------------------------------------------------------------------------------------------------------
# faster function -------------------------------------------- (ChatGPT)
func_prep_data_fast <- function(output, save_figures, lake_path, lewSpec_dir){
  
  # ---------------------------------------------------------------------------------------------------------
  # load data (keine Parallelisierung nötig – IO-limitiert)
  res <- read.table(file.path(output, "all_res_biomass_number_weight_height_daily.txt"), header =TRUE)
  env <- read.table(file.path(output,"env.txt"), header =TRUE)
  gen.conf <- readLines(file.path(output,"general.config.txt"))
  
  load(file.path(lewSpec_dir, "data-raw/observed/Morphology.rda"))
  load(file.path(lewSpec_dir, "data/data_lakes_env_class.rda"))
  
  # ---------------------------------------------------------------------------------------------------------
  # --- species groups (vektorisiert) -----------------------------------------------------------------------
  
  res$speciesGroup <- case_when(
    res$speciesID > 14000 & res$speciesID < 14301 ~ "oligotroph",
    res$speciesID > 15000 & res$speciesID < 15301 ~ "mesotroph",
    res$speciesID > 16000 & res$speciesID < 16301 ~ "eutroph"
  )
  
  env$speciesGroup <- case_when(
    env$speciesID > 14000 & env$speciesID < 14301 ~ "oligotroph",
    env$speciesID > 15000 & env$speciesID < 15301 ~ "mesotroph",
    env$speciesID > 16000 & env$speciesID < 16301 ~ "eutroph"
  )
  
  res$speciesGroup <- factor(res$speciesGroup)
  env$speciesGroup <- factor(env$speciesGroup)
  
  # ---------------------------------------------------------------------------------------------------------
  # --- lake classes (JOIN statt match) --------------------------------------------------------------------
  
  lake_class_df <- data_lakes_env_class %>%
    select(Lake, class) %>%
    rename(lakeID = Lake, lakeClass = class)
  
  res <- res %>% left_join(lake_class_df, by = "lakeID")
  env <- env %>% left_join(lake_class_df, by = "lakeID")
  
  res$lakeClass <- factor(res$lakeClass)
  env$lakeClass <- factor(env$lakeClass)
  
  # ---------------------------------------------------------------------------------------------------------
  # --- lake area group (JOIN + vektorisieren) -------------------------------------------------------------
  
  lake_area <- func_getAreaKm2(lake_path)
  lake_area$AreaGroup <- sapply(lake_area$areakm2, func_getAreaGroup)
  
  res <- res %>% left_join(lake_area %>% select(id, AreaGroup),
                           by = c("lakeID" = "id"))
  env <- env %>% left_join(lake_area %>% select(id, AreaGroup),
                           by = c("lakeID" = "id"))
  
  # ---------------------------------------------------------------------------------------------------------
  # --- lake depth (JOIN) ----------------------------------------------------------------------------------
  
  lake_depth <- func_getLakeDepth(lake_path)
  
  res <- res %>% left_join(lake_depth %>% select(id, lakeDepth),
                           by = c("lakeID" = "id"))
  env <- env %>% left_join(lake_depth %>% select(id, lakeDepth),
                           by = c("lakeID" = "id"))
  
  # Save intermediate
  save(env, file = file.path(save_figures, "env_dep10_Tprofile.RData"))
  save(res, file = file.path(save_figures, "res_dep10_Tprofile.RData"))
  
  print("Data loaded and lake groups added.")
  
  # ---------------------------------------------------------------------------------------------------------
  # ---- sort data Macrophytes (100% vektorisert, sehr schnell) --------------------------------------------
  
  sort_res <- res %>%
    filter(biomass > 0) %>%
    group_by(lakeClass, speciesGroup, AreaGroup, depth, day) %>%
    summarise(
      biomass_mean     = mean(biomass),
      numberInd_mean   = mean(numberInd),
      indWeight_mean   = mean(indWeight),
      height_mean      = mean(height),
      lakeDepth_mean   = mean(lakeDepth),
      .groups = "drop"
    ) %>%
    mutate(AreaGroup = factor(AreaGroup, 
                              levels=c("very.small","small","medium","large","very.large")))
  
  save(sort_res, file = file.path(save_figures, "sortRES_dep10_Tprofile.RData"))
  print("Data sorted for Macrophytes.")
  
  # ---------------------------------------------------------------------------------------------------------
  # ---- sort data Environment -----------------------------------------------------------------------------
  
  sort_env <- env %>%
    group_by(lakeClass, AreaGroup, day) %>%
    summarise(
      tempEpi_mean        = mean(tempEpi),
      tempHypo_mean       = mean(tempHypo),
      metaDepth_mean      = mean(metaDepth),
      irradiance_mean     = mean(irradiance),
      waterlevel_mean     = mean(waterlevel),
      lightAtt_mean       = mean(lightAttenuation),
      lakeDepth_mean      = mean(lakeDepth),
      .groups = "drop"
    ) %>%
    mutate(AreaGroup = factor(AreaGroup,
                              levels=c("very.small","small","medium","large","very.large")))
  
  save(sort_env, file = file.path(save_figures, "sortENV_dep10_Tprofile.RData"))
  print("Data sorted for Environment.")
  print("Data preparation done.")
}

# ---------------------------------------------------------------------------------------------------------
# Gradient potential species richness vs observed species richness  --------------------------------
# in data&visual_specPot-Observed. R

# multicore_data_table_prep.R

func_prep_data_dt_parallel <- function(output, save_figures, lake_path, lewSpec_dir, ncores = NULL){ # chatGPT help
  # ncores: NULL -> detectCores()-1
  if (is.null(ncores)) ncores <- max(1, detectCores() - 1)
  
  # ---------- 1. FAST IO ----------
  message("Reading data with data.table::fread() ...")
  # res <- fread(file.path(output, "all_res_biomass_number_weight_height_daily.txt"))
  # env <- fread(file.path(output, "env.txt"))
  
  res <- all_df <- readRDS(file.path(output, "all_df.rds"))
  res <- as.data.table(res)
  env <- all_df <- readRDS(file.path(output, "all_env.rds"))
  env <- as.data.table(env)

  gen.conf <- readLines(file.path(output,"general.config.txt"))
  load(file.path(lewSpec_dir, "data-raw/observed/Morphology.rda"))
  load(file.path(lewSpec_dir, "data/data_lakes_env_class.rda"))
  
  # ---------- 2. species groups (vectorized) ----------
  res[, speciesGroup := fifelse(speciesID > 14000 & speciesID < 14301, "oligotrophentic",
                                fifelse(speciesID > 15000 & speciesID < 15301, "mesotrophentic",
                                        fifelse(speciesID > 16000 & speciesID < 16301, "eutrophentic", NA_character_)))]
  env[, speciesGroup := fifelse(speciesID > 14000 & speciesID < 14301, "oligotrophentic",
                                fifelse(speciesID > 15000 & speciesID < 15301, "mesotrophentic",
                                        fifelse(speciesID > 16000 & speciesID < 16301, "eutrophentic", NA_character_)))]
  
  # ---------- 3. lakeClass join ----------
  lake_class_dt <- as.data.table(data_lakes_env_class)[, .(lakeID = Lake, lakeClass = class)]
  setkeyv(lake_class_dt, "lakeID")
  setkeyv(res, "lakeID"); setkeyv(env, "lakeID")
  res <- lake_class_dt[res]   # left join (lake_class_dt on left) -> preserves res cols
  env <- lake_class_dt[env]
  
  # ---------- 4. lake area and depth ----------
  lake_area <- func_getAreaKm2(lake_path)      # expect data.frame with id and areakm2
  lake_area_dt <- as.data.table(lake_area)[, .(id, areakm2)]
  lake_area_dt[, AreaGroup := sapply(areakm2, func_getAreaGroup)]
  setnames(lake_area_dt, "id", "lakeID")
  setkeyv(lake_area_dt, "lakeID")
  res <- lake_area_dt[res]
  env <- lake_area_dt[env]
  
  lake_depth <- func_getLakeDepth(lake_path)   # expect data.frame with id and lakeDepth
  lake_depth_dt <- as.data.table(lake_depth)
  setnames(lake_depth_dt, "id", "lakeID")
  setkeyv(lake_depth_dt, "lakeID")
  res <- lake_depth_dt[res]
  env <- lake_depth_dt[env]
  
  # ---------- 5. Save intermediate ----------
  save(env, file = file.path(save_figures, "env_dep10.RData"))
  save(res, file = file.path(save_figures, "res_dep10.RData"))
  message("Data loaded and lake groups added.")
  
  # ---------- 6. Parallel aggregation strategy ----------
  # Idea: split by lakeClass (oder AreaGroup) -> chunked parallel aggregations, danach rbindlist
  message("Starting parallel aggregation (macrophytes) ...")
  # keep only biomass>0
  res_filt <- res[biomass > 0]
  
  # choose splitting variable: lakeClass (adjust if too few groups)
  split_var <- "lakeClass"
  groups <- unique(res_filt[[split_var]])
  groups <- groups[!is.na(groups)]
  
  cl <- makeCluster(ncores)
  # export necessary objects and libraries
  clusterExport(cl, varlist = c("res_filt", "split_var"), envir = environment())
  clusterEvalQ(cl, { library(data.table) })
  
  # do parallel grouped summarise per chunk
  chunks <- split(groups, ceiling(seq_along(groups)/1)) # one group per chunk (fine, groups small)
  agg_list <- parLapplyLB(cl, chunks, function(grps){
    library(data.table)
    dt <- res_filt[get(split_var) %in% grps]
    # data.table fast aggregation
    out <- dt[, .(
      biomass_mean    = mean(biomass),
      numberInd_mean  = mean(numberInd),
      indWeight_mean  = mean(indWeight),
      height_mean     = mean(height),
      lakeDepth_mean  = mean(lakeDepth)
    ), by = .(lakeClass, speciesGroup, AreaGroup, depth, day)]
    setorderv(out, c("lakeClass","speciesGroup","AreaGroup","depth","day"))
    out
  })
  
  sort_res <- rbindlist(agg_list, use.names = TRUE, fill = TRUE)
  # ensure factor levels for AreaGroup
  sort_res[, AreaGroup := factor(AreaGroup, levels=c("very.small","small","medium","large","very.large"))]
  
  save(sort_res, file = file.path(save_figures, "sortRES_dep10.RData"))
  message("Data sorted for Macrophytes.")
  
  # ---------- Environment aggregation ----------
  message("Starting parallel aggregation (environment) ...")
  # split by lakeClass or AreaGroup
  split_var_env <- "lakeClass"
  groups_env <- unique(env[[split_var_env]])
  groups_env <- groups_env[!is.na(groups_env)]
  
  clusterExport(cl, varlist = c("env", "split_var_env"), envir = environment())
  
  agg_env_list <- parLapplyLB(cl, split(groups_env, seq_along(groups_env)), function(grp){
    library(data.table)
    dt <- env[get(split_var_env) %in% grp]
    out <- dt[, .(
      tempEpi_mean = mean(tempEpi),
      tempHypo_mean = mean(tempHypo),
      metaDepth_mean = mean(metaDepth),
      irradiance_mean = mean(irradiance),
      waterlevel_mean = mean(waterlevel),
      lightAttenuation_mean = mean(lightAttenuation),
      lakeDepth_mean = mean(lakeDepth)
    ), by = .(lakeClass, AreaGroup, day)]
    out
  })
  
  sort_env <- rbindlist(agg_env_list, use.names = TRUE, fill = TRUE)
  sort_env[, AreaGroup := factor(AreaGroup, levels=c("very.small","small","medium","large","very.large"))]
  
  save(sort_env, file = file.path(save_figures, "sortENV_dep10.RData"))
  message("Data sorted for Environment.")
  
  stopCluster(cl)
  message("All done.")
  # return(list(sort_res = sort_res, sort_env = sort_env))
}

# -----------------------------------------------------------------------------------------------
# Depth diversity gradient of potential and observed species richness (%): ------------------

func_DDG <- function(res_reshape, lewSpec_dir, scenario, save_figures){
  
  # NSPECbase
  surv_spec <- res_reshape %>%
    filter(Biomass_cat!= 0) %>%
    distinct(speciesID)
  
  NSPECtotal <- res_reshape %>% select(speciesID) %>% unique() %>% count()
  NLAKEStotal <- res_reshape %>% select(lakeID) %>% unique() %>% count()
  
  NSPECbase <- dim(surv_spec)[[1]]
  
  # DDG
  load(file.path(lewSpec_dir, "data/MAK_mapped_grouped.rda"))
  load(file.path(lewSpec_dir, "data/fulllakenames.rda"))
  # head(res_reshape)
  
  lakesDDGModel <- res_reshape %>% 
    group_by(Lake, Group) %>%
    summarise_at(vars(depth_1, depth_2, depth_3, depth_4), ~ sum(. != 0)) %>%
    gather("depth", "NSpec", c(3:6))%>%
    mutate(NSpecP=(NSpec/NSPECbase)*100) %>% 
    select(-NSpec) %>%
    mutate(type=paste0("model_", scenario))  
  # head(lakesDDGModel)
  
  lakesDDGMapped <- MAK_mapped_grouped %>%
    mutate(depth=ifelse(Depth==-5.0, "depth_4", 
                        ifelse(Depth==-3.0, "depth_3",
                               ifelse(Depth==-1.5, "depth_2",
                                      ifelse(Depth==-0.5, "depth_1",NA))))) %>%
    mutate(type="mapped") %>% 
    rename(NSpecP=NSPECperc) %>% 
    ungroup() %>%
    select(-NSPEC, -Depth, -Lake) %>% 
    relocate(depth, .after = Group) %>%
    relocate(LakeID, .before = Group) %>%
    rename(Lake=LakeID) %>%
    filter(Group!="none")%>%
    mutate(Group=ifelse(Group==1, "oligotroph", 
                        ifelse(Group==2, "mesotroph", 
                               ifelse(Group==3, "eutroph", NA)))) %>%
    filter(Lake %in% lakesDDGModel$Lake)
  # head(lakesDDGMapped)
  
  # check rows
  mapped_keys <- lakesDDGMapped %>% select(Lake, Group, depth)
  model_keys <- lakesDDGModel %>% select(Lake, Group, depth)
  
  mapped_missing <- anti_join(mapped_keys, model_keys, 
                              by = c("Lake","Group","depth")) # nur in mapped 
  
  if(nrow(mapped_missing) > 0){
    DDG_mapped_selected <- lakesDDGMapped %>%
      semi_join(mapped_missing, by = c("Lake", "Group", "depth"))
    DDG_mapped_selected$NSpecP <- 0
    DDG_mapped_selected$type <- paste0("model_", scenario)
    
    lakesDDGModel <- rbind(lakesDDGModel, DDG_mapped_selected)
  }
  
  
  lakesDDG2 <- rbind(lakesDDGModel,lakesDDGMapped)
  
  lakesDDG <- lakesDDG2 %>% 
    left_join(fulllakenames, by=c("Lake"="LakeID")) %>%
    rename(LakeName = Lake.y) %>% 
    mutate(depth=ifelse(depth=="depth_1","-0.5",
                        ifelse(depth=="depth_2","-1.5",
                               ifelse(depth=="depth_3","-3.0",
                                      ifelse(depth=="depth_4","-5.0","NA"))))) %>%
    mutate(Group=ifelse(Group=="eutroph","eutraphentic",
                        ifelse(Group=="mesotroph","mesotraphentic",
                               ifelse(Group=="oligotroph","oligotraphentic","NA"))))
  # head(lakesDDG)

  save(lakesDDG,
       file = file.path(save_figures, paste0("lakesDDG_dep10", scenario, ".RData")))
  
  return(list(lakesDDG = lakesDDG, NSPECbase = NSPECbase, NSPECtotal = NSPECtotal, NLAKEStotal = NLAKEStotal))
}
