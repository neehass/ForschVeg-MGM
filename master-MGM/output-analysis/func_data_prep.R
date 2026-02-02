library(dplyr)
library(tidyr)
library(stringr)
# multicore_data_table_prep.R
library(data.table)
library(parallel)

# Functions to prep modeloutput ---------------------------------------------------------------
# Function to process a single combination
process_one_chunk <- function(i, chunk_split, scenario_name){
  
  mod <- chunk_split[[i]]
  nameLAK  <- mod$lake
  nameSPEC <- mod$species
  Lid <- as.integer(unlist(stringr::str_extract_all(nameLAK, "\\d+")))
  Sid <- as.integer(unlist(stringr::str_extract_all(nameSPEC, "\\d+")))
  name <- as.character(paste0(Lid, "-", Sid))
  
  res <- mod$results
  env <- mod$environment
  
 # results ----------------------------------------
  df_depth <- vector("list", 4)
  res_bind <- vector("list", length(chunk_split))
  for(d in 1:4){
    data <- data.table::as.data.table(res[[d]]$data)
    data <- data.table::setDT(data)
    colnames(data) <- c("biomass", "numberInd", "indWeight", "height")
    data[,  name := name]
    data[,  depth := res[[d]]$depth]
    data[, speciesID := Sid]
    data[, lakeID := Lid]
    data[, day := 1:365]
    data[, scenario := scenario_name]
    df_depth[[d]] <- data
  }
  
  res_dt  <- data.table::rbindlist(df_depth)
  
  # env -------------------------

  env_dt  <- data.table::as.data.table(do.call(cbind, env))
  colnames(env_dt) <- c("tempEpi","tempHypo","metaDepth","irradiance","waterlevel","lightAttenuation")
  env_dt[,  name := name]
  env_dt[, speciesID := Sid]
  env_dt[, lakeID := Lid]
  env_dt[, day := 1:365]
  env_dt[, scenario := scenario_name]
  
  # return ------------------
  list(res = res_dt, env = env_dt)
}

process_modeloutput <- function(model, chunk_size, modelrun, scenario_name){
  
  ## model2 split
  chunks <- split(
    model,
    ceiling(seq_along(model) / chunk_size)
  )
  
  # length(chunks) 
  #View(chunks)
  
  all_res_list <- vector("list", length = length(chunks))
  all_env_list <- vector("list", length = length(chunks))
  # cluster settings
  # ncores <- detectCores() - 1
  # cl <- makeCluster(ncores)
  # clusterEvalQ(cl, { # load librarys
  #   library(stringr)
  #   library(data.table)
  # })
  # clusterExport(cl, varlist = c("chunk_split", "scenario_name"))
  
  for(c in 1:length(chunks)){
    chunk_split <- chunks[[c]]
    # View(chunk_split)
    
    message("saving macrophyte & environment data, chunk ", c, "/", length(chunks))
    
    # results ---------------------------
    # Parallel processing
    # chunk_list <- parLapply(cl, seq_along(chunk_split), process_chunk)
    
    # lapply faste - load to each core is slower
    
    out <- lapply(seq_along(chunk_split), process_one_chunk, chunk_split = chunk_split,
                  scenario_name = scenario_name)
   
    chunk_res <- data.table::rbindlist(lapply(out, `[[`, "res"))
    chunk_env <- data.table::rbindlist(lapply(out, `[[`, "env"))
    
    all_res_list[[c]] <- chunk_res
    all_env_list[[c]] <- chunk_env
    
    # save 
    chunk_path <- file.path("output", modelrun, "chunks")
    if(!dir.exists(chunk_path)){dir.create(chunk_path)}
    
    saveRDS(chunk_res,  file = file.path(chunk_path, paste0("res_chunk_",c,".rds")))
    rm(chunk_res)
    gc() # only removes unreachable memory
    
    saveRDS(chunk_env,  file = file.path(chunk_path, paste0("env_chunk_",c,".rds")))
    rm(chunk_env)
    gc() # only removes unreachable memory

    message("chunk saved")
    
  }
  # parallel::stopCluster(cl)
  return(list(res = all_res_list, env = all_env_list))
}


# ---------------------------------------------------------------------------------------------------------
# Function to prepare data --------------------------------------------------------------------------------

func_prep_data <- function(output, save_figures, lake_path, lewSpec_dir){
  # ---------- 1. FAST IO ----------
  message("Reading data with data.table::fread() ...")
  # res <- fread(file.path(output, "all_res_biomass_number_weight_height_daily.txt"))
  # env <- fread(file.path(output, "env.txt"))
  
  res <- readRDS(file.path(output, "all_res.rds"))
  res <- as.data.table(res)
  env <- readRDS(file.path(output, "all_env.rds"))
  env <- as.data.table(env)
  
  res$lakeID <- as.numeric(res$lakeID)
  env$lakeID <- as.numeric(env$lakeID)
  
  gen.conf <- readLines(file.path(output,"general.config.txt"))
  load(file.path(lewSpec_dir, "data-raw/observed/Morphology.rda"))
  load(file.path(lewSpec_dir, "data/data_lakes_env_class.rda"))
  
  # ---------- 2. species groups (vectorized) ----------
  res[, speciesGroup := fifelse(speciesID > 14000 & speciesID < 14301, "oligotraphentic",
                                fifelse(speciesID > 15000 & speciesID < 15301, "mesotraphentic",
                                        fifelse(speciesID > 16000 & speciesID < 16301, "eutraphentic", NA_character_)))]
  env[, speciesGroup := fifelse(speciesID > 14000 & speciesID < 14301, "oligotraphentic",
                                fifelse(speciesID > 15000 & speciesID < 15301, "mesotraphentic",
                                        fifelse(speciesID > 16000 & speciesID < 16301, "eutraphentic", NA_character_)))]
  
  # ---------- 3. lakeClass join ----------
  res <- as.data.table(res)
  lake_class_dt <- as.data.table(data_lakes_env_class)[, .(lakeID = Lake, lakeClass = class)]
  res <- res[lake_class_dt, on = "lakeID"] 
  env <- env[lake_class_dt, on = "lakeID"]
  
  # ---------- 4. lake area and depth ----------
  lake_area <- func_getAreaKm2(lake_path)      # expect data.frame with id and areakm2
  lake_area_dt <- as.data.table(lake_area)[, .(id, areakm2)]
  lake_area_dt[, AreaGroup := sapply(areakm2, func_getAreaGroup)]
  setnames(lake_area_dt, "id", "lakeID")
  res <- res[lake_area_dt, on = "lakeID"]
  env <- env[lake_area_dt, on = "lakeID"]
  
  lake_depth <- func_getLakeDepth(lake_path)   # expect data.frame with id and lakeDepth
  lake_depth_dt <- as.data.table(lake_depth)
  setnames(lake_depth_dt, "id", "lakeID")
  res <- res[lake_depth_dt, on = "lakeID"]
  env <- env[lake_depth_dt, on = "lakeID"] # left join (lake_class_dt on left) -> preserves res cols
  
  # ---------- 5. Save intermediate ----------
  message("start saving - Data loaded and lake groups added.")
  saveRDS(res, file = file.path(output, "added_all_res.rds"))
  saveRDS(env, file = file.path(output, "added_all_env.rds"))
  message("saved")

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
  saveRDS(sort_res, file = file.path(save_figures, "sortRES.rds"))
  message("sorted data saved Macrophytes.")
  
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
  sort_env <- sort_env[!is.na(sort_env$lakeClass),]
  sort_env <- sort_env[!is.na(sort_env$AreaGroup),]
  
  saveRDS(sort_env, file = file.path(save_figures, "sortENV.rds"))
  message("sorted data saved Environment.")
  message("Data preparation done.")
}

# --------------------------------------------------------------------------------------------------------------
# not faster !!! - more time to load it to coreas!! 
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
  
  res <- readRDS(file.path(output, "all_res.rds"))
  res <- as.data.table(res)
  env <- readRDS(file.path(output, "all_env.rds"))
  env <- as.data.table(env)
  
  res$lakeID <- as.numeric(res$lakeID)
  env$lakeID <- as.numeric(env$lakeID)

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
  res <- as.data.table(res)
  lake_class_dt <- as.data.table(data_lakes_env_class)[, .(lakeID = Lake, lakeClass = class)]
  res <- res[lake_class_dt, on = "lakeID"] 
  env <- env[lake_class_dt, on = "lakeID"]
  
  # ---------- 4. lake area and depth ----------
  lake_area <- func_getAreaKm2(lake_path)      # expect data.frame with id and areakm2
  lake_area_dt <- as.data.table(lake_area)[, .(id, areakm2)]
  lake_area_dt[, AreaGroup := sapply(areakm2, func_getAreaGroup)]
  setnames(lake_area_dt, "id", "lakeID")
  res <- res[lake_area_dt, on = "lakeID"]
  env <- env[lake_area_dt, on = "lakeID"]
  
  lake_depth <- func_getLakeDepth(lake_path)   # expect data.frame with id and lakeDepth
  lake_depth_dt <- as.data.table(lake_depth)
  setnames(lake_depth_dt, "id", "lakeID")
  res <- res[lake_depth_dt, on = "lakeID"]
  env <- env[lake_depth_dt, on = "lakeID"] # left join (lake_class_dt on left) -> preserves res cols
  
  # ---------- 5. Save intermediate ----------
  message("start saving - Data loaded and lake groups added.")
  saveRDS(res, file = file.path(save_figures, "all_res.rds"))
  saveRDS(env, file = file.path(save_figures, "all_env.rds"))
  message("saved")
  
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
  agg_list <- parLapplyLB(cl, chunks, function(grps){ # parLapplyLB = LB load balancing Workers grab tasks dynamically as they finish
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
  
  saveRDS(sort_res, file = file.path(save_figures, "sortRES_dep10.rds"))
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
  sort_env <- na.omit(sort_env5)
  saveRDS(sort_env, file = file.path(save_figures, "sortENV_dep10.rds"))
  message("Data sorted for Environment.")
  
  stopCluster(cl)
  message("All done.")
  # return(list(sort_res = sort_res, sort_env = sort_env))
}

# -----------------------------------------------------------------------
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
    mutate(dataset=paste0("model_", scenario))  
  # head(lakesDDGModel)
  
  # drop na 
  lakesDDGModel <- lakesDDGModel[!is.na(lakesDDGModel$Group), ]
  # View(lakesDDGModel)
  
  lakesDDGMapped <- MAK_mapped_grouped %>%
    mutate(depth=ifelse(Depth==-5.0, "depth_4", 
                        ifelse(Depth==-3.0, "depth_3",
                               ifelse(Depth==-1.5, "depth_2",
                                      ifelse(Depth==-0.5, "depth_1",NA))))) %>%
    mutate(dataset="mapped") %>% 
    rename(NSpecP=NSPECperc) %>% 
    ungroup() %>%
    select(-NSPEC, -Depth, -Lake) %>% 
    relocate(depth, .after = Group) %>%
    relocate(LakeID, .before = Group) %>%
    rename(Lake=LakeID) %>%
    filter(Group!="none")%>%
    mutate(Group=ifelse(Group==1, "oligotraphentic", 
                        ifelse(Group==2, "mesotraphentic", 
                               ifelse(Group==3, "eutraphentic", NA)))) %>%
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
    DDG_mapped_selected$dataset <- paste0("model_", scenario)
    
    lakesDDGModel <- rbind(lakesDDGModel, DDG_mapped_selected)
  }
  # View(lakesDDG2)
  
  lakesDDG2 <- rbind(lakesDDGModel,lakesDDGMapped)
  
  lakesDDG <- lakesDDG2 %>% 
    left_join(fulllakenames, by=c("Lake"="LakeID")) %>%
    rename(LakeName = Lake.y) %>% 
    mutate(depth=ifelse(depth=="depth_1","-0.5",
                        ifelse(depth=="depth_2","-1.5",
                               ifelse(depth=="depth_3","-3.0",
                                      ifelse(depth=="depth_4","-5.0","NA"))))) 
  # head(lakesDDG)

  save(lakesDDG,
       file = file.path(save_figures, paste0("lakesDDG_dep10", scenario, ".RData")))
  
  return(list(lakesDDG = lakesDDG, NSPECbase = NSPECbase, NSPECtotal = NSPECtotal, NLAKEStotal = NLAKEStotal))
}

# for depth > 5m 
func_DDG_deep <- function(res_reshape, lewSpec_dir, scenario, save_figures){
  
  # NSPECbase
  surv_spec <- res_reshape %>%
    filter(Biomass_cat!= 0) %>%
    distinct(speciesID)
  
  NSPECtotal <- res_reshape %>% select(speciesID) %>% unique() %>% count()
  NLAKEStotal <- res_reshape %>% select(lakeID) %>% unique() %>% count()
  
  NSPECbase <- dim(surv_spec)[[1]]
  
  # DDG
  
  load(file.path(lewSpec_dir, "data/fulllakenames.rda"))
  # head(res_reshape)
  
  lakesDDGModel <- res_reshape %>% 
    group_by(Lake, Group) %>%
    summarise_at(vars(depth_1, depth_2, depth_3, depth_4, 
                      depth_5, depth_6, depth_7, depth_8), ~ sum(. != 0)) %>%
    gather("depth", "NSpec", c(3:10))%>%
    mutate(NSpecP=(NSpec/NSPECbase)*100) %>% 
    select(-NSpec) %>%
    mutate(dataset=paste0("model_", scenario))  
  # head(lakesDDGModel) 
  # unique(lakesDDGModel$depth)
  
  # drop na 
  lakesDDGModel <- lakesDDGModel[!is.na(lakesDDGModel$Group), ]
  # View(lakesDDGModel)
 
  # head(lakesDDGMapped)
  
  lakesDDG <- lakesDDGModel %>% 
    left_join(fulllakenames, by=c("Lake"="LakeID")) %>%
    rename(LakeName = Lake.y) %>% 
    mutate(depth=ifelse(depth=="depth_1","-0.5",
                        ifelse(depth=="depth_2","-1.5",
                               ifelse(depth=="depth_3","-3.0",
                                      ifelse(depth=="depth_4","-5.0",
                                             ifelse(depth=="depth_5","-5.5",
                                                    ifelse(depth=="depth_6","-6.0",
                                                           ifelse(depth=="depth_7","-7.0",
                                                                  ifelse(depth=="depth_8","-9.0","NA"))))))))) 
  # head(lakesDDG)
  # unique(lakesDDG$depth)
  
  save(lakesDDG,
       file = file.path(save_figures, paste0("lakesDDG_dep10", scenario, ".RData")))
  
  return(list(lakesDDG = lakesDDG, NSPECbase = NSPECbase, NSPECtotal = NSPECtotal, NLAKEStotal = NLAKEStotal))
}


# -----------------------------------------------------------------------------------------------------
# data prep for comparison
func_dataprep_comparison <- function(res1, res2, name1 = "_baseTP", name2 = "_baseTS", save_comparison){
  # prepare base T_profile data
  res1_prep <- res1 %>%
    mutate(scenario = "base_Tprofile") %>%
    group_by(lakeID, speciesID, depth, speciesGroup, lakeClass) %>%
    summarise(
      Biomass_cat = if_else(mean(biomass, na.rm = TRUE) > 0, 1, 0, missing = 0))
  
  saveRDS(res1_prep, file = file.path(save_comparison, paste0("res", name1, "_prep.rds")))
  
  # prepare T_steady data
  res2_prep <- res2 %>%
    mutate(scenario = "base_Tsteady") %>%
    group_by(lakeID, speciesID, depth, speciesGroup, lakeClass) %>%
    summarise(
      Biomass_cat = if_else(mean(biomass, na.rm = TRUE) > 0, 1, 0, missing = 0))
  
  saveRDS(res2_prep, file = file.path(save_comparison, paste0("res", name2, "_prep.rds")))
  
  
  # combine both datasets
  res_combined <- res1_prep %>%
    left_join(res2_prep, by = c("lakeID", "depth", "speciesID", "speciesGroup", "lakeClass"), 
              suffix = c(name1, name2)) %>%
    mutate(
      Biomass_cat_baseTP = replace_na(Biomass_cat_baseTP, 0),
      Biomass_cat_baseTS   = replace_na(Biomass_cat_baseTS, 0)
    ) %>%
    rename(
      baseTP = Biomass_cat_baseTP,
      baseTS = Biomass_cat_baseTS
    )
  
  print(any(res_combined$baseTP != res_combined$baseTS)) # TRUE  -> mindestens ein Wert ist unterschiedlich
  saveRDS(res_combined, file = file.path(save_comparison, "res_combined_base.rds"))
  
  return(list(res1_prep = res1_prep, res2_prep = res2_prep, res_combined = res_combined))
}

# ----------------------------------------------------------
# DDG DATA PREP BIOMASS Comparision between T_profile vs without T_profile ------------------
# Depth diversity gradient of potential and observed species richness (%):

func_dataprep_compare_DDG <- function(res1, res2, name1 = "base_Tprofile", name2 = "base_Tsteady", save_comparison){
  # prepare base T_profile data
  res1_prep <- res1 %>%
    group_by(lakeClass, speciesGroup, depth, lakeID, speciesID) %>%
    summarise(biomass = sum(biomass)) %>%  ungroup() %>%
    mutate(biomass_orig = biomass) %>%
    mutate(depth_label = paste0("depth_", dense_rank(abs(depth)))) %>% # abs wichitg hier sonst werden depths falsch herum zugeordnet!
    pivot_wider(
      names_from = depth_label,
      values_from = biomass
    ) %>% relocate(biomass_orig) %>%
    
    # Replace NA in all pivoted columns with 0
    replace_na(list(
      depth_1 = 0, # -0.5
      depth_2 = 0,
      depth_3 = 0,
      depth_4 = 0
    )) %>% 
    mutate(Biomass_cat = if_else(biomass_orig > 0, 1, 0, missing = 0))# %>% filter(biomass_orig!= 0)
  
  res1_prep$Group <- as.factor(res1_prep$speciesGroup)
  res1_prep$Species <- res1_prep$speciesID
  res1_prep$Lake <- paste0("lake_", res1_prep$lakeID)
  res1_prep$scenario <- name1
  
  saveRDS(res1_prep, file = file.path(save_comparison, paste0("DDG_reshape_",name1,".rds")))
  
  # prepare T_steady data
  res2_prep <- res2 %>%
    group_by(lakeClass, speciesGroup, depth, lakeID, speciesID) %>%
    summarise(biomass = sum(biomass)) %>%  ungroup() %>%
    mutate(biomass_orig = biomass) %>%
    mutate(depth_label = paste0("depth_", dense_rank(abs(depth)))) %>% # abs wichitg, sonst depth falschherum 
    pivot_wider(
      names_from = depth_label,
      values_from = biomass
    ) %>%  relocate(biomass_orig) %>%
    
    # Replace NA in all pivoted columns with 0
    replace_na(list(
      depth_1 = 0, # -0.5
      depth_2 = 0,
      depth_3 = 0,
      depth_4 = 0
    )) %>% 
    mutate(Biomass_cat = if_else(biomass_orig > 0, 1, 0, missing = 0))# %>% filter(biomass_orig!= 0)
  
  res2_prep$Group <- as.factor(res2_prep$speciesGroup)
  res2_prep$Species <- res2_prep$speciesID
  res2_prep$Lake <- paste0("lake_", res2_prep$lakeID)
  res2_prep$scenario <- name2
  
  saveRDS(res2_prep, file = file.path(save_comparison, paste0("DDG_reshape_",name2,".rds")))
  
  return(list(res1_prep= res1_prep, res2_prep = res2_prep))
}

# -------------------------------------
# data prep for DDG
func_prep_DDG <- function(output, lewSpec_dir, save_out, name1){
  
    # load data --------------------------------------------------------------------------------
  load(file.path(lewSpec_dir, "data/data_lakes_env_class.rda")) # lake info
  
  res <- readRDS(file.path(output, "added_all_res.rds")) 
  
  gen.conf <- readLines(file.path(output,"general.config.txt"))
  k <- as.numeric(strsplit(gen.conf[8], " ")[[1]][2])
  
  unique(res$scenario)

  load(file.path(lewSpec_dir, "data/all_diff_presabs_tobase.rda"))
  head(all_diff_presabs_tobase)
  # bring data in this format
  # ------------------------------------------------------------------------------------------
  # prepare data for DDG
  res1_prep <- res %>%
    group_by(lakeClass, speciesGroup, depth, lakeID, speciesID) %>%
    summarise(biomass = sum(biomass)) %>%  ungroup() %>%
    mutate(biomass_orig = biomass) %>%
    mutate(depth_label = paste0("depth_", dense_rank(abs(depth)))) %>% # abs wichitg hier sonst werden depths falsch herum zugeordnet!
    pivot_wider(
      names_from = depth_label,
      values_from = biomass
    ) %>% relocate(biomass_orig) %>%
    
    # Replace NA in all pivoted columns with 0
    replace_na(list(
      depth_1 = 0, # -0.5
      depth_2 = 0,
      depth_3 = 0,
      depth_4 = 0
    )) %>% 
    mutate(Biomass_cat = if_else(biomass_orig > 0, 1, 0, missing = 0))# %>% filter(biomass_orig!= 0)
  
  res1_prep$Group <- as.factor(res1_prep$speciesGroup)
  res1_prep$Species <- res1_prep$speciesID
  res1_prep$Lake <- paste0("lake_", res1_prep$lakeID)
  res1_prep$scenario <- name1
  
  dir.create(save_out)
  saveRDS(res1_prep, file = file.path(save_out, paste0("DDG_reshape_",name1,".rds")))
  
}

# ---------------------------------------------------------------------------------------------------
# data prep for Biomass comparison 
func_prepBIO_compare <- function(res_baseTP, scenTP, res_baseTS, scenTS, save_out){
  
  # Presence Absence
  # ------------------------------------------------------------------------------------------
  # prepare base T_profile data
  res_baseTprofile_prep <- res_baseTP %>%
    mutate(scenario = "base_Tprofile") %>%
    group_by(lakeID, speciesID, depth, speciesGroup, lakeClass) %>%
    summarise(
      Biomass_cat = if_else(mean(biomass, na.rm = TRUE) > 0, 1, 0, missing = 0))
  head(res_baseTprofile_prep)
  save(res_baseTprofile_prep, file = file.path(save_comparison, paste0("res_", scenTP, "_prep.rda")))
  
  # prepare T_steady data
  res_baseTSteady_prep <- res_baseTS %>%
    mutate(scenario = "base_Tsteady") %>%
    group_by(lakeID, speciesID, depth, speciesGroup, lakeClass) %>%
    summarise(
      Biomass_cat = if_else(mean(biomass, na.rm = TRUE) > 0, 1, 0, missing = 0))
  head(res_baseTSteady_prep)
  save(res_baseTSteady_prep, file = file.path(save_comparison, paste0("res_", scenTS, "_prep.rda")))
  
  
  # combine both datasets
  res_combined <- res_baseTprofile_prep %>%
    left_join(res_baseTSteady_prep, by = c("lakeID", "depth", "speciesID", "speciesGroup", "lakeClass"), 
              suffix = c("_baseTP", "_baseTS")) %>%
    mutate(
      Biomass_cat_baseTP = replace_na(Biomass_cat_baseTP, 0),
      Biomass_cat_baseTS   = replace_na(Biomass_cat_baseTS, 0)
    ) %>%
    rename(
      baseTP = Biomass_cat_baseTP,
      baseTS = Biomass_cat_baseTS
    )
  head(res_combined)
  any(is.na(res_combined))
  unique(res_combined$baseTP)
  unique(res_combined$baseTS)
  
  any(res_combined$baseTP != res_combined$baseTS) # TRUE  -> mindestens ein Wert ist unterschiedlich
  save(res_combined, file = file.path(save_comparison, "res_combined_base.rda"))
  
  # PERMANOVA DATA PREP BIOMASS Comparision between T_profile vs without T_profile ------------------
  
  # ------------------------------------------------------------------------------------------
  # prepare base T_profile data
  res_baseTprofile_bio <- res_baseTP %>%
    mutate(scenario = scenTP) %>%
    group_by(lakeID, speciesID, depth, speciesGroup, lakeClass) %>%
    summarise(
      biomass = mean(biomass, na.rm = TRUE))
  
  save(res_baseTprofile_bio, file = file.path(save_out, paste0("res_", scenTP,"_bio.rda")))
  
  # prepare T_steady data
  res_baseTSteady_bio <- res_baseTS %>%
    mutate(scenario = scenTS) %>%
    group_by(lakeID, speciesID, depth, speciesGroup, lakeClass) %>%
    summarise(
      biomass = mean(biomass, na.rm = TRUE))
  
  save(res_baseTSteady_bio, file = file.path(save_out, paste0("res_", scenTS,"_bio.rda")))
  
  
  # combine both datasets
  res_combined_BIO <- res_baseTprofile_bio %>%
    left_join(res_baseTSteady_bio, by = c("lakeID", "depth", "speciesID", "speciesGroup", "lakeClass"), 
              suffix = c("_baseTP", "_baseTS")) %>%
    mutate(
      biomass_baseTP = replace_na(biomass_baseTP, 0),
      biomass_baseTS   = replace_na(biomass_baseTS, 0)
    ) %>%
    rename(
      baseTP = biomass_baseTP,
      baseTS = biomass_baseTS
    )
  # head(res_combined_BIO)
  # any(is.na(res_combined_BIO))
  # unique(res_combined_BIO$baseTP)
  # unique(res_combined_BIO$baseTS)
  
  any(res_combined_BIO$baseTP != res_combined_BIO$baseTS) # TRUE  -> mindestens ein Wert ist unterschiedlich
  save(res_combined_BIO, file = file.path(save_out, "res_combined_base_BIO.rda"))
}

# --------------------------------------------------
# DDG comparison tbale:
# Potential species richness per lake type and species group (%):
func_DDG_table <- function(reshape_data, NSPECbase,lewSpec_dir){
  load(file.path(lewSpec_dir, "data/data_lakes_env_class.rda")) # lake info
  
  Nspecies_P_groupes<-reshape_data %>%
    left_join((data_lakes_env_class %>% 
                 select(Lake, class)%>%
                 mutate(Lake=paste0("lake_",Lake))),
              by=c("Lake"))%>%
    group_by(class, speciesID, Group) %>%
    summarise_at(vars(Biomass_cat), ~ sum(. != 0)) %>%
    mutate(Biomass_cat=ifelse(Biomass_cat>0,1,0))%>%
    
    ungroup() %>% group_by(class, Group) %>%
    summarise(Biomass_cat=sum(Biomass_cat)) %>%
    rename("Nspecies" = Biomass_cat) %>%
    mutate(Nspecies_P = (Nspecies/NSPECbase)*100) %>%
    select(-Nspecies) %>%
    spread(Group, Nspecies_P)
  
  Nspecies_P_groupes_all<-reshape_data %>%
    left_join((data_lakes_env_class %>% 
                 select(Lake, class)%>%
                 mutate(Lake=paste0("lake_",Lake))),
              by=c("Lake"))%>%
    
    group_by(class, speciesID) %>%
    summarise_at(vars(Biomass_cat), ~ sum(. != 0)) %>%
    mutate(Biomass_cat=ifelse(Biomass_cat>0,1,0))%>%
    
    ungroup() %>% group_by(class) %>%
    summarise(Biomass_cat=sum(Biomass_cat)) %>%
    rename("Nspecies" = Biomass_cat) %>%
    mutate(Nspecies_P_all = (Nspecies/NSPECbase)*100) %>%
    select(-Nspecies) 
  
  return(list(Nspecies_P_groupes = Nspecies_P_groupes, Nspecies_P_groupes_all  = Nspecies_P_groupes_all))
}

# Observes species richness within lake types and per species groups (%)
func_DDG_observ_table <- function(lewSpec_dir){
  load(file.path(lewSpec_dir, "data/Makroph_comm_S.rda")) # observed data
  load(file.path(lewSpec_dir, "data/GroupsSpecies.rda")) # observed data
  load(file.path(lewSpec_dir, "data/MAK_MAPPED_NSPEC_indicationspec.rda")) # observed data
  
  
  Nspaclakclass<-Makroph_comm_S %>%
    gather("Species", "Kohler",5:89) %>%
    mutate(Species = str_replace(Species, " ", "."))%>%
    left_join(GroupsSpecies, by = c("Species"="Taxon")) %>%
    select(-Trophie,-Typ, -Depth) %>%
    mutate(Gruppe = ifelse(is.na(Gruppe),"none", Gruppe)) %>%
    filter(!Lake %in% c("Altmuehlsee", "Drachensee", "Eixendorfer See",
                        "Grosser Brombachsee",
                        "Gruentensee","Igelsbachsee", "Kleiner Brombachsee",
                        "Liebensteinspeicher","Rottachsee",
                        "Steinberger See","Hofstaetter See",
                        "Untreusee","Walchensee","Murnersee","Rothsee",
                        "Seehamer See"))%>%
    rename(Group=Gruppe)%>%
    ungroup() %>%
    group_by(Lake) %>%
    filter(YEAR==max(YEAR))%>% #select last mapping per lake
    ungroup() %>%
    left_join(data_lakes_env_class, by=c("Lake"="LakeName")) %>%
    rename(LakeID=Lake.y) %>%
    
    
    group_by(Species, Group, class) %>%
    summarise_at(vars("Kohler"), sum, na.rm=T)%>% # Mean of Kohler per Lake, YEAR, Depth, Species and Group
    ungroup()%>%
    group_by(Group, class) %>%
    mutate(Kohler=ifelse(Kohler>0,1,0)) %>%
    summarise(NSPEC=sum(Kohler)) %>%
    mutate(NSPECperc = NSPEC/MAK_MAPPED_NSPEC_indicationspec$n*100) %>%
    select(-NSPEC)%>%
    spread(Group,NSPECperc)%>%
    rename(oligotrophic="1", mesotrophic="2", eutrophic="3")
  
  
  Nspaclakclass_all<-Makroph_comm_S %>%
    gather("Species", "Kohler",5:89)%>%
    mutate(Species = str_replace(Species, " ", ".")) %>%
    left_join(GroupsSpecies, by = c("Species"="Taxon")) %>%
    select(-Trophie,-Typ, -Depth) %>%
    mutate(Gruppe = ifelse(is.na(Gruppe),"none", Gruppe)) %>%
    filter(!Lake %in% c("Altmuehlsee", "Drachensee", "Eixendorfer See",
                        "Grosser Brombachsee",
                        "Gruentensee","Igelsbachsee", "Kleiner Brombachsee",
                        "Liebensteinspeicher","Rottachsee",
                        "Steinberger See","Hofstaetter See",
                        "Untreusee","Walchensee","Murnersee","Rothsee",
                        "Seehamer See"))%>%
    rename(Group=Gruppe)%>%
    ungroup() %>%
    group_by(Lake) %>%
    filter(YEAR==max(YEAR))%>% #select last mapping per lake
    ungroup() %>%
    left_join(data_lakes_env_class, by=c("Lake"="LakeName")) %>%
    rename(LakeID=Lake.y) %>%
    filter(Group!="none")%>%
    
    group_by(Species, class) %>%
    summarise_at(vars("Kohler"), sum, na.rm=T)%>% # Mean of Kohler per Lake, YEAR, Depth, Species and Group
    ungroup()%>%
    group_by(class) %>%
    mutate(Kohler=ifelse(Kohler>0,1,0)) %>%
    summarise(NSPEC=sum(Kohler)) %>%
    mutate(NSPECperc_all = NSPEC/MAK_MAPPED_NSPEC_indicationspec$n*100) %>%
    select(-NSPEC)
  
  return(list(Nspaclakclass = Nspaclakclass, Nspaclakclass_all  = Nspaclakclass_all, NsBase = MAK_MAPPED_NSPEC_indicationspec))
}
