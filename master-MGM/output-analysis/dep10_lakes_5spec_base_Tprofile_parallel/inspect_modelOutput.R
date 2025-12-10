# inspect model output

model_Tprofile_par <- "output/dep10_lakes_5spec_base_Tprofile_parallel"
save <- "output-analysis/test_Tprofile_parallel_model"

dir.create(save)

model <- readRDS(file.path(model_Tprofile_par, "model.rds"))
View(model)

gen.conf <- readLines(file.path(model_Tprofile_par,"general.config.txt"))

# species 
species <- strsplit(gen.conf[6], " ")[[1]]
species <- species[2:length(species)]

NSpec <- length(species)
species_id <- unlist(str_extract_all(species, "\\d+"))

# lake 
lake <- strsplit(gen.conf[5], " ")[[1]]
lake <- lake[2:length(lake)]

Nlak <- length(lake)
lake_id <- unlist(str_extract_all(lake, "\\d+"))

# get output lake[1]
# iLak = idx for lake 1 = Nspec * 2 (macro + environment)

iLAK <- 1:(NSpec * 2) # idx for lake[1]

# get output species[1]
# iSPEC[1]  idx for specei[1] = 1 = iLAK[iSPEC]
iSpec <- 1:NSpec
idx <- iLAK[iSpec[1]]
res <- model[[1]] # "biomass", "numberInd", "indWeight", "height"
view(res) # for 4 depths
res_sel <- as.data.table(res[[4]]) # depth1 

colnames(res_sel) <- c("biomass", "numberInd", "indWeight", "height")

res_sel[, "lakeID" := rep(lake_id[1], nrow(res_sel))]
res_sel[, "speciesID" := rep(species_id[1], nrow(res_sel))]
# View(res_sel)
# class(res_sel)
  
# get output species[1] - ENV
# iSPECenv  idx for specei[1] = 2 = iLAK[iSPEC+1]
env_all <- list()
idx_env <- iLAK[iSpec[1]+1]
env <- model[[idx_env]] # "tempEpi", "tempHypo", "metaDepth", "irradiance", "waterlevel", "lightAttenuation"

env_list <- lapply(env, as.list) # JuliaTulp umwandeln 
env_dt <- as.data.table(t(rbindlist(env_list)))
colnames(env_dt) <- c("tempEpi", "tempHypo", "metaDepth", "irradiance", "waterlevel", "lightAttenuation")
env_dt[, "lakeID" := rep(lake_id[1], nrow(env_dt))]
env_dt[, "speciesID" := rep(species_id[1], nrow(env_dt))]
# View(env_dt)

env_all[[lake_id[1]]] <- env_dt


###################
# loop save 

# get output lake[1]
# iLak = idx for lake 1 = Nspec * 2 (macro + environment)
iLAK <- 1:Nlak # idx for lake[1]
iSpec <- 1:NSpec
iCOMBO <- 1:(NSpec*Nlak) # data combo (species pro lake)
length(iCOMBO)
iDepth <- 1:4 # depth 1:4 (deep to shallow)
scenario_name <- "test-model-out"
cl <- ncores <- detectCores() - 1
cl <- makeCluster(ncores)
# macrophyt data = 1,3,5...
idxmacro <- seq(1, NSpec*2*Nlak, 2)
length(idxmacro)*2 == length(model)

# env data = 2,4,6
idxenv <- seq(2, NSpec*2*Nlak, 2)
length(idxenv)*2 == length(model)

# lake = NSpec 1-27 = lake1, 28-54 = lake1
idxlak <- seq(NSpec, NSpec*2*Nlak, NSpec)
idxlak_start <- seq(1, NSpec*2*Nlak, NSpec)
length(idxlak_start) == 2*Nlak
length(idxlak) == 2*Nlak

# bsp lake 1
im <- iCOMBO[idxlak_start[1]:idxlak[1]]
res_lak1 <- model[idxmacro[im]]
length(res_lak1)
View(res_lak1)
# species 1 depth -0.5 (depth 4)
spec1_dep4 <- as.data.table(res_lak1[[1]][[4]])
colnames(spec1_dep4) <- c("biomass", "numberInd", "indWeight", "height")
View(spec1_dep4) # depth 1:4 (deep to shallow)

# --------------------------------------------------------------
# macrophyte data
df_all <- vector("list", Nlak)
for(l in iLAK){
  im <- iCOMBO[idxlak_start[l]:idxlak[l]]
  res_lak <- model[idxmacro[im]]
  
  df_lak <- vector("list", NSpec)
  for(s in iSpec){
    
    print(s)
    res <- res_lak[[s]] # 15 
    df_depth <- vector("list", 4)
    
    for(d in 1:4){
      df <- as.data.frame(res[[d]])
      colnames(df) <- c("biomass", "numberInd", "indWeight", "height")
      df$depth <- depths[d]
      df$speciesID <- species_id[s]
      df$lakeID <- lake_id[l]
      df$day <- 1:365
      df$scenario <- scenario_name
      df_depth[[d]] <- df
    }
    depth_bind <- do.call(rbind, df_depth)
    df_lak[[s]] <- depth_bind
  }
  
  lak_bind <- do.call(rbind, df_lak)
  df_all[[l]] <- lak_bind
}
View(df_all)

# final binding
final_res <- do.call(rbind, df_all)
View(final_res)

unique(final_res$depth)
unique(final_res$lakeID)
length(unique(final_res$lakeID))
unique(final_res$speciesID)
length(unique(final_res$speciesID))

# --------------------------------------------------------------
# env data
env_all <- vector("list", Nlak)
for(l in iLAK){
  im <- iCOMBO[idxlak_start[l]:idxlak[l]]
  res_lak <- model[idxenv[im]]
  
  df_lak <- vector("list", NSpec)
  for(s in iSpec){
    print(s)
    res <- res_lak[[s]] # 15 
   
    df <- as.data.frame(matrix(nrow = 365, ncol = 6))
    env_par <- c("tempEpi", "tempHypo", "metaDepth", "irradiance", "waterlevel", "lightAttenuation")
    
    for(par in 1:6){
      df[, par] <- as.vector(res[[par]])
    }
    colnames(df) <- env_par
    df$speciesID <- species_id[s]
    df$lakeID <- lake_id[l]
    df$day <- 1:365
    df$scenario <- scenario_name

    df_lak[[s]] <- df
  }
  
  lak_bind <- do.call(rbind, df_lak)
  env_all[[l]] <- lak_bind
}
View(env_all)

# final binding
final_env <- do.call(rbind, env_all)
View(final_env)

unique(final_env$lakeID)
length(unique(final_env$lakeID))
unique(final_env$speciesID)
length(unique(final_env$speciesID))

# plot
mean <- final_env %>% 
  group_by(day) %>%
  summarise(mean = mean(tempEpi, na.rm = TRUE))
plot(mean$day, mean$mean)
