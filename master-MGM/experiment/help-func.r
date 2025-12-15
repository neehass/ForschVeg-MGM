# help-functions
# Model run and Visual
# .libPaths(c("/home/ifgg1/R/x86_64-pc-linux-gnu-library/4.2", "/home/ifgg1/R/x86_64-pc-linux-gnu-library/4.5")) # packages Path NoMachine
# install.packages("gtable") # reinstall, to update ggplot
# install.packages("scales") # reinstall, to update ggplot
# install.packages("ggplot2")
# install.packages("patchwork")

library(ggplot2)
library(patchwork)
library(stringr)
library(ggrepel)
library(ggpmisc)
library(ggpubr)
library(rcartocolor)

# install.packages("patchwork")

set.seed(42)

# func_getAreaKm2 ----------------------------------
func_getAreaKm2 <- function(lake_path) {
  files <- list.files(lake_path, full.name = TRUE)

  areakm2 <- c()
  id <- c()
  for(i in 1:length(files)){
    
    lake <- read.table(files[i])
    a <- lake$V2[lake$V1 == "Areakm2"]
    if(identical(a, character(0))){
      print("Areakm2 NA")
      areakm2[i] <- NA
      
    } else {areakm2[i] <- as.numeric(a)}
    
    
    id[i] <- as.numeric(unlist(str_extract_all(lake$V2[lake$V1 == "Lake"], "\\d+")))
  }
  return(list(areakm2 = areakm2, id = id))
  
}

# func_getAreaKm2 ----------------------------------
func_getLakeDepth <- function(lake_path) {
  files <- list.files(lake_path, full.name = TRUE)
  
  lakeDepth <- c()
  id <- c()
  i <- 1
  for(i in 1:length(files)){
    
    lake <- read.table(files[i])
    a <- lake$V2[lake$V1 == "lakeDepth"]
    if(identical(a, character(0))){
      print("lakeDepth NA")
      lakeDepth[i] <- NA
      
    } else {lakeDepth[i] <- as.numeric(a)}
    

    id[i] <- as.numeric(unlist(str_extract_all(lake$V2[lake$V1 == "Lake"], "\\d+")))
  }
  return(list(lakeDepth = lakeDepth, id = id))

}

# func_getAreaGroup ----------------------------
# same like in input.jl
# by Areakm2 --> AreaGroup is derived
    # ”very.small”: <1, ”small”: <2, ”medium”: 2km2, ”large”: 5km2 and ”very.large”: 20km2

    # Returns the area group for a given lake configuration file.
func_getAreaGroup <- function(areakm2) {
  if(is.na(areakm2)){
    g <- NA
  } else if (areakm2 <= 1) {
    g <- "very.small"
  } else if (areakm2 <= 2) {
    g <- "small"
  } else if (areakm2 <= 5) {
    g <- "medium"
  } else if (areakm2 <= 20) {
    g <- "large"
  } else {
    g <- "very.large"
  }
  return(g)
}


# Function to detect Julia (CHatGPT) ---------------------------------------
find_julia <- function() {
  # Common locations
  candidates <- c(
    Sys.which("julia"),             # julia in PATH
    "/usr/local/bin/julia",         # typical symlink
    list.files("/opt", pattern="^julia-", full.names=TRUE) # /opt/julia-*
  )
  
  # Filter only existing files/folders
  candidates <- Filter(file.exists, candidates)
  
  # Resolve symlinks if needed
  for (path in candidates) {
    if (file.info(path)$isdir) {
      julia_bin <- file.path(path, "bin")
      if (file.exists(file.path(julia_bin, "julia"))) {
        return(normalizePath(julia_bin))
      }
    } else if (basename(path) == "julia") {
      return(normalizePath(dirname(path)))
    }
  }
  
  stop("Julia not found on the system. Please install Julia.")
}



# --- select species ----------------------------------------
# 14xxx oligotroph
# 15xxx mesotroph
# 16xxx eutroph

# random selection of certain number (n) per species group

func_sel_spec <- function(n, species_path){
    files <- list.files(species_path)
    cleaned <- sub("\\.config\\.txt$", "", files)

    # select groups 
    oli <- cleaned[grepl("species_14\\d*", cleaned)]
    meso <- cleaned[grepl("species_15\\d*", cleaned)]
    eut <- cleaned[grepl("species_16\\d*", cleaned)]

    # select random 
    oli_n  <- sample(oli,  min(n, length(oli)))
    meso_n <- sample(meso, min(n, length(meso)))
    eut_n <- sample(eut,  min(n, length(eut)))

    selected_files <- c(oli_n, meso_n, eut_n)
    
    # length(selected_files)
    return(selected_files)
}
# --------------------------------------------

# ---- func_getSpecies_config ------------------------------
func_getSpecies_config <- function(path_configFile){
  file <- readLines(path_configFile)
  spec <- strsplit(file[grep("species", file)][[1]], " ")[[1]][-1]
  cleaned <- sub(".*(?=species_)", "", spec, perl = TRUE)
  species <- sub("\\.config\\.txt$", "", cleaned)
  # species_id <- as.numeric(sub(".*species_", "", species))

  return(species)
}

# -------------------------------------------------------------------
# model Data processing 
# --------------------------------------------------------------
# saving modeloutput for CHARISMA_biomass_N_weight_hight_env_parallel_name()
func_PREPmodeloutput <- function(model, Nlak, NSpec, depths, species_id, lake_id, scenario_name){
  
  iCOMBO <- 1:(NSpec*Nlak) # data combo (species pro lake)
  length(iCOMBO) == length(model2)
  
  df_allRES <- vector("list", length(iCOMBO))
  df_allENV <- vector("list", length(iCOMBO))
  for(i in iCOMBO){
    nameLAK <- model2[[i]]$lake
    nameSPEC <- model2[[i]]$species
    Lid <- unlist(str_extract_all(nameLAK, "\\d+"))
    Sid <- unlist(str_extract_all(nameSPEC, "\\d+"))
    print(paste(Lid, Sid))
    
    res <- model2[[i]]$results
    env <- model2[[i]]$environment
    
    # results of macrohyts
    df_depth <- vector("list", 4)
    for(d in 1:4){
      depth <- res[[d]]$depth
      print(depth)
      
      data <- as.data.table(res[[d]]$data)
      colnames(data) <- c("biomass", "numberInd", "indWeight", "height")
      data$depth <- depth
      data$speciesID <- Sid
      data$lakeID <- Lid
      data$day <- 1:365
      data$scenario <- scenario_name
      df_depth[[d]] <- data
      
    }
    depth_bind <-  rbindlist(df_depth)
    # View(depth_bind)
    
    # ENV 
    env_bind <- do.call(cbind, env)
    
    env_bind <- as.data.table(env_bind)
    colnames(env_bind) <- c("tempEpi", "tempHypo", "metaDepth", "irradiance", "waterlevel", "lightAttenuation")
    env_bind$speciesID <- Sid
    env_bind$lakeID <- Lid
    env_bind$day <- 1:365
    env_bind$scenario <- scenario_name
    
    # save in list
    df_allRES[[i]] <- depth_bind
    df_allENV[[i]] <- env_bind
  }
  
  return(list(df_allRES = df_allRES, df_allENV = df_allENV))
}

# "help function" --> workaround that model2 is not loaded to every core
worker_process_chunk <- function(chunk, scenario_name) {
  
  lapply(chunk, function(mod){ # wie foor loop chunk[[i]]
    
    nameLAK  <- mod$lake
    nameSPEC <- mod$species
    Lid <- unlist(stringr::str_extract_all(nameLAK, "\\d+"))
    Sid <- unlist(stringr::str_extract_all(nameSPEC, "\\d+"))
    
    res <- mod$results
    env <- mod$environment
    
    # -------------------------------
    # Depth results
    # -------------------------------
    df_depth <- vector("list", length(res))
    
    for(d in seq_along(res)){
      depth <- res[[d]]$depth
      
      data <- data.table::as.data.table(res[[d]]$data)
      colnames(data) <- c("biomass", "numberInd", "indWeight", "height")
      
      data[, depth := depth]
      data[, speciesID := Sid]
      data[, lakeID := Lid]
      data[, day := 1:365]
      data[, scenario := scenario_name]
      
      df_depth[[d]] <- data
    }
    
    depth_bind <- do.call(rbind, df_depth)
    
    # -------------------------------
    # ENV data
    # -------------------------------
    env_bind <- data.table::as.data.table(do.call(cbind, env))
    colnames(env_bind) <-
      c("tempEpi", "tempHypo", "metaDepth",
        "irradiance", "waterlevel", "lightAttenuation")
    
    env_bind[, speciesID := Sid]
    env_bind[, lakeID := Lid]
    env_bind[, day := 1:365]
    env_bind[, scenario := scenario_name]
    
    list(df_res = depth_bind, df_env = env_bind)
  })
}

func_PREPmodeloutput_final_para <- function(model2, Nlak, NSpec, depths,
                                            species_id, lake_id, scenario_name){
  
  iCOMBO <- 1:(NSpec * Nlak)
  model2_sub <- model2[iCOMBO]
  
  chunk_size <- 200
  chunks <- split( # split in smaller list with length 122
    model2_sub,
    ceiling(seq_along(model2_sub) / chunk_size)
  )
  
  results_parallel <- future_lapply(
    chunks,
    worker_process_chunk, # defined before
    scenario_name = scenario_name,
    future.globals = FALSE
  )
  
  results_parallel <- do.call(c, results_parallel) # unlist/ flaten chunks
  
  df_allRES <- lapply(results_parallel, `[[`, "df_res")
  df_allENV <- lapply(results_parallel, `[[`, "df_env")
  
  list(df_allRES = df_allRES, df_allENV = df_allENV)
}

# Problem here: model2 is loaded to every core --> too large --Y> crash
# func_PREPmodeloutput_final_para <- function(model2, Nlak, NSpec, depths, species_id, lake_id, scenario_name){
#   
#   iCOMBO <- 1:(NSpec*Nlak)
#   results_parallel <- future_lapply(iCOMBO, function(i){
#     
#     mod <- model2[[i]]
#     nameLAK <- mod$lake
#     nameSPEC <- mod$species
#     Lid <- unlist(stringr::str_extract_all(nameLAK, "\\d+"))
#     Sid <- unlist(stringr::str_extract_all(nameSPEC, "\\d+"))
#     
#     res <- mod$results
#     env <- mod$environment
#     
#     # -------------------------------
#     #   Depth results
#     # -------------------------------
#     df_depth <- vector("list", length(res))
#     
#     for(d in seq_along(res)){
#       depth <- res[[d]]$depth
#       
#       data <- as.data.table(res[[d]]$data)
#       colnames(data) <- c("biomass", "numberInd", "indWeight", "height")
#       
#       data[, depth := depth]
#       data[, speciesID := Sid]
#       data[, lakeID := Lid]
#       data[, day := 1:365]
#       data[, scenario := scenario_name]
#       
#       df_depth[[d]] <- data
#     }
#     
#     depth_bind <- rbindlist(df_depth)
#     
#     # -------------------------------
#     #   ENV data
#     # -------------------------------
#     env_bind <- as.data.table(do.call(cbind, env))
#     colnames(env_bind) <- c("tempEpi", "tempHypo", "metaDepth", "irradiance", "waterlevel", "lightAttenuation")
#     
#     env_bind[, speciesID := Sid]
#     env_bind[, lakeID := Lid]
#     env_bind[, day := 1:365]
#     env_bind[, scenario := scenario_name]
#     
#     # return combined output
#     list(df_res = depth_bind, df_env = env_bind)
#   })
#   
#   df_allRES <- lapply(results_parallel, `[[`, "df_res")
#   df_allENV <- lapply(results_parallel, `[[`, "df_env")
#   
#   return(list(df_allRES = df_allRES, df_allENV = df_allENV))
#   
# }


# --------------------------------------------------------------
# not parallel modeloutput
# macrophyte data
func_prepMACRO <- function(model, Nlak, NSpec, depths, species_id, lake_id, scenario_name){
  
  iCOMBO <- 1:(NSpec*Nlak) # data combo (species pro lake)
  
  # macrophyt data = 1,3,5...
  idxmacro <- seq(1, NSpec*2*Nlak, 2)
  
  # env data = 2,4,6
  idxenv <- seq(2, NSpec*2*Nlak, 2)
 
  # lake = NSpec 1-27 = lake1, 28-54 = lake1
  idxlak <- seq(NSpec, NSpec*2*Nlak, NSpec)
  idxlak_start <- seq(1, NSpec*2*Nlak, NSpec)
 
  df_all <- vector("list", Nlak)
  for(l in 1:Nlak){
    im <- iCOMBO[idxlak_start[l]:idxlak[l]]
    res_lak <- model[idxmacro[im]]
    
    df_lak <- vector("list", NSpec)
    for(s in 1:Nspec){
      
      print(s)
      res <- res_lak[[s]] # 15 
      df_depth <- vector("list", 4)
      
      for(d in iDepth){
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

}

# --------------------------------------------------------------
# env data
func_prepENV <- function(model, Nlak, NSpec, depths, species_id, lake_id, scenario_name){
  
  iCOMBO <- 1:(NSpec*Nlak) # data combo (species pro lake)
  
  # macrophyt data = 1,3,5...
  idxmacro <- seq(1, NSpec*2*Nlak, 2)
  
  # env data = 2,4,6
  idxenv <- seq(2, NSpec*2*Nlak, 2)
  
  # lake = NSpec 1-27 = lake1, 28-54 = lake1
  idxlak <- seq(NSpec, NSpec*2*Nlak, NSpec)
  idxlak_start <- seq(1, NSpec*2*Nlak, NSpec)
  
  env_all <- vector("list", Nlak)
  for(l in 1:Nlak){
    im <- iCOMBO[idxlak_start[l]:idxlak[l]]
    res_lak <- model[idxenv[im]]
    
    df_lak <- vector("list", NSpec)
    for(s in 1:NSpec){
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
 
  # final binding
  final_env <- do.call(rbind, env_all)
  return(final_env)
}

# -------------------------------------------------------------------
# ---- Temp profiles ------------------------------
# same as in functions.jl
T_profile <- function(z, T_epi, T_hypo, z0, k) {
  t <- T_hypo + (T_epi - T_hypo) / (1+exp((abs(z) - abs(z0)) / k))
  if (length(z) > 1) {
    t[1] <- T_epi
     t[length(t)] <- T_hypo
  }
  return(t)
}

T_profile_1 <- function(z, T_epi, T_hypo, z0, k) {
  t <- T_hypo + (T_epi - T_hypo) / (1+exp((abs(z) - abs(z0)) / k))
  t[length(t)] <- T_hypo
  return(t)
}

# --- env plot function -----------------------------
func_sortENV_plot <- function(sort_env, save_figures, scenario){
    p_temp <- ggplot(sort_env, aes(x = day)) +
        geom_line(aes(y = tempEpi_mean, color = AreaGroup, linetype = "TempEpi")) +
                        # color = "TempEpi", linetype = lakeClass)) +
        geom_line(aes(y = tempHypo_mean, color = AreaGroup, linetype = "TempHypo")) +
                        #color = "TempHypo", linetype = lakeClass)) +
        
        theme_bw() +
        scale_color_brewer(palette = "Set2", name = "AreaGroup") +
        scale_linetype_manual(values = c("TempEpi" = "solid", "TempHypo" = "dashed"), name = "a)") +
        # scale_color_manual(values = c("TempEpi" = "red", "TempHypo" = "blue"), name = "a)") +
        labs(title = "a) Temperatur Data",
            y = "Temp [°C]", x = "Days",  linetype = "AreaGroup",color = "Linecolor") + 
        facet_grid( ~  lakeClass) 
    # p_temp

    p_metadepth <- ggplot(sort_env, aes(x = day)) +
            geom_line(aes(y = metaDepth_mean, color = AreaGroup, linetype = "meatDpeth")) + # color = "metaDepth", linetype = lakeClass)) +
            geom_line(aes(y = waterlevel_mean, color = AreaGroup, linetype = "waterlevel")) + # color ="waterlevel",  linetype = lakeClass)) +
            # scale_color_manual(values = c("metaDepth" = "magenta", "waterlevel" = "darkgreen"),
            #     labels = c("Mesolimnion Depth", "Waterlevel"), name = "b)") +
            scale_linetype_manual(values = c("meatDpeth" = "solid", "waterlevel" = "dashed"), name = "b)") +
            scale_color_brewer(palette = "Set2", name = "AreaGroup") +
            facet_grid( ~  lakeClass) +
            # annotate("rect",
            #         xmin = min(days), xmax = max(days),
            #         ymin = -Inf, ymax = Inf,
            #         alpha = 0.2, fill = "grey") +
            labs(title = "b) Waterlevel & Metalimnion Depth",
            y = "Depth [m]", x = "Days",  linetype = "AreaGroup",color = "Linecolor") +
            theme_bw()
    # p_metadepth

    p_light <- ggplot(sort_env, aes(x = day)) +
        geom_line(aes(y = irradiance_mean, color = "irradiance")) +
        facet_grid( ~  lakeClass) +
        scale_color_manual(values = c("irradiance" = "orange"),
            labels = c("Irradiance"), name = "c)") +
        # annotate("rect",
        #         xmin = min(days), xmax = max(days),
        #         ymin = -Inf, ymax = Inf,
        #         alpha = 0.2, fill = "grey") +
        labs(title= "c) Irradiance related", x = "Day",y = "[W/m2]") +
        theme_bw()
    # p_light


    p_env <- p_temp + p_metadepth + p_light + plot_layout(ncol = 1, guides = "collect") &  # collect all legends
        theme(legend.position = "right")               # move legend to left

    p_env <- p_env + 
        plot_annotation(title = paste("Environmental Data","\nScenario: ", scenario))
    
    # save
    ggsave(file.path(save_figures, "env_day.png"), p_env, height = 10, width = 10)
    
    return(p_env)
}


# test plot functions ------------------------------------------------
# ---- Plot results -------------------------------
func_plot_macroDay <- function(macrodata, speciesID_nam, name, days, scenario) {
    
    # Plot
    p_macro <- ggplot(data = macrodata, aes(x = day, y = biomass, 
            group = interaction(speciesID, depth), 
            color = depth)) +
        geom_line() +
        theme_bw() +
        labs(
        title = paste("Biomass over time per species:",name,"\nScenario: ", scenario), x = "Day",
        y = "Biomass", color = "Depth"
        ) + facet_wrap(~ speciesID, labeller = labeller(speciesID = speciesID_nam))
    # p_macro

    # Save
    ggsave(paste0(name, "_macrophytesDay.png"), plot = p_macro, width = 10, height = 8)


}

func_plot_macroDepth <- function(macrodata, speciesID_nam, name, days, scenario) {
     macrodata$depth <- as.numeric(as.character(macrodata$depth))
    p_macro <- ggplot(data = macrodata, aes(x = depth, y = biomass, 
        color = as.factor(day), group = as.factor(day))) +
      geom_line() +
      theme_bw() +
        theme(legend.position = "none") +
      labs(title = paste("Biomass per Species per Depth:",name,
            "\n(grouped by day)",
            "\nScenario: ", scenario), 
      x = "Depth [m]", y = "Biomass") + 
      facet_wrap(~ speciesID, labeller = labeller(speciesID = speciesID_nam))+
      scale_x_reverse()
  # p_macro

    # Save
    ggsave(paste0(name, "_macrophytesDepth.png"), plot = p_macro, width = 10, height = 8)

}

# plot ENV -----------------------------------
func_plot_env <- function(envdata, name, days, scenario){
    # --- Env 
    p_temp <- ggplot(envdata, aes(x = day)) +
        geom_line(aes(y = tempEpi, colour = factor("tempEpi"))) +
        geom_line(aes(y = tempHypo, colour = factor("tempHypo"))) +
        scale_color_manual(values = c("tempEpi" = "red", "tempHypo" = "blue"),
            labels = c("Epilimnion Temp.", "Hypolimnion Temp."),
            name = "Parameter") +
        annotate("rect",
                xmin = min(days), xmax = max(days),
                ymin = -Inf, ymax = Inf,
                alpha = 0.2, fill = "grey") +
        labs(title = "Temperature",x = "Day",y = "Temp [°C]") +
        theme_bw()
    # p_temp

    p_mesodepth <- ggplot(envdata, aes(x = day)) +
        geom_line(aes(y = mesoDepth, colour = factor("mesoDepth"))) +
        geom_line(aes(y = waterlevel, colour = factor("waterlevel"))) +
        scale_color_manual(values = c("mesoDepth" = "magenta", "waterlevel" = "darkgreen"),
            labels = c("Mesolimnion Depth", "Waterlevel"), name = "") +
        annotate("rect",
                xmin = min(days), xmax = max(days),
                ymin = -Inf, ymax = Inf,
                alpha = 0.2, fill = "grey") +
        labs(title= "Waterlevel & Mesolimnion Depth", x = "Day",y = "Depth [m]") +
        theme_bw()
    # p_mesodepth

    p_light <- ggplot(envdata, aes(x = day)) +
        geom_line(aes(y = irradiance, colour = factor("irradiance"))) +
        annotate("text", x = 50, y = max(envdata$irradiance)-50, 
            label = paste("Light Attenuation \nCoefficient [1/m]:", unique(envdata$lightAttenuation)), 
        color = "red", size = 3) +
        scale_color_manual(values = c("irradiance" = "orange"),
            labels = c("Irradiance"), name = "") +
        annotate("rect",
                xmin = min(days), xmax = max(days),
                ymin = -Inf, ymax = Inf,
                alpha = 0.2, fill = "grey") +
        labs(title= "Light related", x = "Day",y = "[W/m2]") +
        theme_bw()
    # p_light

    #geom_line(aes(y = irradiance, colour = factor("irradiance"))) +

    p_env <- p_temp + p_mesodepth + p_light + plot_layout(ncol = 1, guides = "collect") &  # collect all legends
        theme(legend.position = "right")               # move legend to left

    p <- p_env + 
        plot_annotation(title = paste("Environmental Data: ",name,"\nScenario: ", scenario))
    # p
    ggsave(paste0(name, "_env.png"), plot = p, width = 8, height = 10)
}

# plot temp Profile ------------------------
func_plot_Tprofile <- function(envdata, depth, name, days, scenario, k){

  temp <- envdata[envdata$day %in% days,]

  max <- temp[temp$tempEpi == max(temp$tempEpi), ]
  dmax <- unique(max$day)

  min <- temp[temp$tempEpi == min(temp$tempEpi), ]
  dmin <- unique(min$day)

  meanTEpi <- mean(temp$tempEpi)
  meanTHypo <- mean(temp$tempHypo)
  meanZ0 <- mean(temp$mesoDepth) 

  prof_max <- T_profile(0:depth, unique(temp$tempEpi[temp$day == dmax[1]]), 
                      unique(temp$tempHypo[temp$day == dmax[1]]), 
                      z0= unique(temp$mesoDepth[temp$day == dmax[1]]), 
                      k)
  prof_min <- T_profile(0:depth, unique(temp$tempEpi[temp$day == dmin[1]]), 
                      unique(temp$tempHypo[temp$day == dmin[1]]), 
                      z0= unique(temp$mesoDepth[temp$day == dmin[1]]), 
                      k)
  prof_mid <- T_profile(0:depth, meanTEpi, meanTHypo, 
                      z0= meanZ0, 
                      k)
  df_profile <- data.frame(depth = 0:depth, prof_max = prof_max, 
                    prof_mid = prof_mid, prof_min = prof_min)

  p_profile <- ggplot(df_profile, aes(y = depth))+
            geom_line(aes(x = prof_max, color = "max")) + 
            geom_line(aes(x = prof_min, color = "min")) + 
            geom_line(aes(x = prof_mid, color = "mid")) + 
            scale_color_manual(values = c("max" = "red", "mid" =  "orange", "min" = "blue"),
              labels = c("Max Temp.", "mean Temp.", "Min. Temp"),
              name = "Parameter") + theme_bw() +
            annotate("text", x = max(prof_max)-2, y = depth, 
                label = paste("steepness parameter \nk = ", k), 
                color = "black", size = 3) +
            labs(
              title = paste("Temperature Profile: ",name,
                "\nat the max, min and mean Temp. of timespan \nDays: ", dmax[1] , dmin[1], 
                "\nScenario: ", scenario), 
                x = "Temp [°C]", y = "Depth [m]") 

#   p_profile
  ggsave(paste0(name, "_Tprofile.png"), plot = p_profile, width = 8, height = 10)
}

# --------------------------------------------------------------------------------------
# plot 
func_plot_DDG <- function(lewSpec_dir, lakesDDG, scenario){
  load(file.path(lewSpec_dir, "data/data_lakes_env_class.rda"))
  
  WinnerLoserPalette<- c(carto_pal(2,"PinkYl")[c(2)],carto_pal(2,"TealGrn")[c(1)])
  TrophiePalette <- c("cornflowerblue","aquamarine4","coral4")
  
  # plot Gradient -----------------------------------------------
  my.formula <- y ~ x 
  
  lakeclasses <- c("clear lakes","intermediate lakes", "turbid lakes")
  names(lakeclasses)<-c("clear","medium","turb")
  
  A1<-lakesDDG %>%
    filter(type == paste0("model_", scenario))%>%
    left_join((data_lakes_env_class %>% mutate(Lake=paste0("lake_",Lake)) %>%
                 select(-LakeName)),
              by=c("Lake"))%>%
    ggplot(aes(depth, NSpecP, col=Group, group=interaction(Group,Lake)))+
    #geom_path(alpha=0.5)+
    geom_boxplot(aes(group=interaction(depth,Group), fill=Group))+
    facet_grid(~class, 
               labeller = labeller(class = lakeclasses))+
    scale_colour_manual(values = c(rev(TrophiePalette)))+
    theme(legend.title = element_blank()) +
    xlab("Depth (m)")+ 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
    theme(legend.position = "none")+
    #ggtitle("Potential species richness (model)")+
    ylab("")+
    scale_fill_manual(values = c(rev(TrophiePalette)))+
    ylab("Potential \nspec. richness (%)")
  
  A2<-lakesDDG %>%
    filter(type=="mapped")%>%
    left_join((data_lakes_env_class %>% mutate(Lake=paste0("lake_",Lake)) %>%
                 select(-LakeName)),
              by=c("Lake"))%>%
    ggplot(aes(depth, NSpecP, col=Group, group=interaction(Group,Lake)))+
    #geom_path(alpha=0.5)+
    geom_boxplot(aes(group=interaction(depth,Group), fill=Group))+
    facet_grid(~class, 
               labeller = labeller(class = lakeclasses))+
    scale_colour_manual(values = c(rev(TrophiePalette)))+
    theme(legend.title = element_blank()) +
    xlab("")+ 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
    theme(legend.position = "none")+
    #ggtitle("Realised species richness (mapped)")+
    scale_fill_manual(values = c(rev(TrophiePalette)))+
    ylab("Observed \nspec. richness (%)")
  
  A3<-lakesDDG %>%
    #filter(type=="mapped")%>%
    left_join((data_lakes_env_class %>% mutate(Lake=paste0("lake_",Lake)) %>%
                 select(-LakeName)),
              by=c("Lake")) %>%
    spread(type,NSpecP) %>%
    ggplot(aes(x= !!sym(paste0("model_", scenario)) ,y=mapped, col=Group))+
    geom_point()+
    facet_grid(~class, 
               labeller = labeller(class = lakeclasses))+ #aes(col=Name)
    #geom_smooth(model=lm, method=lm, se=F, formula = my.formula,
    #            #aes(group=factor(depth), col=factor(depth))
    #            )+
    stat_correlation(vstep = 0.1,label.x = "centre")+
    #stat_poly_eq(formula = my.formula,
    #              eq.with.lhs = "italic(hat(y))~`=`~",
    #              aes(label = paste(..rr.label.., sep = "~~~")))+
    scale_colour_manual(values = c(rev(TrophiePalette)))+
    #ggtitle("comparison")+ 
    geom_abline(intercept = 0, slope = 1)+
    #annotate("text", x = 22, y = 25, label = "model = mapped", angle=34)+
    xlab("Potential spec. richness (%)")+
    ylab("Observed \nspec. richness (%)")+
    ylim(0,40)+xlim(0,40)
  
  p_DDG <-((A2/A1) / A3) +theme(legend.position = "bottom")+ 
    labs(col="Spec. group") + 
    plot_annotation(tag_levels = 'a',
                    tag_prefix = '(',
                    tag_suffix = ')')& 
    theme(plot.tag = element_text(size = 12))& 
    guides(colour = guide_legend(override.aes = list(size=3)))
  
  return(p_DDG)
}

# Function to assign significance codes like in ANOVA ----------------------------
get_signif <- function(p) {
  if (is.na(p)) return("")
  else if (p < 0.001) return("***")
  else if (p < 0.01) return("**")
  else if (p < 0.05) return("*")
  else if (p < 0.1) return(".")
  else return(" ")
}

getformat_p <- function(p) {
  if (is.na(p)) {
    return(NA)
  } else if (p < 2.2e-16) {
    return("<2.2e-16")
  } else if (p < 0.0001) {
    return("<0.0001")
  } else if (p < 0.001) {
    return(format(p, scientific = TRUE, digits = 2))
  } else {
    return(round(p, 3))
  }
}

