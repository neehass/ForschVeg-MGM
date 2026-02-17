# --------------------------------
# help functions
# --------------------------------

library(dplyr)
library(tidyr)
library(stringr)

# preprocess ------------------------------------

prepro <- function(file){
  
  lake <- read.csv(file, sep = ";", skip = 8)
  lake[lake == ""] <- NA
  
  # drop where Prüfstatus != "Geprueft"
  lake <- lake[lake$Prüfstatus == "Geprueft",]
  
  # drop Prüfstatus
  lake <- lake[, !(colnames(lake) %in% "Prüfstatus")]
  
  # replace , to . & as.numeric
  lake[2:ncol(lake)] <- lapply(lake[2:ncol(lake)], function(x){
    x <- gsub(",", ".", as.character(x)) ; as.numeric(x)}) 
  
  # date and time seperated
  lake <- lake %>%
    separate(Datum, into = c("Datum", "time"), sep = " ")
  
  # datum as date
  lake$Datum <- as.Date(lake$Datum)
  
  # rename cols
  colnames(lake) <- sub("Wassertemp\\.\\.vor\\.Ort\\.\\.\\.\\.C\\.\\.\\.", "", colnames(lake))
  
  lake <- lake[, !(colnames(lake) %in% ".1.0.m.ü..Grund.")]
  
  return(lake)
}

# preprocess long formate ---------------------------

func_prepro_long_formate <- function(file, lake_names){
  
  lake <- read.csv(file, sep = ";", skip = 8)
  lake[lake == ""] <- NA

  # drop where Prüfstatus != "Geprueft"
  lake <- lake[lake$Prüfstatus == "Geprueft",]
  # drop Prüfstatus
  if ("Prüfstatus" %in% names(lake)) {
    lake <- lake[, names(lake) != "Prüfstatus"]}

  # replace , to . & as.numeric
  lake[2:ncol(lake)] <- lapply(lake[2:ncol(lake)], function(x){
    x <- gsub(",", ".", as.character(x)) ; as.numeric(x)}) 

  # lake NR
  id <- sub(".*temp/([0-9]+)_.*", "\\1", file)
  lake$ID <- id

  # get name 
  name <- lake_names$Name[which(as.character(lake_names$ID) %in% id)]
  lake$Name <- name
  
  # date and time seperated
  lake <- lake %>%
    separate(Datum, into = c("Datum", "time"), sep = " ")
  
  # datum as date
  lake$Datum <- as.Date(lake$Datum)
  lake$Year <- as.numeric(format(lake$Datum, "%Y"))
  lake$Month <- as.numeric(format(lake$Datum, "%m"))
  lake$Day <- as.numeric(format(lake$Datum, "%d"))
  
  # drop time and date 
  lake <- lake[, !(colnames(lake) %in% c("Datum", "time"))]

  # Depths
  # drop "über Grund" 
  grund <- grep("grund", colnames(lake), ignore.case = TRUE, value = TRUE)
  lake <- lake[, !(colnames(lake) %in% grund)]

  # rename cols
  depths <- regmatches(colnames(lake), regexpr("[0-9]+\\.[0-9]+", colnames(lake)))
  depths <- paste0(depths, "_m_beSurf") # below Surface

  tiefe <- grep("tiefe", colnames(lake), ignore.case = TRUE, value = TRUE)
  idx <- which(colnames(lake) %in% tiefe)
  colnames(lake)[idx] <- depths

  
  # long formate
  dep_level <- grep("_m_beSurf", colnames(lake), value = TRUE)
  
  # long formate
  df_long <- lake %>%
    pivot_longer(
    cols = dep_level,
    names_to = "Depth_name",
    values_to = "Temp"
    ) %>%
    mutate(
    Temp = as.numeric(Temp),  # <--- CLEAN THIS!
    Depth = -as.numeric(str_extract(Depth_name, "\\d+\\.?\\d*")) # depth to numeric, stringr
    )
  # clean, remove NA
  df_long_clean <- na.omit(df_long)
 
  return(list(lake = lake, lake_df_long = df_long_clean))
}

# aggregate seasons ---------------------------
# Meterologisch 
# Frühling: 1. März–31. Mai (03-05)
# Sommer: 1. Juni–31. August (06-08)
# Herbst: 1. September–30. November (09-11)
# Winter: 1. Dezember–28./29. Februar (12-02)

agg_seasons <- function(dates, lake){
  lake$Season <- NA # create new colume 
  lake$Year <- NA # create new colume 
  
  months <- as.numeric(format(dates, "%m")) # get month 
  years <- as.numeric(format(dates, "%Y")) # get years
  
  # new colume with seasons
  lake$Season[which(months %in% c(3:5))] <- "spring"
  lake$Season[which(months %in% c(6:8))] <- "summer"
  lake$Season[which(months %in% c(9:11))] <- "fall"
  lake$Season[which(months %in% c(12, 1, 2))] <- "winter"
  
  # new colume year
  lake$Year <- years
  
  # aggregate by seasons and years --> mean temp per year
  lake_agg <- lake %>%
    group_by(Season, Year) %>%
    summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)), .groups = "drop")
  
  # clean aggregate without NA
  lake_agg_clean <- na.omit(lake_agg)
  
  if(nrow(lake_agg_clean)== 0){
    lake_agg_clean <- NULL
  }
  
  return(list(lake_agg = lake_agg, lake = lake, lake_agg_clean = lake_agg_clean))
}

