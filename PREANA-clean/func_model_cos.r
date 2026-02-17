# Model function - cos
# !! typo meso anstatt meta !!
# | Parameter       | Unit                                       | Description                                                                            |
# |:----------------|:-------------------------------------------|:---------------------------------------------------------------------------------------|
# | maxTemp         | °*C*                                       | Maximal mean daily temperature of a year                                               |
# | minTemp         | °*C*                                       | Minimal mean daily temperature of a year                                               |
# | tempDelay       | *d*                                        | Days after 1st of January where Temp is minimal                                        |
# | tempDev         | \-                                         | Share of temp                                                                          |

# ------------------------------------------------------------------------------------------
# as in final MGM model, like: input.jl & function.jl ----------------------------------
# ---- MGM Areagroup -------------------------
getAreaGroup <- function(area) {
  if (area <= 1.0) {
    return("very.small")
  } else if (area <= 2.0) {
    return("small")
  } else if (area <= 5.0) {
    return("medium")
  } else if (area <= 20.0) {
    return("large")
  } else {
    return("very.large")
  }
}


# ---- MGM function T-profile per depth -----------------------------------
getTemperatureProfile_depth <- function(depth, tempEpi, tempHypo, metaDepth, k) {
  t <- tempHypo + (tempEpi - tempHypo) /
    (1 + exp((abs(depth) - abs(metaDepth)) / k))
  
  if (length(depth) > 1) {
    t[length(t)] <- tempHypo
  }
  
  return(t)
}

# ---- MGM function Tepi_daily -----------------------------------
# im MGM Jahresverlauf für Tepi: 
getTemperature_Epi <- function(day, settings, dynamicData) {
  
  # Check for missing epilimnion temperature
  if (is.na(dynamicData[day,]$tempEpi)) {
    
    dynamicData[day,]$tempEpi <-
      settings$tempDev * (
        settings$maxTemp -
          ((settings$maxTemp - settings$minTemp) / 2) *
          (1 + cos((2 * pi / settings$yearlength) *
                   (day - settings$tempDelay)))
      )
  }
  
  return(dynamicData[day,]$tempEpi)
}


# ---_ MGM function Thypo_daily -----------------------------------
getTemperature_Hypo_area <- function(day, settings, dynamicData, HypoFrac_dir) {
  
  HypoFrac <- read.csv(HypoFrac_dir)
  
  # Check for missing hypolimnion temperature
  if (is.na(dynamicData[day,]$tempHypo)) {
    
    Fmin <- HypoFrac$min[HypoFrac$AreaGroup == settings$AreaGroup][1]
    Fmax <- HypoFrac$max[HypoFrac$AreaGroup == settings$AreaGroup][1]
    
    # Mean max/min hypolimnion temperature fractions
    maxTemp_hypo <- settings$maxTemp * Fmin
    minTemp_hypo <- settings$minTemp * Fmax
    
    # Seasonal cosine model
    dynamicData[day,]$tempHypo <-
      settings$tempDev * (
        maxTemp_hypo -
          ((maxTemp_hypo - minTemp_hypo) / 2) *
          (1 + cos((2 * pi / settings$yearlength) *
                   (day - settings$tempDelay)))
      )
  }
  
  return(dynamicData[day,]$tempHypo)
}


# ---- MGM Metalimnion Depth per Area Group ------------------------------
getMetalimnion_Depth_area <- function(day, tempEpi, tempHypo, settings, dynamicData, MetaFrac_dir) {
  
  metaFrac <- read.csv(MetaFrac_dir)
  
  # Check for missing metaDepth
  if (is.na(dynamicData[day,]$metaDepth)) {
    
    # Get delay day: 2nd smallest temperature difference
    dif <- abs(tempEpi - tempHypo)
    delay_day <- order(dif)[2]
    
    Tepi_max  <- max(tempEpi, na.rm = TRUE)
    Thypo_max <- max(tempHypo, na.rm = TRUE)
    
    # Get minimum fraction for area group
    Fmin <- metaFrac$min[metaFrac$AreaGroup == settings$AreaGroup][1]
    depth <- settings$lakeDepth
    
    # Calculate maximum metalimnion depth
    z0_max <- (Fmin * depth) * (abs(Thypo_max - Tepi_max) / Tepi_max)
    
    # Seasonal cosine model
    z0 <- (z0_max / 2) *
      (1 + cos((2 * pi / settings$yearlength) *
               (day - delay_day - settings$yearlength / 2)))
    
    dynamicData[day,]$metaDepth <- z0
  }
  
  return(dynamicData[day,]$metaDepth)
}

# ------------------------------------------------------------------------------------------
# ---- get patalas z0 depth --------------------------------------
getMetaDepth_patalas <- function(day, settings, dynamicData) {
    Areakm2 <- settings$Areakm2
    z0_max <- (4.6 * Areakm2^0.41)*-1

    dif <- abs(dynamicData$tempEpi - dynamicData$tempHypo)
    delay_day <- order(dif)[2]

    z0 <- (z0_max / 2) *
      (1 + cos((2 * pi / settings$yearlength) *
               (day - delay_day - settings$yearlength / 2)))
    
    return(z0)
}

# ------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------

# ---- Jahresverlauf Thypo Mittel -------
# minmax_frac <- read.csv("data/minmax_frac_area_group.csv")
# im Mittel 
# Thypo_min = (1/3) * max_Tepi
# Thypo_max = 1.2 * min_Tepi

getTdaily_hypo <- function(day, tempDev, max_Tepi, min_Tepi, tempDelay, yearlength) {

    Thypo_min <- 1.2 * min_Tepi
    Thypo_max <- (1/3) * max_Tepi

    # Compute temperature using cosine-based seasonal model
    T_daily <- tempDev * (
        Thypo_max - ((Thypo_max - Thypo_min) / 2) *
        (1 + cos((2 * pi / yearlength) * (day - tempDelay)))
        )
    return(T_daily)
}

# ---- Jahresverlauf Thypo Mittel !Abhänigkeit von der Größe -------
# minmax_frac
# abhänig von der See Größe 

getTdaily_hypo_area <- function(day, tempDev, max_Tepi, min_Tepi, tempDelay, yearlength, Area_group, minmax_frac) {

    #filter for Area_group
    Fmin <- round(minmax_frac[Area_group, "min"], 2)
    Fmax <- round(minmax_frac[Area_group, "max"], 2)

    # calculate Thypo max and min by fraction
    Thypo_min <- Fmax * min_Tepi
    Thypo_max <- Fmin * max_Tepi

    # Compute temperature using cosine-based seasonal model
    T_daily <- tempDev * (
        Thypo_max - ((Thypo_max - Thypo_min) / 2) *
        (1 + cos((2 * pi / yearlength) * (day - tempDelay)))
        )
    
    return(T_daily)
}

# ---- Z0 - Faktorz0 (z0/depth) --------------------------------------
# z0 can be aswell discribed as modified cos funktion, but at time some times z0 = 0
# also created from fraction z0/depth of lake - defided in Area groups
# set z0 = 0 if 
# norm_frac_z0 <- read.csv("data/norm_z0_frac_depth_group_summary.csv")

getZ0_daily <- function(day, depth, yearlength, Area_group, delay_day, norm_frac_z0) {
    
    nam <- paste0("F_maxZ_", Area_group)
    #filter for Area_group
    Fmin <- norm_frac_z0[norm_frac_z0$Depth_type == nam, "Min_Value"]

    # calculate Thypo max and min by fraction
    Z0_max <- Fmin * depth
    
    # Compute temperature using cosine-based seasonal model
    z0_daily <- (Z0_max/2) *
        (1 + cos((2 * pi / yearlength) * (day - delay_day - yearlength/2)))
    
    return(z0_daily)
} 

getZ0_set_0_daily <- function(day, depth, yearlength, Area_group, delay_day1, delay_day2, norm_frac_z0) {
    
    nam <- paste0("F_maxZ_", Area_group)
    #filter for Area_group
    Fmin <- norm_frac_z0[norm_frac_z0$Depth_type == nam, "Min_Value"]
    # min_month <- as.numeric(norm_frac_z0[norm_frac_z0$Depth_type == nam, "Min_Month"])
    # min_month <- (min_month-1)*30+15

    # calculate max MesoDepth by fraction
    Z0_max <- Fmin * depth

    # Compute temperature using cosine-based seasonal model
    if(day > delay_day1 & day < delay_day2){
        z0_daily <- (Z0_max/4) * (1 + cos((2 * pi / yearlength) * (day - delay_day1 - yearlength/3))) * # min ca an schnittpunkt der Epi & hypo TEmp
            (1 + cos((2 * pi / yearlength) * (day - delay_day2 - yearlength/2))) #* 
             #(1 - cos((2 * pi / yearlength) * (day - min_month - yearlength/2)))

    } else {
        z0_daily <- 0 }
    
    
    return(z0_daily)
} 

getZ0_temp_daily <- function(day, depth, yearlength, Area_group, delay_day, norm_frac_z0, Thypo_max, Tepi_max) {
    
    nam <- paste0("F_maxZ_", Area_group)
    #filter for Area_group
    Fmin <- norm_frac_z0[norm_frac_z0$Depth_type == nam, "Min_Value"]

    # calculate max MesoDepth by fraction
    Z0_max <- (Fmin * depth) * (abs(Thypo_max-Tepi_max)/Tepi_max)
    
    # Compute temperature using cosine-based seasonal model
    z0_daily <- (Z0_max/2) * (1 + cos((2 * pi / yearlength) * (day - delay_day - yearlength/2))) # * (day - delay_day - yearlength/2)
    
    return(z0_daily)
} 


# ---- Temp profiles ------------------------------
T_profile <- function(z, T_epi, T_hypo, z0, k) {
  T_hypo + (T_epi - T_hypo) / (1+exp((abs(z) - abs(z0)) / k))
}

