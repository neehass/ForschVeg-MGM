# cos-function Jahresverlauf
library(ggplot2)
library(patchwork)
library(lubridate)
library(dplyr)
library(tidyr)
library(cowplot)

getwd()
setwd("C:/Users/maiim/Documents/25-25SS/Forschungsprojekt_Vegetationskunde/R_scripte")

source("func_model_cos.R")
source("help-func-analysis.r")

# from ODD MGM
# | Parameter       | Unit                                       | Description                                                                            |
# |:----------------|:-------------------------------------------|:---------------------------------------------------------------------------------------|
# | maxTemp         | °*C*                                       | Maximal mean daily temperature of a year                                               |
# | minTemp         | °*C*                                       | Minimal mean daily temperature of a year                                               |
# | tempDelay       | *d*                                        | Days after 1st of January where Temp is minimal                                        |
# | tempDev         | \-                                         | Share of temp                                                                          |
# | lakeDepth       | *m*                                        | Maximum lake depth                                                                     |
# | Areakm2         | *km*^2^                                    | Lake area in

# -----------------------------------------------------------------------
# load data
EpiHyp_dep10 <- read.csv("data/dep10_epi-hypo_zPatalas.csv") 
head(EpiHyp_dep10)
EpiHyp_dep10_mean <- EpiHyp_dep10 %>% group_by(Area_group) %>%
    summarise(Area_km2_mean = mean(Area_km2, na.rm=TRUE))

dep10_smooth_all <- read.csv("data/dep10_patalas_smooth_all.csv")
head(dep10_smooth_all)
dep10_zPatalas <- read.csv("data/dep10_epi-hypo_zPatalas.csv")
head(dep10_zPatalas)

# Hypo and Metafraction 
dir_input <- "C:/Users/maiim/Documents/25-25SS/Forschungsprojekt_Vegetationskunde/scripte-data-MGM/master-MGM/"

HypoFrac_dir <- file.path(dir_input, "./input/lakeFractionParameters/HypoTemp_fraction.config.txt")
MetaFrac_dir <- file.path(dir_input, "./input/lakeFractionParameters/MetaDepth_fraction.config.txt")

lewSpec_dir <- "C:/Users/maiim/Documents/25-25SS/Forschungsprojekt_Vegetationskunde/scripte-data-MGM/LewerentzEtAl2023_ModelledMacrophyteSpeciesRichness-1.0"
load(file.path(lewSpec_dir, "data/data_lakes_env_class.rda"))

# example lake form MGM
# Chiemsee 6 
# Koenigssee 14
# 1 AbtsdorferSee
# Eibsee 7
i <- 1
all_dynamicData <- list()
all_T_profiles <- list()

cols <- c( # chatgpt
  "#3b5aa9",  # 1 tiefblau
  "#4f79c7",  # 2 blau
  "#74a9cf",  # 3 hellblau
  "#a6bddb",  # 4 sehr hellblau
  "#fdd49e",  # 5 hell warm
  "#fdae61",  # 6 orange
  "#f46d43",  # 7 rot-orange
  "#e34a33",  # 8 warm rot
  "#fee8c8",  # 9 warm hell
  "#c6dbef",  # 10 sehr hellblau
  "#9ecae1",  # 11 hellblau
  "#5b8fd1"   # 12 blau
)

for(i in 1:31){
    
    file_path <- file.path(dir_input, "./input/lakes/", paste0("lake_",i,".config.txt"))
    test_dat <- read.table(file_path) 
    test_dat <- as.data.frame(test_dat)
    colnames(test_dat) <- c("para", "value")
    name <- test_dat$value[test_dat$para == "Name"]

    print(name)
    if(any(EpiHyp_dep10$Name == name)){
        
        print(paste("exists", i, name))
        
        # set parameter ------
        settings <- list()
        yearlength <- 365
        settings$yearlength <- yearlength

        settings$maxTemp <- as.numeric(test_dat$value[test_dat$para == "maxTemp"])
        settings$minTemp <-  as.numeric(test_dat$value[test_dat$para == "minTemp"])
        settings$tempDev <-  as.numeric(test_dat$value[test_dat$para == "tempDev"])
        settings$tempDelay <-  as.numeric(test_dat$value[test_dat$para == "tempDelay"])
        settings$lakeDepth <- as.numeric(test_dat$value[test_dat$para == "lakeDepth"])
        settings$Areakm2 <- as.numeric(test_dat$value[test_dat$para == "Areakm2"])

        settings$AreaGroup <- getAreaGroup(settings$Areakm2)
        AreaGroup <- settings$AreaGroup
        
        # smoothed temperatur data plot 
        dat_smooth <- dep10_smooth_all %>% filter(Name == name) 
        min_maxZ <- min(dat_smooth$maxZ, na.rm=TRUE)


        # define dynamicData 
        dynamicData <- data.frame(day = 1:yearlength, Month = rep(NA,yearlength), tempEpi = rep(NA, yearlength), tempHypo = rep(NA, yearlength), 
                                    metaDepth = rep(NA, yearlength), z0_patalas = rep(NA, yearlength), 
                                    z0_smooth_infPoint = rep(NA, yearlength), AreaGroup = rep(AreaGroup, yearlength))
       
        # get tempEpi & tempHypo 
        for(day in 1:yearlength){
            month_name <- month(ymd("2023-01-01") + days(day - 1))
            
            dynamicData$Month[day] <- month_name

            dynamicData$tempEpi[day] <- getTemperature_Epi(day, settings, dynamicData)
            dynamicData$tempHypo[day] <- getTemperature_Hypo_area(day, settings, dynamicData, HypoFrac_dir)
   
        }

        # get metaDepth & metaDepth patalas
        for(day in 1:yearlength){
            dynamicData$metaDepth[day] <- getMetalimnion_Depth_area(day, dynamicData$tempEpi, dynamicData$tempHypo, settings, dynamicData, MetaFrac_dir)
            dynamicData$z0_patalas[day] <- getMetaDepth_patalas(day, settings, dynamicData)
        }
        all_dynamicData[[name]] <- dynamicData

        # plot epi hypo temp & metalimnion depth
        p1 <- ggplot(dynamicData, aes(x = 1:yearlength)) + 
                geom_line(aes(y = tempEpi, color = "Epilimnion")) +
                geom_line(aes(y = tempHypo, color = "Hypolimnion")) +
                labs(title = paste(name, "Area group:", AreaGroup), 
                        x= "day", y = "Temp [°C]", color = "Temperature")+
                theme_bw()

        p2 <- ggplot(dynamicData, aes(x = 1:yearlength)) + 
            geom_line(aes(y = metaDepth, color = "z0_temp")) +
            geom_line(aes(y = z0_patalas, color = "z0_patalas"), linetype = "longdash") +
            scale_color_manual(values = c("z0_temp" = "blue", "z0_patalas" = "red"),
            labels = c("z0_temp" = expression(z[0]~"(temperature-based)"), 
                        "z0_patalas" = expression(z[0]~"(Patalas)"))) +
        labs(title= "Mesolimnion Depth",x= "day", y = "Depth [m]", color = "metalimnion Depth") +
                theme_bw()
        
        combined_plot <- p1 / p2 + plot_layout(heights = c(1, 0.5))
        print(combined_plot)

        ggsave(file.path("plots/cos-func-final2.0", paste0(name,"_hyp_epi_cos.png")), plot = combined_plot, width = 8, height = 10, units = "in", dpi = 300, scale = 1.1)
        print("ggplot temp saved")

        # --- temp profile plot
        dynamicData_sum <- dynamicData %>% group_by(Month) %>%
            summarise(across(where(is.numeric), mean, na.rm = TRUE)) 
        depth <- ceiling(settings$lakeDepth)
    
        temp_profiles <- dynamicData_sum %>%
            group_by(Month) %>%
            summarise(T_profile = list(T_profile(0:depth, tempEpi, tempHypo, metaDepth, k = 5)),
                    T_profile_patalas = list(T_profile(0:depth, tempEpi, tempHypo, z0_patalas, k = 5))) %>%
            unnest(cols = c(T_profile, T_profile_patalas))%>%
            mutate(
                Depth = rep(0:depth, length.out = n()),
                T_profile = as.numeric(T_profile),
                T_profile_patalas = as.numeric(T_profile_patalas),
            )
        all_T_profiles[[name]] <- temp_profiles
        # View(dynamicData_sum)
        temp_profiles$Month <- factor(temp_profiles$Month,
                              levels = 1:12,
                              labels = month.abb)   # Jan, Feb, Mär, ...
        dynamicData_sum$Month <- factor(dynamicData_sum$Month,
                                        levels = 1:12,
                                        labels = month.abb)
        dat_smooth$Month <- factor(dat_smooth$Month,
                                levels = 1:12,
                                labels = month.abb)
        # plot temp profile and temp smoothed
        p <-ggplot() + 
            geom_line(data = temp_profiles, aes(x = T_profile, y = Depth, group = Month, color = factor(Month), linetype = "z0_temp"), linewidth = 1) +
            geom_hline(data = dynamicData_sum, aes(yintercept = metaDepth, group = Month, color = factor(Month), linetype = "z0_temp"), linewidth = 1) + 
            
            geom_line(data = temp_profiles, aes(x = T_profile_patalas, y = Depth, group = Month, color = factor(Month), linetype = "z0_patalas")) +
            geom_hline(data = dynamicData_sum, aes(yintercept = z0_patalas, group = Month, color = factor(Month), linetype = "z0_patalas")) + 
          
            geom_line(data = dat_smooth, aes(x = Temp_smooth, y = Depth, group = Month, linetype = "Smoothed_Temp"), color = "gray50") +
            geom_hline(data = dat_smooth, aes(yintercept = maxZ, group = Month, linetype = "Smoothed_Temp"), color = "gray50") +

            
            facet_wrap(~Month) + 
            labs(# title = paste0("estimated Tempprofile ",name, " (Area group: ", AreaGroup, ")"), 
                        x= "Temp [°C]", y = "Depth [m]", color = "Month", linetype = "Line Type") +
            scale_linetype_manual(values = c("z0_temp" = "solid", "z0_patalas" = "longdash", 
                                            "Smoothed_Temp" = "solid"), 
                                            labels = c("z0_temp" =  expression(z[0]~"(temperature-based)") , 
                                            "z0_patalas" = expression(z[0]~"(Patalas)"), 
                                            "Smoothed_Temp" =  expression(atop("Smoothed Profile", z[0]~"(inflection point)")))) +
            theme_bw() +
            theme(legend.position = "none") + guides(color = "none") +
            scale_color_manual(values = cols) # + theme(legend.position = "bottom", legend.direction = "horizontal" , legend.box = "vertical") 
        p
        ggsave(file.path("plots/cos-func-final2.0", paste0(name,"_cos_Tprofile.png")), plot = p, width = 4.5, height = 4.5, units = "in", dpi = 300, scale = 1.1)
    print("ggplot temp profile saved")

    }
    
}
# bullshit plot, just for legend
p <-ggplot() + 
        geom_line(data = temp_profiles, aes(x = T_profile, y = Depth, group = Month, color = factor(Month), linetype = "z0_temp"), linewidth = 1) +
        geom_hline(data = dynamicData_sum, aes(yintercept = metaDepth, group = Month, color = factor(Month), linetype = "z0_temp"), linewidth = 1) + 
        
        geom_line(data = temp_profiles, aes(x = T_profile_patalas, y = Depth, group = Month, color = factor(Month), linetype = "z0_patalas")) +
        geom_hline(data = dynamicData_sum, aes(yintercept = z0_patalas, group = Month, color = factor(Month), linetype = "z0_patalas")) + 
        
        geom_line(data = dat_smooth, aes(x = Temp_smooth, y = Depth, group = Month, linetype = "Smoothed_Temp"), color = "gray50") +
        geom_hline(data = dat_smooth, aes(yintercept = maxZ, group = Month, linetype = "Smoothed_Temp"), color = "gray50") +

        
        facet_wrap(~Month) + 
        labs(# title = paste0("estimated Tempprofile ",name, " (Area group: ", AreaGroup, ")"), 
                    x= "Temp [°C]", y = "Depth [m]", color = "Month", linetype = "Line Type") +
        scale_linetype_manual(values = c("z0_temp" = "solid", "z0_patalas" = "longdash", 
                                        "Smoothed_Temp" = "solid"), 
                                        labels = c("z0_temp" =  expression(z[0]~"(temperature-based)") , 
                                        "z0_patalas" = expression(z[0]~"(Patalas)"), 
                                        "Smoothed_Temp" =  expression("Smoothed Profile"~z[0]~"(inflection point)"))) +
        theme_bw() + 
        scale_color_manual(values = cols) +
            theme(legend.position = "bottom", legend.direction = "horizontal" , legend.box = "vertical") + 
            guides(color = guide_legend(nrow = 1),linetype = guide_legend(nrow = 1)) 
p
# Nur die Legende extrahieren
legend <- get_legend(p)

# Zeige die Legende
plot_grid(legend)
ggsave(file.path("plots/cos-func-final2.0", paste0("legend_cos_Tprofile.png")), plot = legend, width = 8, height = 1, units = "in", dpi = 300, scale = 1.1)

# --------------------------------------------------------------------
# select Coords of lakes 
unique(data_lakes_env_class$Lake)
head(data_lakes_env_class)

# exlcude c(11, 12, 30, 4) lakes
exclude <- c(11, 12, 30, 4)
data_lakes_env_class$LakeName <- gsub(" ", "", data_lakes_env_class$LakeName)
selected_lakes <- data_lakes_env_class[!data_lakes_env_class$Lake %in% exclude,]
length(unique(selected_lakes$LakeName))

lookup_areagroup <- dep10_zPatalas %>%
  select(Name, Area_group) %>%   
  distinct()                        
head(lookup_areagroup) 
unique(lookup_areagroup$Name)  
lookup_areagroup$Name[lookup_areagroup$Name == "GrosserAlpseebeiImmenstadt"] <-  "Gr.Alpsee"
setdiff(selected_lakes$LakeName, lookup_areagroup$Name)

selected_lakes <- merge(selected_lakes, lookup_areagroup, by.x = "LakeName", by.y = "Name")
head(selected_lakes)
length(unique(selected_lakes$LakeName))

# load coordinaten data
coords_lakes <- read.csv("C:\\Users\\maiim\\Documents\\25-25SS\\Forschungsprojekt_Vegetationskunde\\R_scripte\\data\\lake_coords_area_qgis.csv")
head(coords_lakes)
unique(coords_lakes$Name)
coords_lakes$Name[coords_lakes$Name == "GrosserAlpseebeiImmenstadt"] <-  "Gr.Alpsee"

# select coords of selected lakes
selected_coords <- coords_lakes[coords_lakes$Name %in% selected_lakes$LakeName, ]
head(selected_coords)
nrow(selected_coords)

unique(selected_coords$Name)

setdiff(unique(selected_coords$Name), unique(selected_lakes$LakeName))

# bind coords and selected lakes
selected_lakes_coords <- merge(selected_coords, selected_lakes, by.x = "Name", by.y = "LakeName")
head(selected_lakes_coords)

# save as csv
write.csv(selected_lakes_coords, "data/selected_lakes_coords_area_qgis.csv", row.names = FALSE)

# load extracted_lakes_area_qgis.csv
library(terra)
extracted_lakes_area <- vect("data/extracted_lakes_bayern.shp")
head(extracted_lakes_area)
extracted_lakes_area$name <- gsub(" ", "", extracted_lakes_area$name)
unique(extracted_lakes_area$name)
extracted_lakes_area$name[extracted_lakes_area$name == "GroßerAlpsee"] <-  "Gr.Alpsee"
extracted_lakes_area$name[extracted_lakes_area$name == "Alpsee"] <-  "AlpseebeiSchwangau"
extracted_lakes_area$name[extracted_lakes_area$name == "Wörthsee"] <-  "Woerthsee"
extracted_lakes_area$name[extracted_lakes_area$name == "Königssee"] <-  "Koenigssee"
extracted_lakes_area$name[extracted_lakes_area$name == "Weißensee"] <-  "Weissensee"
extracted_lakes_area$name[extracted_lakes_area$name == "LangbürgnerSee"] <-  "LangbuergnerSee"
extracted_lakes_area$name[extracted_lakes_area$name == "GroßerOstersee"] <-  "GrosserOstersee"

# select lakes and add infos from selected_lakes
selected_extracted_lakes <- extracted_lakes_area[extracted_lakes_area$name %in% c(selected_lakes$LakeName, "Staffelsee"), ]
head(selected_extracted_lakes)
length(unique(selected_extracted_lakes$name))

setdiff(selected_lakes$LakeName, selected_extracted_lakes$name)

# add class infos 
class_lookup <- setNames(selected_lakes$class, selected_lakes$LakeName)
class_lookup["Staffelsee"] <- "medium"
selected_extracted_lakes$class <- class_lookup[selected_extracted_lakes$name]
head(selected_extracted_lakes)

# add area group infos
areagroup_lookup <- setNames(selected_lakes$AreaGroup, selected_lakes$LakeName)
areagroup_lookup["Staffelsee"] <- "medium"
selected_extracted_lakes$AreaGroup <- areagroup_lookup[selected_extracted_lakes$name]

# save as shapefile
writeVector(selected_extracted_lakes, "data/selected_lakes_bayern.shp", overwrite=TRUE)

# -------------------------------------------------------------------
# plot sorted nach lake class - turb, medium, clear

head(data_lakes_env_class)
data_lakes_env_class$LakeName <- gsub(" ", "", data_lakes_env_class$LakeName)
unique(data_lakes_env_class$LakeName)

length(names(all_dynamicData))

for(i in 1:length(all_dynamicData)){
    lake_name <- names(all_dynamicData)[i]
    lake_class <- data_lakes_env_class$class[data_lakes_env_class$LakeName == lake_name]
    all_dynamicData[[lake_name]]$class <- lake_class
    dep10_smooth_all$class[dep10_smooth_all$Name == lake_name] <- lake_class 
}

all_dynamicData_df <- bind_rows(all_dynamicData, .id = "LakeName")
head(all_dynamicData_df)
all_T_profiles_df <- bind_rows(all_T_profiles, .id = "LakeName")
head(all_T_profiles_df)

all_dynamicData_byClass <- all_dynamicData_df %>%
    group_by(class, day, AreaGroup) %>%
    summarise(across(where(is.numeric), mean, na.rm = TRUE)) %>% 
    ungroup()
head(all_dynamicData_byClass)
all_dynamicData_byClass$class <- as.factor(all_dynamicData_byClass$class)
all_dynamicData_byClass$AreaGroup <- as.factor(all_dynamicData_byClass$AreaGroup)

# grouped by area group
all_dynamicData_byArea <- all_dynamicData_df %>%
    group_by(day, AreaGroup) %>%
    summarise(across(where(is.numeric), mean, na.rm = TRUE)) %>% 
    ungroup()
head(all_dynamicData_byArea)
all_dynamicData_byArea$AreaGroup <- as.factor(all_dynamicData_byArea$AreaGroup)

# smoothed temp profile by class - metalimnion depth
head(dep10_smooth_all)
all_smoothed_metaDepth <- dep10_smooth_all %>%
    group_by(class, Month, Area_group) %>%
    summarise(across(where(is.numeric), mean, na.rm = TRUE)) %>% 
    ungroup() %>% drop_na(class) %>%  rename(
    AreaGroup = Area_group)
 
head(all_smoothed_metaDepth)
unique(all_smoothed_metaDepth$class)


# plot 
p_class_temp <- ggplot(all_dynamicData_byClass, aes(x = day)) + 
    geom_line(aes(y = tempEpi, linetype = "Epilimnion", color = AreaGroup)) +
    geom_line(aes(y = tempHypo, linetype = "Hypolimnion", color = AreaGroup)) +
    # geom_line(data = all_smoothed_metaDepth, aes(x = Month,y = Temp, linetype = "z0_smoothed", color = AreaGroup)) +
    labs(title = "Mean Daily Temperature by Lake Clarity Class", 
            x= "day", y = "Temp [°C]", linetype = "Temperature", color = "Area Group")+
    scale_linetype_manual(values = c("Epilimnion" = "solid", "Hypolimnion" = "dashed", "z0_smoothed" = "dotted")) +
    facet_wrap(~class) +
    scale_color_brewer(palette = "Set2") +
    theme_bw()

p_class_temp
ggsave(file.path("plots/cos-func-final/class", paste0("all_lakes_byClass_temp_cos.png")), plot = p_class_temp, width = 10, height = 6, units = "in", dpi = 300)

p_class_z0 <- ggplot(all_dynamicData_byClass, aes(x = day)) + 
        geom_line(aes(y = metaDepth, linetype = "z0_temp", color = AreaGroup), linewidth = 1) +
        geom_line(aes(y = z0_patalas, linetype = "z0_patalas", color = AreaGroup)) +
        #geom_line(data = all_smoothed_metaDepth, aes(x = Month,y = maxZ, linetype = "z0_smoothed", color = AreaGroup)) +
        labs(title = "Mean Daily Metalimnion Depth by Lake Clarity Class", 
                x= "day", y = "Temp [°C]", linetype = "metalimnion Depth", color = "Area Group")+
        scale_linetype_manual(values = c("z0_temp" = "solid", "z0_patalas" = "longdash",
                                        "z0_smoothed" = "dotted"), 
         labels = c("z0_temp" = expression(z[0]~"(temperature-based)"), 
                    "z0_patalas" = expression(z[0]~"(Patalas)"),
                    "z0_smoothed" = expression(z[0]~"(smoothed)"))) +
        facet_wrap(~class) +
        scale_color_brewer(palette = "Set2") +
        theme_bw()
p_class_z0
ggsave(file.path("plots/cos-func-final/class", paste0("all_lakes_byClass_z0_cos.png")), plot = p_class_z0, width = 10, height = 6, units = "in", dpi = 300)

p_area_z0 <- ggplot(all_dynamicData_byArea, aes(x = day)) + 
        geom_line(aes(y = metaDepth, linetype = "z0_temp", color = AreaGroup), linewidth = 1) +
        geom_line(aes(y = z0_patalas, linetype = "z0_patalas", color = AreaGroup)) +
        #geom_line(data = all_smoothed_metaDepth, aes(x = Month,y = maxZ, linetype = "z0_smoothed", color = AreaGroup)) +
        labs(title = "", 
                x= "Day", y = "Depth [m]", linetype = "c) metalimnion Depth", color = "LSAG")+
        scale_linetype_manual(values = c("z0_temp" = "solid", "z0_patalas" = "longdash",
                                        "z0_smoothed" = "dotted"), 
         labels = c("z0_temp" = expression(z[0]~"(temperature-based)"), 
                    "z0_patalas" = expression(z[0]~"(Patalas)"),
                    "z0_smoothed" = expression(z[0]~"(smoothed)"))) +
        scale_color_brewer(palette = "Set2") + theme_bw() +theme(legend.position = "none") #+ theme(legend.position = "bottom", legend.box = "vertical") 
p_area_z0
ggsave(file.path("plots/cos-func-final2.0/class", paste0("all_lakes_byArea_z0_cos.png")), plot = p_area_z0, 
    width = 4.5, height = 4.5, units = "in", dpi = 300, scale = 1)

# get legend
legend <- p_area_z0 + theme(legend.position = "bottom", legend.box = "vertical") 
legend <- get_legend(legend)
ggsave(file.path("plots/cos-func-final2.0/class", paste0("all_lakes_byArea_z0_cos_LEGEND.png")), plot = legend, 
    width = 6, height = 1, units = "in", dpi = 300, scale = 1)
