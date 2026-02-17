# cos-function Jahresverlauf
library(ggplot2)
library(patchwork)
library(lubridate)
library(dplyr)
library(tidyr)

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


# ---- test functions -------------------------------------------------------------------
i <- 6
span <- 0.5
# general settings
yearlength <- 365

EpiHyp_dep10 <- read.csv("data/dep10_epi-hypo_zPatalas.csv") 
EpiHyp_dep10_mean <- EpiHyp_dep10 %>% group_by(Area_group) %>%
    summarise(Area_km2_mean = mean(Area_km2, na.rm=TRUE))

dep10_smooth_all <- read.csv("data/dep10_patalas_smooth_all.csv")

minmax_frac <- read.csv("data/minmax_frac_area_group.csv") # calculated from data (mean per month by area_group)
rownames(minmax_frac) <- minmax_frac$X

norm_frac_z0 <- read.csv("data/norm_z0_frac_depth_group_summary.csv")

# cos-func2
min_val <- c(0.4, 0.47,0.66, 0.30,0.25, 0.5, 0.15, 0.15) # selbst justiert z0 fraktion
norm2_frac_z0 <- read.csv("data/norm_z0_frac_depth_group_summary.csv")
norm2_frac_z0$Min_Value <- min_val
norm_frac_z0$Min_Value2 <- min_val

# example lake form MGM
# Chiemsee 6 
# Koenigssee 14
# 1 AbtsdorferSee
# Eibsee 7
for(i in 1:31){
    
    file_path <- file.path("C:/Users/maiim/Documents/25-25SS/Forschungsprojekt_Vegetationskunde/scripte-data-MGM/master-MGM/input/lakes/",
                            paste0("lake_",i,".config.txt"))
    test_dat <- read.table(file_path) 
    test_dat <- as.data.frame(test_dat)
    colnames(test_dat) <- c("para", "value")
    name <- test_dat$value[test_dat$para == "Name"]

    print(name)
    if(any(EpiHyp_dep10$Name == name)){
        
        print(paste("exists", i))
        # set parameter
        max_Tepi <- as.numeric(test_dat$value[test_dat$para == "maxTemp"])
        min_Tepi <-  as.numeric(test_dat$value[test_dat$para == "minTemp"])
        tempDev <-  as.numeric(test_dat$value[test_dat$para == "tempDev"])
        tempDelay <-  as.numeric(test_dat$value[test_dat$para == "tempDelay"])

        # for Thypo by area
        Area_group <- unique(EpiHyp_dep10$Area_group[EpiHyp_dep10$Name == name])
        depth <- unique(EpiHyp_dep10$Depth[EpiHyp_dep10$Name == name])
        
        # data for smoothed temp plot 
        dat_smooth <- dep10_smooth_all %>% filter(Name == name) 

        # test functions
        dynamicData <- data.frame(Month = rep(NA,yearlength), Tepi = rep(NA, yearlength), Thypo_mean = rep(NA, yearlength), Thypo_mean_dif = rep(NA, yearlength),
                                    Thypo_area = rep(NA, yearlength), Thypo_area_dif = rep(NA, yearlength), 
                                    z0_mean = rep(NA, yearlength), z0_mean_set0 = rep(NA, yearlength))
        for(day in 1:yearlength){
            month_name <- month(ymd("2023-01-01") + days(day - 1))
            dynamicData$Month[day] <- month_name

            Tepi <- getTdaily_epi(day, tempDev, max_Tepi , min_Tepi,  tempDelay, yearlength)
            Thypo_mean <- getTdaily_hypo(day, tempDev, max_Tepi , min_Tepi,  tempDelay, yearlength)
            Thypo_area <- getTdaily_hypo_area(day, tempDev, max_Tepi, min_Tepi, tempDelay, yearlength, Area_group, minmax_frac)

            dynamicData$Tepi[day] <- Tepi
            dynamicData$Thypo_mean[day] <- Thypo_mean
            dynamicData$Thypo_mean_dif[day] <- abs(Thypo_mean - Tepi)

            dynamicData$Thypo_area[day] <- Thypo_area
            dynamicData$Thypo_area_dif[day] <- abs(Thypo_area - Tepi)
            
        }

        delay <- order(dynamicData$Thypo_mean_dif)[1:2] # Tage der zwei kleinste WErte
        Thypo_max <- max(dynamicData$Thypo_area)
        Tepi_max <- max(dynamicData$Tepi)

        for(day in 1:yearlength){
            z0 <- getZ0_daily(day, depth, yearlength, Area_group = Area_group, delay_day = max(delay), norm_frac_z0)
            z0_set0 <- getZ0_set_0_daily(day, depth, yearlength, Area_group = Area_group, delay_day1 = min(delay), delay_day2 = max(delay) ,norm_frac_z0)
            dynamicData$z0_mean[day] <- z0
            dynamicData$z0_mean_set0[day] <- z0_set0

            z02 <- getZ0_daily(day, depth, yearlength, Area_group = Area_group, delay_day = max(delay), norm2_frac_z0)
            z0_set02 <- getZ0_set_0_daily(day, depth, yearlength, Area_group = Area_group, delay_day1 = min(delay), delay_day2 = max(delay) ,norm2_frac_z0)
            dynamicData$z0_mean_2[day] <- z02
            dynamicData$z0_mean_set0_2[day] <- z0_set02

            # Thyp <- dynamicData$Thypo_area[i]
            # Tepi <- dynamicData$Tepi[i]

            # Temp-based z0 calculation (1 delay day), norm_frac_z0 = fraction = estimated inflection point of depth per area group
            z0_temp <- getZ0_temp_daily(day, depth, yearlength, Area_group, delay_day = max(delay), 
                                        norm_frac_z0, Thypo_max, Tepi_max)
            z0_temp_2 <- getZ0_temp_daily(day, depth, yearlength, Area_group, delay_day = max(delay), 
                                        norm2_frac_z0,Thypo_max, Tepi_max)
            dynamicData$z0_temp[day] <- z0_temp
            dynamicData$z0_temp_2[day] <- z0_temp_2

            # Temp-based z0 calculation (2 delay days), norm_frac_z0 = fraction = estimated inflection point of depth per area group

            # Temp-based z0 calculation (1 delay day), patalas approach

            
        }
            

        p1 <- ggplot(dynamicData, aes(x = 1:yearlength)) + 
                geom_line(aes(y = Tepi, color = "Tepi")) +
                geom_line(aes(y = Thypo_mean, color = "Thypo_mean")) +
                geom_line(aes(y = Thypo_area, color = "Thypo_area")) +
                labs(title = paste(name, "Area group:", Area_group), 
                        x= "day", y = "Temp [°C]", color = "Layer")+
                theme_bw()
        p2 <- ggplot(dynamicData, aes(x = 1:yearlength)) + 
            geom_line(aes(y = z0_mean, color = "z0_mean")) +
            geom_line(aes(y = z0_mean_set0, color = "z0_mean_set0")) +
            #geom_line(aes(y = z0_mean_2, color = "z0_mean_2"), linetype = "dotted") +
            geom_line(aes(y = z0_mean_set0_2, color = "z0_mean_set0_2"), linetype = "dotted") +
            geom_line(aes(y = z0_temp, color = "z0_temp"), linetype = "dashed") +
            #geom_line(aes(y = z0_temp_2, color = "z0_temp_2"), linetype = "longdash") +
        labs(title= "Mesolimnion Depth",x= "day", y = "Depth [m]", color = "Layer") +
                theme_bw()
        
        combined_plot <- p1 / p2 + plot_layout(heights = c(1, 0.5))
        print(combined_plot)

        ggsave(file.path("plots/cos-func", paste0(name,"_hyp_epi_cos.png")), plot = combined_plot, width = 8, height = 10, units = "in", dpi = 300)


        # bsp Temp profile, summarise by month
        
        # View(dynamicData)
        dynamicData_sum <- dynamicData %>% group_by(Month) %>%
        summarise(across(where(is.numeric), mean, na.rm = TRUE)) 
    
        temp_profiles <- dynamicData_sum %>%
        group_by(Month) %>%
        summarise(T_profile = list(T_profile(0:depth, Tepi, Thypo_area, z0_mean, k = 5)),
                T_profile_2 = list(T_profile(0:depth, Tepi, Thypo_area, z0_mean_2, k = 5)),
                T_profile_z0temp = list(T_profile(0:depth, Tepi, Thypo_area, z0_temp, k = 5)),
                T_profile_z0temp_2= list(T_profile(0:depth, Tepi, Thypo_area, z0_temp_2, k = 5))) %>%
        unnest(cols = c(T_profile, T_profile_2, T_profile_z0temp,T_profile_z0temp_2))%>%
        mutate(
            Depth = rep(0:depth, length.out = n()),
            T_profile = as.numeric(T_profile),
            T_profile_2 = as.numeric(T_profile_2),
            T_profile_z0temp = as.numeric(T_profile_z0temp),
            T_profile_z0temp_2 = as.numeric(T_profile_z0temp_2)
        )

        # View(dynamicData_sum)

        # plot temp profile and temp smoothed
        p <-ggplot(data = temp_profiles, aes(x = T_profile, y = Depth, group = Month, color = factor(Month), linetype = "z0_mean")) + geom_line()+
            geom_hline(data = dynamicData_sum, aes(yintercept = z0_mean, group = Month, color = factor(Month), linetype = "z0_mean")) + 
            
            # geom_line(data = temp_profiles, aes(x = T_profile_2, y = Depth, group = Month, color = factor(Month), linetype = "z0_mean_2")) +
            # geom_hline(data = dynamicData_sum, aes(yintercept = z0_mean_2, group = Month, color = factor(Month), linetype = "z0_mean_2")) + 
            
            geom_line(data = temp_profiles, aes(x = T_profile_z0temp, y = Depth, group = Month, color = factor(Month), linetype = "z0_temp")) +
            geom_hline(data = dynamicData_sum, aes(yintercept = z0_temp, group = Month, color = factor(Month), linetype = "z0_temp")) + 
          
            # geom_line(data = temp_profiles, aes(x = T_profile_z0temp_2, y = Depth, group = Month, color = factor(Month), linetype = "z0_temp_2")) +
            # geom_hline(data = dynamicData_sum, aes(yintercept = z0_temp_2, group = Month, color = factor(Month), linetype = "z0_temp_2")) + 

            geom_line(data = dat_smooth, aes(x = Temp_smooth, y = Depth, group = Month, linetype = "Smoothed_Temp"), color = "gray50") +
            geom_hline(data = dat_smooth, aes(yintercept = maxZ, group = Month, linetype = "Smoothed_Temp"), color = "gray50") +

            
            facet_wrap(~Month) + 
            labs(title = paste0("estimated Tempprofile ",name, " (Area group: ", Area_group, ")"), 
                        x= "Temp [°C]", y = "Depth [m]", color = "Month") +
            scale_linetype_manual(values = c("z0_mean" = "solid", "z0_temp" = "longdash", 
                                            # "z0_mean_2" = "dotted", "z0_temp_2" = "dashed", 
                                            "Smoothed_Temp" = "solid")) +
            theme_bw()
        p
        ggsave(file.path("plots/cos-func", paste0(name,"_cos_Tprofile.png")), plot = p, width = 10, height = 8, units = "in", dpi = 300)


    }
    
}
