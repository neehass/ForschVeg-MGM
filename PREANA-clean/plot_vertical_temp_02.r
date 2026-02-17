# --------------------------
# Vertical Temp 
# --------------------------
# Plotting 
# --------------------------
# Author: Neele 
# --------------------------

# packages 
library(ggplot2)
library(patchwork) # grids for ggplots
# graphics.off()
library(plotly)
source("help_func_vertical_temp.R")

getwd()

# ---- load files ---------
lake_names <- read.csv("data/lake_names.csv")[, -1]
all_lake_long <- read.csv("data/all_lake_temp_long.csv")[, -1] # drop Row NR X

View(lake_names)
View(all_lake_long)
View(lake)
colnames(lake[2:ncol(lake)])
colnames(all_lake_long)

# filter lakes depths < -5 m
deep_ids <- all_lake_long %>%
  group_by(ID) %>%
  filter(min(Depth, na.rm = TRUE) < -5) %>%
  distinct(ID)


all_lake_dep5 <- all_lake_long %>% 
    filter(ID %in% as.character(deep_ids$ID))

# filter lakes depths < -10
deep_ids <- all_lake_long %>%
  group_by(ID) %>%
  filter(min(Depth, na.rm = TRUE) < -10) %>%
  distinct(ID)

all_lake_dep10 <- all_lake_long %>% 
    filter(ID %in% as.character(deep_ids$ID))

# ---- plot -----------------------------------------
# ---- all months -------------------------------------------
plot <- all_lake_dep5 %>% # all_lake_long %>%
    mutate(date = as.Date(paste(Year, Month, Day, sep = "-"))) %>%
    ggplot(aes(y=Depth, x = Temp, color = factor(Month), group=date)) +
    #geom_point(alpha = 0.5, size = 2) +
    geom_path() +
    facet_wrap(~ID, scales="free_y") +
    labs(x = "Temp. (°C)", y = "Depth (m)", color = "Month", title = "Temperature depth profiles over the months") +
    theme_bw() +
    scale_color_discrete(name = "Month")

plot # plot all months
# ggplotly(plot) # interactice plot
ggsave("plots/dep5_all_months_temp_profiles.png", plot = plot, width = 100, height = 50, units = "cm", dpi = 300)


# ---- spring and summer -------------------------------------------
# Frühling: 1. März–31. Mai (03-05)
# Sommer: 1. Juni–31. August (06-08)
# month 3-8
time <- "spring & summer"
t <- 3:8
p_ss <- all_lake_dep5 %>% # all_lake_long %>%
    filter(Month %in% t) %>%
    mutate(date = as.Date(paste(Year, Month, Day, sep = "-"))) %>%
    ggplot(aes(y=Depth, x = Temp, color = factor(Month), group=date)) +
    #geom_point(alpha = 0.5, size = 2) +
    geom_path() +
    facet_wrap(~ID, scales="free_y") +
    labs(x = "Temp. (°C)", y = "Depth (m)", color = "Month", title = paste("Temperature depth profiles", time)) +
    theme_bw() +
    scale_color_discrete(name = "Month")

p_ss # plot all months
# ggplotly(plot) # interactice plot
ggsave(file.path("plots", paste0("dep5_",time, "_temp_profiles.png")), plot = p_ss, width = 100, height = 50, units = "cm", dpi = 300)

# ---- 1, 3, 6, 8, 10 -------------------------------------------
# Frühling: 1. März–31. Mai (03-05)
# Sommer: 1. Juni–31. August (06-08)
time <- "1_3_6_8_10"
t <- c(1, 3, 6, 8, 10)
p_ss <- all_lake_dep5 %>% # all_lake_long %>%
    filter(Month %in% t) %>%
    mutate(date = as.Date(paste(Year, Month, Day, sep = "-"))) %>%
    ggplot(aes(y=Depth, x = Temp, color = factor(Month), group=date)) +
    #geom_point(alpha = 0.5, size = 2) +
    geom_path() +
    facet_wrap(~ID, scales="free_y") +
    labs(x = "Temp. (°C)", y = "Depth (m)", color = "Month", title = paste("Temperature depth profiles", time)) +
    theme_bw() +
    scale_color_discrete(name = "Month")

p_ss # plot all months
# ggplotly(plot) # interactice plot
ggsave(file.path("plots", paste0("dep5_",time, "_temp_profiles.png")), plot = p_ss, width = 100, height = 50, units = "cm", dpi = 300)

# ---- 5:10 -------------------------------------------
# Frühling: 1. März–31. Mai (03-05)
# Sommer: 1. Juni–31. August (06-08)
time <- "5-10"
t <- 5:10
p_ss <- all_lake_dep5 %>% # all_lake_long %>%
    filter(Month %in% t) %>%
    mutate(date = as.Date(paste(Year, Month, Day, sep = "-"))) %>%
    ggplot(aes(y=Depth, x = Temp, color = factor(Month), group=date)) +
    #geom_point(alpha = 0.5, size = 2) +
    geom_path() +
    facet_wrap(~Name, scales="free_y") +
    labs(x = "Temp. (°C)", y = "Depth (m)", color = "Month", title = paste("Temperature depth profiles", time)) +
    theme_bw() +
    scale_color_discrete(name = "Month")

p_ss # plot all months
# ggplotly(plot) # interactice plot
ggsave(file.path("plots", paste0("dep5_",time, "_temp_profiles.png")), plot = p_ss, width = 100, height = 50, units = "cm", dpi = 300)


# ---- dep < -10 5:10 -------------------------------------------
# Frühling: 1. März–31. Mai (03-05)
# Sommer: 1. Juni–31. August (06-08)
time <- "5-10"
t <- 5:10
p_ss <- all_lake_dep10 %>% # all_lake_long %>%
    filter(Month %in% t) %>%
    mutate(date = as.Date(paste(Year, Month, Day, sep = "-"))) %>%
    ggplot(aes(y=Depth, x = Temp, color = factor(Month), group=date)) +
    #geom_point(alpha = 0.5, size = 2) +
    geom_path() +
    facet_wrap(~Name, scales="free_y") +
    labs(x = "Temp. (°C)", y = "Depth (m)", color = "Month", title = paste("Temperature depth profiles", time)) +
    theme_bw() +
    scale_color_discrete(name = "Month")

p_ss # plot all months
# ggplotly(plot) # interactice plot
ggsave(file.path("plots", paste0("dep10_",time, "_temp_profiles.png")), plot = p_ss, width = 100, height = 50, units = "cm", dpi = 300)


