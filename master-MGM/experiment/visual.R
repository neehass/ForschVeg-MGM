# -------------------------------------------------------------------------------------------
# Visualise Results
# -------------------------------------------------------------------------------------------
# beginne nur mit Chiemsee = lake 6
# general.config: Lakes: chiemsee (large) = 6, AbtsdorfSee (very.small) = 1, Eibsee (medium) = 7
# projekt: final_Chiem_Abts_Eib_1-5

# packages
library(ggplot2)
library(dplyr)

# working directory
getwd()
dir <- "output/final_Chiem_Abts_Eib_1-5"
setwd(dir)

# load data
res <- read.table("all_res_biomass_number_weight_height_daily.txt", header =TRUE)
env <- read.table("env.txt", header =TRUE)

View(res) # > 0 from day 140-240
View(env)

# -------------------------------------------------------------------------------------------
# Chiemsee
# -------------------------------------------------------------------------------------------
# --- Macrophyt data 
chiem <- res[res$lakeID == "6",]
chiem <- res[chiem$biomass > 0,] # select biomass > 0

ndays <- unique(chiem$day)
unique(chiem$lakeID) #!!!

chiem$speciesID <- as.factor(chiem$speciesID)
chiem$depth <- as.factor(chiem$depth)
# View(chiem)

# -----------------
# Plot
p <- ggplot(data = chiem, aes(x = day, y = biomass, 
        group = interaction(speciesID, depth), 
        color = depth, linetype = speciesID)) +
     geom_line() +
     theme_bw() +
     labs(
       title = paste("Chiemsee - Biomass over time per species"), x = "Day",
       y = "Biomass", color = "Depth", linetype = "Species"
     ) + facet_wrap(~ speciesID)
p

# Sav
ggsave("chiem_plot.png", plot = p, width = 10, height = 8)

# --------------------------------------
# depth 1
d <- unique(chiem$depth)
d1_chiem <- chiem[chiem$depth == d[1],]

# -----------------------------
# --- Env 
env_chiem <- res[env$lakeID == 6,]