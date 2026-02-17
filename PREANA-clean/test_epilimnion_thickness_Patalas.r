# ----------------------------------------------------------------
# test epilimnioon thickness with Patalas
# ----------------------------------------------------------------
# The most central regression originates from Patalas [1984]
# $$z_{epi} = 4.6A^{0.205}$$
# which is close to previously used formulas from Ventz [1972] and Fachbereichsstandard [1983] (as cited by Klapper [1992]). The differences between fitted curves (factor 1.5) give a good impression of the accuracy at which ==**epilimnion thickness can be parameterized with surface area only. **

# Gorham and Boyce [1989] ==**confirm surface area and fetch as the most important factors** for the epilimnion thickness, as does Davies-Colley [1988] for lakes in New Zealand.

# - Wind speed considered through lake surface $A$ & fittet exponant
#------------------------------------------------------------------
# Author: Neele
# ----------------------------------------------------------------

# ---- packages ------------------------------------------------
library(writexl)
library(dplyr)
library(sf)
library(stringr)
library(ggplot2)
# ---- function -------------------------------------------------
z_epi_b <- function(df_area){ # after Boeahr 2008
  id <- df_area$ID

  area <- df_area$Area_km2
  z <- -4.6*area^(0.205)

  z_epi_df <- data.frame(ID = id, z_epi_b = z)
  return(z_epi_df)
}

z_epi_p <- function(df_area){ # after Buch pys verhältnisse gewässer
  id <- df_area$ID

  area <- df_area$Area_km2
  z <- -4.6*area^(0.41)

  z_epi_df <- data.frame(ID = id, z_epi_p = z)
  return(z_epi_df)
}


# ---- get area from Qgis -------
lake_area <- read.csv("data/lake_coords_area_qgis.csv")

z_boehr_df <- z_epi_b(lake_area) # boehr
z_phy_df <- z_epi_p(lake_area) # buch

dep10_patalas <- all_lake_dep10 %>%
  filter(ID %in% lake_area$ID) %>%
  mutate(date = as.Date(paste(Year, Month, Day, sep = "-"))) %>%
  left_join(lake_area[,c("ID", "Area_km2")], by = c("ID")) %>%
  left_join(z_boehr_df[,], by = "ID") %>%
  left_join(z_phy_df, by = "ID")

write.csv(dep10_patalas, file = "data/dep10_patalas.csv")

# ---- plot ------------------------------------
# ---- dep < -10 5:10 -------------------------------------------
# Frühling: 1. März–31. Mai (03-05)
# Sommer: 1. Juni–31. August (06-08)
time <- "5-10"
t <- 5:10

plot_data <- dep10_patalas %>% filter(Month %in% t) 
  
names(plot_data)
names(all_lake_dep10)
View(plot_data)

avg_plot_data <- plot_data %>%
  group_by(ID, Name, Month, Depth, z_epi_b, z_epi_p) %>%
  summarise(Temp = mean(Temp, na.rm = TRUE), .groups = "drop")
names(avg_plot_data)
# plot -------------------------
p_ss <- plot_data %>% 
    ggplot(aes(y=Depth, x = Temp, color = factor(Month), group=date)) +
    #geom_point(alpha = 0.5, size = 2) +
    geom_path() +
    facet_wrap(~Name, scales="free_y") +
    labs(x = "Temp. (°C)", y = "Depth (m)", color = "Month", title = paste("Temperature depth profiles", time)) +
    geom_hline(aes(yintercept = z_epi_b), linetype = "dashed", color = "black") +
    geom_hline(aes(yintercept = z_epi_p),linetype = "dotted", color = "red") +
    theme_bw() +
    scale_color_discrete(name = "Month")

p_ss # plot all months
# ggplotly(plot) # interactice plot

ggsave(file.path("plots", paste0("test_epi_dep10_",time, "_temp_profiles.png")), plot = p_ss, width = 100, height = 50, units = "cm", dpi = 300)

# plot average pe rmonth ----------------------
p_ss_avg <-  avg_plot_data %>%
  ggplot(aes(x = Temp, y = Depth, color = factor(Month), group = Month)) +
  geom_path() +
  facet_wrap(~Name, scales = "free_y") +
  labs(
    x = "Temp. (°C)", y = "Depth (m)", color = "Month",
    title = paste("Monthly Average Temperature Depth Profiles", time)
  ) +
  geom_hline(aes(yintercept = z_epi_b), linetype = "dashed", color = "black") +
  geom_hline(aes(yintercept = z_epi_p), linetype = "dotted", color = "red") +
  theme_bw() +
  scale_color_discrete(name = "Month")

p_ss_avg
ggsave(file.path("plots", paste0("test_epi_dep10_",time, "_temp_profiles_avg.png")), 
  plot = p_ss_avg, width = 100, height = 50, units = "cm", dpi = 300)



# area, z_epi --------------------------------
png("plots/z_epi-area_plot.png", width = 800, height = 600)
plot(plot_data$Area_km2, plot_data$z_epi_b, 
main = "function boehr, 2008 & Book \n area/z_epi",
ylab = "z_epi [m]", xlab = "lake area [km2]")
points(plot_data$Area_km2, plot_data$z_epi_p, col ="red")
legend("topright", legend = c("z_epi_b", "z_epi_p" ), col = c("black", "red"), pch = 1)
dev.off()

