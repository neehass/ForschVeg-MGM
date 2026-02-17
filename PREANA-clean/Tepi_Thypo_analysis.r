# ------------------------------------------------------------
# Tepi_Thypo_analysis 
# ------------------------------------------------------------
# This script analyses the temperature data of lakes, specifically focusing on the surface temperature (epilimnion) and bottom temperature (hypolimnion).
# It calculates the thermal gradient between these two layers and visualizes the results.

#  1) calculate thermal_gradient
##  1.1) Plot thermal gradient - boxplot for each month & depth 
##  1.2) Plot mean thermal gradient per month & depth
##  1.3) Plot mean and variance of thermal gradient per month & depth

#  2) ANOVA for thermal gradient & depth

#  3) thermal gradient & Area
##  3.1) ANOVA for thermal gradient & Area

# 4) estimate Mesolimnion depth (z0) from temperature data

# Copilot used:
# for modifying the code, suggesting improvements, debugging
# ------------------------------------------------------------
# Author: Neele 
# ------------------------------------------------------------

# packages
library(dplyr)
library(ggplot2)
library(viridis)
library(purrr)
library(tidyr)

# ---- load data ------------------------------------------------
# Load the temperature data for lakes with depth < -10 m
all_lake_dep10 <- read.csv("data/all_lake_dep10_short.csv")[, -1]
lake_area <- read.csv("data/lake_coords_area_qgis.csv") # used in 3) ANOVA for thermal gradient & Area

# bottom temp hypo & surface temp epi 
T_filterd <- all_lake_dep10 %>%
            mutate(date = as.Date(paste(Year, Month, Day, sep = "-"))) %>%
            group_by(ID, Name) %>%
            filter(Depth == min(Depth, na.rm = TRUE) | Depth == 0)%>%
            ungroup()

View(T_filterd)
# 
T_epi <- T_filterd[T_filterd$Depth == 0,]
T_hypo <- T_filterd[T_filterd$Depth < 0,]


plot <- T_filterd %>% 
    ggplot(aes(y=Depth, x = Temp, color = factor(Month), group=date)) +
    #geom_point(alpha = 0.5, size = 2) +
    geom_path() +
    facet_wrap(~Name, scales="free_y") +
    labs(x = "Temp. (°C)", y = "Depth (m)", color = "Month", title = paste("Temperature depth = 0 & max depth")) +
    theme_bw() +
    scale_color_discrete(name = "Month")
plot

ggsave(file.path("plots","Tepi_Thypo_analysis.png"), plot = plot, width = 100, height = 50, units = "cm", dpi = 300)


# ---- 1) calculate thermal_gradient ------------------------------
#  = slope m --> linear function y = mx + b (b = intercept with y)
# m <- (T_hypo-T_epi)/(Depth_max-0)

# surface temp epi
all_T_epi <- all_lake_dep10 %>%
            filter(Depth == 0) 

# bottom temp hypo
all_T_hypo <- all_lake_dep10 %>%
  group_by(ID) %>%
  filter(Depth == min(Depth, na.rm = TRUE))%>%
  ungroup()

epi_summary <- all_T_epi %>%
  group_by(ID, Month) %>%
  summarise(T_epi = mean(Temp, na.rm = TRUE), .groups = "drop")%>%
  ungroup()

hypo_summary <- all_T_hypo %>%
  group_by(ID, Name, Depth, Month) %>%
  summarise(T_hypo = mean(Temp, na.rm = TRUE), .groups = "drop")%>%
  ungroup()

T_joined <- hypo_summary %>%
  left_join(epi_summary, by = c("ID", "Month"))%>% 
  left_join(lake_area[,c("ID", "Area_km2")], by = c("ID")) %>% # join lake area
  mutate(thermal_gradient = (T_hypo-T_epi)/(Depth-0), # Thermal gradient calculation
         Month_w = factor(month.abb[Month], levels = month.abb), # convert Month to factor with month names
         t_diff = T_hypo - T_epi) # calculate temperature difference

unique(T_joined$Month_w)

View(T_joined)


## --- 1.1) Plot thermal gradient - boxplot for each month & depth -----------------------

p <- T_joined %>%
    ggplot(aes(y=thermal_gradient, x = Month , color = factor(Depth), group=Depth)) +
    geom_boxplot() +
    geom_line()
    labs(x = "Month", y = "thermal gradient", color = "Depth") +
    theme_bw() +
    scale_color_discrete(name = "Depth")
p

## ---- 1.2) Plot mean thermal gradient per month & depth -----------------------

p <- T_joined %>%
    group_by(Month_w, Depth) %>%
    summarise(mean_thermal_gradient = mean(thermal_gradient, na.rm = TRUE)) %>%
    ggplot(aes(y=mean_thermal_gradient, x = Month_w , color = factor(Depth), group=Depth)) +
    geom_line(size = 1.2) +
    labs(x = "Month", y = "thermal gradient", color = "Depth", 
    title = "mean thermal gradient per Month & lake depth" ) +
    theme_bw() +
    scale_color_viridis_d(name = "Depth")
p
ggsave(file.path("plots","Tepi_Thypo_analysis_thermal_gradient.png"), 
  plot = p, width = 15, height = 30, units = "cm", dpi = 300)

## ---- 1.3) Plot mean and variance of thermal gradient per month & depth -----------------------
p_var <- T_joined %>%
  group_by(Month_w, Depth) %>%
  summarise(
    mean = mean(thermal_gradient, na.rm = TRUE),
    sd = sd(thermal_gradient, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = Month_w, y = mean, color = factor(Depth), group = Depth)) +
  geom_line(size = 1.2) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2, alpha = 0.4, size = 0.5) +
  scale_color_viridis_d(name = "Depth") +
  labs(
    x = "Month",
    y = "Thermal Gradient (±SD)",
    title = "Mean Thermal Gradient with Variability"
  ) +
  theme_bw() 

p_var
ggsave(file.path("plots","Var_Tepi_Thypo_analysis_thermal_gradient.png"), 
  plot = p_var, width = 15, height = 30, units = "cm", dpi = 300)


# ---- 2) ANOVA for thermal gradient & depth -----------------------
# test if thermal gradient is significantly different between depths, per month
# H0 (Nullhypothese): Der Mittelwert des thermischen Gradienten ist für alle Tiefen gleich.
# H1 (Alternativhypothese): Mindestens eine Tiefe unterscheidet sich signifikant im Mittelwert des thermischen Gradienten.
anova_results <- T_joined %>%
  group_by(Month, Month_w) %>%
  filter(n_distinct(Depth) > 1) %>%  # Nur Monate mit mehreren Tiefen
  summarise(
    p_value = tryCatch(
      summary(aov(thermal_gradient ~ factor(Depth)))[[1]][["Pr(>F)"]][1],
      error = function(e) NA_real_
    ),
    .groups = "drop"
  ) %>%
  mutate(significant = p_value < 0.05)

sign_months_dep <- anova_results %>% filter(significant) %>% pull(Month)

# Post-hoc test if ANOVA is significant
tukey_results <- list()

for (m in sign_months_dep) {
  tg_data <- T_joined %>% filter(Month == m) # loops thorugh significant bands
  model <- aov(thermal_gradient ~ factor(Depth), data = tg_data)
  tukey <- TukeyHSD(model)
  tukey_results[[as.character(m)]] <- tukey
}
# Clean up the results
tukey_results <- compact(tukey_results)
head(tukey_results)

# Convert the Tukey results to a data frame 
tukey_df <- map_dfr(names(tukey_results), function(m) {  # depth
  res <- as.data.frame(tukey_results[[m]][[1]])  # extract the first element of the list
  res$Comparison <- rownames(res)
  res$Month <- m
  res
})

head(tukey_df)
tukey_df$Month_w <- factor(month.abb[as.numeric(tukey_df$Month)],
                           levels = month.abb)

# filter significant differences  
tukey_df$significant <- ifelse(tukey_df$`p adj` < 0.05, "Yes", "No")
tukey_df_sig <- tukey_df[which(tukey_df$significant == "Yes"),]
tukey_df_notsig <- tukey_df[which(tukey_df$significant == "No"),]

head(tukey_df_sig)

# print results per month
for( m in sign_months) {
  cat(paste("Month:", m, "\n"))
  print(tukey_df_sig$Comparison[tukey_df_sig$Month == m])
  cat("\n")
}
for( m in sign_months) {
  cat(paste("Month:", m, "\n"))
  print(tukey_df_notsig$Comparison[tukey_df_notsig$Month == m])
  cat("\n")
}

# plot significant diff of thermal gradient within depths
aov_p_sig <- ggplot(tukey_df_sig, aes(x = Comparison, y = diff, color = Comparison)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.2) +
  facet_wrap(~Month_w, scales = "free_x") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  labs(title = paste("Tukey HSD for", "significant diff of thermal gradient within depths"),
       x = "Group Comparison",
       y = "Mean Difference (± CI)",
       color = "Significant diff of thermal grad between Depths") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_color_viridis_d(name = "Significant diff of thermal grad between Depths") 
aov_p_sig

ggsave(file.path("plots","sig_diff_Tepi_Thypo_analysis_anova.png"), 
  plot = aov_p_sig, width = 30, height = 30, units = "cm", dpi = 300)

# not significant diff of thermal gradient within depths
aov_p_notsig <- ggplot(tukey_df_notsig, aes(x = Comparison, y = diff, color = Comparison)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.2) +
  facet_wrap(~Month_w, scales = "free_x") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  labs(title = paste("Tukey HSD for", "NO significant diff of thermal gradient within depths"),
       x = "Group Comparison",
       y = "Mean Difference (± CI)",
       color = "NO Significant diff of thermal grad between Depths") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_color_viridis_d(name = "NO Significant diff of thermal grad between Depths") 
aov_p_notsig
ggsave(file.path("plots","notsig_diff_Tepi_Thypo_analysis_anova.png"), 
  plot = aov_p_notsig, width = 40, height = 30, units = "cm", dpi = 300)


# ---- 3) thermal gradient & Area -----------------------
# breakdown of AREA in categories 0%, 20%, 40%, 60%, 80%, 100%.
T_joined <- T_joined %>%
  mutate(Area_group = cut(
    Area_km2,
    breaks = quantile(Area_km2, probs = seq(0, 1, length.out = 6), na.rm = TRUE),
    labels = c("very.small", "small", "medium", "large", "very.large"),
    include.lowest = TRUE
  ))
# plot mean thermal gradient per Area group
p_area <- T_joined %>%
  group_by(Area_group, Month_w) %>%
  summarise(mean_thermal_gradient = mean(thermal_gradient, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = Month_w, y = mean_thermal_gradient, color = Area_group, group = Area_group)) +
  geom_line(size = 1.2) +
  labs(
    x = "Area Group",
    y = "Mean Thermal Gradient",
    title = "Mean Thermal Gradient per Area Group and Month"
  ) +
  theme_bw() +
  scale_color_viridis_d(name = "Area Group")
p_area
ggsave(file.path("plots","area_group_Tepi_Thypo_analysis_thermal_gradient.png"), 
  plot = p_area, width = 15, height = 30, units = "cm", dpi = 300)

# plot T_hyp & T_epi per Area group
p_area_temp <- T_joined %>%
  group_by(Area_group, Month_w) %>%
  summarise(
    T_epi = mean(T_epi, na.rm = TRUE),
    T_hypo = mean(T_hypo, na.rm = TRUE),
    .groups = "drop") %>%
  pivot_longer(cols = c(T_epi, T_hypo), names_to = "Layer", values_to = "Temperature") %>%
  ggplot(aes(
    x = Month_w,
    y = Temperature,
    color = Area_group,
    linetype = Layer,
    group = interaction(Area_group, Layer)
  )) +
  geom_line(size = 1.2) +
  labs(
    x = "Month",
    y = "Temperature (°C)",
    title = "Surface and Bottom Temperature per Area Group and Month",
    color = "Area Group",
    linetype = "Layer"
  ) +
  theme_bw() +
  scale_color_viridis_d(name = "Area Group") +
  scale_linetype_manual(values = c("T_epi" = "solid", "T_hypo" = "dashed"))
p_area_temp
ggsave(file.path("plots","area_group_Tepi_Thypo_analysis_temp.png"), 
  plot = p_area_temp, width = 15, height = 30, units = "cm", dpi = 300)

## ---- 3.1) ANOVA for thermal gradient & Area -----------------------
# test if thermal gradient is significantly different between Area groups, per month

anova_results <- T_joined %>%
  group_by(Month, Month_w) %>%
  filter(n_distinct(Depth) > 1) %>%  # Nur Monate mit mehreren Tiefen
  summarise(
    p_value = tryCatch(
      summary(aov(thermal_gradient ~ Area_group))[[1]][["Pr(>F)"]][1],
      error = function(e) NA_real_
    ),
    .groups = "drop"
  ) %>%
  mutate(significant = p_value < 0.05)

sign_months <- anova_results %>% filter(significant) %>% pull(Month)
sign_months

# Post-hoc test if ANOVA is significant
tukey_results <- list()

for (m in sign_months) {
  tg_data <- T_joined %>% filter(Month == m) # loops thorugh significant bands
  model <- aov(thermal_gradient ~ Area_group, data = tg_data)
  tukey <- TukeyHSD(model)
  tukey_results[[as.character(m)]] <- tukey
}

# Clean up the results
tukey_results <- compact(tukey_results)
head(tukey_results)

# Convert the Tukey results to a data frame 
tukey_df <- map_dfr(names(tukey_results), function(m) {  # depth
  res <- as.data.frame(tukey_results[[m]][[1]])  # extract the first element of the list
  res$Comparison <- rownames(res)
  res$Month <- m
  res
})

head(tukey_df)
tukey_df$Month_w <- factor(month.abb[as.numeric(tukey_df$Month)],
                           levels = month.abb)
# filter significant differences  
tukey_df$significant <- ifelse(tukey_df$`p adj` < 0.05, "Yes", "No")
tukey_df_sig <- tukey_df[which(tukey_df$significant == "Yes"),]
tukey_df_notsig <- tukey_df[which(tukey_df$significant == "No"),]

head(tukey_df_sig)

# plot significant diff of thermal gradient within depths
aov_p_sig <- ggplot(tukey_df_sig, aes(x = Comparison, y = diff, color = Comparison)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.2) +
  facet_wrap(~Month_w, scales = "free_x") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  labs(title = paste("Tukey HSD for", "significant diff of thermal gradient dependent on Area"),
       x = "Group Comparison",
       y = "Mean Difference (± CI)",
       color = "Significant diff of thermal grad between Areas") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_color_viridis_d(name = "Significant diff of thermal grad between Areas") 
aov_p_sig

ggsave(file.path("plots","sig_diff_AREA_Tepi_Thypo_analysis_anova.png"), 
  plot = aov_p_sig, width = 30, height = 30, units = "cm", dpi = 300)

# ---- 4) estimate Mesolimnion depth (z0) from temperature data -----------------
# get depth z0 = Sprungschicht 
View(all_lake_dep10)

mean_T_perdepth <- all_lake_dep10 %>%
  group_by(ID, Name, Month, Depth) %>%
  summarise(mean_Temp = mean(Temp, na.rm = TRUE), .groups = "drop")  %>%
  complete(ID, Name, Month = 1:12, fill = list(mean_Temp = NA)) # fill missing months with NA

View(mean_T_perdepth)

# iteration over each moth --> calculate Tz

# finde z where temp <= T_epi (T depth = 0) * 0.3
nam <- "Hartsee"
sep_lake <- mean_T_perdepth %>%
  filter(Name == nam) %>%
  group_by(ID, Name, Month) %>%
  mutate(Tz = mean_Temp[Depth == 0] * 0.3)  # 30% of T_epi
View(sep_lake)





plot <- sep_lake %>%
  ggplot(aes(x = mean_Temp, y = Depth, color = factor(Month), group = Month)) +
  geom_line(size = 1.2) +
  labs(x = "Mean Temperature (°C) ", y = "Depth (m)", color = "Month", 
       title = paste("Temperature depth profile for", sep_lake$Name[1])) +
  theme_bw() +
  scale_color_viridis_d(name = "Month")
plot

# get z0 from temperature data
# T_join contains Temp diff between epi and hypo



T_info <- T_joined %>%
  filter(Name == nam, Month %in% sign_months_dep) %>%
  select(ID, Name, Month, T_epi, T_hypo) %>%
  mutate(T_z = T_epi * 0.3 ) # 30% of T_epi
  # 30% of the temperature difference T_epi + 0.3 * (T_epi - T_hypo)

z <- sep_lake %>%


View(T_info)
View(T_needed)
