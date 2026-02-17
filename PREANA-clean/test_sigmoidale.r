# test_sigmoidale
# $$T(z) = T_{\text{hypo}} + \frac{T_{\text{epi}} - T_{\text{hypo}}}{1 + e^{\frac{z - z_0}{k}}}$$

library(dplyr)
library(ggplot2)

all_lake_dep10 <- read.csv("data/all_lake_dep10_short.csv")[, -1]
dep10_smooth <- read.csv("data/dep10_patalas_smooth.csv")
EpiHyp_dep10 <- read.csv("data/dep10_epi-hypo_zPatalas.csv")  


# get depth z0 = Sprungschicht ------------------
Tz <- T_all$T_1[1]
T_epi <- T_all$T_epi[1]
T_hypo <- T_all$T_hypo[1]
z <- -1
k <- 1

get_z0_from_temp <- function(Tz, T_epi, T_hypo, z, k) {
  ratio <- (T_epi - T_hypo) / (Tz - T_hypo)
  inside_log <- abs(ratio - 1)

  if (is.na(inside_log) || inside_log <= 0) {
    return(NA)  # oder NA
  }

  z0 <- z + k * log(inside_log)
  return(z0)
}

get_z0_from_temp <- function(Tz, T_epi, T_hypo, z, k) {
  z0 <- z + k * log((T_epi - T_hypo) / (Tz - T_hypo) - 1)
  return(z0)
}

# Temperaturprofil-Funktion in R
T_profile <- function(z, T_epi, T_hypo, z0, k) {
  T_epi + (T_hypo - T_epi) / (1 + exp((z - z0) / k))
}

# ---- data --------------------------------------------------------------------------
# surface temp epi
all_T_epi <- dep10_smooth %>%
            filter(Depth == 0) 

# bottom temp hypo
all_T_hypo <- dep10_smooth %>%
  group_by(ID, Name, Month) %>%
  filter(Depth == min(Depth, na.rm = TRUE)) %>%
  ungroup()

all_T_1 <- dep10_smooth %>%
  group_by(ID, Name, Month) %>%
  filter(Depth == -1)%>%
  ungroup()

epi_summary <- all_T_epi %>%
  group_by(ID, Month) %>%
  summarise(T_epi = mean(Temp, na.rm = TRUE), .groups = "drop")
View(epi_summary)

hypo_summary <- all_T_hypo %>%
  group_by(ID, Depth, Month) %>%
  summarise(T_hypo = mean(Temp, na.rm = TRUE), .groups = "drop") 
View(hypo_summary)

d1_summary <- all_T_1 %>%
  group_by(ID, Depth, Month) %>%
  summarise(T_1 = mean(Temp, na.rm = TRUE), .groups = "drop")

T_all <- epi_summary %>%
  left_join(hypo_summary, by = c("ID", "Month")) %>%
  left_join(d1_summary, by = c("ID", "Month")) %>%
  ungroup()
View(T_all)

k <- 2.5
T_all_z0 <- T_all %>%
  rowwise() %>%
  mutate(z0 = get_z0_from_temp(T_1, T_epi, T_hypo, z = 1, k)) %>%
  mutate(T0 = T_profile(z = z0, T_epi, T_hypo, z0, k))

new_dep10_smooth <- dep10_smooth %>%
      left_join(T_all_z0, by = c("ID", "Month")) 

new_dep10_smooth <- new_dep10_smooth %>%
    mutate(dif_z0 = abs(maxZ - z0))

View(T_all_z0)
View(new_dep10_smooth)

min(new_dep10_smooth$dif_z0 , na.rm = TRUE)
max(new_dep10_smooth$dif_z0 , na.rm = TRUE)

# ---- plot ------------------------
unique(new_dep10_smooth$ID)
span = 0.5
for(i in unique(new_dep10_smooth$ID)) {
  # select one lake for example
  ex <- new_dep10_smooth %>% filter(ID == i)
  p_ex <- ggplot(ex, aes(x = Temp, y = Depth, color = factor(Month), group = Month)) +
      geom_line() +
      geom_point(data = ex, aes(x = Temp_smooth, y = Depth), 
                color = "red") +

      # get z0 function
      geom_vline(data = ex, aes(xintercept = T0), 
                linetype = "dashed", color = "magenta") +
      geom_hline(data = ex, aes(yintercept = z0), 
                linetype = "dashed", color = "cyan") +

      # max slope
      geom_vline(data = ex, aes(xintercept = maxT), 
                linetype = "dashed", color = "red") +
      geom_hline(data = ex, aes(yintercept = maxZ), 
                linetype = "dashed", color = "blue") +
      
      # Patalas et al. 2023
      geom_hline(data = ex, aes(yintercept = z_epi_b), 
                linetype = "dotted", color = "black") +
      geom_hline(data = ex, aes(yintercept = z_epi_p), 
                linetype = "dotted", color = "darkgreen") +
      facet_wrap(~Month, scales = "free_y") +
      labs(title = paste("Temperature Profiles for Lake", unique(ex$Name)),
          x = "Temperature (°C)", y = "Depth (m)") +
      theme_bw()
  p_ex
  ggsave(file.path("plots/func_z0", paste0(unique(ex$Name),"_temp_profile_", i, "_s", span,"_k",k, ".png")), plot = p_ex, width = 10, height = 8, units = "in", dpi = 300)
}


p <- ggplot(new_dep10_smooth, aes(x = factor(Month), color = factor(Month))) +
  geom_point(aes(y = maxZ, shape = "maxZ"), size = 2) +
  geom_point(aes(y = z0, shape = "z0"), size = 2) +
  geom_point(aes(y = z_epi_b, shape = "z_epi_b"), size = 2) +
  geom_point(aes(y = z_epi_p, shape = "z_epi_p"), size = 2) +
  facet_wrap(~Name, scales = "fixed") +
  labs(
    y = "maxZ / z0 / z_epi_b / z_epi_p",
    x = "Monat",
    color = "Monat",
    shape = "Messgröße"
  ) +
  scale_shape_manual(
    values = c("maxZ" = 16, "z0" = 1, "z_epi_b" = 2, "z_epi_p" = 3)
  ) +
  theme_bw()

p
# ---- Temp-Profil function ---------------------------------
# Temperaturprofil-Funktion in R
T_profile <- function(z, T_epi, T_hypo, z0, k) {
  z <- sort(z, decreasing = TRUE)
  T_fit <- T_epi + (T_hypo - T_epi) / (1 + exp((z - z0) / k))
  T_fit <- rev(T_fit)
  return(T_fit) # reverse
}

Tz_dep10 <- new_dep10_smooth %>% group_by(ID, Month, Depth) %>%
      mutate(Tz_k3 = T_profile(z = Depth, T_epi, T_hypo, z0, k = 3),
              Tz_epi_b_k3 = T_profile(z = Depth, T_epi, T_hypo, z0= z_epi_b, k = 3),
              Tz_maxZ_k3 = T_profile(z = Depth, T_epi, T_hypo, z0= maxZ, k = 3),
              Tz_k5 = T_profile(z = Depth, T_epi, T_hypo, z0, k = 5),
              Tz_epi_b_k5 = T_profile(z = Depth, T_epi, T_hypo, z0= z_epi_b, k = 5))

for(i in unique(Tz_dep10$ID)) {
  dat <- Tz_dep10 %>% filter(ID == i)
  p <- ggplot(dat, aes(x = Temp, y = Depth, color = factor(Month), group = Month)) +
        geom_line() +
        geom_point(aes(x = Temp_smooth, shape = "T_smooth"), color = "grey") +
        geom_line(aes(x = Tz_k3, linetype = "Tz_k3"), color = "black") + 
        geom_line(aes(x = Tz_epi_b_k3, linetype = "Tz_epi_b_k3"), color = "black") +
        geom_line(aes(x = Tz_maxZ_k3, linetype = "Tz_maxZ_k3"), color = "black") +
        geom_line(aes(x = Tz_k5, linetype = "Tz_k5"), color = "red") + 
        geom_line(aes(x = Tz_epi_b_k5, linetype = "Tz_epi_b_k5"), color = "red") +
        facet_wrap(~Month, scales = "free_y") + 
        labs(title = paste("function TProfile")) +
        scale_linetype_manual(values = c("Tz_k3" = "dotted", 
        "Tz_epi_b_k3" = "dashed", "Tz_k5" = "dotted", "Tz_epi_b_k5" = "dashed",
        "Tz_maxZ_k3" = "dotdash")) +
        scale_shape_manual(values = c("T_smooth" = 16)) +
        theme_bw()      
  p
  ggsave(file.path("plots/func_Tprofile/single", paste0(unique(dat$Name),"_func_temp_profile.png")), plot = p, width = 10, height = 8, units = "in", dpi = 300)
}



# # apply for z_epi --------
# # define z_epi as the depth where temperature is, e.g., 30% of the T from T_hypo to T_epi
# # T_z <- T_hypo + 0.3 * (all_T_epi - all_T_hypo)

# epi_summary <- all_T_epi %>%
#   group_by(ID) %>%
#   summarise(T_epi = mean(Temp, na.rm = TRUE), .groups = "drop")

# hypo_summary <- all_T_hypo %>%
#   group_by(ID, Depth) %>%
#   summarise(T_hypo = mean(Temp, na.rm = TRUE), .groups = "drop")

# T_joined <- epi_summary %>%
#   left_join(hypo_summary, by = "ID") %>%
#   mutate(Tz = T_hypo + 0.05 * (T_epi - T_hypo))

# #View(T_joined)
# idx <- which(colnames(T_joined) %in% "Depth")
# colnames(T_joined)[idx] <- "z"
# T_joined$k <- 6 # Maß für die **Übergangsbreite** der Sprungschicht  
# # z_epi <- get_depth_from_temp(T_90, T_epi, T_hypo, z0, k)
# library(purrr)
# T_joined <- T_joined %>%
#   mutate(
#     z_0 = pmap_dbl(
#       list(Tz, T_epi, T_hypo, z, k),
#       get_z0_from_temp
#     )
#   )
# View(T_joined)



