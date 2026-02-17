# data preparation 
# 1) preprocess data names
# 2) coordinates of lakes
# 3) data epi- & hypolimnion temperature, calculate thermal gradient, and thermal difference

# file = "data/dep10_patalas.csv" in test_epi_thicknes_patalas
# file = "data/dep10_patalas_smooth.csv" in analysis_test_epi...


# Area_groups and thermal dif and gradien in Tepi_Thypo_analysis


# ---- 1) preprocess data names ---------------------------
# names ------------------------------
lake_names <- read.csv("data/raw_lake_names.csv",sep = ";", header = F)
colnames(lake_names) <- c("Name", "ID")

lake_names <- lake_names %>%
  mutate(Name = str_replace_all(Name, c(
    "ä" = "ae",
    "ö" = "oe",
    "ü" = "ue",
    "Ä" = "Ae",
    "Ö" = "Oe",
    "Ü" = "Ue",
    "ß" = "ss"  # optional, if you want to catch 'ß' too
  )))

lake_names_clean <- lake_names %>%
  mutate(lake_only = str_match(Name, "\\(([^)]+)\\)")[, 2]) %>%
  filter(!is.na(lake_only))

View(lake_names_clean)

lake_names_clean$lake_only <- str_replace_all(lake_names_clean$lake_only , " ", "")

lake_names_clean <- lake_names_clean[, !colnames(lake_names_clean) %in% "Name"]
colnames(lake_names_clean) <- c("ID", "Name")
write.csv(lake_names_clean, "data/lake_names.csv")

# ---- lake temp profiles 
files <- list.files("data/seen-vertical-temp/", pattern = "csv", full.names = T)
files <- files[-length(files)]
lake_names <- read.csv("data/lake_names.csv")[, -1]

list_of_lake_longs <- lapply(files, function(f) func_prepro_long_formate(f, lake_names)[[2]])

all_lake_long <- bind_rows(list_of_lake_longs)
write.csv(all_lake_long, "data/all_lake_temp_long.csv")

# ---- 2) coordinates of lakes --------------------------------
# depth < -10 and months 5-10
# see dep10_5-10_temp_profiles.png
all_lake_long <- read.csv("data/all_lake_temp_long.csv")[, -1] # drop Row NR X
all_lake_dep10 <- read.csv("data/all_lake_dep10_short.csv")[, -1]

View(all_lake_long)
colnames(all_lake_long)

# filter_lakes_depths < -10 m
deep_ids <- all_lake_long %>%
  group_by(ID) %>%
  filter(min(Depth, na.rm = TRUE) < -10) %>%
  distinct(ID)

all_lake_dep10 <- all_lake_long %>% 
    filter(ID %in% as.character(deep_ids$ID))

write.csv(all_lake_dep10, "data/all_lake_dep10_short.csv")
write_xlsx(all_lake_dep10, "data/all_lake_dep10_short.xlsx")

# get names depth < -10
unique(all_lake_dep10$Name)
df <- data.frame(Name = unique(all_lake_dep10$Name), ID = unique(all_lake_dep10$ID))
write_xlsx(df, "data/lake_names_dep10.xlsx")

 # ---- coordinates of lakes 
lakes <- read.csv("data/lake_names_dep10_Ost_nord_UTM.csv", sep = ";")
lakes <- na.omit(lakes)
lakes$Ost_v <- as.numeric(str_replace_all(lakes$Ost_v, "[^0-9]", ""))
lakes$Nord_v <- as.numeric(str_replace_all(lakes$Nord_v, "[^0-9]", ""))
View(lakes)

# Step 2: Convert to sf object using UTM Zone 32N (EPSG: 32632)
lakes_sf <- st_as_sf(lakes, coords = c("Ost_v", "Nord_v"), crs = 32632)

# Step 3: Transform to WGS84 (lat/lon; EPSG: 4326)
lakes_latlon <- st_transform(lakes_sf, crs = 4326)

# Step 4: Extract coordinates into columns
lakes_coords <- cbind(
  lakes,
  st_coordinates(lakes_latlon)
)

# Optional: rename cols
colnames(lakes_coords)[ncol(lakes_coords)-1:0] <- c("Longitude", "Latitude")

# save result
write.csv(lakes_coords, "data/lake_coords_dep10.csv")
write_xlsx(lakes_coords, "data/lake_coords_dep10.xlsx")

# ---- 3) data epi- & hypolimnion temperature, calculate thermal gradient, and thermal difference ------------------------------------
# seclect epi- & hypolimnion temperature, calculate thermal gradient, and thermal difference
# thermal_gradient <- (T_hypo-T_epi)/(Depth_max)
# thermal_dif <- T_hypo - T_epi

# ! just nessecary if data is not already prepared

# summarise the mean temperature for each month
epi_summary <- all_lake_dep10 %>%
  filter(Depth == 0) %>%
  group_by(ID, Month) %>%
  summarise(T_epi = mean(Temp, na.rm = TRUE), .groups = "drop")

hypo_summary <- all_lake_dep10 %>%
  group_by(ID) %>%
  filter(Depth == min(Depth, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(ID, Name, Depth, Month) %>%
  summarise(T_hypo = mean(Temp, na.rm = TRUE), .groups = "drop")

T_joined <- hypo_summary %>%
  left_join(epi_summary, by = c("ID", "Month"))%>% 
  mutate(thermal_gradient = (T_hypo-T_epi)/(Depth-0), # Thermal gradient calculation
         Month_w = factor(month.abb[Month], levels = month.abb), # convert Month to factor with month names
         thermal_dif = T_hypo - T_epi)  %>% # calculate temperature difference
    left_join(lake_area[,c("ID", "Area_km2")], by = c("ID")) %>% # join lake area
  left_join(dep10_patalas_sum, by = c("ID", "Depth"))  %>% # add calculated epilimnion thickness
  mutate(Area_group = cut(
    Area_km2,
    breaks = quantile(Area_km2, probs = seq(0, 1, length.out = 6), na.rm = TRUE),
    labels = c("very.small", "small", "medium", "large", "very.large"),
    include.lowest = TRUE
  ))
View(T_joined)

write.csv(T_joined, file = "data/dep10_epi-hypo_zPatalas.csv", row.names = FALSE)