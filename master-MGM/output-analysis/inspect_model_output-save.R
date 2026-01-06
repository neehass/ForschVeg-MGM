# compare Tprofile spec100 und spec5 - different data saving
base_Tprofile <- "output/dep10_lakes_100spec_base_Tprofile"
save_figures100 <- "output-analysis/dep10_lakes_100spec_base_Tprofile"
load(file.path(save_figures100, "res_dep10_Tprofile.RData"))   
res100 <- res
View(res100)
load(file.path(save_figures100, "env_dep10_Tprofile.RData"))   
env100 <- env

save_figures5 <- "output-analysis/dep10_lakes_5spec_base_Tprofile_parallelNAME"
load(file.path(save_figures5, "res_dep10_Tprofile.RData"))   
res5 <- res

load(file.path(save_figures5, "sortENV_dep10.RData"))   
sort_env5 <- sort_env

rm(res)
rm(env)

# 100 spec
sort_env100 <- env100 %>%
  group_by(lakeClass, AreaGroup, day) %>%
  summarise(
    tempEpi_mean = mean(tempEpi), 
    tempHypo_mean = mean(tempHypo),
    metaDepth_mean = mean(metaDepth),
    irradiance_mean = mean(irradiance),
    waterlevel_mean = mean(waterlevel),
    lightAttenuation_mean = mean(lightAttenuation), 
    lakeDepth_mean = mean(lakeDepth)
  ) %>% ungroup()  %>%
  mutate(AreaGroup = factor(AreaGroup,
                            levels = c("very.small", "small", "medium", "large", "very.large")))
View(sort_env100)
scenario <- "base-100spec_Tprofile"
func_sortENV_plot(sort_env100, save_figures100, scenario) # defined in help-func.R

# 5 spec
sort_env5 <- env5 %>%
  group_by(lakeClass, AreaGroup, day) %>%
  summarise(
    tempEpi_mean = mean(tempEpi), 
    tempHypo_mean = mean(tempHypo),
    metaDepth_mean = mean(metaDepth),
    irradiance_mean = mean(irradiance),
    waterlevel_mean = mean(waterlevel),
    lightAttenuation_mean = mean(lightAttenuation), 
    lakeDepth_mean = mean(lakeDepth)
  ) %>% ungroup()  %>%
  mutate(AreaGroup = factor(AreaGroup,
                            levels = c("very.small", "small", "medium", "large", "very.large")))
View(sort_env5)
scenario <- "base-5spec_Tprofile"
func_sortENV_plot(sort_env5, save_figures5, scenario) # defined in help-func.R

# --- TProfile Development over Days ------
# mean Depths per lakeClass and lakeGroup_Area
head(sort_env)

sort_env_Tprof_DAY <- na.omit(sort_env5) %>% # sort_env[sort_env$day %in% unique(sort_res$day), ] 
  group_by(lakeClass, AreaGroup, day) %>%
  # mutate(day_bin = floor((day - 1) / 30) * 30 + 1) %>%  # days 1–7 → 1, 8–14 → 8, etc.
  mutate(month_bin = floor((day - 1) / 31) + 1) %>%  # month 1, 2, 3...
  group_by(lakeClass, AreaGroup, month_bin) %>%
  summarise(
    tempEpi_av = mean(tempEpi_mean, na.rm = TRUE),
    tempHypo_av = mean(tempHypo_mean, na.rm = TRUE),
    metaDepth_av = mean(metaDepth_mean, na.rm = TRUE),
    lakeDepth_av = round(mean(lakeDepth_mean, na.rm = TRUE)), 
    T_prof = list(T_profile_1(z = sort(seq(lakeDepth_av, 0, 0.5), decreasing = TRUE), 
                            T_epi = tempEpi_av, T_hypo = tempHypo_av,
                            z0 = metaDepth_av, k = k))) %>% 
  
  ungroup()
# View(sort_env_Tprof_DAY)
length(sort_env_Tprof_DAY$month_bin %>% unique())

# View(sort_env_Tprof_DAY)
sort_env_Tprof_DAY_long <- sort_env_Tprof_DAY %>%
  group_by(lakeClass, AreaGroup, month_bin) %>%
  mutate(depth = list(sort(seq(lakeDepth_av, 0, 0.5), decreasing = TRUE))) %>%  # depth for each T_prof
  unnest(c(T_prof, depth)) 
# View(sort_env_Tprof_DAY_long)

all_days <- sort(unique(sort_env_Tprof_DAY_long$month_bin))
mid_idx <- round(quantile(1:length(all_days), probs = c(0.25, 0.5, 0.75)))
q25 <- c(mid_idx[1]-1, mid_idx[1], mid_idx[1]+1)
mid <- c(mid_idx[2]-1, mid_idx[2], mid_idx[2]+1)
q75 <-  c(mid_idx[3]-1, mid_idx[3], mid_idx[3]+1)
show_days <- as.character(c(head(all_days, 3), all_days[q25], 
                            all_days[mid], all_days[q75], tail(all_days, 3)))

p_Tprofile_DAY <- ggplot(sort_env_Tprof_DAY_long, aes(x = T_prof, y = depth, 
                                                      color = factor(month_bin))) +
  geom_line(linewidth = 1) +
  
  # scale_x_reverse( breaks = c(-5.0, -3.0, -1.5, -0.5),  limits = c(0, -5)) + # limits = c(0, -5),
  facet_wrap(AreaGroup ~lakeClass, ncol = 3) +
  theme_bw() +
  
  labs(title = paste("monthly mean Temperature Profiles \n(Days 1-365 summairsed in 30 day steps)" ),
       x =  "mean Temperature [°C]",
       y = "Depth [m]",
       color = "approx. Months")  +
  #scale_color_discrete(breaks = show_days) +   # << show only selected days
  theme(legend.position = "bottom") + guides(color = guide_legend(nrow = 1))


p_Tprofile_DAY
ggsave(file.path(save_figures5, "Tprof_perAproxMonth.png"), p_Tprofile_DAY, height = 10, width = 10)

# -----------------------------------------------------
# spec 100

sort_res100 <- res100[res100$biomass > 0, ] %>%
  group_by(lakeClass, speciesGroup, AreaGroup) %>%
  mutate(lakeDepth = mean(lakeDepth)) %>% ungroup() %>%
  group_by(lakeClass, speciesGroup, AreaGroup, depth, day) %>%
  summarise(
    biomass_mean = mean(biomass), 
    numberInd_mean = mean(numberInd),
    indWeight_mean = mean(indWeight),
    height_mean = mean(height),
    lakeDepth_mean = mean(lakeDepth)
  ) %>% ungroup()  %>%
  mutate(AreaGroup = factor(AreaGroup,
                            levels = c("very.small", "small", "medium", "large", "very.large"))) 

View(sort_res100)
# sepc 5
sort_res5 <- res5[res5$biomass > 0, ] %>%
  group_by(lakeClass, speciesGroup, AreaGroup, depth, day) %>%
  summarise(
    biomass_mean = mean(biomass), 
    numberInd_mean = mean(numberInd),
    indWeight_mean = mean(indWeight),
    height_mean = mean(height),
    lakeDepth_mean = mean(lakeDepth)
  ) %>% ungroup()  %>%
  mutate(AreaGroup = factor(AreaGroup,
                            levels = c("very.small", "small", "medium", "large", "very.large")))
View(sort_res5)

# ---- boxplot per dephts ------------------------------------------------------------------------------------------
maxDay <- max(unique(sort_res100$day))
minDay <- min(unique(sort_res100$day))

p_box <- ggplot(sort_res100, aes(x = factor(depth, levels = rev(sort(unique(depth)))), 
                              y = biomass_mean, fill = AreaGroup)) +
  geom_boxplot() +
  facet_grid(speciesGroup ~ lakeClass ) +
  theme_bw() +
  labs(title = paste("Biomass over Active Days, days", minDay, "to", maxDay),
       y = "Mean Biomass", x = "Depths [m]", fill = "Turbidity")+
  scale_fill_brewer(palette = "Accent") 
p_box

maxDay <- max(unique(sort_res5$day))
minDay <- min(unique(sort_res5$day))
p_box <- ggplot(sort_res5, aes(x = factor(depth, levels = rev(sort(unique(depth)))), 
                                 y = biomass_mean, fill = AreaGroup)) +
  geom_boxplot() +
  facet_grid(speciesGroup ~ lakeClass ) +
  theme_bw() +
  labs(title = paste("Biomass over Active Days, days", minDay, "to", maxDay),
       y = "Mean Biomass", x = "Depths [m]", fill = "Turbidity")+
  scale_fill_brewer(palette = "Accent") 
p_box
ggsave(file.path(save_figures, "BOX_biomass_day.png"), p_box, height = 20, width = 15)

#################################################################
# inspect final restults --------------------------------------------

save_base_Tprofile <- "output-analysis/dep10_300spec_base_Tprofile_final"

sort_res <- readRDS(file.path(save_base_Tprofile, "sortRES.rds"))
sort_res <- as.data.table(sort_res)
sort_env <- readRDS(file.path(save_base_Tprofile, "sortENV.rds"))
sort_env <- as.data.table(sort_env)
View(sort_res)

# print nrow where biomass > 0
specgroup <- unique(sort_res$speciesGroup)
for(i in 1:3){
  oli <- sort_res %>% filter(speciesGroup == specgroup[i])
  message(specgroup[i], ":",nrow(oli))
}

#
save_comparison <- "output-analysis/comparison/final_TprofileVSTsteady_sortRES"
dir.create(save_comparison)

base_Tprofile <- "output/dep10_300spec_base_Tprofile_final"
base_Tsteady <- "output/dep300_5spec_base_Tsteady_final"

res1 <- readRDS(file.path(base_Tprofile, "added_all_res.rds")) 
res2 <- readRDS(file.path(base_Tsteady, "added_all_res.rds"))
View(res1)

data <- func_dataprep_compare_DDG(res1, res2, name1 = "base_Tprofile", name2 = "base_Tsteady", save_comparison) # in func_data_prep.R

View(data$res1_prep) # depth bennenung ist umgekehrt 

d <- c(-0.5, -1.5, -3, -5)
dense_rank(d)
# prepare base T_profile data
res1_prep <- res1 %>%
  group_by(lakeClass, speciesGroup, depth, lakeID, speciesID) %>%
  summarise(biomass = sum(biomass)) %>%  ungroup() %>%
  mutate(biomass_orig = biomass) %>%
  mutate(depth_label = paste0("depth_", dense_rank(abs(depth)))) %>% # abs hier wichtig
  pivot_wider(
    names_from = depth_label,
    values_from = biomass
  ) %>%  relocate(biomass_orig) %>%
  
  # Replace NA in all pivoted columns with 0
  replace_na(list(
    depth_1 = 0, # -0.5
    depth_2 = 0,
    depth_3 = 0,
    depth_4 = 0
  )) %>% 
  mutate(Biomass_cat = if_else(biomass_orig > 0, 1, 0, missing = 0))# %>% filter(biomass_orig!= 0)


oli <- res1_prep %>% filter(speciesGroup == "oligotrophentic")  %>% filter(Biomass_cat > 0)
sum(oli$Biomass_cat)
View(res1_prep)


# --------------------
base_Tprofile <- "output/dep10_300spec_base_Tprofile_final"
res <- readRDS(file.path(base_Tprofile, "all_res.rds"))
res <- as.data.table(res)
res1 <- readRDS(file.path(base_Tprofile, "added_all_res.rds"))

colnames(res1)

res_oli <- res %>% filter(speciesID > 14000 & speciesID < 14301) %>% filter(biomass > 0) 
nrow(res_oli)

res_oli <- res1 %>% filter(speciesID > 14000 & speciesID < 14301) %>% filter(biomass > 0) 
nrow(res_oli)

res_oli <- res1 %>% filter(speciesGroup == "oligotrophentic") %>% filter(biomass > 0) 
nrow(res_oli)

res_oli <- res1 %>% filter(speciesGroup == "oligotrophentic") %>% filter(biomass > 0) %>%
  group_by(lakeClass, speciesGroup, AreaGroup, depth, lakeID, speciesID) %>%
  summarise(
    biomass_mean = mean(biomass)) %>% ungroup()
nrow(res_oli)
