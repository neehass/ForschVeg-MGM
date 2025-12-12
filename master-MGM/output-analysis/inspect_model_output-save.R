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

load(file.path(save_figures5, "env_dep10_Tprofile.RData"))   
env5 <- env

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
