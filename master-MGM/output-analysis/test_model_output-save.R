# compare Tprofile spec100 und spec5 - different data saving
base_Tprofile <- "output/dep10_lakes_100spec_base_Tprofile"
save_figures100 <- "output-analysis/dep10_lakes_100spec_base_Tprofile"
load(file.path(save_figures100, "res_dep10_Tprofile.RData"))   
res100 <- res
View(res100)
load(file.path(save_figures100, "env_dep10_Tprofile.RData"))   
env100 <- env

save_figures5 <- "output-analysis/dep10_lakes_5spec_base_Tprofile_parallel/parallel_func"
load(file.path(save_figures5, "res_dep10.RData")) 
res5 <- res
load(file.path(save_figures5, "env_dep10.RData"))   
env5 <- env

rm(res)
rm(env)

View(res5)

# 100 spec
sort_env100 <- env100 %>%
  group_by(lakeClass, AreaGroup) %>%
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
  group_by(lakeClass, AreaGroup) %>%
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
  group_by(lakeClass, speciesGroup, AreaGroup, depth) %>%
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