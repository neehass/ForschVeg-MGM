###### Code to prepare all datasets from raw-da


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(data.table)




# Prepare MGM output base scenario ----------------------------------------

# Read in
data_raw <-data.table::fread("data-raw/MGMoutput/220303_random300grouped_reallakes_V4exp_300grouped_all_V4.csv")

# Clean
data <- data_raw %>%
  rename(Species = specName) %>%
  mutate(Biomass=depth_1+depth_2+depth_3+depth_4) %>% # REEVALUATE
  ungroup() %>%
  mutate(Biomass_cat = ifelse(Biomass == 0, 0, 1)) %>%
  rename(Lake = lakeName) %>% select(-"V1") %>%
  filter(Lake != "lake_11")

# Extract environmental parameters of lakes
data_lakes_env <-
  data %>% group_by(Lake) %>% select(
    "Lake",
    "latitude",
    "maxNutrient",
    "maxTemp",
    "maxKd"
  ) %>% unique() %>%
  select(where( ~ n_distinct(.) > 1))


# Extract parameters of species
data_spec_para <- data %>% group_by(Species) %>%
  select(
    -"Lake",
    -"lakeNr",-"latitude",
    -"Name",-"maxNutrient",
    -"maxW",
    -"minW",
    -"maxTemp",-"tempDelay",
    -"maxKd",
    -"minKd",
    -"levelCorrection",-"depth_1",
    -"depth_2",
    -"depth_3",
    -"depth_4",
    -"Biomass",
    -"Biomass_cat"
  ) %>%
  unique() %>%
  select(where( ~ n_distinct(.) > 1))

usethis::use_data(data, overwrite = TRUE)
usethis::use_data(data_lakes_env, overwrite = TRUE)
usethis::use_data(data_spec_para, overwrite = TRUE)



# Preprocess observed species distribution --------------------------------

# Read in raw data
MacrophData_raw<-fread("data-raw/observed/Makrophyten_WRRL_05-17_nurMakrophytes.csv")

# Filter data For complete datasets
MacrophData <- MacrophData_raw %>%
  filter(!(Gewaesser=="Chiemsee" & (YEAR==2011))) %>%
  filter(!(Gewaesser=="Chiemsee" & YEAR==2012)) %>% # 1 plot per year -> wrong
  filter(!(Gewaesser=="Chiemsee" & (YEAR==2014))) %>%
  filter(!(Gewaesser=="Chiemsee" & (YEAR==2015))) %>%
  filter(!(Gewaesser=="Chiemsee" & (YEAR==2017))) %>%
  filter(!(Gewaesser=="Staffelsee - Sued" & (YEAR==2012))) %>%
  filter(!(Gewaesser=="Gr. Alpsee" & (YEAR==2012))) %>%
  filter(!(Gewaesser=="Gr. Alpsee" & (YEAR==2013))) %>%
  filter(!(Gewaesser=="Pilsensee" & (YEAR==2015))) %>%
  filter(!(Gewaesser=="Langbuergner See" & (YEAR==2014))) %>%
  filter(!(Gewaesser=="Pelhamer See" & (YEAR==2017))) %>%
  filter(!(Gewaesser=="Weitsee" & (YEAR==2017))) %>%
  distinct()

# Rename depth
MacrophData$Probestelle <- plyr::revalue(MacrophData$Probestelle, c("0-1 m"="0-1", "1-2 m"="1-2", "2-4 m"="2-4",">4 m"="4-x" ))

# Selection of species that were determined until species level
MacrophData <- MacrophData %>%
  filter(str_detect(Taxon, " ")) %>%
  filter(Taxon != "Ranunculus, aquatisch")

# Reduce columns
MacrophData <- MacrophData %>%
  select(-`Mst.-Name`,-`DV-Nr.`,-Dimension, -CF, -LAKE_TYPE_SIMPLIFIED, -LAKE_TYPE2, -LAKE_TYPE)


# Create Dataset with all possible PLOTS
Makroph_dataset <- MacrophData %>% group_by(Gewaesser, MST_NR, YEAR) %>%
  select(Gewaesser, MST_NR, YEAR) %>% #3590 * Gew?sser, MST_NR, YEAR, Probestelle IIII 1013 *4 => 4052 m?sstens eigentlich mind sein
  distinct()
Probestelle <- tibble(Probestelle=c("4-x","0-1","1-2", "2-4")) # tibble(Probestelle)
Makroph_dataset <- merge(Makroph_dataset, Probestelle, by=NULL) #996 * 4 = 3984

# Select for submerged species
Makroph_comm_S2 <- MacrophData %>% #group_by(Gewässer, MST_NR, DATUM, Probestelle, Taxon) %>%
  filter(Form=="S") %>%
  filter(Erscheinungsform!="Bryophyta" ) %>%
  filter(Erscheinungsform!="Pteridophyta" ) %>%
  group_by(Gewaesser, MST_NR, Probestelle, YEAR, Taxon) %>%
  summarise(Messwert = mean(Messwert)) %>% #get rid of double values for DATUM
  select(Gewaesser, MST_NR, YEAR, Probestelle, Taxon, Messwert) %>%  #duplicated %>% which %>% #check for duplications
  spread(Taxon, Messwert)%>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
  select_if(~sum(!is.na(.)) > 0)

# Fill up dataset with all possible plots to have also zero values
Makroph_comm_S <-  right_join(Makroph_comm_S2,
                              Makroph_dataset, by=c("Gewaesser", "MST_NR", "YEAR", "Probestelle"))
Makroph_comm_S$Tiefe <- plyr::revalue(Makroph_comm_S$Probestelle, c("0-1"="-0.5", "1-2"="-1.5", "2-4"="-3","4-x"="-5"))
Makroph_comm_S<-Makroph_comm_S%>%
  mutate(Tiefe=as.numeric(Tiefe))%>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0))%>%
  rename(Lake=Gewaesser, Depth=Tiefe)

# Calculate Gamma richness per depth
Makroph_commLastYear <- Makroph_comm_S %>%
  group_by(Lake, YEAR,Depth) %>%
  summarise_at(vars(-"MST_NR", -"Probestelle"), mean, na.rm=T)%>%
  ungroup() %>%
  group_by(Lake) %>%
  #filter(YEAR==max(YEAR))%>%
  select(where(~ any(. != 0)))

# Save output
usethis::use_data(Makroph_comm_S, overwrite = TRUE)
usethis::use_data(Makroph_commLastYear, overwrite = TRUE)


# Select for emerg / floating-leaf species
Makroph_comm_EFSB <- MacrophData %>% #group_by(Gewässer, MST_NR, DATUM, Probestelle, Taxon) %>%
  filter(Form %in% c("Em","F-SB")) %>%
  filter(Erscheinungsform!="Bryophyta" ) %>%
  filter(Erscheinungsform!="Pteridophyta" ) %>%
  group_by(Gewaesser, MST_NR, Probestelle, YEAR, Taxon) %>%
  summarise(Messwert = mean(Messwert)) %>% #get rid of double values for DATUM
  select(Gewaesser, MST_NR, YEAR, Probestelle, Taxon, Messwert) %>%  #duplicated %>% which %>% #check for duplications
  spread(Taxon, Messwert)%>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
  select_if(~sum(!is.na(.)) > 0)

# Fill up dataset with all possible plots to have also zero values
Makroph_comm_EFSB <-  right_join(Makroph_comm_EFSB,
                              Makroph_dataset, by=c("Gewaesser", "MST_NR", "YEAR", "Probestelle"))
Makroph_comm_EFSB$Tiefe <- plyr::revalue(Makroph_comm_EFSB$Probestelle, c("0-1"="-0.5", "1-2"="-1.5", "2-4"="-3","4-x"="-5"))
Makroph_comm_EFSB<-Makroph_comm_EFSB%>%
  mutate(Tiefe=as.numeric(Tiefe))%>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0))%>%
  rename(Lake=Gewaesser, Depth=Tiefe)

# Calculate Gamma richness per depth
Makroph_comm_EFSBLastYear <- Makroph_comm_EFSB %>%
  group_by(Lake, YEAR,Depth) %>%
  summarise_at(vars(-"MST_NR", -"Probestelle"), mean, na.rm=T)%>%
  ungroup() %>%
  group_by(Lake) %>%
  #filter(YEAR==max(YEAR))%>%
  select(where(~ any(. != 0)))

usethis::use_data(Makroph_comm_EFSB, overwrite = TRUE)
usethis::use_data(Makroph_comm_EFSBLastYear, overwrite = TRUE)




# Species groups ----------------------------------------------------------

GroupsSpecies <- fread("data-raw/observed/Indexmakrophyten_Gruppenzuordnung.csv")

GroupsSpecies$Taxon <- gsub(" ", ".", GroupsSpecies$Taxon)
usethis::use_data(GroupsSpecies, overwrite = TRUE)



# Lake morphology ---------------------------------------------------------

load("data-raw/observed/Morphology.rda")
usethis::use_data(Morphology, overwrite = TRUE)




# Scenario data -----------------------------------------------------------

# Read in
scenariofolder1<-"/data-raw/MGMoutput/scenarios_V4_first50each/"
scenariofolder2<-"/data-raw/MGMoutput/scenarios_V4_Rest/"

filenames1 <- list.files(path=paste0(here::here(),scenariofolder1), pattern="*.txt", full.names=TRUE)
filenames2 <- list.files(path=paste0(here::here(),scenariofolder2), pattern="*.txt", full.names=TRUE)
filenames<-cbind(filenames1,filenames2)

scenario_data <- read.table(filenames[1], header = T)

for (f in 2:length(filenames)){
  scenario_data <- rbind(scenario_data, read.table(filenames[f], header = T))
}

scenario_data <- scenario_data %>% filter(lakeID!=11)
usethis::use_data(scenario_data, overwrite = TRUE)


# Preprocess scenario data
# all_diff_tobase <- scenario_data %>%
#   group_by(speciesID,lakeID,variable)%>%
#   unique() %>% # ich weiß nicht warum
#   spread(scenario, value) %>%
#   mutate(
#     S0_m1minusBase = S0_m1 - base,
#     S0_1minusBase = S0_1 - base,
#     S0_2minusBase = S0_2 - base,
#     S1_m1minusBase = S1_m1 - base,
#     S1_0minusBase = S1_0 - base,
#     S1_1minusBase = S1_1 - base,
#     S1_2minusBase = S1_2 - base,
#     S2_m1minusBase = S2_m1 - base,
#     S2_0minusBase = S2_0 - base,
#     S2_1minusBase = S2_1 - base,
#     S2_2minusBase = S2_2 - base
#   )
# usethis::use_data(all_diff_tobase, overwrite = TRUE)


all_diff_presabs_tobase <- scenario_data %>%
  mutate_at(vars(value), ~ 1 * (. != 0)) %>%
  group_by(speciesID,lakeID,variable)%>%
  unique() %>%
  spread(scenario, value) %>%
  mutate(
    S0_m1minusBase = S0_m1 - base,
    S0_1minusBase = S0_1 - base,
    S0_2minusBase = S0_2 - base,
    S1_m1minusBase = S1_m1 - base,
    S1_0minusBase = S1_0 - base,
    S1_1minusBase = S1_1 - base,
    S1_2minusBase = S1_2 - base,
    S2_m1minusBase = S2_m1 - base,
    S2_0minusBase = S2_0 - base,
    S2_1minusBase = S2_1 - base,
    S2_2minusBase = S2_2 - base
  )
usethis::use_data(all_diff_presabs_tobase, overwrite = TRUE)


# all_diff_presabs_TempEffect <- scenario_data %>%
#   mutate_at(vars(value), ~ 1 * (. != 0)) %>%
#   group_by(speciesID,lakeID,variable)%>%
#   unique() %>% # ich weiß nicht warum
#   spread(scenario, value) %>%
#   mutate(
#     S1_m1minS0_m1 = S1_m1 - S0_m1,
#     S1_0minBase = S1_0 - base,
#     S1_1minS0_1 = S1_1 - S0_1,
#     S1_2minS0_2 = S1_2 - S0_2,
#     S2_m1minS1_m1 = S2_m1 - S1_m1,
#     S2_0minS1_0 = S2_0 - S1_0,
#     S2_1minS1_1 = S2_1 - S1_1,
#     S2_2minS1_2 = S2_2 - S1_2
#   ) %>%
#   select(speciesID,lakeID,variable,S1_0minBase,S1_m1minS0_m1,S1_1minS0_1,S1_2minS0_2,
#          S2_m1minS1_m1,S2_0minS1_0,S2_1minS1_1,S2_2minS1_2)
# usethis::use_data(all_diff_presabs_TempEffect, overwrite = TRUE)


# all_diff_prsabs_TurbEffect <- scenario_data %>%
#   mutate_at(vars(value), ~ 1 * (. != 0)) %>%
#   group_by(speciesID,lakeID,variable)%>%
#   unique() %>% # ich weiß nicht warum
#   spread(scenario, value) %>%
#   mutate(
#     S0_m1minBase = S0_m1 - base,
#     S0_1minBase = S0_1 - base,
#     S0_2minBase = S0_2 - base,
#     S1_m1minS1_0 = S1_m1 - S1_0,
#     S1_1minS1_0 = S1_1 - S1_0,
#     S1_2minS1_0 = S1_2 - S1_0,
#     S2_m1minS2_0 = S2_m1 - S2_0,
#     S2_1minS2_0 = S2_1 - S2_0,
#     S2_2minS2_0 = S2_2 - S2_0
#   )
# usethis::use_data(all_diff_prsabs_TurbEffect, overwrite = TRUE)


# Process Nutrient / Turbidity scenarios
scenTurbNutr_diff_presabs_toZero <- scenario_data %>%
  mutate_at(vars(value), ~ 1 * (. != 0)) %>%
  group_by(speciesID,lakeID,variable)%>%
  unique() %>% # ich weiß nicht warum
  spread(scenario, value) %>%
  mutate(
    S2_m1minusS2_0 = S2_m1 - S2_0,
    S2_N0_Tm1minusS2_0 = S2_N0_Tm1 - S2_0,
    S2_N1_Tm1minusS2_0 = S2_N1_Tm1 - S2_0,
    S2_Nm1_T0minusS2_0 = S2_Nm1_T0 - S2_0,
    S2_N1_T0minusS2_0 = S2_N1_T0 - S2_0,
    S2_Nm1_T1minusS2_0 = S2_Nm1_T1 - S2_0,
    S2_N0_T1minusS2_0 = S2_N0_T1 - S2_0,
    S2_1minusS2_0 = S2_1 - S2_0
  )
usethis::use_data(scenTurbNutr_diff_presabs_toZero, overwrite = TRUE)



# Observed gamma richness -------------------------------------------------

SPlength <- length(Makroph_commLastYear)-3
Makroph_commLastYear$GAMMA <- vegan::specnumber(Makroph_commLastYear[,c(4:85)])

Makroph_commLastYearfin<-Makroph_commLastYear %>% # %>%
  filter(!Lake %in% c("Altmuehlsee", "Drachensee", "Eixendorfer See",
                      "Grosser Brombachsee",
                      "Gruentensee","Igelsbachsee",
                      "Kleiner Brombachsee",
                      "Liebensteinspeicher","Rottachsee",
                      "Steinberger See","Untreusee",
                      "Walchensee","Murnersee","Rothsee",
                      "Seehamer See"))%>%
  select(where(~ any(. != 0)))

MAK_MAPPED_NSPEC <- Makroph_commLastYearfin %>%
  select(-Lake, -YEAR, -Depth, -GAMMA) %>% ncol()

MAK_MAPPED<-Makroph_commLastYearfin %>%
  #rename(Lake=Gewässer, Depth=Tiefe) %>% #%>%  spread(Depth, GAMMA)
  select(Lake, YEAR, Depth, GAMMA) %>%
  mutate(GAMMAperc = GAMMA/MAK_MAPPED_NSPEC*100)

MAK_MAPPED$LakeID <- MAK_MAPPED  %>% group_indices(Lake)

MAK_MAPPED <- MAK_MAPPED %>% mutate(LakeID=paste0("lake_",LakeID))

fulllakenames <- MAK_MAPPED %>% select(Lake,LakeID) %>% unique() %>%
  filter(LakeID!="lake_11")

usethis::use_data(fulllakenames, overwrite = TRUE)
usethis::use_data(MAK_MAPPED, overwrite = TRUE)

# MAK_MAPPED_di<-Makroph_commLastYearfin %>%
#   group_by(Lake) %>%
#   summarise_at(vars(Callitriche.cophocarpa:Zannichellia.palustris), mean, na.rm = TRUE)
#
# MAK_MAPPED_di$GAMMA <- vegan::specnumber(MAK_MAPPED_di[,c(2:72)])
#
# MAK_MAPPED_din<-MAK_MAPPED_di %>%
#   select(Lake,GAMMA)%>%
#   mutate(GAMMAperc = GAMMA/MAK_MAPPED_NSPEC*100)
#
#
# MAK_MAPPED_din$LakeID <- MAK_MAPPED_din  %>% group_indices(Lake)
#
# MAK_MAPPED_din <- MAK_MAPPED_din %>% mutate(LakeID=paste0("lake_",LakeID))

# Observed gamma richness EFSB -------------------------------------------------

SPlengthEFSB <- length(Makroph_comm_EFSBLastYear)-3
Makroph_comm_EFSBLastYear$GAMMA <- vegan::specnumber(Makroph_comm_EFSBLastYear[,c(4:55)])

Makroph_comm_EFSBLastYearfin<-Makroph_comm_EFSBLastYear %>% # %>%
  filter(!Lake %in% c("Altmuehlsee", "Drachensee", "Eixendorfer See",
                      "Grosser Brombachsee",
                      "Gruentensee","Igelsbachsee",
                      "Kleiner Brombachsee",
                      "Liebensteinspeicher","Rottachsee",
                      "Steinberger See","Untreusee",
                      "Walchensee","Murnersee","Rothsee",
                      "Seehamer See"))%>%
  select(where(~ any(. != 0)))

MAK_MAPPED_NSPEC_EFSB <- Makroph_comm_EFSBLastYearfin %>%
  select(-Lake, -YEAR, -Depth, -GAMMA) %>% ncol()

MAK_MAPPED_EFSB<-Makroph_comm_EFSBLastYearfin %>%
  #rename(Lake=Gewässer, Depth=Tiefe) %>% #%>%  spread(Depth, GAMMA)
  select(Lake, YEAR, Depth, GAMMA) %>%
  mutate(GAMMAperc = GAMMA/MAK_MAPPED_NSPEC_EFSB*100)

MAK_MAPPED_EFSB$LakeID <- MAK_MAPPED_EFSB  %>% group_indices(Lake)

MAK_MAPPED_EFSB <- MAK_MAPPED_EFSB %>% mutate(LakeID=paste0("lake_",LakeID))

fulllakenames_EFSB <- MAK_MAPPED_EFSB %>% select(Lake,LakeID) %>% unique() %>%
  filter(LakeID!="lake_11")

#usethis::use_data(fulllakenames, overwrite = TRUE)
#usethis::use_data(MAK_MAPPED_EFSB, overwrite = TRUE)



MAK_MAPPED_ALL <- left_join(MAK_MAPPED_EFSB, MAK_MAPPED, by=join_by(Lake, Depth, LakeID, YEAR)) %>%
  rename("GAMMA_EFSB"="GAMMA.x",
         "GAMMAperc_EFSB"="GAMMAperc.x",
         "GAMMA_S"="GAMMA.y",
         "GAMMAperc_S"="GAMMAperc.y")

usethis::use_data(MAK_MAPPED_ALL, overwrite = TRUE)




## Gamma richness per indicator group
MAK_MAPPED_NSPEC_indicationspec <- Makroph_comm_S %>%
  gather("Species", "Kohler",5:89) %>%
  mutate(Species = str_replace(Species, " ", "."))%>%
  left_join(GroupsSpecies, by = c("Species"="Taxon")) %>%
  dplyr::select(-Trophie,-Typ) %>%
  mutate(Gruppe = ifelse(is.na(Gruppe),"none", Gruppe)) %>%

  group_by(Lake, YEAR,Depth, Species, Gruppe) %>%
  summarise_at(vars(-"MST_NR", -"Probestelle"), mean, na.rm=T)%>% # Mean of Kohler per Lake, YEAR, Depth, Species and Group
  ungroup() %>%
  group_by(Lake) %>%
  filter(YEAR==max(YEAR))%>% #select last mapping per lake
  filter(!Lake %in% c("Altmuehlsee", "Drachensee", "Eixendorfer See",
                      "Grosser Brombachsee",
                      "Gruentensee","Igelsbachsee", "Kleiner Brombachsee",
                      "Liebensteinspeicher","Rottachsee",
                      "Steinberger See",
                      "Untreusee","Walchensee","Murnersee","Rothsee",
                      "Seehamer See")) %>%
  filter(Gruppe!="none") %>% ungroup() %>%
  filter(Kohler!=0) %>%
  dplyr::select(Species) %>% unique() %>% count()
usethis::use_data(MAK_MAPPED_NSPEC_indicationspec, overwrite = TRUE)

MAK_grouped <- Makroph_comm_S %>% gather("Species", "Kohler",5:89) %>%
  mutate(Species = str_replace(Species, " ", "."))%>%
  left_join(GroupsSpecies, by = c("Species"="Taxon")) %>% select(-Trophie,-Typ) %>%
  mutate(Gruppe = ifelse(is.na(Gruppe),"none", Gruppe)) %>%
  group_by(Lake, YEAR,Depth, Species, Gruppe) %>%
  summarise_at(vars(-"MST_NR", -"Probestelle"), mean, na.rm=T)%>% # Mean of Kohler per Lake, YEAR, Depth, Species and Group
  ungroup() %>%
  group_by(Lake) %>%
  filter(YEAR==max(YEAR))%>% #select last mapping per lake
  ungroup() %>% group_by(Lake, Depth, Gruppe) %>%
  mutate(Kohler=ifelse(Kohler>0,1,0)) %>%
  #select(where(~ any(. != 0))) %>%
  summarise(NSPEC=sum(Kohler)) %>%
  filter(!Lake %in% c("Altmuehlsee", "Drachensee", "Eixendorfer See",
                      "Grosser Brombachsee",
                      "Gruentensee","Igelsbachsee", "Kleiner Brombachsee",
                      "Liebensteinspeicher","Rottachsee",
                      "Steinberger See",
                      "Untreusee","Walchensee","Murnersee","Rothsee",
                      "Seehamer See"))%>%
  rename(Group=Gruppe)%>%
  mutate(NSPECperc = NSPEC/MAK_MAPPED_NSPEC_indicationspec$n*100)

MAK_grouped$LakeID <- MAK_grouped  %>% group_indices(Lake)

MAK_mapped_grouped <- MAK_grouped %>% filter(LakeID!=11) %>%
  mutate(LakeID=paste0("lake_",LakeID))

## Depth independent
MAK_grouped_depthindep <- Makroph_comm_S %>% gather("Species", "Kohler",5:89) %>%
  mutate(Species = str_replace(Species, " ", "."))%>%
  left_join(GroupsSpecies, by = c("Species"="Taxon")) %>%
  select(-Trophie,-Typ) %>%
  mutate(Gruppe = ifelse(is.na(Gruppe),"none", Gruppe)) %>%
  group_by(Lake, YEAR,Species, Gruppe) %>%
  summarise_at(vars(-"MST_NR", -"Probestelle"), mean, na.rm=T)%>% # Mean of Kohler per Lake, YEAR, Depth, Species and Group
  ungroup() %>%
  group_by(Lake) %>%
  filter(YEAR==max(YEAR))%>% #select last mapping per lake
  ungroup() %>% group_by(Lake, Gruppe) %>%
  mutate(Kohler=ifelse(Kohler>0,1,0)) %>%
  #select(where(~ any(. != 0))) %>%
  summarise(NSPEC=sum(Kohler)) %>%
  filter(!Lake %in% c("Altmuehlsee", "Drachensee", "Eixendorfer See",
                      "Grosser Brombachsee",
                      "Gruentensee","Igelsbachsee", "Kleiner Brombachsee",
                      "Liebensteinspeicher","Rottachsee",
                      "Steinberger See",
                      "Untreusee","Walchensee","Murnersee","Rothsee",
                      "Seehamer See"))%>%
  rename(Group=Gruppe)%>%
  mutate(NSPECperc = NSPEC/MAK_MAPPED_NSPEC_indicationspec$n*100)

MAK_grouped_depthindep$LakeID <- MAK_grouped_depthindep  %>% group_indices(Lake)

MAK_mapped_depthind_grouped <- MAK_grouped_depthindep %>%
  filter(LakeID!=11) %>%
  mutate(LakeID=paste0("lake_",LakeID))

## Depth independent and alle lakes
MAK_grouped_depthindep_alllakes <- Makroph_comm_S %>%
  gather("Species", "Kohler",5:89)%>%
  mutate(Species = str_replace(Species, " ", ".")) %>%
  left_join(GroupsSpecies, by = c("Species"="Taxon")) %>%
  select(-Trophie,-Typ, -Depth) %>%
  mutate(Gruppe = ifelse(is.na(Gruppe),"none", Gruppe)) %>%

  filter(!Lake %in% c("Altmuehlsee", "Drachensee", "Eixendorfer See",
                      "Grosser Brombachsee",
                      "Gruentensee","Igelsbachsee", "Kleiner Brombachsee",
                      "Liebensteinspeicher","Rottachsee",
                      "Steinberger See","Hofstaetter See",
                      "Untreusee","Walchensee","Murnersee","Rothsee",
                      "Seehamer See"))%>%
  rename(Group=Gruppe)%>%
  ungroup() %>%
  group_by(Lake) %>%
  filter(YEAR==max(YEAR))%>% #select last mapping per lake
  ungroup() %>%
  group_by(Species, Group) %>%
  summarise_at(vars("Kohler"), sum, na.rm=T)%>% # Mean of Kohler per Lake, YEAR, Depth, Species and Group
  ungroup()%>%
  group_by(Group) %>%
  mutate(Kohler=ifelse(Kohler>0,1,0)) %>%
  #select(where(~ any(. != 0))) %>%
  #filter(Kohler!=0)%>%
  summarise(NSPEC=sum(Kohler)) %>%
  mutate(NSPECperc = NSPEC/MAK_MAPPED_NSPEC_indicationspec$n*100)

usethis::use_data(MAK_grouped_depthindep_alllakes, overwrite = TRUE)
usethis::use_data(MAK_grouped_depthindep, overwrite = TRUE)
usethis::use_data(MAK_mapped_grouped, overwrite = TRUE)


# Lake clustering ---------------------------------------------------------

data_lakes_envscale<-scale(data_lakes_env[,-1] )

distance = dist(data_lakes_envscale)
mydata.hclust = hclust(distance, method = "ward.D2")

#plot(mydata.hclust)
member = cutree(mydata.hclust,3)
data_lakes_env_class=cbind(data_lakes_env,member) %>% rename("class"="...6") %>%
  left_join(fulllakenames, by=c("Lake"="LakeID")) %>%
  mutate(Lake=as.numeric(str_remove(Lake,"lake_"))) %>%
  rename(LakeName=Lake.y) %>%
  mutate(class=ifelse(class==1,"turb",
                      ifelse(class==2,"clear",
                             ifelse(class==3,"medium","NA")))) %>%
  ungroup()



usethis::use_data(data_lakes_env_class, overwrite = TRUE)
