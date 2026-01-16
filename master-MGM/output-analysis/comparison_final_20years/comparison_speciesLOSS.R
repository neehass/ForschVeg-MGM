# Comparison Species LOSS
# as in analysis.Rmd
# load data from data_prep_comparison.R

# packages & functions
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(patchwork)

# home
dir <- "C:/Users/maiim/Documents/25-25SS/Forschungsprojekt_Vegetationskunde/scripte-data-MGM/master-MGM"
lewSpec_dir <- "C:/Users/maiim/Documents/25-25SS/Forschungsprojekt_Vegetationskunde/scripte-data-MGM/LewerentzEtAl2023_ModelledMacrophyteSpeciesRichness-1.0"

# Workstation
dir <- "C:/Users/student/Documents/Neele-ForschVeg-2025/ForschVeg-MGM/master-MGM"
lewSpec_dir <- "C:/Users/student/Documents/Neele-ForschVeg-2025/ForschVeg-MGM/LewerentzEtAl2023_ModelledMacrophyteSpeciesRichness-1.0"

getwd()
setwd(dir)

# packages & functions

source("./experiment/help-func.r")
source("./output-analysis/func_data_prep.R")

# dir setup ---------------------------------------------------------------------------------
# Folder output of MGM experiment and Analysis results folder
save_comparison <- "output-analysis/comparison"
dir.create(save_comparison)

# input files
lake_path <- "input/lakes"

# load data --------------------------------------------------------------------------------
# load data prep in data_prep_comparison.R

# Presence/Absence data 
load(file.path(save_comparison, "res_combined_base.rda")) # res_combined base scenrario Tsteady and Tprofile 

# seperate data Tprofile and Tsteady base
load(file.path(save_comparison, "res_baseTprofile_prep.rda")) # res_baseTprofile_prep
load(file.path(save_comparison, "res_baseTSteady_prep.rda")) # res_baseTSteady_prep

# -------------------------------------------------------------------------------------------

# Number of lost species per scenario (%) -------------
NSPECbase <- res_combined %>% ungroup() %>% select(speciesID) %>% unique() %>% nrow()

SPEC_DEAD <- res_combined %>% 
  group_by(speciesID) %>% select(-lakeID,-depth, -speciesGroup, -lakeClass) %>%
  summarise_all(sum) %>% #Pro Art Summe aus Tiefen und Seen
  #filter(base>0) %>% #nur Arten, die schon vorkamen
  select(baseTP, baseTS) %>%
  summarise_all(list(~sum(. > 0, na.rm = TRUE))) %>% # N species pro Szen if present
  gather("scenario","Ntotspec", (1:(ncol(res_combined)-5))) %>% 
  mutate(Lost= Ntotspec - NSPECbase) %>% 
  mutate(Lost_percent = (Lost/NSPECbase)*100)%>%
  filter(scenario!= "baseTS") %>% 
  mutate(Temp=c(1)) %>%
  mutate(TurbN=c(0)) %>% 
  select(-Ntotspec,-Lost,-scenario) %>%
  spread(TurbN,Lost_percent) %>% 
  rename("lostsp(%)"="Temp")

head(SPEC_DEAD)

# ------------------------------------------------------------------------------------------
#  Nspec lost per functional type -----------------
load(file.path(lewSpec_dir, "data/scenario_data.rda"))
head(scenario_data)

Nspec_los_finc_type <- list()
types <- res_baseTP$speciesGroup %>% unique()
for(i in 1:3){
  print(types[i])
  NSPECbaseGroup <- res_combined %>% 
    filter(speciesGroup == types[i]) %>% ungroup() %>% select(speciesID) %>% unique() %>% nrow()
  
  SPEC_DEAD_group_oli <- res_combined %>% 
    filter(speciesGroup == types[i]) %>%
    group_by(speciesID) %>% select(-lakeID,-depth, -speciesGroup, -lakeClass) %>%
    summarise_all(sum) %>% filter(baseTP > 0) %>% select(baseTP, baseTS) %>%
    summarise_all(list(~sum(. > 0, na.rm = TRUE))) %>%
    gather("scenario","Ntotspec", (1:(ncol(res_combined)-5))) %>% 
    mutate(Lost= Ntotspec - NSPECbaseGroup) %>% 
    mutate(Lost_percent = (Lost/NSPECbaseGroup)*100) %>% 
    filter(scenario!="baseTP") %>% 
    mutate(Temp=c(0)) %>%
    mutate(TurbN=c(0)) %>% 
    select(-Ntotspec,-Lost,-scenario) %>%
    spread(TurbN,Lost_percent) %>% 
    rename("lostspOlig(%)"="Temp")
  
  Nspec_los_finc_type[[i]] <- SPEC_DEAD_group_oli
}
names(Nspec_los_finc_type) <- types
head(Nspec_los_finc_type)

# ------------------------------------------------------------------------------------------
# prep data for ploting ------------
res_comp_sceanrio <-res_combined %>% 
  left_join(data_lakes_env_class %>% select(Lake,class), by=c("lakeID"="Lake")) %>%
  mutate(Trophie = ifelse(speciesGroup == "oligotroph", "oligotraphentic",
                          ifelse(speciesGroup == "mesotroph", "mesotraphentic",
                                 ifelse(speciesGroup == "eutroph", "eutraphentic",NA))))%>%
  
  group_by(lakeID, depth, Trophie, class) %>% 
  summarise( #pro See und Tiefe Anzahl der Arten
    baseTP = sum(baseTP),
    test = sum(test),
    
  ) %>%
  mutate(
    TestMinusbaseTP = test - baseTP, 
    TestPlusbaseTP = test + baseTP
  )  %>% 
  ungroup() %>%
  group_by(depth,Trophie, class) %>% 
  select(-lakeID, -test) %>% summarise_all(list(mean=mean, sd=sd)) %>%
  gather("scenario", "NSpec", c(5:6, 8:9)) %>% # exclude base
  mutate(type=str_extract(scenario,"[^_]+$"),
         scenario=str_extract(scenario, "[^_]+")) %>%
  spread(type, NSpec) %>%
  mutate(scenarioTemp = ifelse(scenario=="baseTP", "Temp0", 
                               ifelse( scenario=="TestMinusbaseTP", "Temp1", 
                                       ifelse(scenario == "TestPlusbaseTP", "Temp1",NA)))) %>%
  mutate(scenarioTurbNut = ifelse(scenario=="baseTP" ,"TurbNut0", 
                                  ifelse( scenario=="TestMinusbaseTP", "TurbNut0", 
                                          ifelse(scenario == "TestPlusbaseTP", "TurbNut1",NA)))) 
head(res_comp_sceanrio) 
unique(res_comp_sceanrio$scenario)
# ------------------------------------------------------------------------------------------
# Per lake and depth   --------------------------------------------------------------------------------
### Per lake and depth 

#### Mean Number of species per lake (+sd) and scenario
# Colums: Turbidity and Nutrient change (-1=-25%;0=+0%;1=+25%;2=+50%)
# Rows: Temperature change(0=+0°C; 1=+1.5°C; 2=+3.0°C)

# Mean n of species per scenario ***
all_diffmean_lake <- res_combined %>%
  group_by(lakeID, speciesID) %>%
  summarise( #tiefenunabhängig Vorkommen in den Seen
    NspecXDepthsB_TP = sum(baseTP),
    NspecXDepthsB_TS = sum(baseTS)
  ) %>%
  mutate_at(vars(NspecXDepthsB_TP, NspecXDepthsB_TS), ~ 1 * (. != 0)) %>% # Pres/Abs
  ungroup() %>%
  group_by(lakeID) %>%
  summarise( #Arten pro See
    baseTP = sum(NspecXDepthsB_TP),
    baseTS = sum(NspecXDepthsB_TS)
  ) %>%
  gather("scenario", "NSpec", 2:3) %>%
  group_by(scenario)  
head(all_diffmean_lake)

all_diff_prsabsMEAN <-  all_diffmean_lake %>% summarise(NSpMean = mean(NSpec))%>%
  arrange(factor(scenario))
head(all_diff_prsabsMEAN)

all_diffSD_lake <- res_combined %>%
  group_by(lakeID, speciesID) %>%
  summarise(
    NspecXDepthsB_TP = sum(baseTP),
    NspecXDepthsB_TS = sum(baseTS)
  ) %>%
  mutate_at(vars(NspecXDepthsB_TP, NspecXDepthsB_TS), ~ 1 * (. != 0)) %>%
  ungroup() %>%
  group_by(lakeID) %>%
  summarise(
    baseTP = sum(NspecXDepthsB_TP),
    baseTS = sum(NspecXDepthsB_TS)
  ) %>%
  gather("scenario", "NSpec", 2:3) %>%
  group_by(scenario)  
head(all_diffSD_lake)

all_diff_prsabsSD <- all_diffSD_lake %>% 
  summarise(NSpMean = sd(NSpec))%>%
  arrange(factor(scenario))
head(all_diff_prsabsSD)
# ------------------------------------------------------------------------------------------
#### Mean Number of species per lake type and species group
# Mean n of species per scenario ***
all_diff_prsabsMEAN_func_type <- list()
all_diff_prsabsMEAN_func_type_lake <- list()
types <- res_baseTP$speciesGroup %>% unique()

for(i in 1:3){
  print(types[i])
  lake <- res_combined %>%
    filter(speciesGroup == types[i]) %>% 
    group_by(lakeID, speciesID) %>%
    summarise(
      NspecXDepthsB_TP = sum(baseTP),
      NspecXDepthsB_TS = sum(baseTS)
    ) %>%
    mutate_at(vars(NspecXDepthsB_TP, NspecXDepthsB_TS), ~ 1 * (. != 0)) %>%
    ungroup() %>%
    group_by(lakeID) %>%
    summarise(
      baseTP = sum(NspecXDepthsB_TP),
      baseTS = sum(NspecXDepthsB_TS)
    ) %>%
    gather("scenario", "NSpec", 2:3) %>%
    group_by(scenario) 
  all_diff_prsabsMEAN_type <- lake %>% 
    summarise(NSpMean = mean(NSpec))%>%
    arrange(factor(scenario))
  
  all_diff_prsabsMEAN_func_type_lake[[i]] <- lake
  all_diff_prsabsMEAN_func_type[[i]] <- all_diff_prsabsMEAN_type
}
names(all_diff_prsabsMEAN_func_type_lake) <- types
head(all_diff_prsabsMEAN_func_type_lake)
all_diff_prsabsMEAN_func_type_lake[[1]][all_diff_prsabsMEAN_func_type_lake[[1]]$scenario == "baseTS",]

names(all_diff_prsabsMEAN_func_type) <- types
head(all_diff_prsabsMEAN_func_type)


# ------------------------------------------------------------------------------------------
# Plotting  --------------------------------------------------------------------------------
theme_analysis <- function(base_size = 14) {
  theme_minimal(base_size = base_size) %+replace%
    theme(
      # changed theme options
    )
}
# Changing the default theme
theme_set(theme_analysis())

# See options within package
#depthPalette <- carto_pal(5, "Teal")[2:5]
#TempPalette<- carto_pal(3, "Peach")
#TurbNutrPalette<- carto_pal(7, "Earth")[c(1:4)]
WinnerLoserPalette<- c(carto_pal(2,"PinkYl")[c(2)],carto_pal(2,"TealGrn")[c(1)])
TrophiePalette <- c("cornflowerblue","aquamarine4","coral4")

lakeclasses <- c("clear lakes","intermediate lakes", "turbid lakes")
names(lakeclasses)<-c("clear","medium","turb")

p_Nspec_change_A <- res_comp_sceanrio %>%
  filter(scenarioTemp=="Temp1")%>%
  filter(scenarioTurbNut=="TurbNut0")%>%
  mutate(scenarioTurbNut=ifelse(scenarioTurbNut=="TurbNut0","test Tsteady - Base Tprofile","NA")) %>%
  ggplot(aes(factor(depth),mean, 
             group=interaction(Trophie, scenarioTemp)))+#, col=factor(Trophie
  geom_point(aes(col=interaction(Trophie)),
             position=position_dodge(width=0.5))+
  #geom_path(alpha=0.5, aes(col=interaction(Trophie)),
  #              position=position_dodge(width=0.5))+
  #geom_bar(stat="identity", aes(fill=factor(Trophie)), 
  #         position = position_dodge(width=0.5),
  #         width=0.5)+
  geom_errorbar(aes(ymax=mean+sd,
                    ymin=mean-sd,
                    col=interaction(Trophie)),
                position=position_dodge(width=0.5), width=.2)+
  #geom_boxplot()+
  facet_grid(scenarioTurbNut~class, 
             labeller = labeller(class = lakeclasses,
                                 scenarioTurbNut = scentunutlabel))+
  ylab("Potential spec. \nrichness change (N)")+
  theme(legend.position = "bottom")+
  scale_color_manual(values=rev(TrophiePalette))+
  theme(legend.title = element_blank())+
  scale_x_discrete(limits=rev)+ 
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  xlab("Depth (m)")+
  scale_y_continuous(limits=c(-60,60),breaks = seq(-60, 60, 20))+#,
  #sec.axis = dup_axis(name = expression(increase %<-% TurbNutr %->% decrease), 
  #                   breaks = NULL))+ 
  theme(axis.title.y.right = element_text(size=10,color = "grey50"))
# p_Nspec_change_A

p_Nspec_change_B <- res_comp_sceanrio %>%
  filter(scenarioTemp=="Temp1")%>%
  filter(scenarioTurbNut =="TurbNut1")%>%
  mutate(scenarioTurbNut=ifelse(scenarioTurbNut=="TurbNut1","test Tsteady + Base Tprofile","NA")) %>%
  ggplot(aes(factor(depth),mean, 
             group=interaction(Trophie, scenarioTemp)))+#, col=factor(Trophie
  geom_point(aes(col=interaction(Trophie)),
             position=position_dodge(width=0.5))+
  #geom_path(alpha=0.5, aes(col=interaction(Trophie)),
  #              position=position_dodge(width=0.5))+
  #geom_bar(stat="identity", aes(fill=factor(Trophie)), 
  #         position = position_dodge(width=0.5),
  #         width=0.5)+
  geom_errorbar(aes(ymax=mean+sd,
                    ymin=mean-sd,
                    col=interaction(Trophie)),
                position=position_dodge(width=0.5), width=.2)+
  #geom_boxplot()+
  facet_grid(scenarioTurbNut~class, 
             labeller = labeller(class = lakeclasses))+
  ylab("Potential spec. \nrichness change (N)")+
  theme(legend.position = "bottom")+
  scale_color_manual(values=rev(TrophiePalette))+
  theme(legend.title = element_blank())+
  scale_x_discrete(limits=rev)+ 
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  xlab("Depth (m)")+
  scale_y_continuous(limits=c(-60,60),breaks = seq(-60, 60, 20))+#,
  #sec.axis = dup_axis(name = expression(increase %<-% TurbNutr %->% decrease), 
  #                   breaks = NULL))+ 
  theme(axis.title.y.right = element_text(size=10,color = "grey50"))
# p_Nspec_change_B

p_Nspec_change_COMBO <- ((p_Nspec_change_A + xlab(""))/ 
                           (p_Nspec_change_B + xlab(""))) +
  plot_annotation(tag_levels = 'a',
                  tag_prefix = '(',
                  tag_suffix = ')')+ 
  #labs(fill="Spec. group") + 
  plot_layout(heights = c(1, 1, 1)) + 
  plot_layout(guides = "collect")  &
  theme(legend.position = "bottom",
        strip.text.x = element_text(size = 8),
        strip.text.y = element_text(size = 10),
        axis.text.x = element_text(angle = 90, 
                                   vjust = 0.5, hjust=1),
        axis.title=element_text(size=10))& 
  theme(plot.margin =  unit(c(-0.0, 0.2, -0.0, 0.2), "cm"),
        plot.tag = element_text(size = 12))& 
  #guides(colour = guide_legend(override.aes = list(size=2)))&
  guides(colour=guide_legend(nrow=1,byrow=TRUE))

p_Nspec_change_COMBO
ggsave(file.path(save_comparison, "test_COMBO_Nspec_change_Tprofile_vs_Tsteady.png"), 
       p_Nspec_change_COMBO, width = 7, height=10, dpi="print", bg="white", scale=0.75)

