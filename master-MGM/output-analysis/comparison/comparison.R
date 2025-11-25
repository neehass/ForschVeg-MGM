# Comparison analysis between different MGM experiment scenarios

# packages & functions
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
source("C:/Users/maiim/Documents/25-25SS/Forschungsprojekt_Vegetationskunde/scripte-data-MGM/master-MGM/experiment/help-func.r")

# dir setup ---------------------------------------------------------------------------------

getwd()
dir <- "C:/Users/maiim/Documents/25-25SS/Forschungsprojekt_Vegetationskunde/scripte-data-MGM/master-MGM"
setwd(dir)

# Folder output of MGM experiment and Analysis results folder
base_Tprofile <- "output/dep10_lakes_100spec_base_Tprofile"
save_base_Tprofile <- "output-analysis/dep10_lakes_100spec_base_Tprofile"

base_Tsteady <- "output/dep10_lakes_100spec_base_Tsteady"
save_base_Tsteady <- "output-analysis/dep10_lakes_100spec_base_Tsteady"

test <- "output/test_spec_14xxx"
save_test <- "output-analysis/test_spec_14xxx"

dir.create(save_base_Tprofile)
dir.create(save_base_Tsteady)

save_comparison <- "output-analysis/comparison"
dir.create(save_comparison)

# input files
lake_path <- "input/lakes"
lewSpec_dir <- "C:/Users/maiim/Documents/25-25SS/Forschungsprojekt_Vegetationskunde/scripte-data-MGM/LewerentzEtAl2023_ModelledMacrophyteSpeciesRichness-1.0"
# ---------------------------------------------------------------------------------------------------------
# load data --------------------------------------------------------------------------------
load(file.path(lewSpec_dir, "data/data_lakes_env_class.rda"))

# ---------------------------------------------------------------------------------------------------------
load(file.path(save_base_Tprofile, "res_dep10_Tprofile.RData"))   # res # all data
load(file.path(save_base_Tprofile, "sortRES_dep10_Tprofile.RData")) # sort_res # biomass > 0 sorted macrophyte data (grouped by lakeClass, speciesGroup, lakeGroup_Area, depth, day)
res_baseTP <- res
sort_res_baseTP <- sort_res

load(file.path(save_base_Tprofile, "env_dep10_Tprofile.RData"))  # env # all environmental data
load(file.path(save_base_Tprofile, "sortENV_dep10_Tprofile.RData")) # env_sort # sorted environmental data (grouped by llakeClass, lakeGroup_Area, day)
env_baseTP <- res
sort_env_baseTP <- sort_res
rm(res, env, sort_res, sort_env)  # remove the generic

# ---------------------------------------------------------------------------------------------------------

load(file.path(save_test, "res_dep10_Tprofile.RData"))   # res # all data
load(file.path(save_test, "sortRES_dep10_Tprofile.RData")) # sort_res # biomass > 0 sorted macrophyte data (grouped by lakeClass, speciesGroup, lakeGroup_Area, depth, day)
res_test <- res
sort_res_test <- sort_res

load(file.path(save_test, "env_dep10_Tprofile.RData"))  # env # all environmental data
load(file.path(save_test, "sortENV_dep10_Tprofile.RData")) # env_sort # sorted environmental data (grouped by llakeClass, lakeGroup_Area, day)
env_test <- res
sort_env_test <- sort_res
rm(res, env, sort_res, sort_env)  # remove the generic

# ---------------------------------------------------------------------------------------------------------
# Comparision between T_profile vs without T_profile ------------------
# ...............
load(file.path(lewSpec_dir, "data/all_diff_presabs_tobase.rda"))
head(all_diff_presabs_tobase)
# bring data in this format

# ------------------------------------------------------------------------------------------
# prepare base T_profile data
res_baseTP_prep <- res_baseTP %>%
    mutate(scenario = "base_Tprofile") %>%
    group_by(lakeID, speciesID, depth, speciesGroup, lakeClass) %>%
    summarise(
        Biomass_cat = if_else(mean(biomass, na.rm = TRUE) > 0, 1, 0, missing = 0))
head(res_baseTP)

# prepare test data
res_test_prep <- res_test %>%
    mutate(scenario = "test") %>%
    group_by(lakeID, speciesID, depth, speciesGroup, lakeClass) %>%
    summarise(
              Biomass_cat = if_else(mean(biomass, na.rm = TRUE) > 0, 1, 0, missing = 0))
head(res_test_prep)

# combine both datasets
res_combined <- res_baseTP_prep %>%
    left_join(res_test_prep, by = c("lakeID", "depth", "speciesID", "speciesGroup", "lakeClass"), 
    suffix = c("_baseTP", "_test")) %>%
    mutate(
        Biomass_cat_baseTP = replace_na(Biomass_cat_baseTP, 0),
        Biomass_cat_test   = replace_na(Biomass_cat_test, 0)
    ) %>%
    rename(
        baseTP = Biomass_cat_baseTP,
        test = Biomass_cat_test
    )
head(res_combined)
any(is.na(res_combined))
# ------------------------------------------------------------------------------------------
# Comparison 
# as in analysis.Rmd

# Number of lost species per scenario (%) -------------
NSPECbase <- res_combined %>% ungroup() %>% select(speciesID) %>% unique() %>% nrow()

SPEC_DEAD <- res_combined %>% 
  group_by(speciesID) %>% select(-lakeID,-depth, -speciesGroup, -lakeClass) %>%
  summarise_all(sum) %>% #Pro Art Summe aus Tiefen und Seen
  #filter(base>0) %>% #nur Arten, die schon vorkamen
  select(baseTP, test) %>%
  summarise_all(list(~sum(. > 0, na.rm = TRUE))) %>% # N species pro Szen if present
  gather("scenario","Ntotspec", (1:(ncol(res_combined)-5))) %>% 
  mutate(Lost= Ntotspec - NSPECbase) %>% 
  mutate(Lost_percent = (Lost/NSPECbase)*100) %>%
  filter(scenario!= "baseTP") %>% 
  mutate(Temp=c(0)) %>%
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
    summarise_all(sum) %>% filter(baseTP > 0) %>% select(baseTP, test) %>%
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
head(Nspec_los_finc_type$oligotroph)

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
# ------------------------------------------------------------------------------------------
# Comparison between scenarios with T_profile ------------------
# ...........

# ------------------------------------------------------------------------------------------
# Traits Winner / Loser  ------------------
# ...........