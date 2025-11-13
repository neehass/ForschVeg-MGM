## Script name: Random_input
## Purpose of script: Create random input files for CHARISMA
## Author: Anne Lewerentz
## Date Created: 2020-12-08
## Email: anne.lewerentz@uni-wuerzburg.de

## Load packages ----
library(data.table)
library(here)

## Set general working directory & Outputfolder ----
wd<-"MGMinput/createVirtualSpecies"
outputfolder<-"species_eutroph_V4"

## Set parameters ----
nspecies_start = 16001
nspecies_end = 16300 #280

## Select species group
specgroup <- "eutroph"

# Read in parameterspace
setwd(wd)
parameterspace<-fread(paste0("parameterspace_", specgroup,".csv"))

## Create config files species ----
dir.create(outputfolder)
setwd(paste0(wd,"/",outputfolder))

for (n in nspecies_start:nspecies_end){
  filenamelake=paste("species_",n, ".config.txt",sep="")
  namespecies=paste("species_",n,sep="")

  # Create data-frame
  species_config <- data.frame("name"="Species", "value"=namespecies)

  # Add all parameters one by one
  seedsStartAge=list("seedsStartAge", round(runif(1, min=parameterspace[Name=="seedsStartAge"]$Min,
                                                  max=parameterspace[Name=="seedsStartAge"]$Max)) ) #

  seedsEndAge=list("seedsEndAge", round(runif(1, min=(seedsStartAge[[2]]+10),
                                              max=parameterspace[Name=="seedsEndAge"]$Max))) #

  tuberStartAge=list("tuberStartAge", round(runif(1, min=parameterspace[Name=="tuberStartAge"]$Min,
                                                  max=parameterspace[Name=="tuberStartAge"]$Max)) ) #

  tuberEndAge=list("tuberEndAge", round(runif(1, min=(tuberStartAge[[2]]+10),
                                              max=parameterspace[Name=="tuberEndAge"]$Max))) #

  cTuber=list("cTuber",  runif(1, min=parameterspace[Name=="cTuber"]$Min,
                               max=parameterspace[Name=="cTuber"]$Max)) #FIXED

  pMax=list("pMax", (runif(1, min=(parameterspace[Name=="pMax"]$Min),
                              max=(parameterspace[Name=="pMax"]$Max)))) #TODO Range

  q10 =list("q10", runif(1, min=parameterspace[Name=="q10"]$Min,
                         max=parameterspace[Name=="q10"]$Max)) #

  resp20=list("resp20", (runif(1, min=(parameterspace[Name=="resp20"]$Min),
                                  max=(parameterspace[Name=="resp20"]$Max)))) #

  # heightMax=list("heightMax", exp(runif(1, min=log(parameterspace[Name=="heightMax"]$Min),
  #                                       max=log(parameterspace[Name=="heightMax"]$Max)))) #LOG scale

  heightMax=list("heightMax", (runif(1, min=(parameterspace[Name=="heightMax"]$Min),
                                        max=(parameterspace[Name=="heightMax"]$Max)))) #LOG scale

  maxWeightLenRatio=list("maxWeightLenRatio", (runif(1, min=(parameterspace[Name=="maxWeightLenRatio"]$Min),
                                                        max=(parameterspace[Name=="maxWeightLenRatio"]$Max)))) #

  rootShootRatio=list("rootShootRatio", runif(1, min=parameterspace[Name=="rootShootRatio"]$Min,
                                              max=parameterspace[Name=="rootShootRatio"]$Max)) #

  fracPeriphyton=list("fracPeriphyton", parameterspace[Name=="fracPeriphyton"]$Const) #FIXED

  hPhotoDist=list("hPhotoDist",  runif(1, min=parameterspace[Name=="hPhotoDist"]$Min,
                                       max=parameterspace[Name=="hPhotoDist"]$Max)) #

  hPhotoLight=list("hPhotoLight", runif(1, min=(parameterspace[Name=="hPhotoLight"]$Min),
                                        max=(parameterspace[Name=="hPhotoLight"]$Max))) #

  hPhotoTemp=list("hPhotoTemp", runif(1, min=parameterspace[Name=="hPhotoTemp"]$Min,
                                      max=parameterspace[Name=="hPhotoTemp"]$Max)) #

  hTurbReduction=list("hTurbReduction", parameterspace[Name=="hTurbReduction"]$Const) #TODO USED??

  plantK=list("plantK",  runif(1, min=parameterspace[Name=="plantK"]$Min,
                               max=parameterspace[Name=="plantK"]$Max)) #

  pPhotoTemp=list("pPhotoTemp", runif(1,min=parameterspace[Name=="pPhotoTemp"]$Min,
                                      max=parameterspace[Name=="pPhotoTemp"]$Max)) #

  pTurbReduction=list("pTurbReduction", parameterspace[Name=="pTurbReduction"]$Const) #TODO

  sPhotoTemp=list("sPhotoTemp", runif(1, min=parameterspace[Name=="sPhotoTemp"]$Min,
                                      max=parameterspace[Name=="sPhotoTemp"]$Max)) #

  BackgroundMort=list("BackgroundMort", parameterspace[Name=="BackgroundMort"]$Const) #FIXED

  cThinning=list("cThinning", round(runif(1, min=parameterspace[Name=="cThinning"]$Min,
                                          max=parameterspace[Name=="cThinning"]$Max))) #TODO

  hWaveMort=list("hWaveMort", runif(1, min=parameterspace[Name=="hWaveMort"]$Min,
                                    max=parameterspace[Name=="hWaveMort"]$Max)) #

  germinationDay=list("germinationDay", round(runif(1, min=parameterspace[Name=="germinationDay"]$Min,
                                                    max=parameterspace[Name=="germinationDay"]$Max))) #März bis Ende Mai

  reproDay=list("reproDay", round(runif(1, min=(germinationDay[[2]]+max(seedsEndAge[[2]], tuberEndAge[[2]])),
                                        max=parameterspace[Name=="reproDay"]$Max))) #>germinationDay+seedEndAge | tuberEndAge

  maxAge=list("maxAge", round(runif(1, min=(reproDay[[2]]+1), max=parameterspace[Name=="maxAge"]$Max))) #NOT FIXED ANY MORE

  maxWaveMort=list("maxWaveMort", runif(1, min=parameterspace[Name=="maxWaveMort"]$Min,
                                        max=parameterspace[Name=="maxWaveMort"]$Max)) #

  pWaveMort=list("pWaveMort", round(runif(1,min=parameterspace[Name=="pWaveMort"]$Min,
                                          max=parameterspace[Name=="pWaveMort"]$Max))) #

  thinning=list("thinning", "true") #FIXED

  hNutrient=list("hNutrient", (runif(1,min=(parameterspace[Name=="hNutrient"]$Min),
                                        max=(parameterspace[Name=="hNutrient"]$Max)))) #LOG

  hNutrReduction=list("hNutrReduction", parameterspace[Name=="hNutrReduction"]$Const) #TODO

  pNutrient=list("pNutrient", round(runif(1,min=parameterspace[Name=="pNutrient"]$Min,
                                          max=parameterspace[Name=="pNutrient"]$Max))) #

  seedBiomass=list("seedBiomass", runif(1, min=parameterspace[Name=="seedBiomass"]$Min,
                                        max=parameterspace[Name=="seedBiomass"]$Max)) #TODO

  seedFraction=list("seedFraction", runif(1, min=parameterspace[Name=="seedFraction"]$Min,
                                          max=parameterspace[Name=="seedFraction"]$Max)) #

  seedGermination=list("seedGermination", runif(1, min=parameterspace[Name=="seedGermination"]$Min,
                                                max=parameterspace[Name=="seedGermination"]$Max)) #TODO

  seedInitialBiomass=list("seedInitialBiomass", parameterspace[Name=="seedInitialBiomass"]$Const) #Fixed

  seedMortality=list("seedMortality", parameterspace[Name=="seedMortality"]$Const) #Fixed

  tuberBiomass=list("tuberBiomass", runif(1, min=parameterspace[Name=="tuberBiomass"]$Min,
                                          max=parameterspace[Name=="tuberBiomass"]$Max))

  tuberFraction=list("tuberFraction", runif(1, min=parameterspace[Name=="tuberFraction"]$Min,
                                            max=parameterspace[Name=="tuberFraction"]$Max)) #

  tuberGermination=list("tuberGermination", parameterspace[Name=="tuberGermination"]$Const) #

  tuberGerminationDay=list("tuberGerminationDay", round(runif(1, min=parameterspace[Name=="tuberGerminationDay"]$Min,
                                                              max=parameterspace[Name=="tuberGerminationDay"]$Max))) #

  tuberInitialBiomass=list("tuberInitialBiomass", parameterspace[Name=="tuberInitialBiomass"]$Const) #

  tuberMortality=list("tuberMortality", parameterspace[Name=="tuberMortality"]$Const) #

  Kohler5=list("Kohler5", round(runif(1, min=parameterspace[Name=="Kohler5"]$Min,
                                      max=parameterspace[Name=="Kohler5"]$Max))) #

  Group = list("Group", specgroup)
  species_config<-rbindlist(list(species_config, seedsStartAge, seedsEndAge, tuberStartAge, tuberEndAge,
                                 cTuber, pMax, q10, resp20, heightMax, maxWeightLenRatio, rootShootRatio,
                                 fracPeriphyton, hPhotoDist, hPhotoLight, hPhotoTemp, hTurbReduction,
                                 plantK, pPhotoTemp, pTurbReduction, sPhotoTemp, BackgroundMort, cThinning,
                                 hWaveMort, germinationDay, reproDay, maxAge, maxWaveMort, pWaveMort, thinning,
                                 hNutrient, hNutrReduction, pNutrient, seedBiomass, seedFraction, seedGermination,
                                 seedInitialBiomass, seedMortality, tuberBiomass, tuberFraction, tuberGermination,
                                 tuberGerminationDay, tuberInitialBiomass, tuberMortality, Kohler5,Group ))

  write.table(species_config, filenamelake, col.names = F, row.names = F, quote=F)
}
