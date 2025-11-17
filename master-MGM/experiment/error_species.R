# ERROR: reproDay < germinationDay + seedsEndAge
# if running Model from R

# ./input/species/species_14249.config.txt
# ./input/species/species_14264.config.txt
# ./input/species/species_14121.config.txt
# ./input/species/species_14233.config.txt
# ./input/species/species_14251.config.txt

# ./input/species/species_15040.config.txt
# ./input/species/species_15044.config.txt
# ./input/species/species_15174.config.txt
# ./input/species/species_15191.config.txt

# ./input/species/species_16043.config.txt
# ./input/species/species_16231.config.txt
# ./input/species/species_16233.config.txt
# ./input/species/species_16299.config.txt
# ./input/species/species_16300.config.txt

# species_id <- c(16001:16300)
# species_id <- species_id[species_id > 16299]
species_id <- c(14249, 14264, 14121, 14233, 14251,
                          15040, 15044, 15174, 15191,
                          16043, 16231, 16233, 16299, 16300)
species <- paste0("species_", species_id)

error <- c()
reproDay <- c()
gemSeeds <- c()

for(i in 1:length(species)){
    spec <- read.table(paste0("input/error_species/error_", species[i], ".config.txt"))

    reproDay[i] <- as.numeric(spec$V2[spec$V1 == "reproDay"])
    germinationDay <- as.numeric(spec$V2[spec$V1 == "germinationDay"])
    seedsEndAge <- as.numeric(spec$V2[spec$V1 == "seedsEndAge"])
    gemSeeds[i] <- germinationDay + seedsEndAge

    error[i] <- reproDay[i] < germinationDay + seedsEndAge

}

# Results 
reproDay == gemSeeds
error

# all reproDay are the same like germinationDay + seedsEndAge !!!
# this causes ERROR !!

# ERROR doesn't occure if running Charisma ??
# find bug in CHARISMA_biomass_N_weight_hight_env function !!
# bug: 
 #Test if setting are logic; if not break
# if testSettings(settings)!=0
#         # break # << causing bug, skipping all other species!
#         continue  # <-- skip to next iteration of species loop
# end