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
species_id <- 14121
species <- paste0("species_", species_id)

spec <- read.table(paste0("input/species/", species, ".config.txt"))

reproDay <- as.numeric(spec$V2[spec$V1 == "reproDay"])
germinationDay <- as.numeric(spec$V2[spec$V1 == "germinationDay"])
seedsEndAge <- as.numeric(spec$V2[spec$V1 == "seedsEndAge"])

reproDay < germinationDay + seedsEndAge
