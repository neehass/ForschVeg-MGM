# select species, reference Species (lewerentz 2023)
# Species Family Trophic Type Nr
# Chara aspera Characeae Oligotraphentic 3
# Myriophyllum spicatum Haloragaceae Mesotraphentic 33
# Potamogeton perfoliatus Potamogetonaceae Mesotraphentic 57
# Potamogeton pectinatus Potamogetonaceae Eutraphentic 56
# Elodea nuttallii Hydrocharitaceae Eutraphentic 21
# Najas intermedia Hydrocharitaceae Eutraphentic 36

getwd()
setwd("C:/Users/maiim/Documents/25-25SS/Forschungsprojekt_Vegetationskunde/scripte-data-MGM/master-MGM/data")

ix <- 0:75
needed <- c("Chara aspera", "Myriophyllum spicatum", "Potamogeton perfoliatus",
            "Potamogeton pectinatus", "Elodea nuttallii", "Najas marina ssp. intermedia")

for(i in ix){
    species <- paste0("species_",i,".txt")
    tab <- read.csv(species, header = TRUE, stringsAsFactors = FALSE)

    species_in_file <- unique(tab$species_name)
    matching_species <- species_in_file[species_in_file %in% needed]
    
    if(length(matching_species) > 0){
        for(s in matching_species){
            speciesID_val <- unique(tab$speciesID[tab$species_name == s])
            print(paste(s, speciesID_val))
        }
    } 
}
