# add AreaKm2 & depth to lake input files
library(dplyr)
getwd()
dir <- "master-MGM"
setwd(dir)

# load lakes depth < -10 m ---------------------
Area <- read.csv("C:/Users/maiim/Documents/25-25SS/Forschungsprojekt_Vegetationskunde/R_scripte/data/lake_coords_area_qgis.csv", header = TRUE)
Area$Name <- gsub("-", "", Area$Name)
depth <- read.csv("C:/Users/maiim/Documents/25-25SS/Forschungsprojekt_Vegetationskunde/R_scripte/data/dep10_name_depth.csv", header = TRUE)
depth$Name <- gsub("-", "", depth$Name)
all_depths <- read.csv("C:/Users/maiim/Documents/25-25SS/Forschungsprojekt_Vegetationskunde/R_scripte/data/all_name_depth.csv", header = TRUE)
head(all_depths)
all_depths$Name <- gsub("-", "", all_depths$Name)

# load lake config files -------------------------
lakes_path <- list.files("input/lakes", full.name = TRUE)

names <- c()
for(i in 1:length(lakes_path)){
    lake <- read.table(lakes_path[i])
    name <- lake$V2[lake$V1 == "Name"]
    names[i] <- name
}


# check names -------------------
# rename 
setdiff(Area$Name, depth$Name)   # in Area aber nicht in depth
setdiff(depth$Name, Area$Name) 

setdiff(all_depths$Name, names) # in  names aber nicht in alldepths
names[!names %in% all_depths$Name] # alle names die nicht in Area$Name
names[!names %in% depth$Name] == names[!names %in% Area$Name]

x <- c("NiedersonthofenerSee", "GrosserAlpseebeiImmenstadt", "AlpseebeiSchwangau", "Barmsee", "LangbuergnerSee")
rep <- c("NiedersonthofnerSee", "GrosserAlpsee", "AlpseeSchongau", "Barmseesee", "LangbuergenerSee")
for(i in 1:length(x)){
    Area$Name[Area$Name == x[i]] <- rep[i]
    depth$Name[depth$Name == x[i]] <- rep[i]
    all_depths$Name[all_depths$Name == x[i]] <- rep[i]
}

names[!names %in% Area$Name] # > -10 
all_depths$Depth[all_depths$Name %in% names[!names %in% Area$Name]] # DEPTHS
lakes_path[which(names %in% names[!names %in% Area$Name])] # IDs in config.txt

# add Depth und Areakm2 to lake config files, Depth < -10 --------------------------
skip <- which(names %in% names[!names %in% Area$Name])

for(i in 1:length(lakes_path)){
    if(i %in% skip) next
    
    lake <- read.table(lakes_path[i])
    name <- lake$V2[lake$V1 == "Name"]

    dm <- depth$Depth[depth$Name == name]
    lake <- rbind(lake, c("lakeDepth", dm))

    km2 <- Area$Area_km2[Area$Name == name]
    lake <- rbind(lake, c("Areakm2", km2))

    lines <- paste(lake$V1, as.character(lake$V2))
    writeLines(lines, lakes_path[i])
}

