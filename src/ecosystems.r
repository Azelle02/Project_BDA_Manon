
# Load the raster package
library(raster)

# Set the file path to my GeoTIFF
file_path <- "/Users/manonpache/Desktop/BDA/Project/data/WorldEcosystem.tif"

# Read the raster GeoTIFF
ecosystem_raster <- raster(file_path)

Swi_Ger <- ne_countries(scale = "medium", returnclass = "sf",country =c("Switzerland", "Germany"))

## crop and mask
r2 <- crop(ecosystem_raster, extent(Swi_Ger))           # crop for Switzerland and Germany
ecosystem_switzerland_germany <- mask(r2, Swi_Ger)
plot(ecosystem_switzerland_germany) # map of the 2 countries with the value of the ecosystems

# Matrix_full is my data frame with latitude and longitude columns, retrieve the coordinate for each of my occurences.
spatial_points <- SpatialPoints(coords = matrix_full[, c("longitude","latitude")], proj4string = CRS("+proj=longlat +datum=WGS84"))


plot(spatial_points,add=T,pch=16,cex=2) # problème d'affichage sur vscode mais ok. 

####################################
####################################
####################################

# Extraire les valeurs d'écosystème pour chacun des points
eco_values <- raster::extract(ecosystem_switzerland_germany, spatial_points) # nous donne une valeur, eco_value, pour chacun des points

# Print the extracted values
# print(eco_values)
# les valeurs extraites correspondent aux écosystèmes auquel appartiennent les points

# Matrix qui contient les coordonées, l'élévation et les données d'écosystèmes. 
matrix_full_elev_eco <- data.frame(matrix_full_elev, eco_values)

######### metadata: 

metadat_eco <- read.delim("/Users/manonpache/Desktop/BDA/Project/data/WorldEcosystem.metadata.tsv")

# Matrix complète qui contient aussi les metadata
matrix_full_elev_eco <- merge(matrix_full_elev_eco, metadat_eco, by.x = "eco_values", by.y = "Value", all.x = TRUE)

# Histogramme qui représente le nombre d'ocurrences qu'on trouve dans chaque écosystème, pour les deux espèces.
    
    ## Plot pas très visuel, trop d'écosystèmes différents avec la variable W_Ecosystm
ggplot(matrix_full_elev_eco, aes(x = W_Ecosystm, fill = species)) +
  geom_bar(position = "dodge") +
  labs(x = "Ecosystem", y = "Occurrences", title = "Occurrences of species in each ecosystem") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


    ## Plot plus approprié en prenant la variable Climate_Re et en enlevant les NA

matrix_full_elev_eco_withoutNA = matrix_full_elev_eco[!is.na(matrix_full_elev_eco$Climate_Re),] # enlever les NA qui se trouve dans la colonne Climate_Re

e <- ggplot(matrix_full_elev_eco_withoutNA, aes(x = Climate_Re, fill = species)) +
  geom_bar(position = "dodge") +
  labs(x = "Climate", y = "Count of observation", title = "Count of Observation of Each Species by Climate") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(e)

## On voit, en prenant la variable Climate_Re, que la majorité des occurences des 2 espèces se situent dans l'écosystème "Cold Temperate Moist". 
## La martre des pins se trouve également en plus grand nombre que le raton laveur dans l'écosystème "Warm Temperate Moist"

