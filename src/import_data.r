library(rgbif)
library(rnaturalearth)
library(ggplot2)
library(sf)

######## RATON LAVEUR ###########
#################################

myspecies1 <- c("Procyon lotor")

# download records in Switzerland and Germany only
gbif_data <- occ_data(scientificName = myspecies1, hasCoordinate = TRUE, limit = 400, country = "CH")
gbif_data_DE <- occ_data(scientificName = myspecies1, hasCoordinate = TRUE, limit = 6000, country = "DE")

occur  <- gbif_data$data # to extract the occurence
occur_DE <- gbif_data_DE$data

#gbif_data_switzerland <- occur[occur$country == "Switzerland",] # autre manière d'extraire les occurences pour la Suisse 

Switzerland <- ne_countries(scale = "medium", returnclass = "sf",country ="Switzerland") # no need for that 
Germany <- ne_countries(scale = "medium", returnclass = "sf",country ="Germany") # neither that

# To have a map with the two countries together
Swi_Ger <- ne_countries(scale = "medium", returnclass = "sf",country =c("Switzerland", "Germany"))

# Plot with the two countries and add the points for racoons
ggplot(data = Swi_Ger) +
  geom_sf() +
  geom_point(data = occur, aes(x = decimalLongitude, y = decimalLatitude), size = 4, 
             shape = 23, fill = "darkgreen") + theme_classic() +
  geom_point(data = occur_DE, aes(x = decimalLongitude, y = decimalLatitude), size = 4, 
             shape = 23, fill = "blue") +
  coord_sf(xlim = c(5, 15), ylim = c(45, 55)) 

# Extract relevant data from GBIF occurrences
    ## SWITZERLAND 
species <- occur$species
latitude <- occur$decimalLatitude
longitude <- occur$decimalLongitude
source <- rep("gbif", length(species)) # attribuer la source pour chaque occurence. 
country <- occur$country

data_gbif_raton_CH <- data.frame(species, latitude, longitude, source, country) # Create a data frame with the data for Switzerland

    ## GERMANY 
species <- occur_DE$species
latitude <- occur_DE$decimalLatitude
longitude <- occur_DE$decimalLongitude
source <- rep("gbif", length(species)) # attribuer la source pour chaque occurence. 
country <- occur_DE$country

data_gbif_raton_DE <- data.frame(species, latitude, longitude, source, country) # Create a data frame with the data for Germany

### Merge Switzerland and Germany data together for the raccoon. 
data_gbif_raton = rbind(data_gbif_raton_CH, data_gbif_raton_DE)


######## MARTRE DES PINS ########
#################################

myspecies2 <- c("Martes martes")

# download records in Switzerland and in Germany
gbif_data2 <- occ_data(scientificName = myspecies2, hasCoordinate = TRUE, limit = 5000, country = "CH") 
gbif_data2_DE <- occ_data(scientificName = myspecies2, hasCoordinate = TRUE, limit = 1050, country = "DE") 

occur_2  <- gbif_data2$data # to extract the occurence
occur_2_DE <- gbif_data2_DE$data 

# Carte martre Suisse et Allemagne 
ggplot(data = Swi_Ger) +
 geom_sf() +
  geom_point(data = occur_2, aes(x = decimalLongitude, y = decimalLatitude), size = 4, 
             shape = 23, fill = "darkgreen") + theme_classic() +
  geom_point(data = occur_2_DE, aes(x = decimalLongitude, y = decimalLatitude), size = 4, 
             shape = 23, fill = "blue") +
  coord_sf(xlim = c(5, 15), ylim = c(45, 55)) 


# Extract relevant data from GBIF occurrences
    ## SWITZERLAND
species <- occur_2$species
latitude <- occur_2$decimalLatitude
longitude <- occur_2$decimalLongitude
country <- occur_2$country
source <- rep("gbif", length(species)) # attribuer la source pour chaque occurence. 

# Create a data frame for GBIF data
data_gbif_martre_CH <- data.frame(species, latitude, longitude, source, country) # Create a data frame with the data for Switzerland

    ## GERMANY 
species <- occur_2_DE$species
latitude <- occur_2_DE$decimalLatitude
longitude <- occur_2_DE$decimalLongitude
country <- occur_2_DE$country
source <- rep("gbif", length(species)) # attribuer la source pour chaque occurence. 

# Create a data frame for GBIF data
data_gbif_martre_DE <- data.frame(species, latitude, longitude, source, country) # Create a data frame with the data for Germany

### Merge Switzerland and Germany data together for Martes martes. 
data_gbif_martre = rbind(data_gbif_martre_CH, data_gbif_martre_DE)


################ MERGE ######################

# Combine the data of the 2 species into the same matrix
matrix_full <- rbind(data_gbif_raton, data_gbif_martre)


# Distribution des occurences des 2 espèces en Suisse et en Allemagne
p <- ggplot(data = Swi_Ger) +
  geom_sf() +
  geom_point(data = matrix_full, aes(x = longitude, y = latitude, fill = species), size = 1.5, 
             shape = 23) +
  theme_classic() +
  coord_sf(xlim = c(5, 15), ylim = c(45, 55)) 

print(p)
