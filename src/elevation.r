library(sf)
library(elevatr)
library(sp)
library(raster)
library(ggplot2)

library(dplyr)
library(tidyr)
library(viridis)


sf_use_s2(FALSE)


Swi_Ger <- ne_countries(scale = "medium", returnclass = "sf",country =c("Switzerland", "Germany")) # get the map of the two countries
elevation_Swi_Ger <- get_elev_raster(Swi_Ger, z = 8)   # get elevation data
plot(elevation_Swi_Ger) # carte de la suisse et de l'allemagne avec l'altitude, mais pas croper

## Crop and mask pour avoir que les pays que l'on souhaite
r2 <- crop(elevation_Swi_Ger, extent(Swi_Ger))  
elevation_Swi_Ger <- mask(r2, Swi_Ger)
plot(elevation_Swi_Ger)


## Add points
latitude <- matrix_full$latitude
longitude <- matrix_full$longitude

# Create a data frame for GBIF data
gbif_coord <- data.frame(longitude,latitude)

# Extract elevation values for each occurrences
ll_prj <- "EPSG:4326" 
points <- sp::SpatialPoints(gbif_coord, 
                            proj4string = sp::CRS(SRS_string = ll_prj))
elevation_points <- raster::extract(elevation_Swi_Ger, points, method='bilinear')
elevation_df <- data.frame(elevation = elevation_points)

# Put the elevation value into the matrix_full for each occurence 
matrix_full_elev = cbind(matrix_full, elevation_df)


## We can do a density graph to compare the distribution of the 2 species according to elevation
density_plot <- ggplot(data= matrix_full_elev, aes(x = elevation, group=species, fill=species)) +
    geom_density(adjust=1.5, alpha=.4) +
    labs(title = "Density Plot of elevation data", x = "Elevation[m]", y = "Density") +
    scale_fill_brewer(palette = "Set2") 

print(density_plot)

## On voit que les occurences des 2 espèces ne sont pas distibuées de la même manière en fonction de l'altitude. 
## Le raton laveur a un pic d'occurence vers 180m alors que les occurences du pommier sauvage ont une plus grande variance. 

#Elevation moyenne pour chaque espèce:
mean_elev <- tapply(matrix_full_elev$elevation, matrix_full_elev$species, mean, na.rm = T)
mean_elev # on voit ici que l'élévation moyenne des occurences est différente pour les 2 espèces. 
