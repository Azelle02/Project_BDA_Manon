#remotes::install_github("wmgeolab/rgeoboundaries")
# install.packages("sf")
library(rgeoboundaries)
library(sf)


## Générer le TIFF pour la Suisse_________________________________________________________________

# Downloading the country boundary of switzerland
#map_boundary_sw <- geoboundaries("Switzerland") # to extract the map of Switzerland
#dir.create("./data/modis", recursive = TRUE) # to create a temporary folder to download the data before extract
                                            # creation d'un dossier modis dans le dossier data

# Defining filepath to save downloaded spatial file
#spatial_filepath <- "./data/modis/switzerland.shp"

# Saving downloaded spatial file on to our computer
#st_write(map_boundary_sw, paste0(spatial_filepath))

#library(devtools)
#install_github("ropensci/MODIStsp")
#library(MODIStsp)

#### check available data
#MODIStsp_get_prodlayers("M*D13Q1")

#MODIStsp(
  #gui = FALSE,
  #out_folder = "./data/modis",
  #out_folder_mod = "./data/modis",
  #selprod = "Vegetation Indexes_16Days_250m (M*D13Q1)",
  #bandsel = "NDVI",
  #user = "mstp_test",
  #password = "MSTP_test_01",
  #start_date = "2020.06.01",
  #end_date = "2020.06.01",
  #verbose = FALSE,
  #spatmeth = "file",
  #spafile = spatial_filepath,
  #out_format = "GTiff"
#)

## Générer le TIFF pour l'Allemagne_________________________________________________________________

#map_boundary_ger <- geoboundaries("Germany") # to extract the map of Germany
#dir.create("./data/modis", recursive = TRUE) # to create a temporary folder to download the data before extract
                                            # creation d'un dossier modis dans le dossier data

# Defining filepath to save downloaded spatial file
#spatial_filepath <- "./data/modis/germany.shp"

# Saving downloaded spatial file on to our computer
#st_write(map_boundary_ger, paste0(spatial_filepath))

#### check available data
#MODIStsp_get_prodlayers("M*D13Q1")

#MODIStsp(
  #gui = FALSE,
  #out_folder = "./data/modis",
  #out_folder_mod = "./data/modis",
  #selprod = "Vegetation Indexes_16Days_250m (M*D13Q1)",
  #bandsel = "NDVI",
  #user = "mstp_test",
  #password = "MSTP_test_01",
  #start_date = "2020.06.01",
  #end_date = "2020.06.01",
  #verbose = FALSE,
  #spatmeth = "file",
  #spafile = spatial_filepath,
  #out_format = "GTiff"
#)



library(raster)
library(here)
library(ggplot2)
library(viridis)
#library(rgdal)


##_____________________________________________________________________________________________

# Reading in the downloaded NDVI raster data
NDVI_raster_sw <- raster("./data/CH_MYD13Q1_NDVI_2020_153.tif")

NDVI_raster_ger <- raster("./data/GER_MYD13Q1_NDVI_2020_153.tiff")

# Merge the raster of the 2 countries together
sw_ger_ndvi <- merge(NDVI_raster_sw, NDVI_raster_ger)


# Transforming the data
NDVI_raster <- projectRaster(sw_ger_ndvi, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
plot(NDVI_raster)

# Cropping the data
map_boundary <- geoboundaries(c("Switzerland", "Germany"))        # get the bounderies for CH and GER
NDVI_raster <- raster::mask(NDVI_raster, as_Spatial(map_boundary)) # Couper le raster en fonction des frontières
plot(NDVI_raster)   # plot des 2 pays avec le NDVI

# Dividing values by 10000 to have NDVI values between -1 and 1
gain(NDVI_raster) <- 0.0001

# Assuming matrix_full is your data frame with latitude and longitude columns, retrieve the coordinate for each of my occurences.
spatial_points <- SpatialPoints(coords = matrix_full[, c("longitude","latitude")], proj4string = CRS("+proj=longlat +datum=WGS84"))
plot(spatial_points,add=T) # problème d'affichage sur vscode mais ok

####################################
####################################
#####################################

# Extraire les valeurs de NDVI pour chaque occurences
NDVI <- raster::extract(NDVI_raster, spatial_points)

# On ajoute les valeurs de NDVI à notre matrice full
matrix_full_elev_eco_clim_ndvi <- data.frame(matrix_full_elev_eco_clim, NDVI)

# Plot de contrôle, densité des occurences en fonction du NDVI
density_plot_ndvi <- ggplot(data= matrix_full_elev_eco_clim_ndvi, aes(x = NDVI, group=species, fill=species)) +
    geom_density(adjust=1.5, alpha=.4) +
    labs(title = "Density Plot of NDVI Data", x = "NDVI", y = "Density") +
    scale_fill_brewer(palette = "Set2") 

print(density_plot_ndvi)

# on remarque que les deux espèces ont quasiment la même distribution des données NDVI