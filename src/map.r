### MAPS ####

library(rayshader)


elmat <- raster_to_matrix(elevation_Swi_Ger)

attr(elmat, "extent") <- extent(elevation_Swi_Ger)

# carte 2D du relief de la Suisse et de l'Allemagne
elmat %>% 
  sphere_shade(texture = "bw") %>%
  plot_map()


# version 3D de la carte du relief de la Suisse et de l'Allemagne 
elmat %>% 
  sphere_shade(texture = "bw") %>%
plot_3d(elmat, zscale = 150, fov = 0, theta = 135, zoom = 0.75, 
        phi = 45, windowsize = c(1500, 800))

# pour rajouter les points, mon ordinateur n'est pas assez puissant, donc je n'arrive pas à les voir sur la carte
render_points(
  extent = extent(Swi_Ger), size = 10,
  lat = matrix_full$latitude, long = matrix_full$longitude,
  altitude = elevation_points + 100, zscale = 150, color = "darkred"
)

# Modèle de densité______________________________________________________________________________

library(sf)
library(elevatr)
library(raster)
library(tidyverse)
library(RColorBrewer)
library(rayshader)
library(eks)


# 2 matrices avec seulement les données de 1 espèce
matrix_full_elev_eco_clim_ndvi_sp1 <-  matrix_full_elev_eco_clim_ndvi[matrix_full_elev_eco_clim_ndvi$species =="Procyon lotor",]
matrix_full_elev_eco_clim_ndvi_sp2 <- matrix_full_elev_eco_clim_ndvi[matrix_full_elev_eco_clim_ndvi$species =="Martes martes",]

sf_use_s2(FALSE)

### kernel espèce 1
 sf_points1 <- data.frame(                                  # prendre les points gsp 
    lat = matrix_full_elev_eco_clim_ndvi_sp1$latitude,
    lon = matrix_full_elev_eco_clim_ndvi_sp1$longitude
  ) |>
    st_as_sf(coords = c("lon", "lat"), crs = 4326)



skde1_sp1 <- st_kde(sf_points1, gridsize = c(100, 100))     # générer modèle de densité
plot(skde1_sp1)

data_sp1 = st_get_contour(skde1_sp1, cont = c(seq(50, 99, 5)), disjoint = FALSE)

# Create a function to generate the color palette
color_palette_sp1 <- colorRampPalette(c("white","darkred"))

# Define the number of colors in the palette
num_colors <- length(data_sp1$contlabel)  # Adjust as needed

# Generate the color palette for sp1
palette1 <- color_palette_sp1(num_colors)


### kernel espèce 2

 sf_points2 <- data.frame(
    lat = matrix_full_elev_eco_clim_ndvi_sp2$latitude,
    lon = matrix_full_elev_eco_clim_ndvi_sp2$longitude
  ) |>
    st_as_sf(coords = c("lon", "lat"), crs = 4326)


skde1_sp2 <- st_kde(sf_points2, gridsize = c(100, 100))     # générer modèle de densité
plot(skde1_sp2)

data_sp2 = st_get_contour(skde1_sp2, cont = c(seq(50, 99, 5)), disjoint = FALSE)

# Create a function to generate the color palette
color_palette_sp2 <- colorRampPalette(c("white","darkgreen"))

# Define the number of colors in the palette
num_colors <- length(data_sp2$contlabel)  # Adjust as needed

# Generate the color palette
palette2 <- color_palette_sp2(num_colors)


########### Modèle de densité - Plot 2D

elmat <- raster_to_matrix(elevation_Swi_Ger)

elmat %>%
 sphere_shade(texture = "desert") %>%
 add_overlay(generate_polygon_overlay(data_sp1, 
                        palette = palette1, linewidth=0,
                        extent = extent(elevation_Swi_Ger), heightmap = elmat),
                        alphalayer=0.7)  %>%
 add_overlay(generate_polygon_overlay(data_sp2, 
                        palette = palette2, linewidth=0,
                        extent = extent(elevation_Swi_Ger), heightmap = elmat),
                        alphalayer=0.7)  %>%

plot_map()


########## Modèle de densité - Plot 3D 

elmat <- raster_to_matrix(elevation_Swi_Ger)

elmat %>%
 sphere_shade(texture = "bw") %>%
 add_overlay(generate_polygon_overlay(data_sp1, 
                        palette = palette1, linewidth=0,
                        extent = extent(elevation_Swi_Ger), heightmap = elmat),
                        alphalayer=0.7)  %>%
 add_overlay(generate_polygon_overlay(data_sp2, 
                        palette = palette2, linewidth=0,
                        extent = extent(elevation_Swi_Ger), heightmap = elmat),
                        alphalayer=0.7)  %>%
plot_3d(elmat, zscale = 150, fov = 0, theta = 135, zoom = 0.75, 
        phi = 45, windowsize = c(1500, 800))


# On voit que la Martre est davantage présente en Suisse et en plus grande densité aussi.
# Le raton laveur lui est présent en Allemagne principalement. 

