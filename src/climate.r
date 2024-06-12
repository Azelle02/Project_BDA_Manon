library(geodata)
library(sp)
library(ggplot2)


# Matrix_full is my data frame with latitude and longitude columns, retrieve the coordinate for each of my occurences.
spatial_points <- SpatialPoints(coords = matrix_full[, c("longitude","latitude")], proj4string = CRS("+proj=longlat +datum=WGS84"))


###################################################################
###################################################################
# TEMPERATURE

# Retrieve average temperature data for Switzerland
sw_clim <- worldclim_country("switzerland", var = "tavg", path = tempdir())

# Retrieve average temperature data for Germany
de_clim <- worldclim_country("germany", var = "tavg", path = tempdir())

# Merge the raster of the 2 countries together
sw_ger_temp <- merge(sw_clim, de_clim)

sw_ger_temp_br <- brick(sw_ger_temp) # transformer le raster en un raster brick
plot(sw_ger_temp_br)
 

## Création d'une boucle pour extraire et rajouter la température de tous les mois dans une matrice
matrix_clim_temp = NULL        # création d'un dataframe vide que l'on va remplir
vec_colnames1 = NULL           # vecteur vide qui sera le nom des colonnes

    # Boucle
for(i in 1:12) {
  raster_temp <- as(sw_ger_temp_br[[i]], "Raster")
  vec_colnames1 <- c(vec_colnames1, names(raster_temp))                       
  raster_month <- raster::extract(raster_temp, spatial_points, method = 'bilinear')
  matrix_clim_temp <- cbind(matrix_clim_temp, raster_month)
}

colnames(matrix_clim_temp) <- vec_colnames1              # assigner des noms aux colonnes

# pour rajouter la moyenne des températures des mois dans une colonne 
vec_mean_temp <- as.vector(rowMeans(matrix_clim_temp))                  # moyenne sur toutes les lignes, pour tous les mois
matrix_clim_temp <- data.frame(matrix_clim_temp, vec_mean_temp)         # ajouter la moyenne au dataframe qui contient déjà les température pour chaque mois


# Plot density of temperature data for my occurrences, just a control graph
ggplot(matrix_clim_temp, aes(x = vec_mean_temp)) +
  geom_density(color = "darkblue", fill = "lightblue", adjust = 3) +
  theme_bw()


###################################################################
###################################################################
# PRECIPITATION

# Retrieve average temperature data for Switzerland
sw_clim_pre <- worldclim_country("switzerland", var = "prec", path = tempdir())

# Retrieve average temperature data for Germany
de_clim_pre <- worldclim_country("germany", var = "prec", path = tempdir())

# Merge the raster of the 2 countries together
sw_ger_pre <- merge(sw_clim_pre, de_clim_pre)

sw_ger_pre_br <- brick(sw_ger_pre) # transformer le raster en un raster brick
plot(sw_ger_pre_br)

## Création d'une boucle pour extraire et rajouter les précipitations de tous les mois dans une matrice
matrix_clim_pre = NULL         # création d'un dataframe vide que l'on va remplir
vec_colnames2 = NULL           # vecteur vide qui sera le nom des colonnes

    # Boucle
for(i in 1:12) {
  raster_pre <- as(sw_ger_pre_br[[i]], "Raster")
  vec_colnames2 <- c(vec_colnames2, names(raster_pre))                       
  raster_month <- raster::extract(raster_pre, spatial_points, method = 'bilinear')
  matrix_clim_pre <- cbind(matrix_clim_pre, raster_month)
}

colnames(matrix_clim_pre) <- vec_colnames2              # assigner des noms aux colonnes

# pour rajouter la moyenne des précipitation des mois dans une colonne 
vec_mean_pre <- as.vector(rowMeans(matrix_clim_pre))                  # moyenne sur toutes les lignes, pour tous les mois
matrix_clim_pre <- data.frame(matrix_clim_pre, vec_mean_pre)          # ajouter la moyenne au dataframe qui contient déjà les précipitations pour chaque mois


# Plot density of precipitation data for all my occurrences, just a control plot
ggplot(matrix_clim_pre, aes(x = vec_mean_pre)) +
  geom_density(color = "black", fill = "lightgreen", adjust = 3) +
  theme_bw()


###################################################################
###################################################################
# Mettre les matrices de précipitations et de température dans la matrice full!
matrix_full_elev_eco_clim <- data.frame(matrix_full_elev_eco, matrix_clim_temp, matrix_clim_pre)

###################################################################
###################################################################
# PLOTS

## Plots de densité pour température et précipitation
plot_temp <- ggplot(data= matrix_full_elev_eco_clim, aes(x = vec_mean_temp, group=species, fill=species)) +
    geom_density(adjust=1.5, alpha=.4) +
    labs(title = "Density plot for temperature data", x = "Temperature", y = "Density") +
    scale_fill_brewer(palette = "Set2")                  # attribuer des couleurs au 2 espèces


plot_precip <- ggplot(data= matrix_full_elev_eco_clim, aes(x = vec_mean_pre, group=species, fill=species)) +
    geom_density(adjust=1.5, alpha=.4) +
    labs(title = "Density plot for precipitation data", x = "Precipitation", y = "Density") +
    scale_fill_brewer(palette = "Set2")                 # attribuer des couleurs au 2 espèces


## Scatter plot pour voir la distribution de chaque occurence en fonction de la température et précipitation moyenne
plot_climate <- ggplot(matrix_full_elev_eco_clim, aes(x = vec_mean_pre, y= vec_mean_temp, color = species)) +
  geom_point(size = 1) +
  labs(title = "Data distribution", x = "Precipitation", y = "Temperature", color = "Species") +
  scale_color_manual(values=c("blue3", "brown2")) +       # attribuer des couleurs aux deux espèces
  theme(legend.position = "bottom",                       # position de la légende
  legend.title = element_text(size = 15, face = "bold"), legend.text = element_text(size = 15))  # changer la taille de la police de la légende


## Mettre les 3 plots ensembles

library("gridExtra")
library("ggpubr")
plots_climate <- grid.arrange(plot_temp, plot_precip, plot_climate + rremove("x.text"), 
             ncol = 2, nrow = 2)


# => On voit que l'on trouve d'avantage de ratons laveur dans des endroits où il y a beaucoup de précipitation que la martre. 
# Au niveau de la température, on retrouve le raton laveur là où il fait plus froid.  