## Main script

source("src/import_data.r")                 # import occurence data from gbif

source("src/elevation.r")                   # add elevation data to the matrix

source("src/ecosystems.r")                  # add ecosystem data to the matrix

source("src/climate.r")                     # add climatic data to the matrix

source("src/NDVI.r")                        # add NDVI data to the matrix 

source("src/plots.r")                       # Plusieurs plots pour représenter les données

source("src/stat.r")                        # Analyse statistique des données

source("src/map.r")                         # Script avec des cartes