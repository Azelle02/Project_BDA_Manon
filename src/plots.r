library(ggfortify)
library(gridExtra)
library(ggpubr)


df <- matrix_full_elev_eco_clim_ndvi                # sélection de ma matrice 
df <- na.omit(df)                                   # enlever toutes les lignes qui contiennent des NA

df_continous <- df[,colnames(df) %in% c("vec_mean_temp","elevation","NDVI","vec_mean_pre")] # sélection des variable continues
df_discrete <- df[,!(colnames(df) %in% c("vec_mean_temp","elevation","NDVI","vec_mean_pre"))] # pour faire l'inverse, prendre toutes les variables discrètes

df_continous <- apply(df_continous,2,as.numeric) # transformer tous les éléments des colonnes en valeur numérique


#### PCA___________________________________________________________________________________________________

pca_res <- prcomp(df_continous, scale. = TRUE) # faire la PCA sur les variables continues

    # Plot de la PCA normale
pca1 <- autoplot(pca_res, data = df_discrete, colour = 'species',
                     loadings = TRUE, loadings.colour = 'black',
                     loadings.label = TRUE, loadings.label.size = 3) +
                     theme(legend.title = element_text(size = 15, face = "bold"), legend.text = element_text(size = 15)) 

    # Autre plot de la PCA, modèle éliptique
pca2 <- autoplot(pca_res, data = df_discrete, colour = 'species',
         loadings = TRUE, loadings.colour = 'black',
         loadings.label = TRUE, loadings.label.size = 3, frame = TRUE, frame.type = 'norm') +
         theme(legend.title = element_text(size = 15, face = "bold"), legend.text = element_text(size = 15)) 

    # Mettre les graph sur la même page
grid.arrange(pca1, pca2 + rremove("x.text"), 
             ncol = 1, nrow = 2)

# On voit que les variables elevation et les températures (vec_mean_temp) semblent corrélées positivement
# à l'inverse, on remarque les précipitations sont négativement corrélés avec l'élévation et les températures.
# la PC1 explique un plus grand poucentage de la variabilité des nos données. 
# On remarque 2 clusters qui corresponds à nos deux espèces. 


### PCA in 3D and interactive_______________________________________________________________________

# On refait la PCA
pca <- princomp(df_continous, scores=T, cor=T)

# on extrait les scores PC
scores <- pca$scores
x <- scores[,1]
y <- scores[,2]
z <- scores[,3]

# on extrait les loadings
loads <- pca$loadings

# Loadings names
load_names <- rownames(loads)

# Scale factor for loadings
scale.loads <- 5

# 3D plot
library(plotly)
p <- plot_ly() %>%
  add_trace(x=x, y=y, z=z,
            type="scatter3d", mode="markers", color = df_discrete$species,
            marker = list(size = 2))


for (k in 1:nrow(loads)) {
   x <- c(0, loads[k,1])*scale.loads
   y <- c(0, loads[k,2])*scale.loads
   z <- c(0, loads[k,3])*scale.loads
   p <- p %>% add_trace(x=x, y=y, z=z,
            type="scatter3d", mode="lines",
            line = list(width=5),
            opacity = 1,
            name = load_names[k])  # Adding names to the loadings
}
print(p)


### Radar chart plot_______________________________________________________________________

library(fmsb)

#### Pour les espèces

# Aggréger les données pour les 2 espèces, en faisant la moyenne
aggregated_data_species <- aggregate(
  cbind(elevation, vec_mean_pre, vec_mean_temp, NDVI) ~ species, 
  data = df, 
  FUN = mean
)

# Afficher les résultats
print(aggregated_data_species)

min1 <- c(0,0,-10,0)                                  # pour définir la valeur minimum des variables 
max2 <- c(apply(aggregated_data_species[,2:5],2,max)) # pour définir la valeur max des variables
sp1 <- aggregated_data_species[1,2:5]                 # extraire les données pour la 1ère espèce
sp2 <- aggregated_data_species[2,2:5]                 # extraire les données pour la 2ème espèce

row_id <- c(1,2,aggregated_data_species$species)      # créer une liste avec le nom des lignes
 
aggregated_data_species <- rbind(min1,max2,sp1,sp2)   # coller les lignes ensemble

row.names(aggregated_data_species) <- row_id          # mettre les noms aux lignes


radarchart(aggregated_data_species) # faire le radarchart
legend("topright", legend = row_id[3:4], col = c("black", "red"), lty = c(1, 2), lwd = 2)
# => la différence entre les 2 espèces se fait principalement au niveau de l'élévation et des précipitations. 

#### Pour les écosystemes

# Aggréger les données pour les 2 pays, en faisant la moyenne
aggregated_data_country <- aggregate(
  cbind(elevation, vec_mean_pre, vec_mean_temp, NDVI) ~ Climate_Re, 
  data = df, 
  FUN = mean
)

# Afficher les résultats
print(aggregated_data_country)

min1 <- c(0,0,-10,0)                                  # pour définir la valeur minimum des variables 
max2 <- c(apply(aggregated_data_country[,2:5],2,max)) # pour définir la valeur max des variables
eco1 <- aggregated_data_country[1,2:5]                # extraire les données pour le 1er écosystème
eco2 <- aggregated_data_country[2,2:5]                # extraire les données pour le 2ème écosystème
eco3 <- aggregated_data_country[3,2:5]                # ""
eco4 <- aggregated_data_country[4,2:5]                # ""
eco5 <- aggregated_data_country[5,2:5]                # ""

row_id <- c(1,2,aggregated_data_country$Climate_Re)      # créer une liste avec le nom des lignes
 
aggregated_data_country <- rbind(min1, max2, eco1, eco2, eco3, eco4, eco5)   # coller les lignes ensemble

row.names(aggregated_data_country) <- row_id          # mettre les noms aux lignes

radarchart(aggregated_data_country) # faire le radarchart
legend("topright", legend = row_id[3:7], col = c("black", "red", "green", "blue", 5), lty = c(1, 2, 2, 2, 2), lwd = 2)

# les écosystèmes diffèrent principalement dans l'élévation et les précipitations. 
# Cool Temperate Dry, Cool Temprerate Moist and Warm Temperate Moist semblent avoir des caractéristiques communes. 


### Plot de corrélation_______________________________________________________________________

library(ggcorrplot)
library(corrplot)

mydata.cor <- cor(df_continous) # faire la matrice de corrélation pour les variables continues

my_corplot <- corrplot(mydata.cor, order = 'hclust', addrect = 3) # faire le plot de la matrice de corrélation, avec le hierarchical clustering
# =>> La PCA précédente nous avait déjà donné des informations sur les relations entre les variables mais là on voit clairement
# que les variables elevation et vec_mean_temp sont corrélée positivement, et que la variable vec_mean_pre est corrélée négativement 
# avec elevation et vec_mean_temp. La variable NDVI est corrélée négativement avec elevation et vec_mean_temp, et positivement avec vec_mean_pre


### Heatmap______________________________________________________________________________________________

library(pheatmap)

#### Heatmap simple, en prenant que les valeurs numériques 

# Préparer les données
data <- df_continous
row.names(data) <- c(1:nrow(data))

heatmap(scale(data)) # générer la heatmap
# on voit que les variables élévation et vec_mean_temp sont regroupés, et vec_mean_pre et NDVI sont regroupés

#### Heatmap plus avancée 

# Sélection des facteurs pour les annotations
my_group <- df_discrete[c("Landcover")]  # on utilise "Landcover" pour les annotations
row.names(my_group) <- c(1:nrow(my_group))

# Generer la heatmap avec les annotations
pheatmap(scale(data),
         annotation_row = my_group)

# Customizer la heatmap
library(randomcoloR)

data_col <- grDevices::colorRampPalette(c("black", "darkgreen", "white", "darkred"))   # définir les couleurs

# display la heatmap. 
ht <- pheatmap(scale(data),
         annotation_row = my_group,
         cutree_rows = 2,
         cutree_cols = 2,
         cellwidth = 100,
         cellheight = 0.2,
         color = data_col(10))
ht
