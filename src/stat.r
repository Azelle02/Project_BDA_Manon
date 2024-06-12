## Petite analyse stat des données 

df <- matrix_full_elev_eco_clim_ndvi                # sélection de ma matrice 
df <- na.omit(df)                                   # enlever toutes les lignes qui contiennent des NA

df_continous <- df[,colnames(df) %in% c("vec_mean_temp","elevation","NDVI","vec_mean_pre")] # sélection des variable continues
df_discrete <- df[,!(colnames(df) %in% c("vec_mean_temp","elevation","NDVI","vec_mean_pre"))] # pour faire l'inverse, prendre toutes les variables discrètes

df_continous <- apply(df_continous,2,as.numeric) # transformer tous les éléments des colonnes en valeur numérique

#### Corrélation entre les variables continues_______________________________________________________________

data_stat  <-  df_continous
data_stat  <- as.data.frame(data_stat)

# Température et Précipitation
P <- ggplot(data = data_stat, mapping = aes(x = vec_mean_temp, y = vec_mean_pre))   # scatter plot avec ligne de régression
P + geom_point(shape = 18) + 
  geom_smooth(method = "lm", se = FALSE)

cor.test(vec_mean_temp, vec_mean_pre, data = data_stat) # test de corrélation de pearson
# la corrélation entre les deux variable est significatement négative (p-value < 2.2e-16, cor = -0.6031371)


# Température et Elevation
P1 <- ggplot(data = data_stat, mapping = aes(x = vec_mean_temp, y = elevation))   # scatter plot avec ligne de régression
P1 + geom_point(shape = 18) + 
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Temperature", y = "Elevation")

cor.test(data_stat$vec_mean_temp, data_stat$elevation)  # test de corrélation
# la corrélation entre les deux variable est significativement positive (p-value < 2.2e-16, cor = 0.2349002)


#### Modèle linéaire____________________________________________________________________________________

df_lm <- matrix_full_elev_eco_clim_ndvi                # sélection de ma matrice 
df_lm <- na.omit(df_lm) 

# Modèle
linear_model <- lm(vec_mean_temp ~ vec_mean_pre + elevation + NDVI + country + Landcover, data = df_lm)
summary(linear_model)
# effet significatif des variables vec_mean_pre et elevation sur la variable des température. 
# Petit effet du pays
# Effet du Landcover, avec Grassland, Settlement et Shrubland qui ont un effet sifnificatif sur les températures. 

anova(linear_model)
# la variabilité des données des températures est expliquée significativement par les précipitation, l'élévation, le pays et le landcover. 


#### Analyse des facteurs____________________________________________________________________________________

library(emmeans)

# Use the original dataset for factor analysis
df_fa <- matrix_full_elev_eco_clim_ndvi

df_fa <- df_fa[!is.na(df_fa$Landcover),] # On enlève les lignes qui ont des NA pour la colonne Landcover

# Box plot de la temperature en fonction des landcovers
P_fact <- ggplot(data = df_fa, mapping = aes(x = Landcover, y = vec_mean_temp, fill = Landcover))

P_fact <- P_fact + geom_boxplot(varwidth = TRUE, outlier.shape = NA) +  # Change boxplot width 
  geom_jitter(alpha = 0.2, size = 2, width = 0.1)  # Add points and spread them

print(P_fact)  

library(plotly)
# graphique interactif
# ggplotly(P_fact) # rien ne s'affiche sur mon ordinateur, je suppose qu'il est trop lent....

# Modèle linéaire avec landcover comme facteur explicatif
linear_model2 <- lm(vec_mean_temp ~ Landcover, data = df_fa)

anova(linear_model2) # Landcover ont un effet significtif sur les températures

# Test post-hoc avec un ajustement de Tukey
library(emmeans)
em <- emmeans(linear_model2, list(pairwise ~ Landcover), adjust = "tukey")
print(em)
# nous indique si les températures des différents landcovers sont significativement différentes d'un landcover à l'autre. 