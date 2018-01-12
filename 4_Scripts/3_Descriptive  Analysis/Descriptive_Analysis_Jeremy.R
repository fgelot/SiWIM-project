
# purge de l'environnement
rm(list = ls())

# chargement des bibliothèques
library(data.table)
library(stringr) # manipulation de chaine de caracteres
library(ggplot2)
library(corrplot)  # affichage matrice des correlations
library(questionr) # pour le calcul du Vcramer
library(FactoMineR) # analyses factorielles  
library(factoextra) # representation sympa analyses factorielles

# import des données
don <- read.table("2_Data/2_Retraitees/SiWIM_data_prepared.csv",sep = ",",header = TRUE)
don <- data.table(don)

# on retire certaines variables inutiles car converties en d'autres plus utiles
don [, c("v","WGV", "W1","W2","W3","W4","W5","W6","W7","W8","W9","W10",
          "W11","W12","W13","W14","W15","W16") := NULL]

# creation de variables date et d'horaire
don[, Annee := format(as.Date(don$Date), format = "%Y")]
don[, Mois1 := format(as.Date(don$Date), format = "%m")]
don[, Mois2 := format(as.Date(don$Date), format = "%B")]
don[, Jour1 := format(as.Date(don$Date), format = "%d")]
don[, Jour2 := format(as.Date(don$Date), format = "%A")]
don[, Heure := str_sub(Horaire, 1, 2)]

# création du champs Anomalie (1 : présence Anomalie, 0 : absence Anomalie)
# [idee] Peut on separer ce qui est une anomalie du au relevé du système (bruit) d'une anomalie du poids lourd (ex : surchargé)
#don[Warning_flags != "00000000",Anomalie := 1]
don[,Anomalie := 0][Warning_flags != "00000000",Anomalie := 1]

# on convertit en factor "Subclass_ID" et "Axle_groups", "Mois2, "Jour2" et "Heure"
don[, Subclass_ID := factor(don$Subclass_ID)]
don[, Axle_groups := factor(don$Axle_groups)]
don[, Mois2 := factor(don$Mois2)]
don[, Jour2 := factor(don$Jour2)]
don[, Heure := factor(don$Heure)]
don[, Anomalie := factor(don$Anomalie)]

# on cree deux jeux de donnees contenant var qualitatives et quantitatives pour les futurs traitements
varquali <- don[,c("X","Timestamp","Horaire","Annee","Mois1","Mois2","Jour1","Jour2","Heure","Site_ID","Warning_flags","Lane","Subclass_ID",
                    "Axle_groups","Date","Anomalie")]

varquanti <- don[,names(varquali)[-1] := NULL]

# analyse descriptives

# Vue d'ensemble des données
summary(varquali)
summary(varquanti)

# De nombreuses données manquantes sont présentes sur les variables A1 à A16 représentant les distances entre essieux
# Si pas d'essieux, alors les variables Ai prennent comme valeur "NA". A voir si il est nécessaire d'imputer la valeur "0" 
# Idem pour le poids de chaque camion. Les variables Mi comportent de nombreuses données manquantes, car il n'y a pas d'essieu correspondant et donc de poids associé.
# A voir si on impute la valeur "0" pour les "NA"

# 9 valeurs manquantes sur les groupes d'essieux
# Conclusion : la qualité du jeu de donnée est bonne du point de vue des valeurs manquantes



####################### Analyse unidimensionnelle : variable qualitative

# Mois de circulation
freq(varquali$Mois2, sort = "dec") 
# on note un léger pic l'été

# jour de circulation
freq(varquali$Jour2, sort = "dec")
# Les jours de circulation sont équilibrés (hors Samedi et Dimanche). On constate quelques circulations les Samedi et Dimanche : légal ?

# heure de circulation
freq(varquali$Heure, sort = "dec")
# Les camions semblent moins circuler de 20h à 5h (donc la nuit) qu'en journée

# voie 
freq(varquali$Lane) # Lors de la capture des données 91% des camions ont roulé à droite. Les 9% ayant roulé à gauche étaient t ils en dépassement ?

# Warning flags
freq(varquali$Warning_flags,sort = "dec") # 71% n'ont pas donné lieu à une anomalie (warning flags), il y a donc eu 29 % d'anomalie

# analyse de la répartition des anomalies relevées par le système (on exclue "00000000")
freq(varquali$Warning_flags, exclude = c("00000000"), sort = "dec")

# Les anomalies les plus fréquentes sont donc :
# 00008000 : vehicle reconstructed
# 00800000 : vehicle overloaded
# 00000001 : multiple truck presence
# 00000080 : negative axle loads or WGV
# 00000081 : ?
# 00000200 : vehicle reclassified

# Subclass_ID (classification réalisée par le système de pesage)
freq(varquali$Subclass_ID, sort = "dec")

# Les deux classifications les plus fréquentes réalisées par le systèmes sont 113 et 61 (50% à elles deux)

# Groupe d'essieux (constatés)
freq(varquali$Axle_groups, sort = "dec")

# Seul 113 ressort comme sur la classification réalisée par le système

# [Idée] : il pourrait etre intéressant de considérer comme un label (Y) le groupe d'essieux, et 
# de réaliser une classification en la comparant à celle réalisée pour le système (Subclass Id) 
# à titre de comparaison  

# [A FAIRE] création d'une variable anomalie (binaire 0 = pas d'anomalie, 1 = anomalie)
# regrouper éventuellement les anomalies

####################### Analyse unidimensionnelle : variable quantitative

# N
boxplot(varquanti$N) # camions à 16 essieux == individus aberrants ?

# total_axle_dist (donne par approximation la longueur du camion)
boxplot(varquanti$total_axle_dist) # camions de 80m == individus aberrants ?

# la mediane est à 12 m ce qui représente la longueur maximum d'un véhicule isolé

# Temperature utilisée par la compensation
boxplot(varquanti$T) # il ya clairement des valeurs aberrantes (température de -273°C)
T <- sort(boxplot(varquanti$T)$out)
length(which (varquanti$T == -273)) # 2014 valeurs de -273°C ! 
# L'observation des indices semblent montrer une continuité dans la captation de cette valeur aberrante
# [IDEE] imputer une valeur moyenne

# Vitesse
boxplot(varquanti$Vitesse) # plusieurs vitesses apparaissent excessives (peut on rouler a plus de 130 km/h avec un camion ?)
# la vitesse mediane est autour de 70km/h

# Masse du camion
boxplot(varquanti$MGV) # plusieurs valeurs semblent aberrantes. A partir de quelle masse considère t on la valeur comme anormale ?
# Mediane a 19 tonnes


####################### Analyse bi dimensionnelle (étude des liaisons)

# Croisement heure  et jour de passage
table(varquali$Jour2,varquali$Heure)
plot(table(varquali$Jour2,varquali$Heure))
# en résumé : les camions circulent peu le Samedi et le Dimanche et nettement moins la nuit

# liaisons entre variables qualitatives (calcul du V de Cramer)
varquali2 <- varquali[,c("Mois2","Jour2","Heure","Warning_flags","Lane","Subclass_ID","Axle_groups","Anomalie")]
varquali2 <- as.data.frame(varquali2)
cramer <- matrix(NA, ncol(varquali2), ncol(varquali2))
for (i in (1:ncol(varquali2))){
  for (j in (1:ncol(varquali2))){
    cramer[i,j] <- cramer.v(table(varquali2[,i], varquali2[,j]))
  }
}
colnames(cramer) <- colnames(varquali2)
rownames(cramer) <- colnames(varquali2)

corrplot(cramer, method="number")

# [Analyse] il semble y avoir un lien entre la voie (lane) et le mois de circulation. Attention car les données ont été récupérées sur 5 mois.
# Même chose pour l'heure mais dans une moindre mesure
# Le Vcramer est important entre SubClasss_ID et Axle_Group ce qui parait logique
# Il semble également y avoir un lien entre l'anomalie (Warning flag et le fait de rouler a gauche ou a droite)
# Anomalie est liée parfaitement avec Warning flag (ce qui etait attendu), et fortement avec "SubClass ID" et "Axle_Group"

# liaisons entre variables quantitatives 

# on se concentre sur les variables quanti utiles
varquanti2 <- varquanti[,c("X","N", "total_axle_dist","T","Reduced_chi_squared","Time_num","Vitesse","MGV")]

# (calcul du coefficient de corrélation de pearson)
corr <- cor(varquanti2)
corrplot(corr, method="number")

# Analyse : 
# La distance totale entre les essieux est corrélée linéairement au nombre d'essieux
# Une corrélation négative, plutot forte, entre vitesse et masse (plus le poids lourd est massique, moins il va vite)
# Une corrélation entre le nombre d'essieux et la masse du camion 
# Une faible corrélation entre masse et nombre d'essieux et entre vitesse et nombre d'essieux (corrélation négative)


# liaisons quanti / quali (anova / kruskall wallis)

# Jeu de données final recombinant var quanti et quali
don2 = cbind(varquali2,varquanti2)

# Température / Ano
bp1 <- ggplot(don2, aes(don2$Anomalie, y=don2$T, group=don2$Anomalie)) + 
  geom_boxplot(aes(fill=don2$Anomalie))
bp1

# Nb d'essieux / Ano
bp2 <- ggplot(don2, aes(don2$Anomalie, y=don2$N, group=don2$Anomalie)) + 
  geom_boxplot(aes(fill=don2$Anomalie))
bp2

# Vitesse / Ano
bp3 <- ggplot(don2, aes(don2$Anomalie, y=don2$Vitesse, group=don2$Anomalie)) + 
  geom_boxplot(aes(fill=don2$Anomalie))
bp3

# Masse / Ano
bp4 <- ggplot(don2, aes(don2$Anomalie, y=don2$MGV, group=don2$Anomalie)) + 
  geom_boxplot(aes(fill=don2$Anomalie))
bp4
# [Analyse] Les camions les plus lourds semblent subir moins d'anomalie. 

###################### Analyses Factorielles

# Analyse en composantes principales sur variables quantitatives 
res_pca <- PCA(don2[,10:16], scale.unit = TRUE, ncp = 5)
summary(res_pca)
#plot(res_pca)
# les 7 premières composantes principales capturent 100% del'inertie
# les 4 premières 79% et les 5 premières 92%


# Analyse des correspondances multiples (ACM) sur variables qualitatives
#res_mca <- MCA(varquali2[,c("Mois2","Jour2","Heure","Lane","Anomalie","Warning")])
#summary(res_mca)
# [ATTENTION] : tres couteux en temps de calcul => reflechir à l'opportunité d'intégrer des variables qualitatives

# Analyse factorielle des données mixtes
# permet de réaliser une analyse factorielle sur var quali et quanti

##################### Clustering
# CAH (# Classification ascendante hiérarchique)

# Kmeans
# Kmeans sur les 5 premières composantes principales (donc clustering sur la base des variables quantitatives)
groupe_kmeans <- kmeans (res_pca$ind$coord, centers = 7, nstart = 4 )
print(groupe_kmeans)





