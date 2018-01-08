
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
library(missMDA) # imputation des données manquantes
library(fpc) # pour appeler dbscan
library(dbscan) # pour appeler hdbscan

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


###################### Gestion des données aberrantes

# On travaille sur le tableau simplifié quantitatif
# Idée générale : attribuer aux données aberrantes (dont nous sommes surs qu'elles soient fausses) un valeur "NA"
# puis reconstituer leur valeur grasse aux techniques d'imputation du package "missMDA"

# on considère comme valeur anormale au dessus de 8 essieux (ici 2843 valeurs dans ce cas)
index_aberrant_N <- which(varquanti2[,"N"] > 8) 
varquanti2[index_aberrant_N,"N"] <- NA

# on considère comme valeur anormale une distance totale entre les axes supérieure à 20 m(ici 5102 valeurs dans ce cas)
index_aberrant_total_axle_dist <- which(varquanti2[,"total_axle_dist"] > 20)
varquanti2[index_aberrant_total_axle_dist,"total_axle_dist"] <- NA

# on considère comme valeur anormale les températures négatives < à -10°C (dans le jeu de données seule -273 est présente comme valeur négative, 2014 valeurs dans ce cas)
index_aberrant_T <- which(varquanti2[,"T"] < 0)
varquanti2[index_aberrant_T,"T"] <- NA

# on considère comme valeur anormale la vitesse d'un camion > 130 km/h (19 valeurs dans ce cas)
index_aberrant_Vitesse <- which(varquanti2[,"Vitesse"] > 130)
varquanti2[index_aberrant_Vitesse,"Vitesse"] <- NA

# on considère comme valeur anormale la masse d'un camion > 70 T (22 valeurs dans ce cas)
index_aberrant_MGV <- which(varquanti2[,"MGV"] > 70)
varquanti2[index_aberrant_MGV,"MGV"] <- NA

# on cherche à présent à imputer les données manquantes en utilisant l'ACP 

# recherche par validation croisée du nombre d'axes optimal pour l'ACP
nb_axe_imput <- estim_ncpPCA(varquanti2[,-1]) # on retire X qui est un index # attention très long on peut utiliser l'option method.cv = "Kfold" si nécesaire

# on retient 4 axes car cela minimise le critère MSEP (mean squared error of prediction)

# imputation des données manquantes par ACP, avec 4 axes
# l'imputation se base sur les corrélations entre variables et les ressemblances entre individus
# on utilise une version régularisée de l'algo pour éviter le surajustement

varquanti3 <- imputePCA(varquanti2,ncp = 4, method ="regularized")
varquanti3 <- as.data.frame(varquanti3$completeObs)

###################### boxplot avec valeurs aberrantes
summary(varquanti2)
summary(varquanti3) # les variables ont été imputées

boxplot(varquanti2$N) 
boxplot(varquanti2$total_axle_dist) 
boxplot(varquanti2$T)
boxplot(varquanti2$Vitesse)
boxplot(varquanti2$MGV) 

#####################

# (calcul du coefficient de corrélation de pearson)
corr <- cor(varquanti3)
corrplot(corr, method="number")

# Analyse : 
# La distance totale entre les essieux est corrélée linéairement au nombre d'essieux
# Une corrélation négative, plutot forte, entre vitesse et masse (plus le poids lourd est massique, moins il va vite)
# Une corrélation entre le nombre d'essieux et la masse du camion 
# Une faible corrélation entre masse et nombre d'essieux et entre vitesse et nombre d'essieux (corrélation négative)

# liaisons quanti / quali (anova / kruskall wallis)

# Jeu de données final recombinant var quanti et quali
don2 = cbind(varquali2,varquanti3)

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
# [Analyse] Les camions les plus lourds comportent moins d'anomalie. 

###################### Analyses Factorielles

# Analyse en composantes principales sur variables quantitatives avec variables qualitatives en illustration (pour l'interprétation)
don_PCA <- cbind(varquanti3[,-1],varquali2$Lane, varquali2$Anomalie)
colnames(don_PCA)[c(8,9)] <- c("Lane","Anomalie")

res_pca <- PCA(don_PCA, scale.unit = TRUE, ncp = Inf, quali.sup = 8:9)
summary(res_pca)
#plot(res_pca)
# les 7 premières composantes principales capturent 100%  de l'inertie (variabilité) du nuage de point initial
# les 4 premières 87% et les 5 premières 92%. Il sera intéressant par exemple de conserver 4 axes car ceux ci capturent l'essentiel de la variabilité
# et évitent d'utiliser 3 axes supplémentaires (couteux et les 13 d'inertie peuvent être assimilés à du bruit)

# affichage du nuage des individus
plot.PCA(res_pca) # il y a trop d'individus à afficher, difficile de voir des groupes se détacher

# affichage du cercle des corrélations sur le premier plan principal résumant environ 59% de l'information
plot(res_pca, choix = "var")

plot(res_pca, choix = "var",axes = 3:4) # affichage du plan des axes 3 et 4

# description automatique des dimensions (pour l'analyse)
dimdesc(res_pca)


# Interprétation du premier plan factoriel grace au cercle des corrélations et à la description auto des dimensions : on observe des oppositions nettes
# le premier axe oppose les camions les plus volumineux (car nombre d'essieux importants, masse importante, et longueur importante) au camion les plus petits, les moins lourds
# Les camions les plus petits sont ceux qui vont le plus vite
# le deuxième axe montre que la température est fortement corrélée négativeement à la variable Time_Num
# Time_num est la variable numérique du temps : ici on montre que plus Time_num est importante, plus la température est basse.
# Ce qui est logique car la prise de mesure s'est déroulée entre l'été et l'hiver !!!

# contribution des individus et des variables à la construction des axes factoriels

# on constate que les variables Nombre d'essieux, total_axle_dist et mgv contrbuent le plus (ctr) à la création de l'axe 1
# la vitesse contribue également bien dans une moindre mesure (2 fois moins)
# pour l'axe 2, sa construction est quasi due aux deux variables T et Time_Num
# la qualité de représentation (le cos2) est très bonne pour N, total_axle_dist. Elle est bonne pour MGV. Vitesse n'a pas représentation excellente
# sur l'axe 2, T et Time_Num sont plus très bien représentées

summary(res_pca)

# représentation sur le premier plan factoriel des 100 individus ayant la plus forte contribution
#plot.PCA(res_pca, select = "contr 50")
#plot.PCA(res_pca, select = "cos2 10")

##################### Clustering

# On réalise un prétraitement par ACP
res_pca2 <- PCA(don_PCA[,1:7],ncp = 4, scale.unit = TRUE) # on garde 4 dimension sur les 7 représentant 87% de l'information
summary(res_pca2)

# dbcan
set.seed(123)
#db<-fpc::dbscan(res_pca2$ind$coord, eps = 0.2, method = "raw") # attention calcul consommant enormement de memoire
db <- dbscan::dbscan(res_pca2$ind$coord, eps = 0.2) # calcul beaucoup plus rapide mais ayant l'air de donner un résultat bizarre

# hdbscan (package dbscan)
#hdb<-dbscan::hdbscan(res_pca2$ind$coord) # attention calcul consommant enormement de memoire

# optics


# CAH mixte
# idée : on réalise un pré-regroupement des individus via kmeans
# le préregroupement est lpont de départ de la création du dendrogramme de la CAH
# nous utilisons un nombre de classes important par kmeas, dans lequel sur de l'homogéneité des individus
# cela permet de réduire la quantité d'individus qui passera de 183928 au nombre de cluster
# afin de ne pas perdre la possibilité d'interpréter, nous relancons ensuite une ACP pondérée par le nombre d'individus de chaque classe

set.seed(123)
obj_kmeans <- kmeans(res_pca2$ind$coord, centers = 100, nstart = 10, iter.max = 40) 
# nstart : execution de plusieurs segmentation en gardant la meilleure
# iter.max : le nombre d'itération maximale pour que l'algo converge
print(obj_kmeans) # (between_SS / total_SS =  93.0 %) == proportion d'intertie expliquée par la partition avec 100
obj_kmeans$iter # donne le nombre d'iteration utilisees pour converger (si convergence)

obj_kmeans_df <- cbind.data.frame(obj_kmeans$size,obj_kmeans$centers)
colnames(obj_kmeans_df)[1] <- "effectifs_classe"

# on réalise de nouveau une acp en prenant bien soin de ne pas centrer réduire et de conserver toutes les composantes
res_pca3 <- PCA(obj_kmeans_df[,2:5],row.w = obj_kmeans_df[,1],scale.unit = FALSE, ncp = Inf)

# a partir du centre des classes et des effectifs on réalise un arbre hiérarchique
# l'arbre correspond ainsi au haut de l'arbre hiérarchique que l'on aurait obtenu à partir de tous les individus (le bas est perdu mais seul le haut de l'arbre est utile pour l'interprétation métier)
res_hcpc <- HCPC(res_pca3$ind$coord,graph=TRUE, order = FALSE, iter.max = 20) # IMPORTANT: order=FALSE pour conserver l'ordre des individus dans le fichier
# attention buggé avec la fonction HCPC de factominer qui déconne cf internet

# autre option avec hclust qui remplace HCPC pour la CAH
# pas de bol, hclust n'est pas compatible avec R 3.3.2 !!!

# CAH mixte directement avec HCPC (si cette fonction marchait on ferait) # attention à bien garder l'ordre == order = FALSE
res_hcpc <- HCPC(res_pca2,graph=TRUE, kk=100, order = FALSE, iter.max = 20)

# acp + kmeans 

# Kmeans 
# l'algorithme de kmeans nécessite de fixer à priori le nombre de classes, et l'initialisation au hazard peut donner des résultats assez variables
# nous pouvons faire varier le nombre de classes et observer le résultat, en optimisant le rapport d'inertie
# nous pouvons lancer Kmeans sur un grand nombre de classes (ex 100) chaque classe 
  

# evaluation de la proportion expliquée 
inertie_expl <- rep(0,times=10)
for (k in 2:10){
  clus <-kmeans(res_pca2$ind$coord, centers = k, nstart = 10, iter.max = 40)
  inertie_expl[k] <- clus$betweenss / clus$totss
} 

plot(1:10,inertie_expl, type = "b", xlab = "Nombre de groupes", ylab = "% d'inertie expliquée par le modèle" )

# à partir de 4 classes le rajout d'un groupe supplémentaire n'augmente pas significativement la part d'inertie expliquée par la partition

# 2e methode - indice de Calinski Harabasz - utilisation du package fpc
library("fpc")

# evaluation des solutions
sol_kmeans <- kmeansruns(res_pca2$ind$coord,krange = 2:10, criterion = "ch")

# graphique 
plot(1:10, sol_kmeans$crit,type = "b", xlab = "Nombre de groupes", ylab = "Silhouette")
# la solution a 2 classes maximise ici le critère 


# on lance plusieurs kmeans
res_kmeans2 <- kmeans(res_pca2$ind$coord, centers = 2, nstart = 10, iter.max = 40) # 31.7 % d'inertie expliquée
res_kmeans3 <- kmeans(res_pca2$ind$coord, centers = 3, nstart = 10, iter.max = 40) # 46.1 % (+15) d'inertie expliquée
res_kmeans4 <- kmeans(res_pca2$ind$coord, centers = 4, nstart = 10, iter.max = 40) # 54.9 % (+13) d'inertie expliquée
res_kmeans5 <- kmeans(res_pca2$ind$coord, centers = 5, nstart = 10, iter.max = 40) # 62,5 % (+8) d'inertie expliquée
res_kmeans6 <- kmeans(res_pca2$ind$coord, centers = 6, nstart = 10, iter.max = 40) # 67.2 % (+5) d'inertie expliquée
res_kmeans7 <- kmeans(res_pca2$ind$coord, centers = 7, nstart = 10, iter.max = 40) # 71.2 % (+4) d'inertie expliquée
res_kmeans8 <- kmeans(res_pca2$ind$coord, centers = 8, nstart = 10, iter.max = 40) # 73.2 % (+2)d'inertie expliquée

# le partitionnement en deux groupes est celui avec le plus grand saut.
# le bon "rapport qualité prix" semble etre le partitionnement à 4 groupes
# quoi qu'il en soit, il faut à présent caractériser et garder le profil de classes correspondant le plus au métier


# représentation des 2 et 4 classes sur le plan factoriel
axes_acp_2groupes <- cbind.data.frame(as.factor(res_kmeans2$cluster), res_pca2$ind$coord) 
colnames(axes_acp_2groupes)[1] <- "cluster"
table(res_kmeans2$cluster)

axes_acp_4groupes <- cbind.data.frame(as.factor(res_kmeans4$cluster), res_pca2$ind$coord) 
colnames(axes_acp_4groupes)[1] <- "cluster"
table(res_kmeans4$cluster)

# nuage initial
ggplot((as.data.frame(res_pca2$ind$coord[,1], res_pca2$ind$coord[,2])), aes(res_pca2$ind$coord[,1], res_pca2$ind$coord[,2])) + geom_point()+ ggtitle("projection des individus en deux groupes sur le plan principal")

# représentation des 2 classes sur le premier plan principal
ggplot(axes_acp_2groupes, aes(Dim.1, Dim.2, color = cluster)) + geom_point() + ggtitle("projection des individus en deux groupes sur le plan principal")

# représentation des deux classes sur le premier plan principal
ggplot(axes_acp_4groupes, aes(Dim.1, Dim.2, color = cluster)) + geom_point() + ggtitle("projection des individus en deux groupes sur le plan principal")


# interprétation des classes
# A faire en utilisant le centre des classes!

# Améliorations
# penser a tester factoinvestigate qui réalise des rapports

# lien web sur factominer et CAH mixte
# HCPC : http://forums.cirad.fr/logiciel-R/viewtopic.php?t=3132
# HCPC : https://groups.google.com/forum/#!topic/factominer-users/lnCW9DAE5z0
# CAH mixte : http://chirouble.univ-lyon2.fr/~ricco/tanagra/fichiers/fr_Tanagra_CAH_Mixte_Gros_Volumes.pdf
# kmeans : https://eric.univ-lyon2.fr/~ricco/cours/didacticiels/R/cah_kmeans_avec_r.pdf

