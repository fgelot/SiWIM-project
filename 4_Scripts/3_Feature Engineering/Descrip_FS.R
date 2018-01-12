setwd("~/GitHub/SiWIM-project/4_Scripts/3_Descriptive  Analysis") # Mettre à jour (repertoire de travail)

datN <- read.csv("../../2_Data/2_Retraitees/SiWIM_data_formated.csv",sep=",")
dim(datN)
nrow(datN)
names(datN)
summary(datN)
View(datN)

# Repartition par nombre d'essieux

min(datN$N) # pas de problèmes d'essieux manquants
max(datN$N) # vehiciles avec "trop" d'essiux, essreur de mesure. 
# On peut utiliser comme regle N <= 8

hist(datN$N)
nrow(datN[datN$N > 8,]) # 2843 lignes avec trop d'essieux

# Repartition par poids total en charge

min(datN$WGV) # pas de PTC négatif ....
max(datN$WGV) # 1374.79: c'est trop

hist(datN$WGV) # un seul mode visible pour le momment
hist(datN$WGV,breaks=20)
hist(datN$WGV,breaks=40) # un mode vers 0 qui est discutable

hist(datN$WGV[datN$WGV>30 & datN$WGV<50])

# Repartition par poids par essieu

datN.poids.ess <- subset(datN,select=c(W1,W2,W3,W4,W5,W6,W7,W8,W9,W10,W11,W12,W13,W14,W15,W16))

min(datN.poids.ess) # pas de poids par essieu négatif, mais il y a des NA...
sum(is.na(datN.poids.ess)) # 2 millions de NA, 
# ça doit juste des valeurs non indiquées (au lieu du zéro quand il n'y a pas d'essieu)

datN.poids.ess[1:10,]

datN.poids.ess[is.na(datN.poids.ess)] <- 0
sum(datN.poids.ess == 0) # Encore plus de valeurs :-)

max(datN.poids.ess)

hist(datN.poids.ess$W1)

windows(record = TRUE)
par(mfrow=c(2,2)) # pas beau!
 hist(datN.poids.ess$W1)
 hist(datN.poids.ess$W2)
 hist(datN.poids.ess$W3)
 hist(datN.poids.ess$W4)
 par(mfrow=c(2,2))
 hist(datN.poids.ess$W5)
 hist(datN.poids.ess$W6)
 hist(datN.poids.ess$W7)
 hist(datN.poids.ess$W8)
 par(mfrow=c(2,2)) # que des 0
 hist(datN.poids.ess$W9)
 hist(datN.poids.ess$W10)
 hist(datN.poids.ess$W11)
 hist(datN.poids.ess$W12)
 par(mfrow=c(2,2)) # que des 0
 hist(datN.poids.ess$W13)
 hist(datN.poids.ess$W14)
 hist(datN.poids.ess$W15)
 hist(datN.poids.ess$W16)
 
 # Creation de nouvelles tables
 
 dim(datN)

datN.1 <- datN[datN$WGV!=0,]# enlever les lignes avec que des 0
dim(datN.1) 

max(datN.1$WGV) # Il existe encore des WV trop grands

dat.WGV.too <- datN.1[datN.1$WGV>700,]
dim(dat.WGV.too)
View((dat.WGV.too))

datN.2 <- datN.1[datN.1$WGV<700,]
dim(datN.2) # Par la suite, je vais travailler avec ce tableau

max(datN.2$W2) # 27t sur le deuxieme essieu, c'est peu crédibale quand même

dim(datN.2[datN.2$W2>200,]) #9 lignes avec poids essieu 2 trop grand: admissible

# Regression et clustering avec seulement les poids et dimensions

names(datN.2)
datN.reduit <- datN.2[,c("WGV","W1","W2","W3","W4","W5","W6","W7","W8","A1","A2","A3","A4","A5","A6","A7")]
dim(datN.reduit)

nrow(is.na(datN.reduit)) # il reste des NA qui vont nous embêter par la suite. 
# Pour le moment, je remplace par des 0, à discuter

datN.reduit[is.na(datN.reduit)] <- 0

lm.weights.dim <- lm(WGV~.,datN.reduit)
print(lm.weights.dim) # Nul: WGV = W1 + W2 + ..., et puis c'est tout

lm.weights.dim <- lm(WGV~ A1+A2+A3+A4+A5+A6+A7,datN.reduit)
summary(lm.weights.dim) # pas convaincue, peut-être à faire en fonction des groupes de PL

# Determine number of clusters

wss <- (nrow(datN.reduit)-1)*sum(apply(datN.reduit,2,var))
for (i in 2:15) 
  wss[i] <- sum(kmeans(datN.reduit, centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

# K-Means
fit <- kmeans(datN.reduit, 5) # 5 cluster solution

aggregate(datN.reduit,by=list(fit$cluster),FUN=mean) # moyenne des cluster

datN.reduit <- data.frame(datN.reduit, fit$cluster) # ajout du cluster dans la table 
