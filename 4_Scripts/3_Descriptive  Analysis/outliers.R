setwd("~/GitHub/SiWIM-project/4_Scripts/3_Descriptive  Analysis")

don <- read.csv('../../2_Data/1_Sources/SiWIM_data_2017-07-01_to_2018-01-24.csv',header=T)
dim(don)

names(don)

# Anomalies/outliers en termes de poids et dimensions
# Donc etape 1: creation de la table correspondante

donPoidsDim <- don[,c("WGV","W1","W2","W3","W4","W5","W6","W7","W8",
                      "W9","W10","W11","W12","W13","W14","W15","W16",
                      "A1","A2","A3","A4","A5","A6","A7","A8","A9","A10",
                      "A11","A12","A13","A14","A15")]

donPoidsDimAno <- don[,c("Warning_flags","WGV","W1","W2","W3","W4","W5","W6","W7","W8",
                      "W9","W10","W11","W12","W13","W14","W15","W16",
                      "A1","A2","A3","A4","A5","A6","A7","A8","A9","A10",
                      "A11","A12","A13","A14","A15","Reduced_chi_squared")]
dim(donPoidsDim)
dim(donPoidsDimAno)

# Approche univariee: poids total 
# en dehors de 1.5 IQR, avec IQR: Inter Quartile Range: difference entre 75th et 25th quantiles

# Sorties possibles:
# stats	a vector of length 5, containing the extreme of the lower whisker, the lower 'hinge', the median, the upper 'hinge' and the extreme of the upper whisker.
# n	the number of non-NA observations in the sample.
# conf	the lower and upper extremes of the 'notch' (if(do.conf)). See the details.
# out	the values of any data points which lie beyond the extremes of the whiskers (if(do.out)).

outlier_values <- boxplot.stats(donPoidsDim$WGV)$out
boxplot(donPoidsDim$WGV, main="Poids total", boxwex=0.1)
length(outlier_values) 

outlier_values <- boxplot.stats(donPoidsDim$WGV, coeff=1.0)$out

# mtext(paste("Outliers: ", paste(outlier_values, collapse=", ")), cex=0.6)
# pour ecrire les valeurs des outliers, mais ici il y en a 314: trop

# Par exemple, pour que les moutaches soit �gales � 10 fois la longueur de la boite

outlier_values_ext <- boxplot.stats(donPoidsDim$WGV, coef=10)$out
boxplot(donPoidsDim$WGV, main="Poids total", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier_values_ext, collapse=", ")), cex=0.6)

# Idem pour le poids sur essieu 2

outlier_values2 <- boxplot.stats(donPoidsDim$W2)$out
boxplot(donPoidsDim$W2, main="Poids 2", boxwex=0.1)

# Idem pour le poids sur essieu 1

outlier_values1 <- boxplot.stats(donPoidsDim$W1)$out
boxplot(donPoidsDim$W1, main="Poids 1", boxwex=0.1) # 38000 v�hicules

boundaries <- seq(-1, max(donPoidsDim$W1)+1, by=10)
hist(donPoidsDim$W1, breaks=boundaries) # Probl�me des valeurs aberrantes

# Approche bivariee: poids total ~longueur totale

boxplot(WGV ~ A1+A2+A3+A4+A5+A6+A7+A8+A9+A10+A11+A12+A13+A14+A15, 
        data=donPoidsDim, main="Poids total en fonction de la longueur totale")
# pas assez de mémoire

boxplot(WGV ~ A1+A2+A3+A4+A5+A6+A7+A8+A9+A10+A11+A12+A13+A14+A15, 
        data=donPoidsDim[1:10,], main="Poids total en fonction de la longueur totale")

boxplot(WGV ~ cut(A1+A2+A3+A4+A5+A6+A7+A8+A9+A10+A11+A12+A13+A14+A15, 
                  pretty(donPoidsDim$A2)), data=donPoidsDim[1:1000], 
        main="Poids total en fonction de la longueur totale", cex.axis=0.5)

# Approce multivariée
# Avec la distance de Cook
# Outliers : distance > à 4 fois la distance moyenne

mod <- lm(WGV ~ ., data=donPoidsDim)
cooksd <- cooks.distance(mod)

plot(cooksd, pch="*", cex=2, 
     main="Observations influentes, avec la distance de Cook") 
abline(h = 4*mean(cooksd, na.rm=T), col="red")  
text(x=1:length(cooksd)+1, y=cooksd, 
     labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), 
     col="red") 

influential <- as.numeric(names(cooksd)[(cooksd > 4*mean(cooksd, na.rm=T))]) # Lignes influentes
head(donPoidsDim[influential, ])

# car package
# fonction outlier, basé sur un modèle
# Par exemple, le modèle mod ci-dessus

install.packages("car")
library(car)

car::outlierTest(mod)

# Package outliers

install.packages("outliers")
library(outliers)

outliers(donPoidsDim)

scores(donPoidsDim, type="chisq", prob=0.9)
scores(donPoidsDim, type="chisq", prob=0.95)

scores(donPoidsDim, type="z", prob=0.95)

scores(donPoidsDim, type="t", prob=0.95)

# Traitement des outliers
# Imputation: par exemple moyenne, médiane, ...
# LImitation: en-dehors de 1.5 IQR
# Prédiction des NA

# Ici exemple de la limitation

x <- donPoidsDim$WGV
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]

# Avec le package outliers

install.packages("outliers")
library(outliers)

