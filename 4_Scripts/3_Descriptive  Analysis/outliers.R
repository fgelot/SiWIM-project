install.packages("data.table")
library(data.table)

install.packages("dplyr")
library(dplyr)

install.packages("ggplot2")
library(ggplot2)

setwd("~/GitHub/SiWIM-project/4_Scripts/3_Descriptive  Analysis")

don <- read.csv('../../2_Data/2_Retraitees/SiWIM_data_after_input_clusters.csv',header=T)
dim(don)

names(don)

# Anomalies/outliers en termes de poids et dimensions
# Donc etape 1: creation de la table correspondante

don <- don[,c("Warning_flags","A1","A2","A3","A4","A5","A6","A7",
                      "M1","M2","M3","M4","M5","M6","M7","M8","N", 
                      "Reduced_chi_squared", "Vitesse", "MGV", "clusters_kmeans")]

dim(don)

is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))
don[is.nan(don)] <- 0

# Approche univariee: poids total 
# en dehors de 1.5 IQR, avec IQR: Inter Quartile Range: difference entre 75th et 25th quantiles

# Sorties possibles:
# stats	a vector of length 5, containing the extreme of the lower whisker, the lower 'hinge', the median, the upper 'hinge' and the extreme of the upper whisker.
# n	the number of non-NA observations in the sample.
# conf	the lower and upper extremes of the 'notch' (if(do.conf)). See the details.
# out	the values of any data points which lie beyond the extremes of the whiskers (if(do.out)).

outlier_values <- boxplot.stats(don$MGV)$out
boxplot(don$MGV, main="Poids total", boxwex=0.1)
length(outlier_values) # 142 outliers

#function that takes in vector of data and a coefficient,
#returns boolean vector if a certain point is an outlier or not
check_outlier <- function(v, coef=1.5){
  quantiles <- quantile(v,probs=c(0.25,0.75))
  IQR <- quantiles[2]-quantiles[1]
  res <- v < (quantiles[1]-coef*IQR)|v > (quantiles[2]+coef*IQR)
  return(res)
}

#apply to our data

don$outlier <- check_outlier(don$MGV)

write.csv(don, file = "../../2_Data/2_Retraitees/SiWIM_data_after_input_clusters_outliers.csv")

ggplot(don,aes(x=MGV,y=Reduced_chi_squared, colour=outlier))+
  geom_boxplot()+
  geom_text(aes(label=Warning_flags),hjust=-0.3)

#plot
ggplot(don,aes(x=outlier,y=Reduced_chi_squared, colour=Warning_flags))+
   geom_boxplot()

ggplot(don, aes(y=Warning_flags, x=Reduced_chi_squared, group=outlier)) +
  geom_point(aes(shape=outlier, color=outlier))

# mtext(paste("Outliers: ", paste(outlier_values, collapse=", ")), cex=0.6)
# pour ecrire les valeurs des outliers, mais ici il y en a 314: trop

# Par exemple, pour que les moutaches soit Ègales ‡ 10 fois la longueur de la boite

outlier_values_ext <- boxplot.stats(donPoidsDim$WGV, coef=10)$out
boxplot(donPoidsDim$WGV, main="Poids total", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier_values_ext, collapse=", ")), cex=0.6)

# Idem pour le poids sur essieu 2

outlier_values2 <- boxplot.stats(donPoidsDim$W2)$out
boxplot(donPoidsDim$W2, main="Poids 2", boxwex=0.1)

# Idem pour le poids sur essieu 1

outlier_values1 <- boxplot.stats(donPoidsDim$W1)$out
boxplot(donPoidsDim$W1, main="Poids 1", boxwex=0.1) # 38000 vÈhicules

boundaries <- seq(-1, max(donPoidsDim$W1)+1, by=10)
hist(donPoidsDim$W1, breaks=boundaries) # ProblËme des valeurs aberrantes

# Approche bivariee: poids total ~longueur totale

boxplot(WGV ~ 0+A1+A2+A3+A4+A5+A6+A7+A8+A9+A10+A11+A12+A13+A14+A15, 
        data=donPoidsDim, main="Poids total en fonction de la longueur totale")


boxplot(WGV ~ 0+A1+A2+A3+A4+A5+A6+A7+A8+A9+A10+A11+A12+A13+A14+A15, 
        data=donPoidsDim[1:10,], main="Poids total en fonction de la longueur totale")

boxplot(WGV ~ cut(A1+A2+A3+A4+A5+A6+A7+A8+A9+A10+A11+A12+A13+A14+A15, 
                  pretty(donPoidsDim$A2)), data=donPoidsDim[1:1000], 
        main="Poids total en fonction de la longueur totale", cex.axis=0.5)

# Approce multivari√©e
# Avec la distance de Cook
# Outliers : distance > √† 4 fois la distance moyenne

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
# fonction outlier

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
# Imputation: par exemple moyenne, m√©diane, ...
# LImitation: en-dehors de 1.5 IQR
# Pr√©diction des NA

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

