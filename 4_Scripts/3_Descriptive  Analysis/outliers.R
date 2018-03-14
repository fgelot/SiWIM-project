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

ggplot(don,aes(x=outlier,y=Reduced_chi_squared, colour=Warning_flags))+
   geom_boxplot()

ggplot(don, aes(y=Warning_flags, x=Reduced_chi_squared, group=outlier)) +
  geom_point(aes(shape=outlier, color=outlier))

# Idem pour le poids sur essieu 2

don$outlier <- check_outlier(don$M2)
ggplot(don, aes(y=Warning_flags, x=Reduced_chi_squared, group=outlier)) +
  geom_point(aes(shape=outlier, color=outlier))

# Idem pour le poids sur vitesse

don$outlier <- check_outlier(don$Vitesse)
ggplot(don, aes(y=Warning_flags, x=Reduced_chi_squared, group=outlier)) +
  geom_point(aes(shape=outlier, color=outlier))


# Approche multivariee:

# recherche d'un modËle 
# puis: les observations avec une distance de Cook > 4 sont considÈrÈs come influents
# outliers: ÈlÈments avec distance de Cook > 4 fois la distance moyenne

# Recherche du "meilleur" modËle

train <- don[,c("A1","A2","A3","A4","A5","A6","A7",
                 "M1","M2","M3","M4","M5","M6","M7","M8","N", 
                 "Vitesse", "MGV")]

install.packages("caret",
                 repos = "http://cran.r-project.org", 
                 dependencies = c("Depends", "Imports", "Suggests"))
library("caret")

train <- don[,c("A1","A2","A3","A4","A5","A6","A7",
                "M1","M2","M3","M4","M5","M6","M7","M8","N", 
                "Vitesse", "MGV")]

str(train)

sum(is.na(train))


#Imputing missing values using KNN.Also centering and scaling numerical columns
preProcValues <- caret::preProcess(train, method = c("knnImpute","center","scale"))

library('RANN')
train_processed <- predict(preProcValues, train)
sum(is.na(train_processed))

str(train_processed)

#Spliting training set into two parts based on outcome: 75% and 25%
index <- createDataPartition(train_processed$WGV, p=0.75, list=FALSE)
trainSet <- train_processed[ index,]
testSet <- train_processed[-index,]

#Checking the structure of trainSet
str(trainSet)

#Feature selection using rfe in caret
control <- rfeControl(functions = rfFuncs,
                      method = "repeatedcv",
                      repeats = 3,
                      verbose = FALSE)
outcomeName<-'WGV'
predictors<-names(trainSet)[!names(trainSet) %in% outcomeName]
Loan_Pred_Profile <- rfe(trainSet[,predictors], trainSet[,outcomeName],
                         rfeControl = control)
Loan_Pred_Profile

names(getModelInfo())

fitControl <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 5)

model_glm<-train(trainSet[,predictors],trainSet[,outcomeName],method='gbm',trControl=fitControl,tuneLength=10)
print(model_glm)
plot(model_glm)

model_rqlasso<-train(trainSet[,predictors],trainSet[,outcomeName],method='rqlasso',
                     ,trControl=fitControl,tuneLength=10)
print(model_rqlasso)
plot(model_rqlasso)

model_enet<-train(trainSet[,predictors],trainSet[,outcomeName],method='enet',
                  ,trControl=fitControl,tuneLength=10)*print(model_glm)
print(model_enet)
plot(model_enet)

model_ridge<-train(trainSet[,predictors],trainSet[,outcomeName],method='ridge',
                   ,trControl=fitControl,tuneLength=10)
print(model_ridge)
plot(model_ridge)

model_rf<-train(trainSet[,predictors],trainSet[,outcomeName],method='rf',
                ,trControl=fitControl,tuneLength=10) # random Forest
print(model_rf)
plot(model_rf)

model_gbm<-train(trainSet[,predictors],trainSet[,outcomeName],method='gbm',
                 ,trControl=fitControl,tuneLength=10) #Stochastic Gradient Boosting
print(model_gbm)
plot(model_gbm)

results <- resamples(list(mod.glm=model_glm, mod.rqlasso=model_rqlasso, mod.enet=model_enet,
                          , mod.ridge=model_ridge, mod.rf=model_rf, mod.gbm=model_gbm))
summary(results)
bwplot(results, metric="Spec")
bwplot(results, metric="ROC")
dotplot(results)

cooksd <- cooks.distance(mod.glm)

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

