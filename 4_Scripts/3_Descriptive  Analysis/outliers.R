install.packages("data.table")
install.packages("dplyr")
install.packages("caret",
                 repos = "http://cran.r-project.org", 
                 dependencies = c("Depends", "Imports", "Suggests"))
install.packages("ggplot2")

library(data.table)
library(caret)
library(ggplot2)
library(dplyr)

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
don[is.nan.data.frame(don)] <- 0

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

# Idem pour la vitesse

don$outlier <- check_outlier(don$Vitesse)
ggplot(don, aes(y=Warning_flags, x=Reduced_chi_squared, group=outlier)) +
  geom_point(aes(shape=outlier, color=outlier))


# Approche multivariee:

# recherche d'un modèle 
# puis: les observations avec une distance de Cook > 4 sont considérés come influents
# outliers: éléments avec distance de Cook > 4 fois la distance moyenne

# Recherche du "meilleur" modèle

ptm <- proc.time()

train <- don[1:1000,c("A1","A2","A3","A4","A5","A6","A7",
                 "M1","M2","M3","M4","M5","M6","M7","M8","N", 
                 "Vitesse", "MGV", "Reduced_chi_squared")]

str(train)

sum(is.na(train))


#Imputing missing values using KNN.Also centering and scaling numerical columns
preProcValues <- caret::preProcess(train, method = c("knnImpute","center","scale"))
#train[is.nan(train)] <- 0
sum(is.na(train))


library('RANN')
train_processed <- predict(preProcValues, train)
sum(is.na(train_processed))

str(train_processed)

#Spliting training set into two parts based on outcome: 75% and 25%
index <- createDataPartition(train_processed$MGV, p=0.75, list=FALSE)
trainSet <- train_processed[ index,]
testSet <- train_processed[-index,]

#Checking the structure of trainSet
str(trainSet)

#Feature selection using rfe in caret
control <- rfeControl(functions = rfFuncs,
                      method = "repeatedcv",
                      repeats = 3,
                      verbose = FALSE)
outcomeName<-'MGV'
predictors<-names(trainSet)[!names(trainSet) %in% outcomeName]
MGV_Pred_Profile <- rfe(trainSet[,predictors], trainSet[,outcomeName],
                         rfeControl = control)
MGV_Pred_Profile

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

model_rf<-train(trainSet[,predictors],trainSet[,outcomeName],method='rf',
                ,trControl=fitControl,tuneLength=10) # random Forest
print(model_rf)
plot(model_rf)

model_gbm<-train(trainSet[,predictors],trainSet[,outcomeName],method='gbm',
                 ,trControl=fitControl,tuneLength=10) #Stochastic Gradient Boosting
print(model_gbm)
plot(model_gbm)

results <- resamples(list(mod.glm=model_glm, 
                          mod.rqlasso=model_rqlasso, 
                          mod.rf=model_rf,
                          mod.rf=model_rf, mod.gbm=model_gbm))
results$values
summary(results)
bwplot(results)
bwplot(results)
dotplot(results)

CPU_time <- proc.time() - ptm
print(CPU_time)

# Avec glm et les donnees entieres

don <- read.csv('../../2_Data/2_Retraitees/SiWIM_data_after_input_clusters.csv',header=T)
train <- don[,c("Warning_flags","A1","A2","A3","A4","A5","A6","A7",
                      "M1","M2","M3","M4","M5","M6","M7","M8","N", 
                      "Vitesse", "MGV", "Reduced_chi_squared")]

model_glm_glm <- glm(MGV~., data=train, family="gaussian")

# Avec la distance de Cook

cooksd <- cooks.distance(model_glm_glm)

plot(cooksd, pch="*", cex=2, 
     main="Observations influentes, avec la distance de Cook") 
abline(h = 4*mean(cooksd, na.rm=T), col="red")  
text(x=1:length(cooksd)+1, y=cooksd, 
     labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), 
     col="red") 

influential <- names(cooksd)[(cooksd > 4*mean(cooksd, na.rm=T))] # Lignes influentes
head(train[influential, ])

train$outlier <- "FALSE"
train[influential,"outlier"] <- "TRUE"

ggplot(train, aes(y=Warning_flags, x=Reduced_chi_squared, group=outlier)) +
  geom_point(aes(shape=outlier, color=outlier))

# Avec les résidus

resid_glm <- tail(resid(model_glm_glm),n=1000)

plot(resid_glm, pch="*", cex=2, 
     main="Observations influentes, avec les résidus") 

influential <- as.numeric(names(resid_glm)) # Lignes influentes
head(train[influential, ])

train$outlier <- "FALSE"
train[influential,"outlier"] <- "TRUE"

ggplot(train, aes(y=Warning_flags, x=Reduced_chi_squared, group=outlier)) +
  geom_point(aes(shape=outlier, color=outlier))

# Avec les hat values

hat_values <- tail(hatvalues(model_glm_glm),n=1000)

plot(hat_values, pch="*", cex=2, 
     main="Observations influentes, avec les hat values") 

influential <- as.numeric(names(hat_values)) # Lignes influentes
head(train[influential, ])

train$outlier <- "FALSE"
train[influential,"outlier"] <- "TRUE"

ggplot(train, aes(y=Warning_flags, x=Reduced_chi_squared, group=outlier)) +
  geom_point(aes(shape=outlier, color=outlier))

# Avec la loi r-student

r_stud <- tail(rstudent(model_glm_glm),n=1000)

plot(r_stud, pch="*", cex=2, 
     main="Observations influentes, avec les résidus") 

influential <- as.numeric(names(r_stud)) # Lignes influentes
head(train[influential, ])

train$outlier <- "FALSE"
train[influential,"outlier"] <- "TRUE"

ggplot(train, aes(y=Warning_flags, x=Reduced_chi_squared, group=outlier)) +
  geom_point(aes(shape=outlier, color=outlier))

# Avec le package car

library(car)

outs <- influencePlot(model_glm_glm)

n <- 1000
Cooksdist <- as.numeric(tail(row.names(outs[order(outs$CookD), ]), n))
Lev <- as.numeric(tail(row.names(outs[order(outs$Hat), ]), n))
StdRes <- as.numeric(tail(row.names(outs[order(outs$StudRes), ]), n))

plot(train$Reduced_chi_squared, train$Warning_flags)
points(train$Reduced_chi_squared[Cooksdist], train$Warning_flags[Cooksdist], col = "red", pch = 0, lwd = 15)
points(train$Reduced_chi_squared[Lev], train$Warning_flags[Lev], col = "blue", pch = 25, lwd = 8)
points(train$Reduced_chi_squared[StdRes], train$Warning_flags[StdRes], col = "green", pch = 20, lwd = 5)
