---
title: "Outliers_car"
output: html_document
html_document:
    theme: united
    df_print: paged
    toc: yes
---

```{r setup, include=FALSE}
# Configuration locale du chemin du projet SIWIM :
knitr::opts_knit$set(root.dir = normalizePath("C:/Users/schmidt/Documents/GitHub/SiWIM-project/")) 
options(encoding = 'UTF-8')
```

# Avec le package car

Le package car apporte des fonctions pour rechercher directement les outliers, et selon différentes méthodes (avec la distance de Cook, les hat values, les valeurs R-student).

```{r}
# install.packages("data.table")
# install.packages("dplyr")
# install.packages("caret",
#                  repos = "http://cran.r-project.org", 
#                  dependencies = TRUE)
# install.packages("ggplot2")
# install.packages("rmarkdown")

library(ggplot2)
library(rmarkdown)

don <- read.csv('2_Data/2_Retraitees/SiWIM_data_after_input_clusters.csv',header=T)
dim(don)

names(don)

don[is.na(don)] <- 0
don <- don[,c("Warning_flags","A1","A2","A3","A4","A5","A6","A7",
                      "M1","M2","M3","M4","M5","M6","M7","M8","N", 
                      "Reduced_chi_squared", "Vitesse", "MGV", "clusters_kmeans")]

model_glm_glm <- glm(MGV~., data=don, family="gaussian")
```

Ce graphe représente les outliers calculés via le package car. 

```{r}
library(car)

outs <- influencePlot(model_glm_glm)

n <- 1000
Cooksdist <- as.numeric(tail(row.names(outs[order(outs$CookD), ]), n))
Lev <- as.numeric(tail(row.names(outs[order(outs$Hat), ]), n))
StdRes <- as.numeric(tail(row.names(outs[order(outs$StudRes), ]), n))

plot(don$Reduced_chi_squared, don$Warning_flags)
points(don$Reduced_chi_squared[Cooksdist], don$Warning_flags[Cooksdist], col = "red", pch = 0, lwd = 15)
points(don$Reduced_chi_squared[Lev], don$Warning_flags[Lev], col = "blue", pch = 25, lwd = 8)
points(don$Reduced_chi_squared[StdRes], don$Warning_flags[StdRes], col = "green", pch = 20, lwd = 5)
```
