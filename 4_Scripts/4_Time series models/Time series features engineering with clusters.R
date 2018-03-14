rm(list = ls())

#Looding libraries
library(forecast)
library(ggplot2)
library(data.table)
library(xts)# extensible time series
library(dygraphs) # Nice graphs for time series
library(imputeTS) #Missing values in time series

#Loading data SIWIM
setwd("D:/Dropbox/Data science/Formation CEPE/Projet/GitHub/SiWIM-project")
siwim_data <-
  fread("2_Data/2_Retraitees/SiWIM_data_after_input_clusters.csv")

summary(siwim_data)

str(siwim_data)

#ID as number
siwim_data[, V1 := as.numeric(siwim_data$V1)]

# Refactor the lane
siwim_data[, Lane := factor(siwim_data$Lane, labels = c("droite", "gauche"))]

# Refactor classes data
cols <- c(
  "Warning_flags",
  "Subclass_ID",
  "Axle_groups",
  "Nb_axles",
  "Anomalie",
  "clusters_kmeans"
)

siwim_data[, (cols) := lapply(.SD, function(x)
  as.factor(x)), .SDcols = cols]

#Format date & time
siwim_data[, Timestamp := as.POSIXct(substr(siwim_data$Timestamp, 1, 19), format = '%Y-%m-%d %H:%M:%S')]

siwim_data[, Date := as.Date(siwim_data$Date)]

# Refactor time data

cols <- c("Mois_annee",
          "Jour_semaine",
          "Heure",
          "Axle_groups")

siwim_data[, (cols) := lapply(.SD, function(x)
  as.factor(x)), .SDcols = cols]

siwim_data[, Heure_num := as.numeric(Heure)]


#Recheck
str(siwim_data)

table(siwim_data$clusters_kmeans)

#Delete unuseful columns
maxN <- 8
axles_load <- paste("A", 1:(maxN - 1), sep = "")
axles_mass <- paste("M", 1:maxN, sep = "")

useless_cols <- c(axles_load, axles_mass, "Site_ID")

siwim_data[, (useless_cols) := NULL]

str(siwim_data)

## Scaling data by day and hour (choosen time window)

siwim_data_hours_clust <-
  siwim_data[, .(
    Count = .N,
    Total_Weight = sum(MGV),
    Total_axle_dist = sum(total_axle_dist),
    T_mean = mean(T),
    Time_mean = mean(Time_num),
    Vitesse_mean = mean(Vitesse)
  ), by = .(Date, Heure, clusters_kmeans)]

# New simple features

period <- 24 ## Period of 24 hours
N <- nrow(siwim_data_hours_clust)
window <- N / period

head(siwim_data_hours_clust)

siwim_data_hours_clust[, ':=' (
  Annee =  as.numeric(format(as.Date(Date), format = "%Y")),
  Mois_annee = factor(format(as.Date(Date), format = "%B")),
  Mois_num = as.numeric(format(as.Date(Date), format = "%m")),
  Jour_num = as.numeric(format(as.Date(Date), format = "%d")),
  Jour_sem_num = as.numeric(format(as.Date(Date), format = "%w")),
  Semaine_num = as.numeric(format(as.Date(Date), format = "%V")),
  Semaine_fac = factor(format(as.Date(Date), format = "%V")),
  Heure_num = as.numeric(Heure)
)]

summary(siwim_data_hours_clust)

table(siwim_data_hours_clust$Jour_sem_num)

# siwim_data_hours[, Jour_semaine := factor(format(as.Date(Date), format = "%A"),
#                       levels= c("lundi",
#                                 "mardi",
#                                 "mercredi",
#                                 "jeudi",
#                                 "vendredi",
#                                 "samedi",
#                                 "dimanche"), ordered = TRUE)]


siwim_data_hours_clust[, Jour_semaine := factor(
  weekdays(Date),
  levels = c(
    "lundi",
    "mardi",
    "mercredi",
    "jeudi",
    "vendredi",
    "samedi",
    "dimanche"
  )
)]

levels(siwim_data_hours_clust$Heure)

siwim_data_hours_clust[, Heure := as.factor(Heure_num)] 


siwim_data_hours_clust$Jour_semaine <-
  relevel(siwim_data_hours_clust$Jour_semaine, ref = "lundi")

str(siwim_data_hours_clust)

#Tests
siwim_data_hours_clust[as.Date(Date) == as.Date("2017-11-19")]#[,sum(Count)]

siwim_data_hours_clust[, sum(Count), by = Jour_semaine]

table(siwim_data_hours_clust$Heure)

#Time step creation
siwim_data_hours_clust[, time_step := as.POSIXct(paste(as.character(Date), paste(Heure, '00', '00', sep = ':'), sep = " "))]

#Some graphs

plot(Count ~ time_step, data = siwim_data_hours_clust[!Jour_sem_num == 0 &
                                                        !Jour_sem_num == 6], type = "l")

plot(Count ~ time_step, data = siwim_data_hours_clust, type = "l")

plot(Total_Weight ~ time_step, data = siwim_data_hours_clust, type = "l")

## Graphs with dygraph and xts
head(siwim_data_hours_clust)

siwim_data_hours_clust_split_1 <-
  siwim_data_hours_clust[clusters_kmeans == 1, .(Count_1 = Count,
                                                 time_step_1 = time_step)]
siwim_data_hours_clust_split_2 <-
  siwim_data_hours_clust[clusters_kmeans == 2, .(Count_2 = Count,
                                                 time_step_2 = time_step)]
siwim_data_hours_clust_split_3 <-
  siwim_data_hours_clust[clusters_kmeans == 3, .(Count_3 = Count,
                                                 time_step_3 = time_step)]


merge(
  siwim_data_hours_clust_split_1,
  siwim_data_hours_clust_split_2,
  by.x = "time_step_1",
  by.y = "time_step_2"
)

siwim_data_hours_clust_split <-
  merge(
    merge(
      siwim_data_hours_clust_split_1,
      siwim_data_hours_clust_split_2,
      by.x = "time_step_1",
      by.y = "time_step_2"
    ),
    siwim_data_hours_clust_split_3,
    by.x = "time_step_1",
    by.y = "time_step_3"
  )
siwim_data_hours_clust_split

siwim_data_xts_clust <-
  xts(siwim_data_hours_clust_split, order.by = siwim_data_hours_clust_split$time_step_1)


dygraph(siwim_data_xts_clust, main = "Trucks time data") %>%
  dyRangeSelector()

siwim_data_hours_clust_split[, ':='(
  Date =  as.Date(strftime(time_step_1, format = "%Y-%m-%d")),
  Annee = as.numeric(strftime(time_step_1, format = "%Y")),
  Mois_annee = factor(strftime(time_step_1, format = "%B")),
  Mois_num = as.numeric(strftime(time_step_1, format = "%m")),
  Jour_num = as.numeric(strftime(time_step_1, format = "%d")),
  Jour_sem_num = as.numeric(strftime(time_step_1, format = "%w")),
  Heure = factor(strftime(time_step_1, format = "%H"))
)]

siwim_data_hours_clust_split[, Jour_semaine := factor(
  weekdays(Date),
  levels = c(
    "lundi",
    "mardi",
    "mercredi",
    "jeudi",
    "vendredi",
    "samedi",
    "dimanche"
  )
)]

boxplot(Count_1 ~ Jour_semaine, data = siwim_data_hours_clust_split)
boxplot(Count_1 ~ Heure, data = siwim_data_hours_clust_split)
boxplot(Count_1 ~ interaction(Heure, Jour_semaine), data = siwim_data_hours_clust_split)

boxplot(Count_2 ~ Jour_semaine, data = siwim_data_hours_clust_split)
boxplot(Count_2 ~ Heure, data = siwim_data_hours_clust_split)
boxplot(Count_2 ~ interaction(Heure, Jour_semaine), data = siwim_data_hours_clust_split)

boxplot(Count_3 ~ Jour_semaine, data = siwim_data_hours_clust_split)
boxplot(Count_3 ~ Heure, data = siwim_data_hours_clust_split)
boxplot(Count_3 ~ interaction(Heure, Jour_semaine), data = siwim_data_hours_clust_split)

ggplot(data = siwim_data_hours_clust, aes(x=Jour_semaine,
                                          y=Count)) + geom_boxplot(aes(fill=clusters_kmeans))

ggplot(data = siwim_data_hours_clust, aes(x=Heure,
                                          y=Count)) + geom_boxplot(aes(fill=clusters_kmeans))

ggplot(data = siwim_data_hours_clust, aes(x=interaction(Heure, Jour_semaine),
                                          y=Count)) + geom_boxplot(aes(fill=clusters_kmeans))


## Write final data sets for time series,
## one witout missing values and one with inputted values

# write.csv(siwim_data_hours, file = "2_Data/2_Retraitees/SiWIM_data_hours_original.csv")

# write.csv(siwim_data_dt_full, file = "2_Data/2_Retraitees/SiWIM_data_hours_full.csv")
