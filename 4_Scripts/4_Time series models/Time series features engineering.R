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
siwim_data <- fread("2_Data/2_Retraitees/SiWIM_data_after_input.csv")

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
  "Anomalie"
)

siwim_data[, (cols) := lapply(.SD, function(x) as.factor(x)), .SDcols=cols]

#Format date & time
siwim_data[, Timestamp := as.POSIXct(substr(siwim_data$Timestamp,1,19), format = '%Y-%m-%d %H:%M:%S')]

siwim_data[, Date := as.Date(siwim_data$Date)]

# Refactor time data

cols <- c(
  "Mois_annee",
  "Jour_semaine",
  "Heure",
  "Axle_groups"
)

siwim_data[, (cols) := lapply(.SD, function(x) as.factor(x)), .SDcols=cols]

siwim_data[,Heure_num := as.numeric(Heure)]


#Recheck
str(siwim_data)

#Delete unuseful columns
maxN <- 8
axles_load <- paste("A", 1:(maxN-1), sep = "")
axles_mass <- paste("M", 1:maxN, sep = "")

useless_cols <- c(axles_load,axles_mass, "Site_ID")

siwim_data[, (useless_cols) := NULL]

str(siwim_data)

## Scaling data by day and hour (choosen time window)

siwim_data_hours <- siwim_data[, .(Count=.N, Total_Weight=sum(MGV),
                                   Total_axle_dist=sum(total_axle_dist), 
                                   T_mean=mean(T), 
                                   Time_mean=mean(Time_num),
                                   Vitesse_mean=mean(Vitesse)), by = .(Date, Heure)]

# New simple features

period <- 24 ## Period of 24 hours
N <- nrow(siwim_data_hours)
window <- N /period

head(siwim_data_hours)

siwim_data_hours[, ':=' (Annee =  as.numeric(format(as.Date(Date), format = "%Y")),
                         Mois_annee = factor(format(as.Date(Date), format = "%B")),
                         Mois_num = as.numeric(format(as.Date(Date), format = "%m")),
                         Jour_num = as.numeric(format(as.Date(Date), format = "%d")),
                         Jour_sem_num = as.numeric(format(as.Date(Date), format = "%w")),
                         Semaine_num = as.numeric(format(as.Date(Date), format = "%V")),
                         Semaine_fac = factor(format(as.Date(Date), format = "%V")),
                         Heure_num = as.numeric(Heure)
                         )]

summary(siwim_data_hours)

table(siwim_data_hours$Jour_sem_num)

# siwim_data_hours[, Jour_semaine := factor(format(as.Date(Date), format = "%A"),
#                       levels= c("lundi", 
#                                 "mardi", 
#                                 "mercredi", 
#                                 "jeudi", 
#                                 "vendredi", 
#                                 "samedi",
#                                 "dimanche"), ordered = TRUE)]


siwim_data_hours[, Jour_semaine := factor(weekdays(Date))]


siwim_data_hours$Jour_semaine <- relevel(siwim_data_hours$Jour_semaine, ref = "lundi")

str(siwim_data_hours)
summary(siwim_data)

#Tests
siwim_data_hours[as.Date(Date) == as.Date("2017-11-19")]#[,sum(Count)]

siwim_data_hours[, sum(Count), by = Jour_semaine]

table(siwim_data_hours$Heure)

#Time step creation
siwim_data_hours[, time_step := as.POSIXct(paste(as.character(Date), paste(Heure, '00','00', sep = ':'), sep = " "))]

#Some graphs

plot(Count ~ time_step, data = siwim_data_hours[!Jour_sem_num == 0 & !Jour_sem_num == 6], type = "l")

plot(Count ~ time_step, data = siwim_data_hours, type = "l")

plot(Total_Weight ~ time_step, data = siwim_data_hours, type = "l")

## Graphs with dygraph and xts
siwim_data_xts <- xts(siwim_data_hours[, .(Count, Total_Weight, Total_axle_dist, T_mean, 
                                           Time_mean, Vitesse_mean)], order.by = siwim_data_hours$time_step)


dygraph(siwim_data_xts, main = "Trucks time data") %>%
  dyRangeSelector()

## multi plots with ggplots2
df <- melt(siwim_data_hours[, c("time_step", "Count", "Total_Weight", "Total_axle_dist", 
                                "T_mean", "Time_mean", "Vitesse_mean" )], id="time_step")
ggplot(df) + geom_line(aes(x=time_step, y=value, color=variable))  + facet_wrap( ~ variable, scales="free")

## Manage missing values

##Get full sequence
full_sequence <- seq(from=min(siwim_data_hours$time_step), 
                     to=max(siwim_data_hours$time_step), by="h")

head(full_sequence)

##Grab the missing sequence
missing_sequence <- full_sequence[!(full_sequence %in% siwim_data_hours$time_step)]

missing_sequence

length(missing_sequence) / length(full_sequence)
siwim_data_hours_missing <- data.table(time_step = missing_sequence, 
                                       Date =  as.Date(strftime(missing_sequence, format = "%Y-%m-%d")),
                                       Annee = as.numeric(strftime(missing_sequence, format = "%Y")),
                                       Mois_annee = factor(strftime(missing_sequence, format = "%B")),
                                       Mois_num = as.numeric(strftime(missing_sequence, format = "%m")),
                                       Jour_num = as.numeric(strftime(missing_sequence, format = "%d")),
                                       Jour_sem_num = as.numeric(strftime(missing_sequence, format = "%w")))

siwim_data_hours_missing[, Jour_semaine := factor(weekdays(Date))]

head(siwim_data_hours_missing)

siwim_data_hours_full <- rbindlist(list(siwim_data_hours, siwim_data_hours_missing), fill = T)

head(siwim_data_hours_full)

siwim_data_hours_full[siwim_data_hours_full$time_step %in% missing_sequence]

siwim_data_hours_count_full_1 <- na.interp(siwim_data_hours_full$Count)
siwim_data_hours_count_full_2 <- na.interpolation(siwim_data_hours_full$Count)
siwim_data_hours_count_full_3 <- na.interpolation(siwim_data_hours_full$Count, option = "spline")

siwim_data_xts_full <- xts(cbind(siwim_data_hours_count_full_1,
                                 siwim_data_hours_count_full_2, 
                                 siwim_data_hours_count_full_3) , order.by = siwim_data_hours_full$time_step)




dygraph(siwim_data_xts_full, main = "Trucks time data") %>%
  dyRangeSelector()

coredata(siwim_data_xts_full)[, "siwim_data_hours_count_full_1"]

siwim_data_dt_full <- data.table(time_step = siwim_data_hours_full$time_step, 
                                 full_interp = siwim_data_hours_count_full_1,
                                 full_interp_lm = siwim_data_hours_count_full_2,
                                 full_interp_spline = siwim_data_hours_count_full_3)

# or with ggplot
df <- melt(siwim_data_dt_full[, c("time_step", "full_interp", "full_interp_lm", 
                                  "full_interp_spline")], id="time_step")
ggplot(df) + geom_line(aes(x=time_step, y=value, color=variable))  + facet_wrap( ~ variable, scales="free")

### Le meilleur est l'interpolation spline

siwim_data_dt_full[, ':=' (Date =  as.Date(strftime(time_step, format = "%Y-%m-%d")),
                           Annee = as.numeric(strftime(time_step, format = "%Y")),
                           Mois_annee = factor(strftime(time_step, format = "%B")),
                           Mois_num = as.numeric(strftime(time_step, format = "%m")),
                           Jour_num = as.numeric(strftime(time_step, format = "%d")),
                           Jour_sem_num = as.numeric(strftime(time_step, format = "%w")),
                           Heure = factor(strftime(time_step, format = "%H")),
                           Semaine_num = as.numeric(strftime(time_step, format = "%V")),
                           Semaine_fac = factor(strftime(time_step, format = "%V")),
                           Heure_num = as.numeric(strftime(time_step, format = "%H")))]

siwim_data_dt_full[, Jour_semaine := factor(weekdays(Date))]

siwim_data_dt_full[, Count := full_interp_spline]

siwim_data_dt_full[, c("full_interp", 
                       "full_interp_lm",
                       "full_interp_spline") := NULL]

str(siwim_data_dt_full)

#Some checks
siwim_data_hours[!Jour_sem_num ==  0 & !Jour_sem_num ==  6][, sum(Count), by = Jour_semaine]

siwim_data_hours[Jour_sem_num ==  0 | Jour_sem_num ==  6][, sum(Count), by = Jour_semaine]

table(siwim_data_hours$Jour_sem_num)

plot(Count ~ time_step, data = siwim_data_hours[Jour_sem_num == 5], type = "l")

plot(Count ~ time_step, data = siwim_data_hours[!Jour_sem_num == 0 & !Jour_sem_num == 6 & 
                                                  as.numeric(Heure) > 6 & as.numeric(Heure) < 20], type = "l")

summary(siwim_data_hours$Count)

boxplot(Count ~ Jour_semaine, data = siwim_data_hours)
boxplot(Count ~ Heure, data = siwim_data_hours)
boxplot(Count ~ Jour_semaine + Heure, data = siwim_data_hours)

siwim_data_hours[, Jour_semaine_heure := factor(paste(Jour_semaine,Heure), ordered = T)]
#siwim_data_hours$Jour_semaine_heure <- factor(siwim_data_hours$Jour_semaine_heure, levels(siwim_data_hours$Jour_semaine_heure)[siwim_data_hours$Jour_semaine_heure])

levels(siwim_data_hours$Jour_semaine_heure)[siwim_data_hours$Jour_semaine_heure]
summary(siwim_data_hours)
levels(siwim_data_hours$Jour_semaine_heure)

str(siwim_data_hours)
str(siwim_data_dt_full)
levels(siwim_data_hours$Jour_semaine)

## Write final data sets for time series,
## one witout missing values and one with inputted values

write.csv(siwim_data_hours, file = "2_Data/2_Retraitees/SiWIM_data_hours_original.csv")

write.csv(siwim_data_dt_full, file = "2_Data/2_Retraitees/SiWIM_data_hours_full.csv")

