library(data.table)

siwim_data <- fread("2_Data/2_Retraitees/SiWIM_data_formated.csv")

siwim_data[, c("Stage_trace", "Offset", "Impact_factor") := NULL]

siwim_data[, Lane := factor(siwim_data$Lane, labels = c("droite", "gauche"))]

siwim_data[, Subclass_ID := factor(siwim_data$Subclass_ID)]

siwim_data[, Axle_groups := factor(siwim_data$Axle_groups)]

head(siwim_data)

str(siwim_data)

nlevels(siwim_data$Axle_groups)
nlevels(siwim_data$Subclass_ID)

table(siwim_data$Subclass_ID)
table(siwim_data$Warning_flags)

prop.table(table(siwim_data$Warning_flags))

summary(siwim_data)

siwim_data[N == 3,]

siwim_data[, Date    := as.POSIXct(substr(siwim_data$Timestamp,1,10), format = '%Y-%m-%d')]
siwim_data[, Horaire := substr(siwim_data$Timestamp,12,19)]
siwim_data[, Timestamp := as.POSIXct(substr(siwim_data$Timestamp,1,19), format = '%Y-%m-%d %H:%M:%S')]

as.numeric(max(siwim_data$Timestamp), units = "hours") - as.numeric(min(siwim_data$Timestamp), units = "hours") 

siwim_data[, Time_num := as.numeric((siwim_data$Timestamp - as.POSIXct("2017-01-01 00:00:00", format = '%Y-%m-%d %H:%M:%S'))/360)]

plot(density(siwim_data$Time_num))


siwim_data[, Vitesse := siwim_data$v * (36/10)]

siwim_data[, MGV := siwim_data$WGV * (1000/9.81)]

boxplot(siwim_data$Vitesse)

plot(density(siwim_data$Vitesse))

plot(x = siwim_data$Subclass_ID, y = siwim_data$Vitesse)


plot(density(siwim_data$MGV))

plot(x = siwim_data$MGV, y = siwim_data$N)


boxplot(siwim_data$MGV)

head(siwim_data$MGV)


write.csv(siwim_data, file = "2_Data/2_Retraitees/SiWIM_data_prepared.csv")

