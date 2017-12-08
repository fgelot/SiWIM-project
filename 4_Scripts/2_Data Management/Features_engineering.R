library(data.table)

#Loading formated data
siwim_data <- fread("2_Data/2_Retraitees/SiWIM_data_formated.csv")

# Delete some useless features
siwim_data[, c("Stage_trace", "Offset", "Impact_factor") := NULL]

# Refactor the lane
siwim_data[, Lane := factor(siwim_data$Lane, labels = c("droite", "gauche"))]

# Refactor classes data
siwim_data[, Subclass_ID := factor(siwim_data$Subclass_ID)]

# Refactor classes data
siwim_data[, Axle_groups := factor(siwim_data$Axle_groups)]

# NUmber of levels for classes

# nlevels(siwim_data$Axle_groups)
# nlevels(siwim_data$Subclass_ID)

# Proportion of warning flags

prop.table(table(siwim_data$Warning_flags))

# Extract data from timestamp : date, time and POSIX format

siwim_data[, Date    := as.POSIXct(substr(siwim_data$Timestamp,1,10), format = '%Y-%m-%d')]
siwim_data[, Horaire := substr(siwim_data$Timestamp,12,19)]
siwim_data[, Timestamp := as.POSIXct(substr(siwim_data$Timestamp,1,19), format = '%Y-%m-%d %H:%M:%S')]

# Quantify time series from the 1st January 2017

siwim_data[, Time_num := as.numeric((siwim_data$Timestamp - as.POSIXct("2017-01-01 00:00:00", format = '%Y-%m-%d %H:%M:%S'))/360)]

# plot(density(siwim_data$Time_num))

# Compute speed into km/h

siwim_data[, Vitesse := siwim_data$v * (36/10)]

# Compute global weights in Tonnes

siwim_data[, MGV := siwim_data$WGV / 9.81]
# 
# boxplot(siwim_data$Vitesse)
# 
# plot(density(siwim_data$Vitesse))
# 
# plot(x = siwim_data$Subclass_ID, y = siwim_data$Vitesse)
# 
# 
# plot(density(siwim_data$MGV))

# 
# plot(x = siwim_data$MGV, y = siwim_data$N)

# 
# boxplot(siwim_data$MGV)

# Compute all axle weights in Tonnes

maxN <- 16
axles_load <- paste("W", 1:maxN, sep = "")
axles_mass <- paste("M", 1:maxN, sep = "")
axles_load
siwim_data[, (axles_mass) := lapply(axles_load, function(x){get(x)/9.81})]

# Suppress V1 index before writing data into csv file

siwim_data[, V1 := NULL]


# Show data

head(siwim_data)

str(siwim_data)

summary(siwim_data)

write.csv(siwim_data, file = "2_Data/2_Retraitees/SiWIM_data_prepared.csv")

