 library(data.table)

siwim_data <- fread("Data/SiWIM_data.csv")

head(siwim_data)
dim(siwim_data)
str(siwim_data)

siwim_data[, lapply(.SD, class)]

summary(siwim_data)

siwim_data[, .N, by = .()]

siwim_data[, Timestamp := as.POSIXct(substr(siwim_data$Timestamp,1,19), format = '%Y-%m-%d-%H-%M-%S')]
siwim_data[, Offset := as.character(siwim_data$Offset)]
siwim_data[, Site_ID := as.character(siwim_data$Site_ID)]
siwim_data[, Warning_flags := as.character(siwim_data$Warning_flags)]


axles_load <- paste("W", 1:maxN, sep = "")
axles_dist <- paste("A", 1:(maxN - 1), sep = "")

cols <- c(
  "Stage_trace",
  "Lane",
  "v",
  "N",
  "Subclass_ID",
  "Axle_groups",
  "WGV",
  axles_load,
  "total_axle_dist",
  axles_dist,
  "T",
  "Impact_factor",
  "Reduced_chi_squared"
)


siwim_data[, (cols) := lapply(.SD, function(x) as.numeric(as.character(x))), .SDcols=cols]

siwim_data$V1 <- NULL
head(siwim_data)

write.csv(siwim_data, file = "Data/SiWIM_data_formated.csv")

