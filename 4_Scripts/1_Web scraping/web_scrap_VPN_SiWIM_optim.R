###########
#Loading packages
library('httr')
library(rvest)
library(stringi)
library(data.table)
library(parallel)


#Access infos
user <- 'fschmidt'
pwd <- 'yBgVLjS7tj'
url <- 'https://vpn.siwim.si/siwim-s/login.php'
values <- list(username = user, password = pwd)


start_date <- as.Date("2017-07-01")

end_date <- as.Date("2018-01-15")

#Login
POST(url, body = values)

## Global page

home_page <-
  "https://vpn.siwim.si/siwim-s/site_nswd_list.php?cnt_id=130&sys_nr=2&site=Normandie"
html <- content(GET(home_page, query = values), as = "parsed")

url_list <- html %>%
  html_node("td") %>%
  html_nodes("a") %>%
  html_attr("href")

max_url <- length(url_list)

file_url_list <- url_list[seq(4, max_url, by = 4)]

url_start <- "https://vpn.siwim.si/siwim-s/"
html_global <- ""
nb_dates <- 0

for (url in file_url_list) {
  #check date in scope
  cur_date = as.Date(stri_extract(url, regex = "[0-9]{4}-[0-9]{2}-[0-9]{2}"))
  
  if (cur_date >= start_date & cur_date <= end_date) {
    nb_dates = nb_dates + 1
    #Download info
    download <- paste0(url_start, url)
    html <- content(GET(download, query = values), "text")
    html_global <- paste0(html_global, html)
  }
}
nchar(html_global)

nb_dates
### Data treatment

records <- unlist(strsplit(html_global, "\r\n"))
length(records)


### Find max N (number of axles)

maxN <- 0
N <- 0

for (line in records) {
  values <- unlist(strsplit(line, "\t"))
  N <- as.numeric(values[8])
  
  if (N > maxN) {
    maxN <- N
  }
}


maxN

#Generate list of list

global_list <- lapply(records, function(x) {
  unlist(strsplit(x, "\t"))
})

### Build a new data frame woth cleaned columns

axles_load <- paste("W", 1:maxN, sep = "")

axles_dist_names <- paste("A", 1:(maxN - 1), sep = "")

nb_cores <- detectCores()

cl <- makeCluster(nb_cores -1)

clusterExport(cl,varlist=c("maxN","axles_load", "axles_dist_names"))

records.list <- parLapply(cl, global_list, function(x) {
  N <- as.numeric(x[8])
  
  #Init target data
  begin <- replicate(11,numeric(0),simplify = F)
  names(begin) <- c(
    "Timestamp",
    "Offset",
    "Site_ID",
    "Stage_trace",
    "Warning_flags",
    "Lane",
    "v",
    "N",
    "Subclass_ID",
    "Axle_groups",
    "WGV")
  weights <- replicate(maxN,NA,simplify = F)
  names(weights) <- axles_load
  total_axle_dist <- list(0)
  names(total_axle_dist) <- c("total_axle_dist")
  axles_dist <- replicate(maxN-1,NA,simplify = F)
  names(axles_dist) <- axles_dist_names
  end <- replicate(3,numeric(0),simplify = F)
  names(end) <- c("T",
                  "Impact_factor",
                  "Reduced_chi_squared")
  
  ##Input values by blocks
  
  begin[1:11] <- x[1:11]
  weights[1:N] <- x[12:(11+N)]
  total_axle_dist[1] <- x[(12+N)]
  axles_dist[1:N-1] <- x[(13+N):(12+2*N-1)]
  end[1:3] <- x[(12+2*N):(14+2*N)]
  
  return (cbind.data.frame(begin,weights, total_axle_dist,axles_dist,end))
})

stopCluster(cl)

records.dt <- rbindlist(records.list)

head(records.dt)

dim(records.dt)

file_name <- paste("SiWIM_data_", start_date, "_to_", end_date, ".csv", sep= "")


write.csv(records.dt, file = paste("2_Data/1_Sources/",file_name, sep = ""))
