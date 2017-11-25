###########
#Loading packages
library('httr')
library(rvest)

#Access infos
user <- 'fschmidt'
pwd <- 'XXXXXXXX'
url <- 'https://vpn.siwim.si/siwim-s/login.php'
values <- list(username = user, password = pwd)

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



file_url_list <- url_list[seq(4, 488, by = 4)]

url_start <- "https://vpn.siwim.si/siwim-s/"
html_global <- ""

for (url in file_url_list) {
  #Download info
  download <- paste0(url_start, url)
  html <- content(GET(download, query = values), "text")
  html_global <- paste0(html_global, html)
}
nchar(html_global)

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

### Build a new data frame woth cleaned columns

axles_load <- paste("W", 1:maxN, sep = "")

axles_dist <- paste("A", 1:(maxN - 1), sep = "")

axles_load

axles_dist

records.df <-
  as.data.frame(setNames(
    replicate(46, numeric(0), simplify = F),
    c(
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
      "WGV",
      axles_load,
      "total_axle_dist",
      axles_dist,
      "T",
      "Impact_factor",
      "Reduced_chi_squared"
    )
  ))

records.df

for (ii in 1:length(records)) {
  values <- unlist(strsplit(records[ii], "\t"))
  N <- as.numeric(values[8])
  
  for (jj in 1:length(values)) {
    if (jj < 12 + N) {
      records.df[ii, jj] <- values[jj]
    } else if (jj < 12 + 2 * N) {
      records.df[ii, jj - N + maxN] <- values[jj]
    } else{
      records.df[ii, jj - 2 * N + 2 * maxN] <- values[jj]
    }
  }
  
  if(ii %% 5000 == 0){print(ii)}
}

head(records.df)

dim(records.df)

write.csv(records.df, file = "SiWIM_data.csv")
