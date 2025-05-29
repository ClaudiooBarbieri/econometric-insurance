rm(list = ls())

# file with info
info <- read.csv("FullDataCsv/master.csv")

attach(info)

# read and create list of data frame  for INDICES, NIFTY50 and NIFTY MID-CAP 100
list_indeces <- list()
for (i in 1:9) {
  list_indeces[[gsub(" ", "", tradingsymbol[i])]] <- read.csv(paste0("FullDataCsv/", key[i], ".csv"))
}
list_nifty50 <- list()
for (i in 10:59) {
  list_nifty50[[gsub(" ", "", tradingsymbol[i])]] <- read.csv(paste0("FullDataCsv/", key[i], ".csv"))
}
list_nifty100_mc <- list()
for (i in 60:159) {
  list_nifty100_mc[[gsub(" ", "", tradingsymbol[i])]] <- read.csv(paste0("FullDataCsv/", key[i], ".csv"))
}

rm(i)

detach(info)

save.image(file = "lists_of_datasets.RData")
