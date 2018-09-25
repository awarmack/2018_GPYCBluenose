#Load the Expedition Data

#Remove any existing data
rm(list=ls())

#Load Packages
library(tidyverse)
library(stringr)
library(lubridate)


#Read the Raw Data
expdat <- read.csv(expdat_file, stringsAsFactors = FALSE)

#Change the Time
expdat$time <- as.POSIXct((expdat$Utc) * (60*60*24), origin = "1899-12-30", tz = "America/Detroit")

#Remove any All NA Columns
#Find columns where all is NA
allna <- which(apply(expdat, 2, function(x) all(is.na(x))))   

#remove those columns
expdat <- expdat %>% select(-allna)

#Select only needed columns
expdat <- expdat %>% select(time, Bsp, Awa, Aws, Twa, Tws, Twd, Lat, Lon, Hdg, Cog, Sog)

#change names to lowercase for easier handling
names(expdat) <- tolower(names(expdat))

str(expdat)

#Remove any rows where we don't have all data
expdat <- na.omit(expdat)

