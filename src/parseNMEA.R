#Functions for reading NMEA Data
library(zoo)





getTimeStamp <- function(datacol){
  #Extract any time stampt
}


getTalkerID <- function(datacol){
  #Talker ID is 2 character immediately after $
  str_extract(datacol, "(?<=\\$).{2}")

}

getSentanceType <- function(datacol){
  #extract Sentance Type
  str_extract(datacol, "(?<=\\$.{2}).{3}")
}

parseGPStime <- function(currentdate, rawgpstime){
  rawgpstime <- paste(currentdate, rawgpstime)
  
  #print(rawgpstime)
  as.POSIXct(rawgpstime, format="%Y-%m-%d %H%M%S", tz="GMT")
  
}

getGPStime <- function(currentdate, nmeadata){
  #if the NMEA sentance is a GPS time column, return the GPS Time. 
  
  nmeatype <- getSentanceType(nmeadata)
  
  hasGPStime <- nmeatype %in% c("BWC", "GGA")
  
  time_part <- str_sub(nmeadata, 8, 12)
  
  #format
  #$--GGA,hhmmss.ss,llll.ll,a,yyyyy.yy,a,x,xx,x.x,x.x,M,x.x,M,x.x,xxxx*hh<CR><LF>
  #$--BWC,hhmmss.ss,llll.ll,a,yyyyy.yy,a,x.x,T,x.x,M,x.x,N,c--c,m,*hh<CR><LF>
  # we only want the hhmmss so we can just use the 7 ~ 12 characters

  #format is hhmmss.ss in UTC time
  out <- parseGPStime(currentdate, time_part)
  
  out[!hasGPStime] <- NA

  return(out)
}


#The remaining sentances will be parsed as 

# for each line
# 1 what kind of sentance
# apply the appropriate parsing function returning a data frame
# rbind that data frame to the existing. 

#KEY variables that we want

transformLatLon <- function(rawlatlon){
  
  #trim any leading zero
  rawlatlon <- str_replace(string = rawlatlon, "^0", "")
  
  rawlatlon <- as.character(rawlatlon)
  
  whole_deg <- as.numeric(str_sub(rawlatlon, 1,2))
  
  minutes <- as.numeric(str_sub(rawlatlon, 3, 6))/60

  return(whole_deg + minutes)
  
}


parseGGA <- function( rawnmea){
  rawnmea <- as.character(rawnmea)
  
  is.gga <- getSentanceType(rawnmea)=="GGA"
  
  gga <- rawnmea
  gga[!is.gga] <- NA
  
  gga <- str_split(gga, pattern = ",", simplify = TRUE)

  gga <- gga[, c(3, 4, 5, 6)]
  
  gga <- as.data.frame(gga, stringsAsFactors=FALSE)
  
  names(gga) <- c("lat", "lat_ns", "lon", "lon_ew")
  
  gga$lat <- transformLatLon(gga$lat)  #change to fractions DD.DDD of degrees from DD MM.MM
  gga$lon <- transformLatLon(gga$lon)
  
  #convert to N/S  E/W to  180/-180
  gga$lat <- ifelse(gga$lat_ns=="N", gga$lat, gga$lat * -1)
  gga$lon <- ifelse(gga$lon_ew=="E", gga$lon, gga$lon * -1)
  
  gga <- gga[,c("lat", "lon")]
  
  return(gga)
  
}

parseHDM <- function(rawnmea){
  rawnmea <- as.character(rawnmea)
  
  is.hdm <- getSentanceType(rawnmea)=="HDM"
  
  hdm <- rawnmea
  hdm[!is.hdm] <- NA
  
  hdm <- str_split(hdm, pattern=",", simplify=TRUE)
  
  hdm <- hdm[, 2]
  
  data.frame("hdg_m"=as.numeric(hdm))
}

parseHDT <- function(rawnmea){
  rawnmea <- as.character(rawnmea)
  
  is.hdt <- getSentanceType(rawnmea)=="HDT"
  
  hdt <- rawnmea
  hdt[!is.hdt] <- NA
  
  hdt <- str_split(hdt, pattern=",", simplify=TRUE)
  
  hdt <- hdt[, 2]
  
  data.frame("hdg_t"=as.numeric(hdt))
}

getOnlyTargetSentance <- function(rawnmea, sentancetype){
  rawnmea <- as.character(rawnmea)
  
  is.targetsentance <- getSentanceType(rawnmea)==sentancetype
  
  rawnmea[!is.targetsentance] <- NA
  
  incols <- str_split(rawnmea, pattern=",", simplify=TRUE)
  
  return(incols)
}

parseMWV <- function(rawnmea){
  #Wind Speed & Angle
  mwv <- getOnlyTargetSentance(rawnmea, "MWV")
  
  mwv <- mwv[, c(2,3,4)]
  
  mwv <- as.data.frame(mwv, stringsAsFactors=FALSE)
  
  names(mwv) <- c("windangle", "TR", "windspeed")
  mwv$windangle <- as.numeric(mwv$windangle)
  mwv$windspeed <- as.numeric(mwv$windspeed)
  
  mwv$twa <- ifelse(mwv$TR=="T", mwv$windangle, NA)
  mwv$awa <- ifelse(mwv$TR=="R", mwv$windangle, NA)
  mwv$tws <- ifelse(mwv$TR=="T", mwv$windspeed, NA)
  mwv$aws <- ifelse(mwv$TR=="R", mwv$windspeed, NA)

  return(mwv[, c("awa", "aws", "twa", "tws")])

}


parseVHW <- function(rawnmea){
  #boatspeed thru water
  
  vhw <- getOnlyTargetSentance(rawnmea, "VHW")
  
  #select columns
  vhw <-as.data.frame(vhw[, c(6) ], stringsAsFactors=FALSE) 
  
  names(vhw) <- "bsp"
  
  vhw$bsp <- as.numeric(vhw$bsp)

  return(vhw)
  
}


parseNMEA <- function(nmea_log){
  
  #turn into a data frame
  nmea <- data.frame("raw_nmea" = nmea_log)
  
  #assign gps time
  nmea$gps_time <- getGPStime("2018-09-15", nmeadata = nmea$raw_nmea)
  
  #parse each useful sentance
  nmea <- bind_cols(nmea, parseGGA(nmea$raw_nmea))
  
  nmea <- bind_cols(nmea, parseHDM(nmea$raw_nmea))
  
  nmea <- bind_cols(nmea, parseHDT(nmea$raw_nmea))
  
  nmea <- bind_cols(nmea, parseMWV(nmea$raw_nmea))
  
  nmea <- bind_cols(nmea, parseVHW(nmea$raw_nmea))
  

  return(nmea)
  
}
