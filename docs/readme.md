2018 GPYC Blue Nose
================

Background
----------

The Bayview Yacht Club hosts an annual Long Distance race around Lake St. Clair in Michigan. This is an analysis of the performance of the Yacht Zubenelgenubi in that race.

Source Data
-----------

The data from the race is collected from three on-board instruments: - Raymarine Wind Guage - Raymarine Boat Speed - Garmin GPS Plotter

These devices transmit NMEA sentances which is multiplexed and then broadcast over wifi using a [MiniPlex-3](http://www.shipmodul.com/en/miniplex-3.html).

The data is then read into [Expedition](http://www.expeditionmarine.com/about.htm) sailing software and logged as a csv file.

Load Data
---------

``` r
library(tidyverse)

#Read the Raw Data
expdat <- read.csv("../raw_data/exp_log.csv", stringsAsFactors = FALSE)

#Change the Time
expdat$time <- as.POSIXct((expdat$Utc) * (60*60*24), origin = "1899-12-30", tz = "GMT")
attributes(expdat$time )$tzone <- "America/Detroit"


#Remove any All NA Columns
#Find columns where all is NA
allna <- which(apply(expdat, 2, function(x) all(is.na(x))))   

#remove those columns
expdat <- expdat %>% select(-allna)

#Select only needed columns
expdat <- expdat %>% select(time, Bsp, Awa, Aws, Twa, Tws, Twd, Lat, Lon, Hdg, Cog, Sog)

#change names to lowercase for easier handling
names(expdat) <- tolower(names(expdat))

#Remove any rows where we don't have all data
expdat <- na.omit(expdat)

summary(expdat)
```

    ##       time                          bsp             awa         
    ##  Min.   :2018-09-22 09:49:12   Min.   :0.000   Min.   :-179.50  
    ##  1st Qu.:2018-09-22 10:44:46   1st Qu.:3.310   1st Qu.: -57.00  
    ##  Median :2018-09-22 11:41:36   Median :4.610   Median : -33.50  
    ##  Mean   :2018-09-22 11:42:25   Mean   :4.029   Mean   : -11.65  
    ##  3rd Qu.:2018-09-22 12:39:49   3rd Qu.:5.300   3rd Qu.:  30.25  
    ##  Max.   :2018-09-22 13:37:26   Max.   :6.700   Max.   : 180.00  
    ##       aws             twa               tws              twd        
    ##  Min.   : 0.50   Min.   :-179.50   Min.   : 0.490   Min.   :  0.00  
    ##  1st Qu.: 7.10   1st Qu.: -85.60   1st Qu.: 6.700   1st Qu.: 15.55  
    ##  Median :10.90   Median : -46.80   Median : 8.880   Median : 33.70  
    ##  Mean   :10.47   Mean   : -19.80   Mean   : 8.959   Mean   :120.13  
    ##  3rd Qu.:13.20   3rd Qu.:  43.15   3rd Qu.:10.850   3rd Qu.:331.50  
    ##  Max.   :77.30   Max.   : 180.00   Max.   :76.030   Max.   :360.00  
    ##       lat             lon              hdg             cog       
    ##  Min.   :42.40   Min.   :-82.88   Min.   :  0.0   Min.   :  0.0  
    ##  1st Qu.:42.43   1st Qu.:-82.87   1st Qu.: 75.4   1st Qu.: 75.4  
    ##  Median :42.46   Median :-82.84   Median :126.7   Median :127.1  
    ##  Mean   :42.45   Mean   :-82.83   Mean   :160.1   Mean   :160.0  
    ##  3rd Qu.:42.48   3rd Qu.:-82.80   3rd Qu.:267.2   3rd Qu.:267.1  
    ##  Max.   :42.50   Max.   :-82.76   Max.   :359.8   Max.   :359.6  
    ##       sog      
    ##  Min.   :0.00  
    ##  1st Qu.:3.85  
    ##  Median :4.80  
    ##  Mean   :4.36  
    ##  3rd Qu.:5.24  
    ##  Max.   :7.12

#### Clean up the Expdition Data

``` r
#Remove huge outliers
expdat <- expdat[expdat$aws < 30, ]   #we definitely didn't encounter anything over 20 kts
```

Path Sailed
-----------

Wind Direction & Speed
----------------------

``` r
source("../src/optimalPerformanceFunctions.R")

ggplot(expdat) + geom_path(aes(x=time, y=tws)) + ggtitle("True Wind Speed")
```

![](readme_files/figure-markdown_github/unnamed-chunk-3-1.png)

``` r
ggplot(expdat) + geom_path(aes(x=time, y=fromNorth(twd))) + ggtitle("True Wind Direction (Deg from North)")
```

![](readme_files/figure-markdown_github/unnamed-chunk-3-2.png)

Calculate Polar Boatspeed
-------------------------

``` r
library(akima)
library(zoo)
```

    ## 
    ## Attaching package: 'zoo'

    ## The following objects are masked from 'package:base':
    ## 
    ##     as.Date, as.Date.numeric

``` r
load("../raw_data/polarmodel.rda")

expdat$polar_bsp_target <- getOptV(btw = expdat$twa, vtw = expdat$tws, pol.model)
expdat$polar_rollmean <- rollmean(expdat$polar_bsp_target, k = 30, na.pad = TRUE)

ggplot(expdat) + 
  geom_path(aes(x=time, y=polar_bsp_target), color="gray")+
  geom_path(aes(x=time, y=polar_rollmean))
```

    ## Warning: Removed 29 rows containing missing values (geom_path).

![](readme_files/figure-markdown_github/unnamed-chunk-4-1.png)

``` r
  #geom_path(aes(x=time, y=bsp)) 
```

Distribution of Performance
---------------------------

``` r
expdat$off_polar <- expdat$polar_bsp_target - expdat$bsp

expdat$polar_bsp_perc <-  expdat$bsp / expdat$polar_bsp_target * 100

ggplot(expdat) + 
  geom_histogram(aes(x=polar_bsp_perc), binwidth=3)+
  scale_x_continuous(limits=c(0,200)) + 
  geom_vline(xintercept=100) +
  ggtitle ("% Performance to Polar Target")
```

    ## Warning: Removed 1342 rows containing non-finite values (stat_bin).

    ## Warning: Removed 1 rows containing missing values (geom_bar).

![](readme_files/figure-markdown_github/unnamed-chunk-5-1.png)

### Distributino by Wind Speed

``` r
expdat$tws_range <- cut(expdat$tws, breaks = c(0, 2, 4, 6,  8, 10, 12))

ggplot(expdat) + 
  geom_histogram(aes(x=polar_bsp_perc), binwidth=3)+
  scale_x_continuous(limits=c(0,200)) + 
  geom_vline(xintercept=100)+
  facet_grid(tws_range ~ .)
```

    ## Warning: Removed 1342 rows containing non-finite values (stat_bin).

    ## Warning: Removed 7 rows containing missing values (geom_bar).

![](readme_files/figure-markdown_github/unnamed-chunk-6-1.png)

### Distribution by Wind Angle

``` r
expdat$twa_range <- cut(expdat$twa, breaks = seq(-180, 180, by=30))

ggplot(expdat) + 
  geom_histogram(aes(x=polar_bsp_perc), binwidth=3)+
  scale_x_continuous(limits=c(0,200)) + 
  geom_vline(xintercept=100)+
  facet_grid(twa_range ~ .)
```

    ## Warning: Removed 1342 rows containing non-finite values (stat_bin).

    ## Warning: Removed 10 rows containing missing values (geom_bar).

![](readme_files/figure-markdown_github/unnamed-chunk-7-1.png)

Optimal VMC
-----------

``` r
library(geosphere)
marks <- data.frame(mk.lon=-82-(48.1/60), mk.lat=42+(29.7/60))


#bearing to mark
expdat$btm <- expdat$btm <- bearingRhumb(expdat[, c("lon", "lat")], marks)

#Calculate Optimal VMC
optimalcourse <- mapply(FUN = optvmc, expdat$btm, expdat$twd, expdat$tws, MoreArgs = list(pol.model), SIMPLIFY = FALSE)

optimalcourse <- do.call(bind_rows, optimalcourse)
  
expdat <- bind_cols(expdat, optimalcourse[, 4:9])
```

Actual VMC
----------

``` r
expdat$act_vmc <- getVMC(btm = expdat$btm, cog = expdat$cog, bsp = expdat$bsp)
```

dHDGMark
--------

``` r
expdat$d_hdg_mark <- fromNorth(expdat$btm) - fromNorth(expdat$hdg)
```

``` r
ggplot(expdat, )+geom_path(aes(x=time, y=d_hdg_mark))
```

![](readme_files/figure-markdown_github/unnamed-chunk-11-1.png)

Creating a Leaflet Map
----------------------

Save Data
---------

``` r
save(marks, file= "../shinyapp/mark.rda")
save(expdat, file =  "../shinyapp/expdat.rda")
```
