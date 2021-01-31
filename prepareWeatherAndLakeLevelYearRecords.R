## Weather for O'Hare Airport
## Marcel Merchat
## December 18, 2019

#############################################################################

## Function Definitions

#############################################################################

## Function-1:  get_real_data                            Line-34
## Function-2:  get_bool_data                            Line-46
## Function-3:  get_quote_trimmed                        Line-52
## Function-4:  get_zero_trimmed                         Line-69

#############################################################################

## Processing Scripts

#############################################################################

## Get Raw Weather Data                                 Line-94
## (chicagoData2018.csv)
## Clean Weather Data                                   Line-327
## Save processed raw data                              Line-362
## (chicagoWeatherMetricUnits.csv)
## Get Raw Lake Huron-Michigan Level Data               Line-367  
## Prepare file of year records                         Line-417
## (yearlyChicagoWeatherAndLakeLevel.csv)


#############################################################################
#############################################################################
#############################################################################
#############################################################################

## Function Definitions

get_real_data <- function(text){
    ##found <- gregexpr("\\d{1,3}", text)
    as.numeric(as.character(text))
}

get_num_data3 <- function(text){
    found <- gregexpr("-?\\d{1,3}", text)
    len <- as.numeric(as.character(attr(found[[1]],'match.length')))
    start <- found[[1]][1]
    as.numeric(as.character(substr(text, start, start+len-1)))
}

get_bool_data <- function(text){
    found <- gregexpr("\\d", text)
    start <- found[[1]][1]
    substr(text, start, start)
}

get_quote_trimmed <- function(text){
    strlen <- nchar(text)
    found <- grep('^"', text)
    len <- length(found)

    if (length(found) > 0){
        text <- substr(text, 2, strlen)
        strlen <- nchar(text)
    } 
    found <- grep('"$', text)
    len <- length(found)
    if (length(found) > 0){
        text <- substr(text, 1, strlen-1)
    } 
    text
}

get_zero_trimmed <- function(text){
    found <- grep("0\\d", text)
    len <- length(found)
    if (length(found) > 0){
        found <- gregexpr("0\\d", text)
        start <- found[[1]][1]
        text <- substr(text, start+1, start+1)
    } else {
        found <- gregexpr("\\d{2}", text)
        start <- found[[1]][1]
        text <- substr(text, start, start+1)
    }
    text
}

#############################################################################
#############################################################################

## Scripts for Processing Data

#############################################################################
#############################################################################

## Raw Data

## Get Raw Weather Data

## The raw data was downloaded following the US Government API at the  
## National Centers for Environmental Information (NCEI) Access Data Service
## using their RESTful application programming interface (API) to access and
## subset data based on a the set of parameters shown below.

## The raw data was automatically downloaded:
fileUrl <- "https://www.ncei.noaa.gov/access/services/data/v1?dataset=daily-summaries&stations=USW00094846&startDate=1958-01-01&endDate=2019-07-01&format=csv"
setwd("~/edu/physics/earthScience/chicago_weather")
## download.file(fileUrl, dest="chicagoData2018.csv")

## The end date query parameter of the internet address should be updated
## for new downloads. The last download was for data up to July 1, 2019. 

## The field definitions are given here.
## https://www1.ncdc.noaa.gov/pub/data/cdo/documentation/GHCND_documentation.pdf
## https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/readme.txt

## List of weather stations:
## ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/ghcnd-inventory.txt
##
## PRCP = Precipitation (tenths of mm) 
##        (This program changes this to express its results in mm.)
## SNOW = Snowfall (mm)
## SNWD = Snow depth (mm)
## TMAX = Maximum temperature (tenths of degrees C)
## TMIN = Minimum temperature (tenths of degrees C)
## AWND = Average daily wind speed (tenths of meters per second)
## WDF2 = Direction of fastest 2-minute wind (degrees)
## WSF2 = Fastest 2-minute wind speed (meters per second)
## WSF5 = Gust intensity as fastest 5-second wind speed (meters per second)
## WT** = Weather Type where * has one of the following values:
##   01 = Fog, ice fog, or freezing fog (may include heavy fog)
##   03 = Thunder
##   05 = Hail (may include small hail)
##   08 = Smoke or haze
##   09 = Blowing or drifting snow
##   10 = Tornado, waterspout, or funnel cloud
##   11 = High or damaging winds
##   13 = Mist
##   16 = Rain (may include freezing rain, drizzle, and freezing drizzle)
##   17 = Freezing rain
##   18 = Snow, snow pellets, snow grains, or ice crystals

###############################################################################

## Get Raw Lake Huron-Michigan Level Data

###############################################################################

## 9075014 Harbor Beach, MI Jan 01, 1860 - present
## 9087044 Calumet Harbor, IL Feb 01, 1905 - present
## Verified Monthly Mean Water Level
## monthly_mean	Verified monthly mean water level data for the station.
huronUrl <- "https://tidesandcurrents.noaa.gov/api/datagetter?begin_date=19590101&end_date=20190701&station=9075014&product=monthly_mean&datum=IGLD&units=metric&time_zone=gmt&application=web_services&format=csv"
##          https://tidesandcurrents.noaa.gov/api/datagetter?begin_date=20130101&end_date=20130101&station=8454000&product=water_level&datum=mllw&units=metric&time_zone=gmt&application=web_services&format=xml 
setwd("~/edu/physics/earthScience/chicago_weather")
## download.file(huronUrl, dest="huron_michigan_harbor_beach19592018.csv")
michUrl <- "https://tidesandcurrents.noaa.gov/api/datagetter?begin_date=19590101&end_date=20190701&station=9087044&product=monthly_mean&datum=IGLD&units=metric&time_zone=gmt&application=web_services&format=csv"
## download.file(fileUrl, dest="huron_michigan_calumet_harbor19592018.csv")


filename <- "chicagoData2018.csv"
conn <- file(filename,open="r")
rawList <- list()
date <- "1970-01-01"
year <- NA
month <- NA
day <- NA
t_max <- NA
t_min <- NA
rain <- NA
snow <- NA
snow_depth <- NA
avg_wind <- NA
wsf2 <- NA
wdf2 <- NA
wsf5 <- NA
fog_wt01  <- NA 
thunder_wt03 <- NA
hail_wt05 <- NA
haze_wt08 <- NA
blowing_snow_wt09 <- NA
funnel_cloud_wt10 <- NA
damaging_wind_wt11 <- NA
rain_wt16 <- NA
freezing_rain_wt17 <- NA
snow_wt18 <- NA
highwater <- NA
msl <- NA
lowwater <- NA
df <- data.frame(date,year,month,day,t_max,t_min,rain,snow,snow_depth,
                 avg_wind,wsf2,wdf2,wsf5,
                 fog_wt01,thunder_wt03,hail_wt05,haze_wt08,blowing_snow_wt09,
                 funnel_cloud_wt10,damaging_wind_wt11,
                 rain_wt16, freezing_rain_wt17,snow_wt18,
                 highwater, msl,lowwater,
                 stringsAsFactors = FALSE)
colnames(df) <- c("Date","Year","Month","Day","MaxTemp","MinTemp",
                  "Rain","Snow","SnowDepth",
                  "Wind","PeakTwoMinWind","PeakWindDirection","PeakGust",
                  "fog_wt01","thunder_wt03","hail_wt05",
                  "haze_wt08","blowing_snow_wt09",
                  "funnel_cloud_wt10","damaging_wind_wt11",
                  "rain_wt16", "freezing_rain_wt17","snow_wt18",
                  "HighWaterLevel","MeanWaterLevel","LowWaterLevel")
i <- 1
j <- 1
## Row-14246 corresponds to January 1, 1997
## Row-22462 corresponds to June 30, 2019
## Row-367 corresponds to January 1, 1959
while (length(one_line <- readLines(conn, n = 1, warn = FALSE)) > 0) {
    #if(i > 20366 && i < 22463)
    if(i > 366 && i < 22463){
        myVector <- strsplit(one_line,  ",")
        datestr <- myVector[[1]][2]
        df[j,"Date"] <- datestr
        #datestr <-  '"2010-01-21"' 
        split <- strsplit(datestr,"-")
        sapplied <- sapply(split, as.character)[,1]
        year <- get_quote_trimmed(sapplied[1])
        month <- get_zero_trimmed(sapplied[2])
        monthday <- get_quote_trimmed(sapplied[3])
        monthday <- get_zero_trimmed(monthday)
        df[j,"Year"] <- as.numeric(as.character(year))
        df[j,"Month"] <- month
        df[j,"Day"] <- monthday

        tmaxtext <- myVector[[1]][95]
        tmax <- get_real_data(tmaxtext)
        df[j,"MaxTemp"] <- tmax/10
        
        tmintext <- myVector[[1]][96]
        tmin <- get_real_data(tmintext)
        df[j,"MinTemp"] <- tmin/10
    
        rain <- get_real_data(myVector[[1]][31])
        
        if(!is.na(rain)){
            df[j,"Rain"] <- (rain)/10
        } else {
            df[j,"Rain"] <- 0
        }
       
        snow <- get_real_data(myVector[[1]][61])
        if(!is.na(snow)){
            df[j,"Snow"] <- snow
        } else {
            df[j,"Snow"] <- 0
        }
    
        snow_depth <- get_real_data(myVector[[1]][62])
        if(!is.na(snow_depth)){
            df[j,"SnowDepth"] <- snow_depth
        } else {
            df[j,"SnowDepth"] <- 0
        }
        if(year > 1983)
        {
            wind_avg <- get_real_data(myVector[[1]][8])
            if(!is.na(wind_avg)){
                df[j,"Wind"] <- wind_avg/10
            } else {
                df[j,"Wind"] <- 0
            }
        }
    
        if(year > 1996)
        {
            wind2min <- get_real_data(myVector[[1]][109])
            if(!is.na(wind2min)){
                df[j,"PeakTwoMinWind"] <- wind2min/10
            } else {
                df[j,"PeakTwoMinWind"] <- 0
            }
            
            wind_dir2 <- get_real_data(myVector[[1]][100])
            if(!is.na(wind_dir2)){
                df[j,"PeakWindDirection"] <- wind_dir2
            } else {
                df[j,"PeakWindDirection"] <- 0
            }
        
            wind5sec <- get_real_data(myVector[[1]][110])
            if(!is.na(wind5sec)){
                df[j,"PeakGust"] <- wind5sec/10
            } else {
                df[j,"PeakGust"] <- 0
            }
        }
            
        fog_wt01 <- get_bool_data(myVector[[1]][114])
        df[j,"fog_wt01"] <- fog_wt01
        
        thunder_wt03 <- get_bool_data(myVector[[1]][116])
        df[j,"thunder_wt03"] <- thunder_wt03
        
        wt05_hail <- get_bool_data(myVector[[1]][118])
        df[j,"hail_wt05"] <- wt05_hail
        
        wt08_haze <- get_bool_data(myVector[[1]][121])
        df[j,"haze_wt08"] <- wt08_haze
        
        wt09_blowing_snow <- get_bool_data(myVector[[1]][122])
        df[j,"blowing_snow_wt09"] <- wt09_blowing_snow
        
        wt10_funnel_cloud <- get_bool_data(myVector[[1]][123])
        df[j,"funnel_cloud_wt10"] <- wt10_funnel_cloud
        
        wt11_damaging_wind <- get_bool_data(myVector[[1]][124])
        df[j,"damaging_wind_wt11"] <- wt11_damaging_wind
        
        wt16_rain <- get_bool_data(myVector[[1]][129])
        df[j,"rain_wt16"] <- wt16_rain
        
        wt17_freezing_rain <- get_bool_data(myVector[[1]][130])
        df[j,"freezing_rain_wt17"] <- wt17_freezing_rain
        
        wt18_snow <- get_bool_data(myVector[[1]][131])
        df[j,"snow_wt18"] <- wt18_snow

        j <- j + 1
    }
    i <- i + 1
} 
close(conn) ## Reading raw weather data is complete

#############################################################################

##  Clean Weather Data and save as chicagoWeatherMetricUnits.csv

#############################################################################

## For obvious cases such as snow in July, replace missing data with 0

df[df$fog_wt01=="","fog_wt01"] <- 0
df[df$thunder_wt03=="","thunder_wt03"] <- 0
df[df$hail_wt05=="","hail_wt05"] <- 0
df[df$haze_wt08=="","haze_wt08"] <- 0
df[df$blowing_snow_wt09=="","blowing_snow_wt09"] <- 0
df[df$funnel_cloud_wt10=="","funnel_cloud_wt10"] <- 0
df[df$damaging_wind_wt11=="","damaging_wind_wt11"] <- 0
df[df$rain_wt16=="","rain_wt16"] <- 0
df[df$freezing_rain_wt17=="","freezing_rain_wt17"] <- 0
df[df$snow_wt18=="","snow_wt18"] <- 0

## The file named chicagoWeatherMetricUnits.csv is similar to the original data,
## consisting of daily records. It is modified to handle obvious cases of
## missing data such as snow in July, missing wind data before the
## introduction wind data, and metric units for temperature and distance. 

## 1. There are separate columns for the year, month, and date as integers
## 2. The data begins in 1959 when maximum and minimium temperature are 
##    are first reported over a full year. 
## 3. Temperatures are now indicated in degrees Centigrade. In the original
##    data, the temperature is multiplied by 10 to eliminate any decimals. 
## 4. The amount of rain precipitation is provided in millimeters.
##    In the original data, the rain data is multiplied by 10
##    to eliminate any decimal.
## 5. Missing data is indicated as 'NA' values. For example, there is only 
##    complete average wind speed data beginning in 1984. Peak daily two minute
##    wind speed and direction as well as peak 5-second wind gusts begin
##    in 1997. 

write.csv(df, file = "chicagoWeatherMetricUnits.csv")

#############################################################################

## Lake-Level Data

#############################################################################

## Import monthly average lake-level data.

filename <- "huron_michigan_calumet_harbor19592018.csv"
conn <- file(filename,open="r")
readLines(conn, n = 1, warn = FALSE)

while (length(one_line <- readLines(conn, n = 1, warn = FALSE)) > 0) {
    myVector <- strsplit(one_line,  ",")
    yearstr <- myVector[[1]][1]
    y <- as.numeric(as.character(yearstr))
    
    monthstr <- myVector[[1]][2]
    m <- as.numeric(as.character(monthstr))
    
    highest <- myVector[[1]][3]
    high_level <- get_real_data(highest)
 ## The monthly average is duplicated for each month day record of the month.
    if(!is.na(high_level)){
        df[df$Year==y & df$Month==m,"HighWaterLevel"] <- high_level
    } else {
        df[df$Year==y & df$Month==m,"HighWaterLevel"] <- NA
    }
    
    mean_level <- myVector[[1]][6]
    meanlevel <- get_real_data(mean_level)
    if(!is.na(mean_level)){
        df[df$Year==y & df$Month==m,"MeanWaterLevel"] <- mean_level
    } else {
        df[df$Year==y & df$Month==m,"MeanWaterLevel"] <- NA
    }
    
    lowest <- myVector[[1]][17]
    low_level <- get_real_data(lowest)
    if(!is.na(low_level)){
        df[df$Year==y & df$Month==m,"LowWaterLevel"] <- low_level
    } else {
        df[df$Year==y & df$Month==m,"LowWaterLevel"] <- NA
    }
} 
close(conn)

## Reading lake level data is completed.


################################################################################

## Prepare file of year records and save as yearlyChicagoWeatherAndLakeLevel.csv

################################################################################


## Thirteen fields are provided for each type of weather item including 
## a year average field and twelve month average fields.

dfr <- read.csv("chicagoWeatherMetricUnits.csv",stringsAsFactors = FALSE)

Year <- c(1959:2018)
yearlyChicagoWeather <- data.frame(Year) 
year <- 1959

while( year > 1958 & year < 2019){
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"YearAverageMaxTemp"] <-
        mean(c(
        dfr[dfr$Year==year & dfr$Month > 6,"MaxTemp"],
        dfr[dfr$Year==(year+1) & dfr$Month < 7,"MaxTemp"]))
    
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"JulAverageMaxTemp"] <-
        mean(dfr[dfr$Year==year & dfr$Month==7, "MaxTemp"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"AugAverageMaxTemp"] <-
        mean(dfr[dfr$Year==year & dfr$Month==8, "MaxTemp"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"SepAverageMaxTemp"] <-
        mean(dfr[dfr$Year==year & dfr$Month==9, "MaxTemp"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"OctAverageMaxTemp"] <-
        mean(dfr[dfr$Year==year & dfr$Month==10, "MaxTemp"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"NovAverageMaxTemp"] <-
        mean(dfr[dfr$Year==year & dfr$Month==11, "MaxTemp"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"DecAverageMaxTemp"] <-
        mean(dfr[dfr$Year==year & dfr$Month==12, "MaxTemp"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"JanAverageMaxTemp"] <-
        mean(dfr[dfr$Year==year+1 & dfr$Month==1, "MaxTemp"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"FebAverageMaxTemp"] <-
        mean(dfr[dfr$Year==year+1 & dfr$Month==2, "MaxTemp"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"MarAverageMaxTemp"] <-
        mean(dfr[dfr$Year==year+1 & dfr$Month==3, "MaxTemp"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"AprAverageMaxTemp"] <-
        mean(dfr[dfr$Year==year+1 & dfr$Month==4, "MaxTemp"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"MayAverageMaxTemp"] <-
        mean(dfr[dfr$Year==year+1 & dfr$Month==5, "MaxTemp"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"JunAverageMaxTemp"] <-
        mean(dfr[dfr$Year==year+1 & dfr$Month==6, "MaxTemp"])
    
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"YearAverageMinTemp"] <-
        mean(c(
        dfr[dfr$Year==year & dfr$Month > 6,"MinTemp"],
        dfr[dfr$Year==(year+1) & dfr$Month < 7,"MinTemp"]))
    
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"JulAverageMinTemp"] <-
        mean(dfr[dfr$Year==year & dfr$Month==7, "MinTemp"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"AugAverageMinTemp"] <-
        mean(dfr[dfr$Year==year & dfr$Month==8, "MinTemp"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"SepAverageMinTemp"] <-
        mean(dfr[dfr$Year==year & dfr$Month==9, "MinTemp"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"OctAverageMinTemp"] <-
        mean(dfr[dfr$Year==year & dfr$Month==10, "MinTemp"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"NovAverageMinTemp"] <-
        mean(dfr[dfr$Year==year & dfr$Month==11, "MinTemp"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"DecAverageMinTemp"] <-
        mean(dfr[dfr$Year==year & dfr$Month==12, "MinTemp"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"JanAverageMinTemp"] <-
        mean(dfr[dfr$Year==year+1 & dfr$Month==1, "MinTemp"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"FebAverageMinTemp"] <-
        mean(dfr[dfr$Year==year+1 & dfr$Month==2, "MinTemp"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"MarAverageMinTemp"] <-
        mean(dfr[dfr$Year==year+1 & dfr$Month==3, "MinTemp"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"AprAverageMinTemp"] <-
        mean(dfr[dfr$Year==year+1 & dfr$Month==4, "MinTemp"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"MayAverageMinTemp"] <-
        mean(dfr[dfr$Year==year+1 & dfr$Month==5, "MinTemp"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"JunAverageMinTemp"] <-
        mean(dfr[dfr$Year==year+1 & dfr$Month==6, "MinTemp"])
    
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"YearRainPrcpTotal"] <-
        sum(c(
        dfr[dfr$Year==year & dfr$Month > 6,"Rain"],
        dfr[dfr$Year==(year+1) & dfr$Month < 7,"Rain"]))
    
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"JulRain"] <-
        sum(dfr[dfr$Year==year & dfr$Month==7, "Rain"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"AugRain"] <-
        sum(dfr[dfr$Year==year & dfr$Month==8, "Rain"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"SepRain"] <-
        sum(dfr[dfr$Year==year & dfr$Month==9, "Rain"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"OctRain"] <-
        sum(dfr[dfr$Year==year & dfr$Month==10, "Rain"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"NovRain"] <-
        sum(dfr[dfr$Year==year & dfr$Month==11, "Rain"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"DecRain"] <-
        sum(dfr[dfr$Year==year & dfr$Month==12, "Rain"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"JanRain"] <-
        sum(dfr[dfr$Year==year+1 & dfr$Month==1, "Rain"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"FebRain"] <-
        sum(dfr[dfr$Year==year+1 & dfr$Month==2, "Rain"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"MarRain"] <-
        sum(dfr[dfr$Year==year+1 & dfr$Month==3, "Rain"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"AprRain"] <-
        sum(dfr[dfr$Year==year+1 & dfr$Month==4, "Rain"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"MayRain"] <-
        sum(dfr[dfr$Year==year+1 & dfr$Month==5, "Rain"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"JunRain"] <-
        sum(dfr[dfr$Year==year+1 & dfr$Month==6, "Rain"])
    
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"YearSnowTotal"] <-
        sum(c(
               dfr[dfr$Year==year & dfr$Month > 6,"Snow"],
               dfr[dfr$Year==(year+1) & dfr$Month < 7,"Snow"]))
    
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"JulSnow"] <-
        sum(dfr[dfr$Year==year & dfr$Month==7, "Snow"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"AugSnow"] <-
        sum(dfr[dfr$Year==year & dfr$Month==8, "Snow"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"SepSnow"] <-
        sum(dfr[dfr$Year==year & dfr$Month==9, "Snow"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"OctSnow"] <-
        sum(dfr[dfr$Year==year & dfr$Month==10, "Snow"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"NovSnow"] <-
        sum(dfr[dfr$Year==year & dfr$Month==11, "Snow"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"DecSnow"] <-
        sum(dfr[dfr$Year==year & dfr$Month==12, "Snow"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"JanSnow"] <-
        sum(dfr[dfr$Year==year+1 & dfr$Month==1, "Snow"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"FebSnow"] <-
        sum(dfr[dfr$Year==year+1 & dfr$Month==2, "Snow"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"MarSnow"] <-
        sum(dfr[dfr$Year==year+1 & dfr$Month==3, "Snow"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"AprSnow"] <-
        sum(dfr[dfr$Year==year+1 & dfr$Month==4, "Snow"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"MaySnow"] <-
        sum(dfr[dfr$Year==year+1 & dfr$Month==5, "Snow"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"JunSnow"] <-
        sum(dfr[dfr$Year==year+1 & dfr$Month==6, "Snow"])
    
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"YearAverageSnowDepth"] <-
        mean(c(
        dfr[dfr$Year==year & dfr$Month > 6,"SnowDepth"],
        dfr[dfr$Year==(year+1) & dfr$Month < 7,"SnowDepth"]))
    
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"JulSnowDepth"] <-
        mean(dfr[dfr$Year==year & dfr$Month==7, "SnowDepth"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"AugSnowDepth"] <-
        mean(dfr[dfr$Year==year & dfr$Month==8, "SnowDepth"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"SepSnowDepth"] <-
        mean(dfr[dfr$Year==year & dfr$Month==9, "SnowDepth"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"OctSnowDepth"] <-
        mean(dfr[dfr$Year==year & dfr$Month==10, "SnowDepth"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"NovSnowDepth"] <-
        mean(dfr[dfr$Year==year & dfr$Month==11, "SnowDepth"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"DecSnowDepth"] <-
        mean(dfr[dfr$Year==year & dfr$Month==12, "SnowDepth"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"JanSnowDepth"] <-
        mean(dfr[dfr$Year==year+1 & dfr$Month==1, "SnowDepth"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"FebSnowDepth"] <-
        mean(dfr[dfr$Year==year+1 & dfr$Month==2, "SnowDepth"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"MarSnowDepth"] <-
        mean(dfr[dfr$Year==year+1 & dfr$Month==3, "SnowDepth"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"AprSnowDepth"] <-
        mean(dfr[dfr$Year==year+1 & dfr$Month==4, "SnowDepth"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"MaySnowDepth"] <-
        mean(dfr[dfr$Year==year+1 & dfr$Month==5, "SnowDepth"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"JunSnowDepth"] <-
        mean(dfr[dfr$Year==year+1 & dfr$Month==6, "SnowDepth"])
    
        year <- year + 1
}

## The first full year of Wind data is 1984 
year <- 1984
while( year > 1983 & year < 2019){
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"YearAverageWind"] <-
        mean(c(
        dfr[dfr$Year==year & dfr$Month > 6,"Wind"],
        dfr[dfr$Year==(year+1) & dfr$Month < 7,"Wind"]), na.rm = TRUE)
    
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"JulWind"] <-
        mean(dfr[dfr$Year==year & dfr$Month==7, "Wind"],na.rm = TRUE)
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"AugWind"] <-
        mean(dfr[dfr$Year==year & dfr$Month==8, "Wind"],na.rm = TRUE)
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"SepWind"] <-
        mean(dfr[dfr$Year==year & dfr$Month==9, "Wind"],na.rm = TRUE)
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"OctWind"] <-
        mean(dfr[dfr$Year==year & dfr$Month==10, "Wind"],na.rm = TRUE)
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"NovWind"] <-
        mean(dfr[dfr$Year==year & dfr$Month==11, "Wind"],na.rm = TRUE)
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"DecWind"] <-
        mean(dfr[dfr$Year==year & dfr$Month==12, "Wind"],na.rm = TRUE)
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"JanWind"] <-
        mean(dfr[dfr$Year==year+1 & dfr$Month==1, "Wind"],na.rm = TRUE)
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"FebWind"] <-
        mean(dfr[dfr$Year==year+1 & dfr$Month==2, "Wind"],na.rm = TRUE)
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"MarWind"] <-
        mean(dfr[dfr$Year==year+1 & dfr$Month==3, "Wind"],na.rm = TRUE)
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"AprWind"] <-
        mean(dfr[dfr$Year==year+1 & dfr$Month==4, "Wind"],na.rm = TRUE,)
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"MayWind"] <-
        mean(dfr[dfr$Year==year+1 & dfr$Month==5, "Wind"],na.rm = TRUE)
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"JunWind"] <-
        mean(dfr[dfr$Year==year+1 & dfr$Month==6, "Wind"],na.rm = TRUE)

    year <- year + 1
}

## The first full year of peak wind data is 1997 
year <- 1997
while( year > 1996 & year < 2019){
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"PeakTwoMinWind"] <-
        max(c(
        dfr[dfr$Year==year & dfr$Month > 6,"PeakTwoMinWind"],
        dfr[dfr$Year==(year+1) & dfr$Month < 7,"PeakTwoMinWind"]),
        na.rm = TRUE)

    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"JulPeakTwoMinWind"] <-
        max(dfr[dfr$Year==year & dfr$Month==7, "PeakTwoMinWind"],na.rm = TRUE)
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"AugPeakTwoMinWind"] <-
        max(dfr[dfr$Year==year & dfr$Month==8, "PeakTwoMinWind"],na.rm = TRUE)
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"SepPeakTwoMinWind"] <-
        max(dfr[dfr$Year==year & dfr$Month==9, "PeakTwoMinWind"],na.rm = TRUE)
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"OctPeakTwoMinWind"] <-
        max(dfr[dfr$Year==year & dfr$Month==10, "PeakTwoMinWind"],na.rm = TRUE)
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"NovPeakTwoMinWind"] <-
        max(dfr[dfr$Year==year & dfr$Month==11, "PeakTwoMinWind"],na.rm = TRUE)
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"DecPeakTwoMinWind"] <-
        max(dfr[dfr$Year==year & dfr$Month==12, "PeakTwoMinWind"],na.rm = TRUE)
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"JanPeakTwoMinWind"] <-
        max(dfr[dfr$Year==year+1 & dfr$Month==1, "PeakTwoMinWind"],na.rm = TRUE)
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"FebPeakTwoMinWind"] <-
        max(dfr[dfr$Year==year+1 & dfr$Month==2, "PeakTwoMinWind"],na.rm = TRUE)
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"MarPeakTwoMinWind"] <-
        max(dfr[dfr$Year==year+1 & dfr$Month==3, "PeakTwoMinWind"],na.rm = TRUE)
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"AprPeakTwoMinWind"] <-
        max(dfr[dfr$Year==year+1 & dfr$Month==4, "PeakTwoMinWind"],na.rm = TRUE)
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"MayPeakTwoMinWind"] <-
        max(dfr[dfr$Year==year+1 & dfr$Month==5, "PeakTwoMinWind"],na.rm = TRUE)
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"JunPeakTwoMinWind"] <-
        max(dfr[dfr$Year==year+1 & dfr$Month==6, "PeakTwoMinWind"],na.rm = TRUE)
    
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"AveragePeakWindDirection"] <-
        mean(c(
        dfr[dfr$Year==year & dfr$Month > 6,"PeakWindDirection"],
        dfr[dfr$Year==(year+1) & dfr$Month < 7,"PeakWindDirection"]),na.rm = TRUE)
    
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"JulPeakWindDirection"] <-
        mean(dfr[dfr$Year==year & dfr$Month==7, "PeakWindDirection"],na.rm = TRUE)
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"AugPeakWindDirection"] <-
        mean(dfr[dfr$Year==year & dfr$Month==8, "PeakWindDirection"],na.rm = TRUE)
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"SepPeakWindDirection"] <-
        mean(dfr[dfr$Year==year & dfr$Month==9, "PeakWindDirection"],na.rm = TRUE)
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"OctPeakWindDirection"] <-
        mean(dfr[dfr$Year==year & dfr$Month==10, "PeakWindDirection"],na.rm = TRUE)
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"NovPeakWindDirection"] <-
        mean(dfr[dfr$Year==year & dfr$Month==11, "PeakWindDirection"],na.rm = TRUE)
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"DecPeakWindDirection"] <-
        mean(dfr[dfr$Year==year & dfr$Month==12, "PeakWindDirection"],na.rm = TRUE)
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"JanPeakWindDirection"] <-
        mean(dfr[dfr$Year==year+1 & dfr$Month==1, "PeakWindDirection"],na.rm = TRUE)
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"FebPeakWindDirection"] <-
        mean(dfr[dfr$Year==year+1 & dfr$Month==2, "PeakWindDirection"],na.rm = TRUE)
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"MarPeakWindDirection"] <-
        mean(dfr[dfr$Year==year+1 & dfr$Month==3, "PeakWindDirection"],na.rm = TRUE)
    
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"AprPeakWindDirection"] <-
        mean(dfr[dfr$Year==year+1 & dfr$Month==4, "PeakWindDirection"],na.rm = TRUE)
    
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"MayPeakWindDirection"] <-
        mean(dfr[dfr$Year==year+1 & dfr$Month==5, "PeakWindDirection"],na.rm = TRUE)
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"JunPeakWindDirection"] <-
        mean(dfr[dfr$Year==year+1 & dfr$Month==6, "PeakWindDirection"],na.rm = TRUE)
    
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"YearPeakGust"] <-
        max(c(
        dfr[dfr$Year==year & dfr$Month > 6,"PeakGust"],
        dfr[dfr$Year==(year+1) & dfr$Month < 7,"PeakGust"]))
    
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"JulPeakGust"] <-
        max(dfr[dfr$Year==year & dfr$Month==7, "PeakGust"],na.rm = TRUE)
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"AugPeakGust"] <-
        max(dfr[dfr$Year==year & dfr$Month==8, "PeakGust"],na.rm = TRUE)
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"SepPeakGust"] <-
        max(dfr[dfr$Year==year & dfr$Month==9, "PeakGust"],na.rm = TRUE)
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"OctPeakGust"] <-
        max(dfr[dfr$Year==year & dfr$Month==10, "PeakGust"],na.rm = TRUE)
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"NovPeakGust"] <-
        max(dfr[dfr$Year==year & dfr$Month==11, "PeakGust"],na.rm = TRUE)
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"DecPeakGust"] <-
        max(dfr[dfr$Year==year & dfr$Month==12, "PeakGust"],na.rm = TRUE)
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"JanPeakGust"] <-
        max(dfr[dfr$Year==year+1 & dfr$Month==1, "PeakGust"],na.rm = TRUE)
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"FebPeakGust"] <-
        max(dfr[dfr$Year==year+1 & dfr$Month==2, "PeakGust"],na.rm = TRUE)
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"MarPeakGust"] <-
        max(dfr[dfr$Year==year+1 & dfr$Month==3, "PeakGust"],na.rm = TRUE)
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"AprPeakGust"] <-
        max(dfr[dfr$Year==year+1 & dfr$Month==4, "PeakGust"],na.rm = TRUE)
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"MayPeakGust"] <-
        max(dfr[dfr$Year==year+1 & dfr$Month==5, "PeakGust"],na.rm = TRUE)
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"JunPeakGust"] <-
        max(dfr[dfr$Year==year+1 & dfr$Month==6, "PeakGust"],na.rm = TRUE)
    
    year <- year + 1
}

year <- 1959
while( year > 1958 & year < 2019){
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"YearFog"] <-
        mean(c(
        dfr[dfr$Year==year & dfr$Month > 6,"fog_wt01"],
        dfr[dfr$Year==(year+1) & dfr$Month < 7,"fog_wt01"]))
    
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"JulFog"] <-
        mean(dfr[dfr$Year==year & dfr$Month==7, "fog_wt01"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"AugFog"] <-
        mean(dfr[dfr$Year==year & dfr$Month==8, "fog_wt01"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"SepFog"] <-
        mean(dfr[dfr$Year==year & dfr$Month==9, "fog_wt01"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"OctFog"] <-
        mean(dfr[dfr$Year==year & dfr$Month==10, "fog_wt01"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"NovFog"] <-
        mean(dfr[dfr$Year==year & dfr$Month==11, "fog_wt01"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"DecFog"] <-
        mean(dfr[dfr$Year==year & dfr$Month==12, "fog_wt01"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"JanFog"] <-
        mean(dfr[dfr$Year==year+1 & dfr$Month==1, "fog_wt01"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"FebFog"] <-
        mean(dfr[dfr$Year==year+1 & dfr$Month==2, "fog_wt01"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"MarFog"] <-
        mean(dfr[dfr$Year==year+1 & dfr$Month==3, "fog_wt01"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"AprFog"] <-
        mean(dfr[dfr$Year==year+1 & dfr$Month==4, "fog_wt01"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"MayFog"] <-
        mean(dfr[dfr$Year==year+1 & dfr$Month==5, "fog_wt01"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"JunFog"] <-
        mean(dfr[dfr$Year==year+1 & dfr$Month==6, "fog_wt01"])
    
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"YearThunder"] <-
        mean(dfr[dfr$Year==year,"thunder_wt03"])
        mean(c(
        dfr[dfr$Year==year & dfr$Month > 6,"thunder_wt03"],
        dfr[dfr$Year==(year+1) & dfr$Month < 7,"thunder_wt03"]))
    
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"JulThunder"] <-
        mean(dfr[dfr$Year==year & dfr$Month==7, "thunder_wt03"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"AugThunder"] <-
        mean(dfr[dfr$Year==year & dfr$Month==8, "thunder_wt03"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"SepThunder"] <-
        mean(dfr[dfr$Year==year & dfr$Month==9, "thunder_wt03"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"OctThunder"] <-
        mean(dfr[dfr$Year==year & dfr$Month==10, "thunder_wt03"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"NovThunder"] <-
        mean(dfr[dfr$Year==year & dfr$Month==11, "thunder_wt03"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"DecThunder"] <-
        mean(dfr[dfr$Year==year & dfr$Month==12, "thunder_wt03"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"JanThunder"] <-
        mean(dfr[dfr$Year==year+1 & dfr$Month==1, "thunder_wt03"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"FebThunder"] <-
        mean(dfr[dfr$Year==year+1 & dfr$Month==2, "thunder_wt03"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"MarThunder"] <-
        mean(dfr[dfr$Year==year+1 & dfr$Month==3, "thunder_wt03"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"AprThunder"] <-
        mean(dfr[dfr$Year==year+1 & dfr$Month==4, "thunder_wt03"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"MayThunder"] <-
        mean(dfr[dfr$Year==year+1 & dfr$Month==5, "thunder_wt03"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"JunThunder"] <-
        mean(dfr[dfr$Year==year+1 & dfr$Month==6, "thunder_wt03"])
    
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"YearHail"] <-
        mean(c(
        dfr[dfr$Year==year & dfr$Month > 6,"hail_wt05"],
        dfr[dfr$Year==(year+1) & dfr$Month < 7,"hail_wt05"]))
    
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"JulHail"] <-
        mean(dfr[dfr$Year==year & dfr$Month==7, "hail_wt05"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"AugHail"] <-
        mean(dfr[dfr$Year==year & dfr$Month==8, "hail_wt05"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"SepHail"] <-
        mean(dfr[dfr$Year==year & dfr$Month==9, "hail_wt05"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"OctHail"] <-
        mean(dfr[dfr$Year==year & dfr$Month==10, "hail_wt05"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"NovHail"] <-
        mean(dfr[dfr$Year==year & dfr$Month==11, "hail_wt05"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"DecHail"] <-
        mean(dfr[dfr$Year==year & dfr$Month==12, "hail_wt05"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"JanHail"] <-
        mean(dfr[dfr$Year==year+1 & dfr$Month==1, "hail_wt05"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"FebHail"] <-
        mean(dfr[dfr$Year==year+1 & dfr$Month==2, "hail_wt05"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"MarHail"] <-
        mean(dfr[dfr$Year==year+1 & dfr$Month==3, "hail_wt05"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"AprHail"] <-
        mean(dfr[dfr$Year==year+1 & dfr$Month==4, "hail_wt05"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"MayHail"] <-
        mean(dfr[dfr$Year==year+1 & dfr$Month==5, "hail_wt05"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"JunHail"] <-
        mean(dfr[dfr$Year==year+1 & dfr$Month==6, "hail_wt05"])
    
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"YearHaze"] <-
        mean(c(
        dfr[dfr$Year==year & dfr$Month > 6,"haze_wt08"],
        dfr[dfr$Year==(year+1) & dfr$Month < 7,"haze_wt08"]))
    
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"JulHaze"] <-
        mean(dfr[dfr$Year==year & dfr$Month==7, "haze_wt08"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"AugHaze"] <-
        mean(dfr[dfr$Year==year & dfr$Month==8, "haze_wt08"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"SepHaze"] <-
        mean(dfr[dfr$Year==year & dfr$Month==9, "haze_wt08"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"OctHaze"] <-
        mean(dfr[dfr$Year==year & dfr$Month==10, "haze_wt08"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"NovHaze"] <-
        mean(dfr[dfr$Year==year & dfr$Month==11, "haze_wt08"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"DecHaze"] <-
        mean(dfr[dfr$Year==year & dfr$Month==12, "haze_wt08"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"JanHaze"] <-
        mean(dfr[dfr$Year==year+1 & dfr$Month==1, "haze_wt08"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"FebHaze"] <-
        mean(dfr[dfr$Year==year+1 & dfr$Month==2, "haze_wt08"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"MarHaze"] <-
        mean(dfr[dfr$Year==year+1 & dfr$Month==3, "haze_wt08"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"AprHaze"] <-
        mean(dfr[dfr$Year==year+1 & dfr$Month==4, "haze_wt08"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"MayHaze"] <-
        mean(dfr[dfr$Year==year+1 & dfr$Month==5, "haze_wt08"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"JunHaze"] <-
        mean(dfr[dfr$Year==year+1 & dfr$Month==6, "haze_wt08"])
    
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"YearBlowingSnow"] <-
        mean(c(
        dfr[dfr$Year==year & dfr$Month > 6,"blowing_snow_wt09"],
        dfr[dfr$Year==(year+1) & dfr$Month < 7,"blowing_snow_wt09"]))
    
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"JulBlowingSnow"] <-
        mean(dfr[dfr$Year==year & dfr$Month==7, "blowing_snow_wt09"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"AugBlowingSnow"] <-
        mean(dfr[dfr$Year==year & dfr$Month==8, "blowing_snow_wt09"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"SepBlowingSnow"] <-
        mean(dfr[dfr$Year==year & dfr$Month==9, "blowing_snow_wt09"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"OctBlowingSnow"] <-
        mean(dfr[dfr$Year==year & dfr$Month==10, "blowing_snow_wt09"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"NovBlowingSnow"] <-
        mean(dfr[dfr$Year==year & dfr$Month==11, "blowing_snow_wt09"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"DecBlowingSnow"] <-
        mean(dfr[dfr$Year==year & dfr$Month==12, "blowing_snow_wt09"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"JanBlowingSnow"] <-
        mean(dfr[dfr$Year==year+1 & dfr$Month==1, "blowing_snow_wt09"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"FebBlowingSnow"] <-
        mean(dfr[dfr$Year==year+1 & dfr$Month==2, "blowing_snow_wt09"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"MarBlowingSnow"] <-
        mean(dfr[dfr$Year==year+1 & dfr$Month==3, "blowing_snow_wt09"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"AprBlowingSnow"] <-
        mean(dfr[dfr$Year==year+1 & dfr$Month==4, "blowing_snow_wt09"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"MayBlowingSnow"] <-
        mean(dfr[dfr$Year==year+1 & dfr$Month==5, "blowing_snow_wt09"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"JunBlowingSnow"] <-
        mean(dfr[dfr$Year==year+1 & dfr$Month==6, "blowing_snow_wt09"])
    
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"YearFunnelCloud"] <-
        #mean(dfr[dfr$Year==year,"funnel_cloud_wt10"])
        mean(c(
        dfr[dfr$Year==year & dfr$Month > 6,"funnel_cloud_wt10"],
        dfr[dfr$Year==(year+1) & dfr$Month < 7,"funnel_cloud_wt10"]))
    
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"JulFunnelCloud"] <-
        mean(dfr[dfr$Year==year & dfr$Month==7, "funnel_cloud_wt10"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"AugFunnelCloud"] <-
        mean(dfr[dfr$Year==year & dfr$Month==8, "funnel_cloud_wt10"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"SepFunnelCloud"] <-
        mean(dfr[dfr$Year==year & dfr$Month==9, "funnel_cloud_wt10"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"OctFunnelCloud"] <-
        mean(dfr[dfr$Year==year & dfr$Month==10, "funnel_cloud_wt10"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"NovFunnelCloud"] <-
        mean(dfr[dfr$Year==year & dfr$Month==11, "funnel_cloud_wt10"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"DecFunnelCloud"] <-
        mean(dfr[dfr$Year==year & dfr$Month==12, "funnel_cloud_wt10"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"JanFunnelCloud"] <-
        mean(dfr[dfr$Year==year+1 & dfr$Month==1, "funnel_cloud_wt10"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"FebFunnelCloud"] <-
        mean(dfr[dfr$Year==year+1 & dfr$Month==2, "funnel_cloud_wt10"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"MarFunnelCloud"] <-
        mean(dfr[dfr$Year==year+1 & dfr$Month==3, "funnel_cloud_wt10"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"AprFunnelCloud"] <-
        mean(dfr[dfr$Year==year+1 & dfr$Month==4, "funnel_cloud_wt10"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"MayFunnelCloud"] <-
        mean(dfr[dfr$Year==year+1 & dfr$Month==5, "funnel_cloud_wt10"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"JunFunnelCloud"] <-
        mean(dfr[dfr$Year==year+1 & dfr$Month==6, "funnel_cloud_wt10"])
    
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"YearDamagingWind"] <-
        mean(c(
        dfr[dfr$Year==year & dfr$Month > 6,"damaging_wind_wt11"],
        dfr[dfr$Year==(year+1) & dfr$Month < 7,"damaging_wind_wt11"]))
    
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"JulDamagingWind"] <-
        mean(dfr[dfr$Year==year & dfr$Month==7, "damaging_wind_wt11"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"AugDamagingWind"] <-
        mean(dfr[dfr$Year==year & dfr$Month==8, "damaging_wind_wt11"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"SepDamagingWind"] <-
        mean(dfr[dfr$Year==year & dfr$Month==9, "damaging_wind_wt11"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"OctDamagingWind"] <-
        mean(dfr[dfr$Year==year & dfr$Month==10, "damaging_wind_wt11"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"NovDamagingWind"] <-
        mean(dfr[dfr$Year==year & dfr$Month==11, "damaging_wind_wt11"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"DecDamagingWind"] <-
        mean(dfr[dfr$Year==year & dfr$Month==12, "damaging_wind_wt11"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"JanDamagingWind"] <-
        mean(dfr[dfr$Year==year+1 & dfr$Month==1, "damaging_wind_wt11"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"FebDamagingWind"] <-
        mean(dfr[dfr$Year==year+1 & dfr$Month==2, "damaging_wind_wt11"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"MarDamagingWind"] <-
        mean(dfr[dfr$Year==year+1 & dfr$Month==3, "damaging_wind_wt11"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"AprDamagingWind"] <-
        mean(dfr[dfr$Year==year+1 & dfr$Month==4, "damaging_wind_wt11"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"MayDamagingWind"] <-
        mean(dfr[dfr$Year==year+1 & dfr$Month==5, "damaging_wind_wt11"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"JunDamagingWind"] <-
        mean(dfr[dfr$Year==year+1 & dfr$Month==6, "damaging_wind_wt11"])
    
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"YearRain_wt16"] <-
        mean(c(
        dfr[dfr$Year==year & dfr$Month > 6,"rain_wt16"],
        dfr[dfr$Year==(year+1) & dfr$Month < 7,"rain_wt16"]))
    
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"JulRain_wt16"] <-
        mean(dfr[dfr$Year==year & dfr$Month==7, "rain_wt16"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"AugRain_wt16"] <-
        mean(dfr[dfr$Year==year & dfr$Month==8, "rain_wt16"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"SepRain_wt16"] <-
        mean(dfr[dfr$Year==year & dfr$Month==9, "rain_wt16"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"OctRain_wt16"] <-
        mean(dfr[dfr$Year==year & dfr$Month==10, "rain_wt16"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"NovRain_wt16"] <-
        mean(dfr[dfr$Year==year & dfr$Month==11, "rain_wt16"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"DecRain_wt16"] <-
        mean(dfr[dfr$Year==year & dfr$Month==12, "rain_wt16"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"JanRain_wt16"] <-
        mean(dfr[dfr$Year==year+1 & dfr$Month==1, "rain_wt16"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"FebRain_wt16"] <-
        mean(dfr[dfr$Year==year+1 & dfr$Month==2, "rain_wt16"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"MarRain_wt16"] <-
        mean(dfr[dfr$Year==year+1 & dfr$Month==3, "rain_wt16"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"AprRain_wt16"] <-
        mean(dfr[dfr$Year==year+1 & dfr$Month==4, "rain_wt16"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"MayRain_wt16"] <-
        mean(dfr[dfr$Year==year+1 & dfr$Month==5, "rain_wt16"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"JunRain_wt16"] <-
        mean(dfr[dfr$Year==year+1 & dfr$Month==6, "rain_wt16"])
    
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"YearFreezingRain_wt16"] <-
        mean(c(
        dfr[dfr$Year==year & dfr$Month > 6,"freezing_rain_wt17"],
        dfr[dfr$Year==(year+1) & dfr$Month < 7,"freezing_rain_wt17"]))
    
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"JulFreezingRain"] <-
        mean(dfr[dfr$Year==year & dfr$Month==7, "freezing_rain_wt17"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"AugFreezingRain"] <-
        mean(dfr[dfr$Year==year & dfr$Month==8, "freezing_rain_wt17"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"SepFreezingRain"] <-
        mean(dfr[dfr$Year==year & dfr$Month==9, "freezing_rain_wt17"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"OctFreezingRain"] <-
        mean(dfr[dfr$Year==year & dfr$Month==10, "freezing_rain_wt17"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"NovFreezingRain"] <-
        mean(dfr[dfr$Year==year & dfr$Month==11, "freezing_rain_wt17"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"DecFreezingRain"] <-
        mean(dfr[dfr$Year==year & dfr$Month==12, "freezing_rain_wt17"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"JanFreezingRain"] <-
        mean(dfr[dfr$Year==year+1 & dfr$Month==1, "freezing_rain_wt17"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"FebFreezingRain"] <-
        mean(dfr[dfr$Year==year+1 & dfr$Month==2, "freezing_rain_wt17"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"MarFreezingRain"] <-
        mean(dfr[dfr$Year==year+1 & dfr$Month==3, "freezing_rain_wt17"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"AprFreezingRain"] <-
        mean(dfr[dfr$Year==year+1 & dfr$Month==4, "freezing_rain_wt17"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"MayFreezingRain"] <-
        mean(dfr[dfr$Year==year+1 & dfr$Month==5, "freezing_rain_wt17"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"JunFreezingRain"] <-
        mean(dfr[dfr$Year==year+1 & dfr$Month==6, "freezing_rain_wt17"])
    
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"YearSnow_wt18"] <-
        #mean(dfr[dfr$Year==year,"snow_wt18"])
        mean(c(
        dfr[dfr$Year==year & dfr$Month > 6,"snow_wt18"],
        dfr[dfr$Year==(year+1) & dfr$Month < 7,"snow_wt18"]))
    
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"JulSnow_wt18"] <-
        mean(dfr[dfr$Year==year & dfr$Month==7, "snow_wt18"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"AugSnow_wt18"] <-
        mean(dfr[dfr$Year==year & dfr$Month==8, "snow_wt18"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"SepSnow_wt18"] <-
        mean(dfr[dfr$Year==year & dfr$Month==9, "snow_wt18"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"OctSnow_wt18"] <-
        mean(dfr[dfr$Year==year & dfr$Month==10, "snow_wt18"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"NovSnow_wt18"] <-
        mean(dfr[dfr$Year==year & dfr$Month==11, "snow_wt18"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"DecSnow_wt18"] <-
        mean(dfr[dfr$Year==year & dfr$Month==12, "snow_wt18"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"JanSnow_wt18"] <-
        mean(dfr[dfr$Year==year+1 & dfr$Month==1, "snow_wt18"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"FebSnow_wt18"] <-
        mean(dfr[dfr$Year==year+1 & dfr$Month==2, "snow_wt18"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"MarSnow_wt18"] <-
        mean(dfr[dfr$Year==year+1 & dfr$Month==3, "snow_wt18"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"AprSnow_wt18"] <-
        mean(dfr[dfr$Year==year+1 & dfr$Month==4, "snow_wt18"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"MaySnow_wt18"] <-
        mean(dfr[dfr$Year==year+1 & dfr$Month==5, "snow_wt18"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"JunSnow_wt18"] <-
        mean(dfr[dfr$Year==year+1 & dfr$Month==6, "snow_wt18"])
    
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"YearAverageLowWaterLevel"] <-
        mean(c(
        dfr[dfr$Year==year & dfr$Month > 6,"LowWaterLevel"],
        dfr[dfr$Year==(year+1) & dfr$Month < 7,"LowWaterLevel"]))
    
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"JulAverageLowWaterLevel"] <-
        mean(dfr[dfr$Year==year & dfr$Month==7, "LowWaterLevel"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"AugAverageLowWaterLevel"] <-
        mean(dfr[dfr$Year==year & dfr$Month==8, "LowWaterLevel"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"SepAverageLowWaterLevel"] <-
        mean(dfr[dfr$Year==year & dfr$Month==9, "LowWaterLevel"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"OctAverageLowWaterLevel"] <-
        mean(dfr[dfr$Year==year & dfr$Month==10, "LowWaterLevel"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"NovAverageLowWaterLevel"] <-
        mean(dfr[dfr$Year==year & dfr$Month==11, "LowWaterLevel"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"DecAverageLowWaterLevel"] <-
        mean(dfr[dfr$Year==year & dfr$Month==12, "LowWaterLevel"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"JanAverageLowWaterLevel"] <-
        mean(dfr[dfr$Year==year+1 & dfr$Month==1, "LowWaterLevel"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"FebAverageLowWaterLevel"] <-
        mean(dfr[dfr$Year==year+1 & dfr$Month==2, "LowWaterLevel"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"MarAverageLowWaterLevel"] <-
        mean(dfr[dfr$Year==year+1 & dfr$Month==3, "LowWaterLevel"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"AprAverageLowWaterLevel"] <-
        mean(dfr[dfr$Year==year+1 & dfr$Month==4, "LowWaterLevel"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"MayAverageLowWaterLevel"] <-
        mean(dfr[dfr$Year==year+1 & dfr$Month==5, "LowWaterLevel"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"JunAverageLowWaterLevel"] <-
        mean(dfr[dfr$Year==year+1 & dfr$Month==6, "LowWaterLevel"])
    
    
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"YearAverageMeanWaterLevel"] <-
        mean(c(
        dfr[dfr$Year==year & dfr$Month > 6,"MeanWaterLevel"],
        dfr[dfr$Year==(year+1) & dfr$Month < 7,"MeanWaterLevel"]))
    
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"JulAverageMeanWaterLevel"] <-
        mean(dfr[dfr$Year==year & dfr$Month==7, "MeanWaterLevel"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"AugAverageMeanWaterLevel"] <-
        mean(dfr[dfr$Year==year & dfr$Month==8, "MeanWaterLevel"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"SepAverageMeanWaterLevel"] <-
        mean(dfr[dfr$Year==year & dfr$Month==9, "MeanWaterLevel"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"OctAverageMeanWaterLevel"] <-
        mean(dfr[dfr$Year==year & dfr$Month==10, "MeanWaterLevel"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"NovAverageMeanWaterLevel"] <-
        mean(dfr[dfr$Year==year & dfr$Month==11, "MeanWaterLevel"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"DecAverageMeanWaterLevel"] <-
        mean(dfr[dfr$Year==year & dfr$Month==12, "MeanWaterLevel"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"JanAverageMeanWaterLevel"] <-
        mean(dfr[dfr$Year==year+1 & dfr$Month==1, "MeanWaterLevel"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"FebAverageMeanWaterLevel"] <-
        mean(dfr[dfr$Year==year+1 & dfr$Month==2, "MeanWaterLevel"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"MarAverageMeanWaterLevel"] <-
        mean(dfr[dfr$Year==year+1 & dfr$Month==3, "MeanWaterLevel"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"AprAverageMeanWaterLevel"] <-
        mean(dfr[dfr$Year==year+1 & dfr$Month==4, "MeanWaterLevel"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"MayAverageMeanWaterLevel"] <-
        mean(dfr[dfr$Year==year+1 & dfr$Month==5, "MeanWaterLevel"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"JunAverageMeanWaterLevel"] <-
        mean(dfr[dfr$Year==year+1 & dfr$Month==6, "MeanWaterLevel"])
    
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"YearAverageHighWaterLevel"] <-
        mean(c(
        dfr[dfr$Year==year & dfr$Month > 6,"HighWaterLevel"],
        dfr[dfr$Year==(year+1) & dfr$Month < 7,"HighWaterLevel"]))
    
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"JulAverageHighWaterLevel"] <-
        mean(dfr[dfr$Year==year & dfr$Month==7, "HighWaterLevel"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"AugAverageHighWaterLevel"] <-
        mean(dfr[dfr$Year==year & dfr$Month==8, "HighWaterLevel"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"SepAverageHighWaterLevel"] <-
        mean(dfr[dfr$Year==year & dfr$Month==9, "HighWaterLevel"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"OctAverageHighWaterLevel"] <-
        mean(dfr[dfr$Year==year & dfr$Month==10, "HighWaterLevel"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"NovAverageHighWaterLevel"] <-
        mean(dfr[dfr$Year==year & dfr$Month==11, "HighWaterLevel"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"DecAverageHighWaterLevel"] <-
        mean(dfr[dfr$Year==year & dfr$Month==12, "HighWaterLevel"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"JanAverageHighWaterLevel"] <-
        mean(dfr[dfr$Year==year+1 & dfr$Month==1, "HighWaterLevel"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"FebAverageHighWaterLevel"] <-
        mean(dfr[dfr$Year==year+1 & dfr$Month==2, "HighWaterLevel"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"MarAverageHighWaterLevel"] <-
        mean(dfr[dfr$Year==year+1 & dfr$Month==3, "HighWaterLevel"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"AprAverageHighWaterLevel"] <-
        mean(dfr[dfr$Year==year+1 & dfr$Month==4, "HighWaterLevel"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"MayAverageHighWaterLevel"] <-
        mean(dfr[dfr$Year==year+1 & dfr$Month==5, "HighWaterLevel"])
    yearlyChicagoWeather[yearlyChicagoWeather$Year==year,"JunAverageHighWaterLevel"] <-
        mean(dfr[dfr$Year==year+1 & dfr$Month==6, "HighWaterLevel"])

    year <- year + 1
}

write.csv(yearlyChicagoWeather, file = "yearlyChicagoWeatherAndLakeLevel.csv",row.names=FALSE)