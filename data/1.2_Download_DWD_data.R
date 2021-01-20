###########################################################################################
###########################################################################################
############# DWD Roth-Dahlem data
###########################################################################################
###########################################################################################
library(readr)        # read file
library(dplyr)        # filter and left_join
library(lubridate)    # date operations
library(ggplot2)      # plots
library(kwb.datetime) # timezone convertion
###########################################################################################
###########################################################################################
### download the DWD data
### hourly data is devided in historical and recent data (need to adjust before merge)
### example for the station number 00403 (see Map_DWD_station to find the ideal station)
###########################################################################################
###########################################################################################

###########################################################################################
### air temperature (Ta)
###########################################################################################
# download historical
temp <- tempfile()
download.url <- "http://ftp-cdc.dwd.de/climate_environment/CDC/observations_germany/climate/hourly/air_temperature/historical/"
zipfile <- 'stundenwerte_TU_00403_20020101_20191231_hist.zip'
download.file(paste0(download.url,zipfile),temp, mode="wb")
metadata <- unzip(temp)
unlink(temp)
metadata 

#read historical
dwd.temp.hist <- read_delim("./data/produkt_tu_stunde_20020101_20191231_00403.txt", # change the file name according to the period and station downloaded
                            ";", escape_double = FALSE, 
                            col_types = cols(MESS_DATUM = col_datetime(format="%Y%m%d%H")), 
                            trim_ws = TRUE)
#dwd.temp.hist$MESS_DATUM
#tail(dwd.temp.hist)
#tail(filter(dwd.temp.hist,year(MESS_DATUM)>=2018 & 
#            ymd_hms(MESS_DATUM)<="2019-04-04 01:00:00 UTC"))

###### recent
temp <- tempfile()
download.url <- "http://ftp-cdc.dwd.de/climate_environment/CDC/observations_germany/climate/hourly/air_temperature/recent/"
zipfile <- 'stundenwerte_TU_00403_akt.zip'
download.file(paste0(download.url,zipfile),temp, mode="wb")
metadata <- unzip(temp)
unlink(temp)
metadata 

dwd.temp.recent <- read_delim("./data/produkt_tu_stunde_20190404_20201004_00403.txt", # change the file name according to the period and station downloaded
                             ";", escape_double = FALSE, 
                             col_types = cols(MESS_DATUM = col_datetime(format="%Y%m%d%H")), 
                             trim_ws = TRUE)
#head(dwd.temp.recent)

# merge the historical and recent data
AirTemp <- rbind(filter(dwd.temp.hist,year(MESS_DATUM)>=2018 & 
                        ymd_hms(MESS_DATUM)<"2019-04-04 02:00:00 UTC"), #update the date according to the new file
                        dwd.temp.recent)
#rename the collumns                                
colnames(AirTemp) <- c("id","timestamp","QN_9","Ta","RH","eor")

#unique(diff(ymd_hms(AirTemp$timestamp)))
#which(diff(ymd_hms(AirTemp$timestamp))!=1)

#############################################################################
###### pressure (p)
##########################################################################
# hist
temp <- tempfile()
download.url <- "http://ftp-cdc.dwd.de/climate_environment/CDC/observations_germany/climate/hourly/pressure/historical/"
zipfile <- 'stundenwerte_P0_00403_19550101_20191231_hist.zip'
download.file(paste0(download.url,zipfile),temp, mode="wb")
metadata <- unzip(temp)
unlink(temp)
metadata 

dwd.pressure.hist <- read_delim("./data/produkt_p0_stunde_19550101_20191231_00403.txt", # change the file name according to the period and station downloaded
                                ";", escape_double = FALSE, 
                                col_types = cols(MESS_DATUM = col_datetime(format="%Y%m%d%H")), 
                                trim_ws = TRUE)

#tail(filter(dwd.temp.hist,year(MESS_DATUM)>=2018 & 
#              ymd_hms(MESS_DATUM)<="2019-04-04 01:00:00 UTC"))

# recent
temp <- tempfile()
download.url <- "http://ftp-cdc.dwd.de/climate_environment/CDC/observations_germany/climate/hourly/pressure/recent/"
zipfile <- 'stundenwerte_P0_00403_akt.zip'
download.file(paste0(download.url,zipfile),temp, mode="wb")
metadata <- unzip(temp)
unlink(temp)
metadata 

dwd.pressure.recent <- read_delim("./data/produkt_P0_stunde_20190404_20201004_00403.txt", # change the file name according to the period and station downloaded
                                  ";", escape_double = FALSE, 
                                  col_types = cols(MESS_DATUM = col_datetime(format="%Y%m%d%H")), 
                                  trim_ws = TRUE)

air.pressure <- rbind(filter(dwd.pressure.hist,year(MESS_DATUM)>=2018 & 
                          ymd_hms(MESS_DATUM)<"2019-04-04 02:00:00 UTC"),
                      dwd.pressure.recent)

colnames(air.pressure) <- c("id","timestamp","QN_8","P.sea","p","eor")

#unique(diff(ymd_hms(air.pressure$timestamp)))
#which(diff(ymd_hms(air.pressure$timestamp))!=1)

#############################################################################
###### wind_synop (ws, wd)
##########################################################################
# hist
temp <- tempfile()
download.url <- "http://ftp-cdc.dwd.de/climate_environment/CDC/observations_germany/climate/hourly/wind_synop/historical/"
zipfile <- 'stundenwerte_F_00403_19550101_20191231_hist.zip'
download.file(paste0(download.url,zipfile),temp, mode="wb")
metadata <- unzip(temp)
unlink(temp)
metadata 

dwd.wind.hist <- read_delim("./data/produkt_f_stunde_19550101_20191231_00403.txt", # change the file name according to the period and station downloaded
                            ";", escape_double = FALSE, 
                            col_types = cols(MESS_DATUM = col_datetime(format="%Y%m%d%H")), 
                            trim_ws = TRUE)

# recent
temp <- tempfile()
download.url <- "http://ftp-cdc.dwd.de/climate_environment/CDC/observations_germany/climate/hourly/wind_synop/recent/"
zipfile <- 'stundenwerte_F_00403_akt.zip'
download.file(paste0(download.url,zipfile),temp, mode="wb")
metadata <- unzip(temp)
unlink(temp)
metadata 

dwd.wind.recent <- read_delim("./data/produkt_f_stunde_20190404_20201004_00403.txt", 
                              ";", escape_double = FALSE, 
                              col_types = cols(MESS_DATUM = col_datetime(format="%Y%m%d%H")), 
                              trim_ws = TRUE)

wind <- rbind(filter(dwd.wind.hist,year(MESS_DATUM)>=2018 & 
                     ymd_hms(MESS_DATUM)<"2019-04-04 02:00:00 UTC"),
                      dwd.wind.recent)

colnames(wind) <- c("id","timestamp","QN_8","ws","wd","eor")
                    
#unique(diff(ymd_hms(wind$timestamp)))
#which(diff(ymd_hms(wind$timestamp))!=1)

#############################################################################
###### precipitation (volume mm, events 0/1)
##########################################################################
# hist
temp <- tempfile()
download.url <- "http://ftp-cdc.dwd.de/climate_environment/CDC/observations_germany/climate/hourly/precipitation/historical/"
zipfile <- 'stundenwerte_RR_00403_20020128_20191231_hist.zip'
download.file(paste0(download.url,zipfile),temp, mode="wb")
metadata <- unzip(temp)
unlink(temp)
metadata 

dwd.precipitation.hist <- read_delim("./data/produkt_rr_stunde_20020128_20191231_00403.txt", # change the file name according to the period and station downloaded
                                    ";", escape_double = FALSE, 
                                    col_types = cols(MESS_DATUM = col_datetime(format="%Y%m%d%H")), 
                                    trim_ws = TRUE)

# recent
temp <- tempfile()
download.url <- "http://ftp-cdc.dwd.de/climate_environment/CDC/observations_germany/climate/hourly/precipitation/recent/"
zipfile <- 'stundenwerte_RR_00403_akt.zip'
download.file(paste0(download.url,zipfile),temp, mode="wb")
metadata <- unzip(temp)
unlink(temp)
metadata 

dwd.precipitation.recent <- read_delim("./data/produkt_rr_stunde_20190404_20201004_00403.txt", # change the file name according to the period and station downloaded
                                      ";", escape_double = FALSE, 
                                      col_types = cols(MESS_DATUM = col_datetime(format="%Y%m%d%H")), 
                                      trim_ws = TRUE)

precipitation <- rbind(filter(dwd.precipitation.hist,year(MESS_DATUM)>=2018 & 
                       ymd_hms(MESS_DATUM)<"2019-04-04 00:00:00 UTC"),
              dwd.precipitation.recent)

colnames(precipitation) <- c("id", "timestamp", "QN_8" , "V_precip",
                             "H_precip", "precip.form","eor")

#unique(diff(ymd_hms(precipitation$timestamp)))
#which(diff(ymd_hms(precipitation$timestamp))!=1)

#####################################################
######################## solar ######################
########### Potsdam station 03987 ###################
#####################################################
#####################################################
# hist
temp <- tempfile()
download.url <- "http://ftp-cdc.dwd.de/climate_environment/CDC/observations_germany/climate/hourly/solar/"
zipfile <- 'stundenwerte_ST_03987_row.zip'
download.file(paste0(download.url,zipfile),temp, mode="wb")
metadata <- unzip(temp)
unlink(temp)
metadata 

solar <- read_delim("./data/produkt_st_stunde_19451231_20201031_03987.txt",
                   ";", escape_double = FALSE, 
                  col_names = TRUE, 
                  col_types = cols(MESS_DATUM = col_datetime(format="%Y%m%d%H:%M"), 
                                    ATMO_LBERG = col_double(),
                                    FD_LBERG = col_double(),
                                    FG_LBERG = col_double(),
                                    SD_LBERG = col_double(),
                                    ZENIT = col_double(),
                                    MESS_DATUM_WOZ = col_datetime(format="%Y%m%d%H:%M")), 
                  trim_ws = TRUE)

colnames(solar) <- c("id","MESS_DATUM", "QN_592" , "Rli",
                     "diffuse.radiation","Rin",
                     "sunshine.duration.Potsdam" ,"sun.zenith.angle",
                     "MESS_DATUM_WOZ","eor")

# convert -999 in NA values
solar <- na_if(solar, -999)
# select data from the year # on
solar <- filter(solar, year(solar$MESS_DATUM)>=2014)

# convert J/cm^2 to W m-2
# Radiation (W/m^2) = Radiation (J/cm^2) *100^2 / (step x 60 x 60 seconds) # step=1
solar$Rin <- round((solar$Rin*100*100)/(60*60),2)
solar$Rli <- round((solar$Rli*100*100)/(60*60),2)
solar$diffuse.radiation <- round((solar$diffuse.radiation*100*100)/(60*60),2)

summary(solar)
#solar$MESS_DATUM
#solar$MESS_DATUM_WOZ

# convert CEST time to UTC
solar$timestamp <- with_tz(force_tz(solar$MESS_DATUM_WOZ,"CEST"),tz="UTC")
table(diff(solar$timestamp))

##################################################################################################
#### others posible timezone transformations
##################################################################################################
#solar$timestamp <- round_date(solar$MESS_DATUM, unit = "hour")
#datesolar <- separate(solar, MESS_DATUM, sep="([\\:])",convert=TRUE,into = c("MESS_DATUM", "minutes"))
#solar$timestamp <- ymd_hms(paste(datesolar$MESS_DATUM, sep = ":", "00:00 UTC")) 
#solar$timestamp <- with_tz(force_tz(solar$MESS_DATUM_WOZ,"CET"),tz="UTC")
#solar$timestamp <- as.character(solar$MESS_DATUM_WOZ)
#solar$timestamp <- berlinNormalTimeToUTC(solar$timestamp5)
#solar$timestamp <- ymd_hms(solar$timestamp5)
#solar$timestamp <- as.POSIXct(solar$MESS_DATUM_WOZ, "Europe/Berlin", format = "%Y%m%d%H:%M")
#solar$timestamp <- textToEuropeBerlinPosix(as.character(solar$MESS_DATUM_WOZ), switches = FALSE)
#solar$timestamp <- with_tz(solar$timestamp6, tz="UTC")
##################################################################################################
##################################################################################################

####################################################
### Merge the DWD data use as SCOPE input parameters
####################################################
# create a timestamp
ts <- seq(as.POSIXct("2018-06-01", tz = "UTC"),
          as.POSIXct("2020-09-01", tz = "UTC"),
          by = "hour") #"30 min"
head(ts)
tail(ts)
unique(diff((ts)))
##################################################################################################
### creating an DWD data.frame
DWD_ROTH <- data.frame(timestamp=ts)
DWD_ROTH <- left_join(DWD_ROTH, AirTemp[,c(2,4,5)], by="timestamp", type="left", match="first")
DWD_ROTH <- left_join(DWD_ROTH, air.pressure[,c(2,5,4)], by="timestamp", type="left", match="first")
DWD_ROTH <- left_join(DWD_ROTH, wind[,c(2,4,5)], by="timestamp", type="left", match="first")
DWD_ROTH <- left_join(DWD_ROTH, precipitation[,c(2,4,5)], by="timestamp", type="left", match="first")
DWD_ROTH <- left_join(DWD_ROTH, solar[,c(6,4,5,7,8,11)], by="timestamp", type="left", match="first")

unique(diff((DWD_ROTH$timestamp)))
# convert -999 in NA values
DWD_ROTH <- na_if(DWD_ROTH, -999)
##################################################################################################
str(DWD_ROTH)
summary(DWD_ROTH)
write.csv(DWD_ROTH, file="DWD_ROTH.csv", row.names = F)


#####################################################################
#####################################################################
# other indicators
#####################################################################
#####################################################################

#####################################################################
###### soil_temperature #
#####################################################################
# hist
temp <- tempfile()
download.url <- "http://ftp-cdc.dwd.de/climate_environment/CDC/observations_germany/climate/hourly/soil_temperature/historical/"
zipfile <- 'stundenwerte_EB_00403_19510101_20191231_hist.zip'
download.file(paste0(download.url,zipfile),temp, mode="wb")
metadata <- unzip(temp)
unlink(temp)
metadata 

dwd.soil.hist <- read_delim("./data/produkt_eb_stunde_19510101_20191231_00403.txt", 
                            ";", escape_double = FALSE, 
                            col_types = cols(MESS_DATUM = col_datetime(format="%Y%m%d%H")), 
                            trim_ws = TRUE)
# recent
temp <- tempfile()
download.url <- "http://ftp-cdc.dwd.de/climate_environment/CDC/observations_germany/climate/hourly/soil_temperature/recent/"
zipfile <- 'stundenwerte_EB_00403_akt.zip'
download.file(paste0(download.url,zipfile),temp, mode="wb")
metadata <- unzip(temp)
unlink(temp)
metadata 

dwd.soil.recent <- read_delim("./data/produkt_eb_stunde_20190404_20201004_00403.txt", 
                              ";", escape_double = FALSE, 
                              col_types = cols(MESS_DATUM = col_datetime(format="%Y%m%d%H")), 
                              trim_ws = TRUE)

soil.temp <- rbind(filter(dwd.soil.hist,year(MESS_DATUM)>=2018 & 
                            ymd_hms(MESS_DATUM)<"2019-04-04 02:00:00 UTC"),
                   dwd.soil.recent)

colnames(soil.temp) <- c("id", "timestamp", "QN_2" , "soil.temp.2cm.dwd",
                         "soil.temp.5cm.dwd", "soil.temp.10cm.dwd",
                         "soil.temp.20cm.dwd","soil.temp.50cm.dwd",
                         "soil.temp.100cm.dwd","eor")

#unique(diff(ymd_hms(soil.temp$timestamp)))
#which(diff(ymd_hms(soil.temp$timestamp))!=1)

#############################################################################
###### visibility #
##########################################################################
# hist
temp <- tempfile()
download.url <- "http://ftp-cdc.dwd.de/climate_environment/CDC/observations_germany/climate/hourly/visibility/historical/"
zipfile <- 'stundenwerte_VV_00403_19550101_20191231_hist.zip'
download.file(paste0(download.url,zipfile),temp, mode="wb")
metadata <- unzip(temp)
unlink(temp)
metadata 

dwd.visibility.hist <- read_delim("./data/produkt_vv_stunde_19550101_20191231_00403.txt", 
                                  ";", escape_double = FALSE, 
                                  col_types = cols(MESS_DATUM = col_datetime(format="%Y%m%d%H")), 
                                  trim_ws = TRUE)
# recent
temp <- tempfile()
download.url <- "http://ftp-cdc.dwd.de/climate_environment/CDC/observations_germany/climate/hourly/visibility/recent/"
zipfile <- 'stundenwerte_VV_00403_akt.zip'
download.file(paste0(download.url,zipfile),temp, mode="wb")
metadata <- unzip(temp)
unlink(temp)
metadata 

dwd.visibility.recent <- read_delim("./data/produkt_vv_stunde_20190404_20201004_00403.txt", 
                                    ";", escape_double = FALSE, 
                                    col_types = cols(MESS_DATUM = col_datetime(format="%Y%m%d%H")), 
                                    trim_ws = TRUE)

visibility <- rbind(filter(dwd.visibility.hist,year(MESS_DATUM)>=2018 & 
                             ymd_hms(MESS_DATUM)<"2019-04-04 02:00:00 UTC"),
                    dwd.visibility.recent)

colnames(visibility) <- c("id", "timestamp", "QN_8" ,  "V_VV_I",
                          "visibility","eor")

#unique(diff(ymd_hms(visibility$timestamp)))
#which(diff(ymd_hms(visibility$timestamp))!=1)

#############################################################################
###### sun #
##########################################################################
# hist
temp <- tempfile()
download.url <- "http://ftp-cdc.dwd.de/climate_environment/CDC/observations_germany/climate/hourly/sun/historical/"
zipfile <- 'stundenwerte_SD_00403_19510101_20191231_hist.zip'
download.file(paste0(download.url,zipfile),temp, mode="wb")
metadata <- unzip(temp)
unlink(temp)
metadata 

dwd.sun.hist <- read_delim("./data/produkt_sd_stunde_19510101_20191231_00403.txt", 
                           ";", escape_double = FALSE, 
                           col_types = cols(MESS_DATUM = col_datetime(format="%Y%m%d%H")), 
                           trim_ws = TRUE)
# recent
temp <- tempfile()
download.url <- "http://ftp-cdc.dwd.de/climate_environment/CDC/observations_germany/climate/hourly/sun/recent/"
zipfile <- 'stundenwerte_SD_00403_akt.zip'
download.file(paste0(download.url,zipfile),temp, mode="wb")
metadata <- unzip(temp)
unlink(temp)
metadata 

dwd.sun.recent <- read_delim("./data/produkt_sd_stunde_20190404_20201004_00403.txt", 
                             ";", escape_double = FALSE, 
                             col_types = cols(MESS_DATUM = col_datetime(format="%Y%m%d%H")), 
                             trim_ws = TRUE)

sun.dwd <- rbind(filter(dwd.sun.hist,year(MESS_DATUM)>=2018 & 
                          ymd_hms(MESS_DATUM)<"2019-04-04 02:00:00 UTC"),
                 dwd.sun.recent)

colnames(sun.dwd) <- c("id", "timestamp", "QN_7" , "sunshine.duration","eor")

#unique(diff(ymd_hms(sun.dwd$timestamp)))
#which(diff(ymd_hms(sun.dwd$timestamp))!=1)

#############################################################################
###### cloud_type #
##########################################################################
# hist
temp <- tempfile()
download.url <- "http://ftp-cdc.dwd.de/climate_environment/CDC/observations_germany/climate/hourly/cloud_type/historical/"
zipfile <- 'stundenwerte_CS_00403_19550101_20191231_hist.zip'
download.file(paste0(download.url,zipfile),temp, mode="wb")
metadata <- unzip(temp)
unlink(temp)
metadata 

dwd.cloud.hist <- read_delim("./data/produkt_cs_stunde_19550101_20191231_00403.txt", 
                             ";", escape_double = FALSE, 
                             col_types = cols(MESS_DATUM = col_datetime(format="%Y%m%d%H")), 
                             trim_ws = TRUE)
# recent
temp <- tempfile()
download.url <- "http://ftp-cdc.dwd.de/climate_environment/CDC/observations_germany/climate/hourly/cloud_type/recent/"
zipfile <- 'stundenwerte_CS_00403_akt.zip'
download.file(paste0(download.url,zipfile),temp, mode="wb")
metadata <- unzip(temp)
unlink(temp)
metadata 

dwd.cloud.recent <- read_delim("./data/produkt_cs_stunde_20190404_20201004_00403.txt", 
                               ";", escape_double = FALSE, 
                               col_types = cols(MESS_DATUM = col_datetime(format="%Y%m%d%H")), 
                               trim_ws = TRUE)

cloud <- rbind(filter(dwd.cloud.hist,year(MESS_DATUM)>=2018 & 
                        ymd_hms(MESS_DATUM)<"2019-04-04 02:00:00 UTC"),
               dwd.cloud.recent)

colnames(cloud) <- c("id", "timestamp", "QN_8" ,
                     "Cloud.coverage.total",
                     "V_N_I",
                     "Cloud.type.1stlayer",
                     "Cloud.type.1st.Abrev",
                     "Cloud.coverage.1stlayer",
                     "Cloud.height.1stlayer",
                     "Cloud.type.2stlayer",
                     "Cloud.type.2st.Abrev",
                     "Cloud.coverage.2stlayer",
                     "Cloud.height.2stlayer",
                     "Cloud.type.3stlayer",
                     "Cloud.type.3st.Abrev",
                     "Cloud.coverage.3stlayer",
                     "Cloud.height.3stlayer",
                     "Cloud.type.4stlayer",
                     "Cloud.type.4st.Abrev",
                     "Cloud.coverage.4stlayer",
                     "Cloud.height.4stlayer",
                     "eor")

#unique(diff(ymd_hms(cloud$timestamp)))
#which(diff(ymd_hms(cloud$timestamp))!=1)

#############################################################################
###### dew_point #
##########################################################################
# hist
temp <- tempfile()
download.url <- "http://ftp-cdc.dwd.de/climate_environment/CDC/observations_germany/climate/hourly/dew_point/historical/"
zipfile <- 'stundenwerte_TD_00403_19550101_20191231_hist.zip'
download.file(paste0(download.url,zipfile),temp, mode="wb")
metadata <- unzip(temp)
unlink(temp)
metadata 

dwd.dew.hist <- read_delim("./data/produkt_td_stunde_19550101_20191231_00403.txt", 
                           ";", escape_double = FALSE, 
                           col_types = cols(MESS_DATUM = col_datetime(format="%Y%m%d%H")), 
                           trim_ws = TRUE)
# recent
temp <- tempfile()
download.url <- "http://ftp-cdc.dwd.de/climate_environment/CDC/observations_germany/climate/hourly/dew_point/recent/"
zipfile <- 'stundenwerte_TD_00403_akt.zip'
download.file(paste0(download.url,zipfile),temp, mode="wb")
metadata <- unzip(temp)
unlink(temp)
metadata 

dwd.dew.recent <- read_delim("./data/produkt_td_stunde_20190404_20201004_00403.txt", 
                             ";", escape_double = FALSE, 
                             col_types = cols(MESS_DATUM = col_datetime(format="%Y%m%d%H")), 
                             trim_ws = TRUE)

dew_point <- rbind(filter(dwd.dew.hist,year(MESS_DATUM)>=2018 & 
                            ymd_hms(MESS_DATUM)<"2019-04-04 02:00:00 UTC"),
                   dwd.dew.recent)

colnames(dew_point) <- c("id", "timestamp", "QN_8" , "dewpoint.temp",
                         "drybulb.temp","eor")

#unique(diff(ymd_hms(dew_point$timestamp)))
#which(diff(ymd_hms(dew_point$timestamp))!=1)
#############################################################################

# merge the other DWD data
#DWD_ROTH <- join(DWD_ROTH, visibility[,c(2,5)], by="timestamp", type="left", match="first")
#DWD_ROTH <- join(DWD_ROTH, sun.dwd[,c(2,4,3)], by="timestamp", type="left", match="first")
#DWD_ROTH <- join(DWD_ROTH, cloud[,c(2,4,6,7,9,10,11,13,14,15,17,18,19,21)], by="timestamp", type="left", match="first")
#DWD_ROTH <- left_join(DWD_ROTH, dew_point[,c(2,4,5)], by="timestamp", type="left", match="first")
#DWD_ROTH <- left_join(DWD_ROTH, soil.temp[,c(2,4,5,6,7,8,9)], by="timestamp", type="left", match="first")
#############################################################################
