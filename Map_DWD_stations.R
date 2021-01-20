###################################################################
library(readr) 
library(sp)        # spDistsN1 function
library(mapview)
###################################################################
##### all DWD stations
DWDstations <- read_table2("./data/TU_Stundenwerte_Beschreibung_Stationen.txt", 
                           locale = locale(encoding = "ASCII"), 
                           skip = 1)

colnames(DWDstations) <- c("id","from_date","to_date","height",
                           "lat", "long", "name", "state")
############################################################
################## coordination ############################
## Steglitz / Rothenburg (ROTH)
# Lat.: 52.457232°
# Lon.: 13.315827°
############################################################
############################################################
### distance from Steglitz / Rothenburg (ROTH) tower
DWDstations$distRoth <- spDistsN1(pts=as.matrix(cbind(DWDstations$long,DWDstations$lat)),
                                  pt=c(13.315827,52.457232), # long lat ROTH
                                  longlat=TRUE)
arrange(DWDstations, distRoth) 
###################################################################
### closer for Steglitz (1.04km)
# "Berlin-Dahlem" # 00403 
# 2002.01.01 to 2020.07.15 
# h=51m 2m above ground 
# station coordination 52.4537   13.3017 
###################################################################
### closer for radiation  (19.2 km) 
### Potsdam ## 03987 
###################################################################


############################################################
############################################################
############################################################
################## coordination ############################
## TUCC 
# Lat.: 52.512283°
# Lon.: 13.327855°
############################################################
############################################################
### distance from 
DWDstations$distTU <- spDistsN1(pts=as.matrix(cbind(DWDstations$long,DWDstations$lat)),
                                pt=c(13.327855,52.512283), # long lat TU
                                longlat=TRUE)

spDistsN1(pts=as.matrix(cbind(c(13.315827,13.327855),
                              c(52.457232,52.512283))) ,
          pt=c(13.327855,52.512283), # long lat TU
          longlat=TRUE)

arrange(DWDstations, distTU)
####################################################################
### closer to TU 
# (5.35km) "Berlin-Alexanderplatz"  ## 00399 # too high
# (5.94km) "Berlin-Tegel"           ## 00430 # more similar to the tower 
# 2002.01.01 to 2020.07.15 
# h=36m 2m above ground 
# station coordination 52.6 13.3 
###################################################################
### closer for radiation  (23.2 km) 
### Potsdam ## 03987 
###################################################################


################################################
### check the station location vs Steglitz
###############################################
temp <- tempfile()
download.url <- "http://ftp-cdc.dwd.de/climate_environment/CDC/observations_germany/climate/hourly/air_temperature/recent/"
zipfile <- 'stundenwerte_TU_00403_akt.zip'
download.file(paste0(download.url,zipfile),temp, mode="wb")
metadata <- unzip(temp)
unlink(temp)
metadata 
############################################################
## coordination Steglitz / Rothenburg (ROTH)
# Lat.: 52.457232°
# Lon.: 13.315827°
############################################################
data.meta <- read.table("./Metadaten_Geographie_00403.txt", sep = ";", header = T)
data.meta <- rbind(data.meta, c(001,40,52.4572,13.31582,19500101,19970711,
                                'Rothemburg-Steglitz (Roth)','active'))
data.meta[,3] <- as.numeric(data.meta[,3])
data.meta[,4] <- as.numeric(data.meta[,4])

data.meta['Status'] <- c('Berlin-Dahlem (not active)', 'Berlin-Dahlem', 'Roth')

data.meta.spp <- SpatialPointsDataFrame(coords = as.matrix(data.meta[,c('Geogr.Laenge','Geogr.Breite')]),
                                        data = data.meta,
                                        proj4string = CRS("+init=epsg:4326")) # WGS 84

mapview(data.meta.spp,
        zcol='Status', 
        color=c('red', 'blue', 'green'))

############################################################

################################################
### check the station location vs TU
###############################################
temp <- tempfile()
download.url <- "http://ftp-cdc.dwd.de/climate_environment/CDC/observations_germany/climate/hourly/air_temperature/recent/"
zipfile <- 'stundenwerte_TU_00430_akt.zip'
download.file(paste0(download.url,zipfile),temp, mode="wb")
metadata <- unzip(temp)
unlink(temp)
metadata 
############################################################
############################################################
################## coordination ############################
## TU 
# Lat 52.512283
# Long 13.327855
############################################################
############################################################
############################################################
data.meta.TU <- read.table("./Metadaten_Geographie_00430.txt", sep = ";", header = T)
data.meta.TU <- rbind(data.meta.TU, c(001,40,52.512283,13.327855,19940101,NA,
                                'TU_tower (TU)','active'))
data.meta.TU[,3] <- as.numeric(data.meta.TU[,3])
data.meta.TU[,4] <- as.numeric(data.meta.TU[,4])

data.meta.TU['Status'] <- c("old1", "old2",'Tegel', 'TU')

data.meta.TU <- SpatialPointsDataFrame(coords =as.matrix(data.meta.TU[,c('Geogr.Laenge','Geogr.Breite')]) ,
                                       data = data.meta.TU,
                                       proj4string = CRS("+init=epsg:4326")) # WGS 84

mapview(data.meta.TU,
        zcol='Status', 
        color=c('red', 'blue', 'green', "black"))



#####################################################
#### DWD Weather data sets
#####################################################
#  air_temperature  ##   14-Nov-2018 08:11 # 1,2,5,7
#  cloud_type       ##   14-Nov-2018 08:10 # 1,2,5,7
#  cloudiness       ##   14-Nov-2018 08:10 # 1,2,5,7
#  dew_point        ##   09-May-2019 08:51 # 1,2,5,7
#  precipitation    ##   14-Nov-2018 08:11 # 1,2,5,7
#  pressure         ##   14-Nov-2018 08:10 # 1,2,5,7
#  soil_temperature ##   14-Nov-2018 08:12 # 1,2,5,7
#  solar            ##   16-Jul-2020 08:45 # 7
#  sun              ##   14-Nov-2018 08:11 # 1,2,5,7
#  visibility       ##   14-Nov-2018 08:12 # 1,2,5,7
#  wind             ##   14-Nov-2018 08:12 # 2,5,7
#  wind_synop       ##   09-May-2019 08:51 # 1,2,5,7
##########################################################
################################################################
##################### Metadata #################################
#################################################################
#TT_TU # air temperature at 2m height (DWD station)
#RF_TU # relative humidity at 2m height (DWDStation)
#P0    # Pressure at station height (2m)
#P     # Pressure at see level
#R1    # hourly precipitation mm
#RS_IND # 0 no precipitation 1 precipitation fell
#WRTR   # WR precipitation form 
# "V_TE002"   # temperature at 2 cm deepth  
# "V_TE005"   # temperature at 5 cm deepth 
# "V_TE010"   # temperature at 10 cm deepth 
# "V_TE020"   # temperature at 20 cm deepth 
# "V_TE050"   # temperature at 50 cm deepth 
# "V_TE100"   # temperature at 100 cm deepth 
#SD_SO) # sunshine duration - minutes
#V_VV)   # Visibility in meter
#V_VV_I) # from the observer
#FF) # Average wind speed 
#DD) # wind direction
#TT) # dry bulb temperature at 2 meter above ground
#TD) # dew point temperature at 2 meter above ground
#V_N)     # Total coverage - eighth levels # same as cloudness
#V_S1_CS) # V_S1_CS - Cloud type (see abbreviationbelow) of the 1st layer
#V_S1_CSA)# V_S1_CSA - cloud type of 1st layer
#V_S1_HHS)# V_S1_HHS - Cloud height of the 1st layer meters
#V_S1_NS) # V_S1_NS - Degree of coverage of the 1st layer
# V_S2_CS -  Cloud type of the 2nd layer code see overview
# V_S2_CSA - cloud type_ abbreviation of 2 layer
# V_S2_HHS - Cloud height of the 2nd layer meters
# V_S2_NS - Degree of coverage of the 2nd layer
# V_S3_CS - Cloud type of the 3rd layer Code see overview
# V_S3_CSA - cloud type_ abbreviation of 3 layer
# V_S3_HHS - Cloud height of the 3rd layer meters
# V_S3_NS - Degree of coverage of the 3rd layer
# V_S4_CS - Cloud type of the 4th layer code see overview
# V_S4_CSA - cloud type_ abbreviation of 4th layer
# V_S4_HHS - Cloud height of the 4th layer meters
# V_S4_NS - Degree of coverage of the 4th layer
# Cirrus        0 CI
# Cirrocumulus  1 CC
# Cirrostratus  2 CS
# Altocumulus   3 AC
# Altostratus   4 AS
# Nimbostratus  5 NS
# Stratocumulus 6 SC
# Stratus       7 ST
# Cumulus       8 CU
# Cumulonimbus  9 CB
# bei Instrumentenmessung -1 -1

#### Weather variables #####################################################   
############################################################################
############################################################################
########################## metadata #######################################
##########################################################################
# read DWD station data near to Steglitz
# id from_date to_date height lat long name       state  distRoth distTU
# 403 20020101 20200715  51  52.5 13.3 Berlin-Dahlem (FU) 1.04   6.76 km
#3987 18930101 20200715  81  52.4 13.1 Potsdam  Branden~  19.2   23.2 
##########################################################################
#DWDdata <- read_csv("DWDdata.csv")
#DWDdata <- na_if(DWDdata[,2:43], -999)
#colnames(DWDdata) <- list("timestamp","air.temperature.DW",
#"relative.humidity.DW", "QN9", 
#"air.pressure.see" , "air.pressure.DW", "QN_8", "precipitation.DW",      
#"precip.no.yes", "precip.form", "soil.temp.2cm", "soil.temp.5cm",          
#"soil.temp.10cm", "soil.temp.20cm", "soil.temp.50cm",         
#"soil.temp.100cm", "QN_2", "sunshine.min",       
#"QN_7", "visibility", "wind.speed.DW",            
#"wind.direction.DW", "dewpoint.temp", "drybulb.temp",          
#"Cloudness.degree", "Cloud.coverage.total", "Cloud.coverage.1stlayer",
#"Cloud.coverage.2stlayer", "Cloud.coverage.3stlayer", "Cloud.coverage.4stlayer",
#"Cloud.type.1stlayer", "Cloud.type.2stlayer", "Cloud.type.3stlayer", 
#"Cloud.type.4stlayer", "Cloud.height.1stlayer", "Cloud.height.2stlayer",
#"Cloud.height.3stlayer",  "Cloud.height.4stlayer", 
#"Radiation.longwave.in.DW", "Radiation.shortwave.in.DW", "diffuse.radiation.DW", "sun.zenith.angle")

#########################################################################
############# Berlin-Dahlem climate station #############################
#########################################################################
# Measurement height Climate station: 51 m above sea level
# Coordinates: WGS 84 [EPSG 4326]  R=13°18'6.2" H=52°27'13.3"
#     ETRS89 UTM 33N [EPSG 25833]  R=384598.7   H=5812859.2
#########################################################################
# Wind measurement: 93 m above sea level
# Wind measurement: 26 m above ground
# Coordinates: WGS 84 [EPSG 4326]: R=13°18'38.3" H=52°27'27.7"
#      ETRS89 UTM 33N [EPSG 25833]: R=385216.1    H=5813287.8
########################################################################
# colnamesD(WDdata)      # original name:                  unit          file
########################################################################
#[1]  "timestamp (UTC)"  # MESS_DATUM - yyyymmddhh               (UTC)
#[2]  "air.temp"         # TT_TU - air temperature at 2m height   (°C)
#[3]  "RH"               # RF_TU - relative humidity at 2m height  (%)          
#[6]  "pressure.station" # P - Pressure at station height        (hPA) 
#[8]  "precipitation.mm" # R1 - hourly precipitation              (mm)          
#[18] "sunshine.min"     # SD_SO - sunshine duration         (minutes)  
#[21] "wind.speed"       # F - Average wind speed - synop        (m/s) stundenwerte_F_00403_akt
#[22] "wind.direction"   # D - wind direction     - synop     (degree) stundenwerte_F_00403_akt
###############################################################################
################ Postdam station ##############################################
###############################################################################
# Coordinates: WGS 84 [EPSG 4326]: R=13°04" H=52°23"
#     ETRS89 UTM 33N [EPSG 25833]: R=368418.6 H=5805433.8
# Measurement height	81 m above sea level
###################################################################################################
#[39] "atm.radiation"           # ATMO_LBERG hourly total of atmospheric Counter radiation (J/cm^2)
#[40] "global.radiation"        # FG_LBERG hours total of Global radiation                 (J/cm^2) 
#[41] "diffuse.solar.radiation" # FD_LBERG hours total of diffuse solar radiation          (J/cm^2)
###################################################################################################

#######