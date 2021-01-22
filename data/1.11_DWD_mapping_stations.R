################################################################################
library(readr)     # read files
library(sp)        # spDistsN1 function
library(mapview)   # mapview
library(dplyr)     # arange
################################################################################
##### all DWD stations
DWDstations <- read_table2("./data/TU_Stundenwerte_Beschreibung_Stationen.txt", 
                           locale = locale(encoding = "ASCII"), 
                           skip = 1)

colnames(DWDstations) <- c("id","from_date","to_date","height",
                           "lat", "long", "name", "state")
################################################################################

################## coordination ############################
## Steglitz / Rothenburg (ROTH)
# Lat.: 52.457232°
# Lon.: 13.315827°
###################################################################
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

################## coordination ############################
## TUCC 
# Lat.: 52.512283°
# Lon.: 13.327855°
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
###################################################################
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
################################################
temp <- tempfile()
download.url <- "http://ftp-cdc.dwd.de/climate_environment/CDC/observations_germany/climate/hourly/air_temperature/recent/"
zipfile <- 'stundenwerte_TU_00430_akt.zip'
download.file(paste0(download.url,zipfile),temp, mode="wb")
metadata <- unzip(temp)
unlink(temp)
metadata 
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

# Metadata
##############################################################################################
############# Berlin-Dahlem climate station ###### 00403 #####################################
##############################################################################################
# Measurement height Climate station: 51m above sea level and 2m above ground
# Coordinates: WGS 84 [EPSG 4326]  R=13°18'6.2" H=52°27'13.3"
#     ETRS89 UTM 33N [EPSG 25833]  R=384598.7   H=5812859.2
##############################################################################################
# Wind measurement: 51 m above sea level
# Wind measurement: 26 m above ground
# Coordinates: WGS 84 [EPSG 4326]: R=13°18'38.3" H=52°27'27.7"
#      ETRS89 UTM 33N [EPSG 25833]: R=385216.1    H=5813287.8
##############################################################################################
# colnamesD(WDdata)      # original name:                  unit          file
##############################################################################################
#[1]  "timestamp (UTC)"  # MESS_DATUM - yyyymmddhh               (UTC)
#[2]  "air.temp"         # TT_TU - air temperature at 2m height   (°C)
#[3]  "RH"               # RF_TU - relative humidity at 2m height  (%)          
#[6]  "pressure.station" # P - Pressure at station height        (hPA) 
#[8]  "precipitation.mm" # R1 - hourly precipitation              (mm)          
#[18] "sunshine.min"     # SD_SO - sunshine duration         (minutes)  
#[21] "wind.speed"       # F - Average wind speed - synop        (m/s) stundenwerte_F_00403_akt
#[22] "wind.direction"   # D - wind direction     - synop     (degree) stundenwerte_F_00403_akt
################################################################################################

# Metadata
##############################################################################################
############# Berlin-Tegelclimate station #### 00430 #########################################
##############################################################################################
# Measurement height Climate station: 36m above sea level and 2m above ground
# Coordinates: WGS 84 [EPSG 4326] 
#     ETRS89 UTM 33N [EPSG 25833]  52.56440     13.30880
##############################################################################################
# Wind measurement: 36 m above sea level
# Wind measurement: 10 m above ground
##############################################################################################

###################################################################################################
################ Postdam station ### 03987 ########################################################
###################################################################################################
# Coordinates: WGS 84 [EPSG 4326]: R=13°04" H=52°23"
#     ETRS89 UTM 33N [EPSG 25833]: R=368418.6 H=5805433.8
# Measurement height	81 m above sea level
###################################################################################################
#[39] "atm.radiation"           # ATMO_LBERG hourly total of atmospheric Counter radiation (J/cm^2)
#[40] "global.radiation"        # FG_LBERG hours total of Global radiation                 (J/cm^2) 
#[41] "diffuse.solar.radiation" # FD_LBERG hours total of diffuse solar radiation          (J/cm^2)
###################################################################################################

