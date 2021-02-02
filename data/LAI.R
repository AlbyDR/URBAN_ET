####################################################################
####################################################################
####################################################################
##### LAI
library(raster)
library(lubridate)
library(ggplot2)
library(zoo)
library(dplyr)
library(sf)
library(rgdal) # for spTransform
library(ncdf4) # read .nc data
##########################################################################
########################################################################
coord <- data.frame(
  ROTHlon= 13.315827, ROTHlat= 52.457232,
  ROTHlonUTM=385566.5, ROTHlatUTM= 5813229,
  DWDROTHlon= 13.3017, DWDROTHlat= 52.4537,
  DWDROTHlonUTM= 384597.4, DWDROTHlatUTM= 5812858,
  TUCClon= 13.32785, TUCClat= 52.51228,
  TUCClonUTM=386525.1, TUCClatUTM= 5819332,
  DWDTUCClon= 13.3088, DWDTUCClat= 52.5644,
  DWDTUCClonUTM= 385368.3, DWDTUCClatUTM=5825159,
  ROTHGreenlonUTM=384850, ROTHGreenlatUTM=5812850,
  TUCCGreenlonUTM=387500,TUCCGreenlatUTM=5819400)
########################################################################

File.names <- list.files(path="D:/Research topics/Data-Modelling/data/LAI300m/",
                         pattern= "*.nc",
                         full.names = T)

File.date <- sapply(1:length(File.names),FUN=function(i) 
  substring(File.names[i], 61, 69)) # year

LAI.copernicus <- sapply(1:length(File.names),FUN=function(i) 
                  stack(raster(File.names[i],
                               crs="+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
                        ))

# Crop to an extent around the two towers/DWD stations to reduce the size
LAI300m <- crop(LAI.copernicus, extent(13.25,13.50,52.4,52.6))
plot(LAI300m[[26]])
points(coord$ROTHlon, coord$ROTHlat)
points(coord$TUCClon, coord$TUCClat)
points(coord$DWDROTHlon, coord$DWDROTHlat)
points(coord$DWDTUCClon, coord$DWDTUCClat)

mapr <- raster(xmn=380000,xmx=399000,ymn=5811000,ymx=5828000, 
               res=10,
               crs=crs("+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs"))

LAI300m.sites <- projectRaster(LAI300m, mapr)
names(LAI300m.sites) <- File.date
plot(LAI300m.sites$X201810100)
points(coord$ROTHlonUTM, coord$ROTHlatUTM)
points(coord$TUCClonUTM, coord$TUCClatUTM)
points(coord$DWDROTHlonUTM, coord$DWDROTHlatUTM)
points(coord$DWDTUCClonUTM, coord$DWDTUCClatUTM)
points(coord$ROTHGreenlonUTM, coord$ROTHGreenlatUTM)
points(coord$TUCCGreenlonUTM, coord$TUCCGreenlatUTM)

#############################################################
#############################################################
### buffer
# extract the average tree height (height is given by the raster pixel value)
# at the tower location use a buffer of 20 meters and mean function (fun) 
LAI_estimations <- c()
  
LAI_estimations$ROTH_buffer <- unlist(sapply(1:102,FUN=function(i) 
                        raster::extract(x = LAI300m.sites[[i]], 
                                y = coord[3:4], 
                                buffer=500, na.rm=TRUE,
                                fun=mean, 
                                df=TRUE))[2,])

LAI_estimations$ROTH_point <- sapply(1:102,FUN=function(i) 
  raster::extract(LAI300m.sites[[i]], coord[3:4]))

LAI_estimations$ROTH_DWD_buffer <- unlist(sapply(1:102,FUN=function(i) 
                    raster::extract(x = LAI300m.sites[[i]], 
                    y = coord[7:8], 
                    buffer=500, na.rm=TRUE, 
                    fun=mean, 
                    df=TRUE))[2,])

LAI_estimations$ROTH_DWD_point <- sapply(1:102,FUN=function(i) 
  raster::extract(LAI300m.sites[[i]], coord[7:8]))

LAI_estimations$ROTH_green_buffer <- unlist(sapply(1:102,FUN=function(i) 
  raster::extract(x = LAI300m.sites[[i]], 
                  y = coord[17:18], 
                  buffer=500,
                  fun=mean, na.rm=TRUE, 
                  df=TRUE))[2,])

LAI_estimations$ROTH_green_point <- sapply(1:102,FUN=function(i) 
  raster::extract(LAI300m.sites[[i]], coord[17:18]))

LAI_estimations$TUCC_buffer <- unlist(sapply(1:102,FUN=function(i) 
  raster::extract(x = LAI300m.sites[[i]], 
                  y = coord[11:12], 
                  buffer=500, na.rm=TRUE,
                  fun=mean, 
                  df=TRUE))[2,])

LAI_estimations$TUCC_point <- sapply(1:102,FUN=function(i) 
  raster::extract(LAI300m.sites[[i]], coord[11:12]))

LAI_estimations$TUCC_DWD_buffer <- unlist(sapply(1:102,FUN=function(i) 
  raster::extract(x = LAI300m.sites[[i]], 
                  y = coord[15:16], 
                  buffer=500, na.rm=TRUE, 
                  fun=mean, 
                  df=TRUE))[2,])

LAI_estimations$TUCC_DWD_point <- sapply(1:102,FUN=function(i) 
  raster::extract(LAI300m.sites[[i]], coord[15:16]))

LAI_estimations$TUCC_green_buffer <- unlist(sapply(1:102,FUN=function(i) 
  raster::extract(x = LAI300m.sites[[i]], 
                  y = coord[19:20], 
                  buffer=500,
                  fun=mean, na.rm=TRUE, 
                  df=TRUE))[2,])

LAI_estimations$TUCC_green_point <- sapply(1:102,FUN=function(i) 
  raster::extract(LAI300m.sites[[i]], coord[19:20]))

LAI_estimations <- data.frame(LAI_estimations)
summary(LAI_estimations)

LAI_estimations$timestamp <- sapply(1:102,FUN=function(i) 
  paste0(substring(names(LAI300m.sites)[i], 2, 5),"-",
         substring(names(LAI300m.sites)[i], 6, 7),"-",
         substring(names(LAI300m.sites)[i], 8, 9)," 12:00:00 UTC"))

LAI_estimations$timestamp <- ymd_hms(LAI_estimations$timestamp)

head(LAI_estimations)
# starting with non-NA
LAI_estimations[1,1:12] <- LAI_estimations[6,1:12]

ggplot(LAI_estimations, aes(x=timestamp)) +
  geom_line(aes(y=ROTH_buffer), colour="green") +
  geom_line(aes(y=ROTH_point), colour="darkgreen") +
  geom_line(aes(y=ROTH_DWD_buffer), colour="blue") +
  geom_line(aes(y=ROTH_DWD_point), colour="darkblue") +
  geom_line(aes(y=ROTH_green_buffer), colour="grey") +
  geom_line(aes(y=ROTH_green_point), colour="darkgrey")

head(LAI_estimations) #2017-12-10 12:00:00
tail(LAI_estimations) #2020-09-30 12:00:00

tsLAI <- seq(as.POSIXct("2018-01-31", tz="UTC"),
             as.POSIXct("2020-10-30", tz="UTC"),
             by="hour") #"30 min"

LAI_est_ts <- data.frame(timestamp=tsLAI)
head(LAI_est_ts)
tail(LAI_est_ts)
LAI_est_ts$timestamp

LAI_est_ts <- left_join(LAI_est_ts, LAI_estimations, by="timestamp")
head(filter(LAI_est_ts, timestamp>="2018-01-31 13:00:00 UTC"))
tail(filter(LAI_est_ts, timestamp<="2020-09-30 14:00:00 UTC"))

LAI_est_ts <- filter(LAI_est_ts, timestamp>="2018-01-31 13:00:00 UTC" &
                                 timestamp<="2020-09-30 14:00:00 UTC")

for (i in 2:13) { # 12 LAI estimations
  LAI_est_ts[i] <- na.approx(LAI_est_ts[i])
}
LAI_est_ts <- data.frame(LAI_est_ts)

#library(zoo)
LAI_est_MA <- LAI_est_ts
for (i in 2:13) {
  LAI_est_MA[,i] <- rollmean(LAI_est_ts[,i], 30*24*2)
}

ggplot(LAI_est_MA, aes(x=timestamp)) +
  geom_line(aes(y=ROTH_buffer), colour="green") +
  geom_line(aes(y=ROTH_point), colour="darkgreen") +
  geom_line(aes(y=ROTH_DWD_buffer), colour="blue") +
  geom_line(aes(y=ROTH_DWD_point), colour="darkblue") +
  geom_line(aes(y=ROTH_green_buffer), colour="grey") +
  geom_line(aes(y=ROTH_green_point), colour="darkgrey")

ggplot(LAI_est_MA, aes(x=timestamp)) +
  geom_line(aes(y=TUCC_buffer), colour="green") +
  geom_line(aes(y=TUCC_point), colour="darkgreen") +
  geom_line(aes(y=TUCC_DWD_buffer), colour="blue") +
  geom_line(aes(y=TUCC_DWD_point), colour="darkblue") +
  geom_line(aes(y=TUCC_green_buffer), colour="grey") +
  geom_line(aes(y=TUCC_green_point), colour="darkgrey")

summary(LAI_est_MA)



















####################################################################
# LAI estimation using footprint 
####################################################################
# function to convert 333m to 10m resolution
resamplef = function(r, fp_rast) {
  r_new = raster::crop(x=r, y=extent(fp_rast))  # first crop 
  r_new = raster::resample(r_new, fp_rast) # reproject, method='bilinear'
  return(r_new)
}

LAI_FP_RO_T <- resamplef(LAI300m.sites,fp_extent_Roth[[1]])
plot(LAI_FP_RO_T[[26:30]])

names(LAI_FP_RO_T) <- ymd_hms(LAI.CO$timestamp)

LAIPropNA_RO <- NULL
for (i in 1:102) {
  LAIPropNA_RO[i] <- table(is.na(values(LAI_FP_RO_T[[i]])))["TRUE"]/
    ncell(LAI_FP_RO_T[[i]])
}
LAIPropNA_RO <- data.frame(LAIPropNA_RO)
LAIPropNA_RO$i <- 1:102
rownames(LAIPropNA_RO) <- names(LAI_FP_RO_T)
LAIPropNA_RO

LAI_FP_RO_T[[6]]
plot(LAI_FP_RO_T[['X2018.01.31.12.00.00']])
LAI_FP_RO_T[[102]]
plot(LAI_FP_RO_T[["X2020.09.30.12.00.00"]])

LAI_FP_RO <- LAI_FP_RO_T[[6:102]]
names(LAI_FP_RO) <- paste0("LAI", year(LAI.CO$timestamp[6:102]),
                           "_DOY",yday(LAI.CO$timestamp[6:102]))

LAI_FP_RO <- approxNA(LAI_FP_RO)

date(LAI.CO$timestamp[6:102])
yday(LAI.CO$timestamp[6:102])
365+365-31+274
973/10

LAI_FP_DOY_RO <- NULL
for (i in 1:973) {
  LAI_FP_DOY_RO[[i]] <- LAI_FP_RO_T[[1]] # ckeck if NA
}

LAI_FP_DOY_RO
LAI_FP_DOY_RO[[1]] <- LAI_FP_RO[[1]]
LAI_FP_DOY_RO[[973]] <- LAI_FP_RO[[97]]



plot(LAI_FP_DOY_RO[[1]])
plot(LAI_FP_DOY_RO[[2]])
plot(LAI_FP_DOY_RO[[31]])
plot(LAI_FP_DOY_RO[[961]])

LAI_FP_DOY_RO <- stack(LAI_FP_DOY_RO)
LAI_FP_DOY_RO <- approxNA(LAI_FP_DOY_RO)

ts_RO <- seq(as.POSIXct("2018-11-10 01:00:00", tz = "UTC"),
             as.POSIXct("2020-03-10 01:00:00", tz = "UTC"),
             by = "day") #"30 min"

names(LAI_FP_DOY_RO) <- paste0("LAI", year(ts_RO),
                               "_DOY",yday(ts_RO))[1:486]

plot(LAI_FP_DOY_RO[[1]])
plot(LAI_FP_DOY_RO[[2]])
plot(LAI_FP_DOY_RO[[295]])
plot(LAI_FP_DOY_RO[[486]])


LAI.CO2 <- c()

LAI.CO2$LAI_RO_buffer <- unlist(sapply(1:486,FUN=function(i) 
  raster::extract(x = LAI_FP_DOY_RO[[i]], 
                  y = coord[3:4], 
                  buffer=500, na.rm=TRUE,
                  fun=mean, 
                  df=TRUE))[2,])

LAI.CO2$LAI_RO_point <- sapply(1:486,FUN=function(i) 
  raster::extract(LAI_FP_DOY_RO[[i]], coord[3:4]))

LAI.CO2$LAI_DWDRO_buffer <- unlist(sapply(1:486,FUN=function(i) 
  raster::extract(x = LAI_FP_DOY_RO[[i]], 
                  y = coord[7:8], 
                  buffer=500, na.rm=TRUE, 
                  fun=mean, 
                  df=TRUE))[2,])

LAI.CO2$LAI_DWDRO_point <- sapply(1:486,FUN=function(i) 
  raster::extract(LAI_FP_DOY_RO[[i]], coord[7:8]))

LAI.CO2$LAI_greenRO_buffer <- unlist(sapply(1:486,FUN=function(i) 
  raster::extract(x = LAI_FP_DOY_RO[[i]], 
                  y = coord[17:18], 
                  buffer=500,
                  fun=mean, na.rm=TRUE, 
                  df=TRUE))[2,])

LAI.CO2$LAI_greenRO_point <- sapply(1:486,FUN=function(i) 
  raster::extract(LAI_FP_DOY_RO[[i]], coord[17:18]))

LAI.CO2 <- data.frame(LAI.CO2)
summary(LAI.CO2)

LAI.CO2$timestamp <- ts_RO[1:486]
  
ggplot(LAI.CO2, aes(x=timestamp)) +
  geom_point(aes(y=LAI_RO_buffer), colour="green") +
  geom_point(aes(y=LAI_RO_point), colour="darkgreen") +
  geom_point(aes(y=LAI_DWDRO_buffer), colour="blue") +
  geom_point(aes(y=LAI_DWDRO_point), colour="darkblue")+
  geom_point(aes(y=LAI_greenRO_buffer), colour="grey") +
  geom_point(aes(y=LAI_greenRO_point), colour="darkgrey")


head(LAI.CO2)
tail(LAI.CO2)

tsLAI2 <- seq(as.POSIXct("2018-10-10 01:00:00", tz="UTC"),
             as.POSIXct("2020-05-30 01:00:00", tz="UTC"),
             by="hour") #"30 min"

LAI.Coper2 <- data.frame(timestamp=tsLAI2)
head(LAI.Coper2)
tail(LAI.Coper2)
LAI.Coper2$timestamp

LAI.Coper2 <- left_join(LAI.Coper2, LAI.CO2, by="timestamp")
head(filter(LAI.Coper2, timestamp=="2018-11-10 02:00:00 UTC"))
tail(filter(LAI.Coper2, timestamp=="2020-03-09 02:00:00 UTC"))
LAI.Coper2 <- filter(LAI.Coper2, timestamp>="2018-11-10 02:00:00 UTC" &
                     timestamp<="2020-03-09 02:00:00 UTC")

for (i in 2:7) {
  LAI.Coper2[i] <- na.approx(LAI.Coper2[i])
}
LAI.Coper2 <- data.frame(LAI.Coper2)

(11641-10202)/2
(11641-720)
LAI.Coper.MA2 <- LAI.Coper2[720:10921,]

for (i in 2:7) {
  LAI.Coper.MA2[,i] <- rollmean(LAI.Coper2[,i], 30*24*2)
}

ggplot(LAI.Coper.MA2, aes(x=timestamp)) +
  geom_line(aes(y=LAI_RO_buffer), colour="green") +
  geom_line(aes(y=LAI_RO_point), colour="darkgreen") +
  geom_line(aes(y=LAI_DWDRO_buffer), colour="grey") +
  geom_line(aes(y=LAI_DWDRO_point), colour="darkgrey") +
  geom_line(aes(y=LAI_greenRO_buffer), colour="blue") +
  geom_line(aes(y=LAI_greenRO_point), colour="darkblue")

###############################################################
#library(readr)
#DWD_ROTH <- read_csv("C:/Users/Alby Rocha/Documents/EC/DWDdata/DWD_ROTH19.csv",col_names = T)
#DWD_TUCC <- read_csv("C:/Users/Alby Rocha/Documents/EC/DWDdata/DWD_TUCC19.csv",col_names = T)
#############
#DWD_ROTH <- left_join(DWD_ROTH, LAI.Coop[,1:7], by="timestamp")
#DWD_TUCC <- left_join(DWD_TUCC, LAI.Coop[,c(1,8,9,10,11,12,13)], by="timestamp")
###############################################################
#write.csv(DWD_ROTH, 
#          file="C:/Users/Alby Rocha/Documents/EC/SCOPEresults/DWD_ROTH19.csv", 
#          row.names = F)
#write.csv(DWD_TUCC, 
#          file="C:/Users/Alby Rocha/Documents/EC/SCOPEresults/DWD_TUCC19.csv", 
#          row.names = F)
################################################################

write.csv(LAI.Coper,
          file="LAI.Coper.csv", 
          row.names = F)

write.csv(LAI.Coper, 
          file="C:/Users/Alby Rocha/Documents/EC/SCOPEpar/LAI.Coper.csv", 
          row.names = F)










# create a list of rasters
LAI.copernicus <- NULL
for (i in 1:length(File.names)) {
  LAI.copernicus[[i]] <- raster(File.names[i],
                                crs="+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
}

# Crop to an extent around the two towers
LAI300m <- NULL
for (i in 1:length(File.names)) {       
  LAI300m[[i]] <- crop(LAI.copernicus[[i]], extent(13.25,13.50,52.4,52.6))
}

plot(LAI300m[[26]])

mapr <- raster(xmn=380000,xmx=399000,ymn=5811000,ymx=5828000, 
               res=10,
               crs=crs("+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs"))

LAI300m_ROTH <- NULL
for (i in 1:length(File.names)) {    
  LAI300m_ROTH[[i]] <- projectRaster(LAI300m[[i]], mapr)
} 
plot(LAI300m_ROTH[[26]])
points(coord$ROTHlonUTM, coord$ROTHlatUTM)

LAI300m.sites <- stack(LAI300m_ROTH)
names(LAI300m.sites) <- File.date
plot(LAI300m.sites$X201810100)
points(coord$ROTHlonUTM, coord$ROTHlatUTM)
points(coord$TUCClonUTM, coord$TUCClatUTM)
points(coord$DWDROTHlonUTM, coord$DWDROTHlatUTM)
points(coord$DWDTUCClonUTM, coord$DWDTUCClatUTM)
points(coord$ROTHGreenlonUTM, coord$ROTHGreenlatUTM)
points(coord$TUCCGreenlonUTM, coord$TUCCGreenlatUTM)

LAI300m.sites[[26]]

##########################################################
#LAI.NA <- sapply(1:length(LAI300m.sites),FUN=function(i) 
#  which(summary(LAI300m.sites[[i]]@data@values)[[2]]=="90000"))
#which(LAI.NA==1)
#summary(LAI300m.sites[[20]]@data@values)[[7]]
###########################################################
