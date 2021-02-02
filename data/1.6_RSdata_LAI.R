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
library(zoo) # rollmean
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
#### download LAI 300m from the Copernicus product portal - https://land.copernicus.eu/global/products/lai
#### Portal Link - https://land.copernicus.vgt.vito.be/PDF/portal/Application.html#Browse;Root=512260;Collection=1000062;Time=NORMAL,NORMAL,-1,,,-1,,
#### Example of file c_gls_LAI300_202009300000_GLOBE_PROBAV_V1.0.1.nc
# List all the files downloaded in the folder 
File.names <- list.files(path="D:/Research topics/Data-Modelling/data/LAI300m/",
                         pattern= "*.nc", full.names = T)
# get the date from the file names
File.date <- sapply(1:length(File.names), FUN=function(i) substring(File.names[i], 61, 69)) # year_month_day

LAI.copernicus <- sapply(1:length(File.names),FUN=function(i) 
                  raster(File.names[i],
                  crs="+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

LAI.copernicus <- stack(LAI.copernicus)

# Crop to an extent around the two towers/DWD stations to reduce the size
LAI300m <- crop(LAI.copernicus, extent(13.25,13.50,52.4,52.6))

plot(LAI300m[[26]])
points(coord$ROTHlon, coord$ROTHlat)
points(coord$TUCClon, coord$TUCClat)
points(coord$DWDROTHlon, coord$DWDROTHlat)
points(coord$DWDTUCClon, coord$DWDTUCClat)

mapr <- raster(xmn=380000,xmx=399000,ymn=5811000,ymx=5828000, res=10,
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
                                                fun=mean, df=TRUE))[2,])

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

# moving means                                    
# (30*24*2)/2 : 23353-(30*24*2)/2
LAI_est_MA <- LAI_est_ts[720:22633,]

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

LAI_est_MA <- filter(LAI_est_MA, date(timestamp)>="2018-11-30" &
                                 date(timestamp)<="2020-02-01")

###########################################################
###########################################################
# Second part - Footprint extration
###########################################################
###########################################################
library(readr)
library(dplyr)
library(lubridate)
library(raster)
library(FREddyPro)
library(sp)
library(sf)
library(spatialEco)
library(openair)
library(ggplot2)

### read EC data
EC_ROTH <- read_csv("C:/Users/Alby Rocha/Documents/EC/FPmaps/EC_ROTH.csv",col_names = T,
                    cols(.default = col_double(),
                         timestamp = col_datetime(format = ""),
                         prec.window = col_integer()))
summary(EC_ROTH)
ROTH19plus1 <- filter(EC_ROTH, date(timestamp)>="2018-11-30" &
                               date(timestamp)<="2020-02-01")
#################################################################
# Functions
#################################################################
#################################################################  
# resample 300m to 10m FP grid                                                               
resamplef = function(r, fp_rast) {
  r_new = raster::crop(x=r, y=extent(fp_rast))  # first crop 
  r_new = raster::resample(r_new, fp_rast) # reproject, method='bilinear'
  return(r_new)
}
#################################################################
# create the FPs
create_footprint_raster <- function(fetch, height, grid, speed, direction,uStar,zol,  
                                    sigmaV, percentage_in=F, date=NULL, lon, lat){
  # make the footprint calculation according to Kormann and Meixner (2001) from FREddyPro
  footprint <- FREddyPro::Calculate(fetch=fetch, height=height, grid=grid,
                                    speed=speed, direction=direction, 
                                    uStar=uStar, zol=zol, sigmaV=sigmaV)
  if(percentage_in == T){
    # evaluate the percentage numbers
    footprint_1 <- FREddyPro::calculatePercentFootprint(footprint)
  }else{
    footprint_1 <- footprint
  }
  # this footprint needs to be exported as grid
  x_cor <- lon
  y_cor <- lat
  # the grid has to be transfered to a raster format
  foot_expo <- FREddyPro::exportFootprintPoints(footprint_1,xcoord=x_cor, ycoord=y_cor)
  foot_raster <- raster::rasterFromXYZ(xyz = foot_expo,
                                       crs = sp::CRS("+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  return(foot_raster)
}
########################################################################
#########################################################################
### Function- extract land surfaces values from the FP
footprint_extraction <- function(fetch, height, grid, speed, direction,
                                 uStar, zol, sigmaV, date=NULL,
                                 surface_cover_info=NULL,return_clip=FALSE,
                                 lon,lat){
  # calculate the footprint
  first_guess_fetch <- create_footprint_raster(fetch = fetch,
                                               height = height,
                                               grid = grid,
                                               speed = speed,
                                               direction = direction,
                                               uStar = uStar,
                                               zol = zol,
                                               sigmaV = sigmaV,
                                               lon = lon, 
                                               lat = lat)
  # generate the 90 percent area
  ###### try without raster.vol but export 0.9
  fgf <- raster.vol(first_guess_fetch, p= 0.90)
  # raster::plot(fgf)
  fgf_polygon_90 <- raster::rasterToContour(fgf)
  
  # make a SpatialPolygonsDataFrame
  sf_90 <- sf::st_as_sf(fgf_polygon_90)
  sp_90 <- sf::st_polygonize(sf_90)
  if(!any(is.na(sf::st_dimension(sp_90)))){
    # as some (only few) might be several polygons
    sp_90_p <- sf::st_collection_extract(sp_90, "POLYGON")
    shp_fp90 <- as(sp_90_p[2,], "Spatial")
  }else{
    fgf_polygon_90@lines[[1]]@Lines[[1]]@coords[] <- rbind(
      as.matrix(sp::coordinates(
        fgf_polygon_90[1,])[[1]][[1]])[1:(
          nrow(sp::coordinates(
            fgf_polygon_90[1,])[[1]][[1]])-1),],
      as.matrix(sp::coordinates(
        fgf_polygon_90[1,])[[1]][[1]])[1,])
    sf_90 <- sf::st_as_sf(fgf_polygon_90)
    sp_90_p <- sf::st_polygonize(sf_90)
    shp_fp90 <- as(sp_90_p[1,], "Spatial")
  }
  # make the clip with a mask
  clip1 <- raster::mask(first_guess_fetch, mask=shp_fp90)
  # raster::plot(clip1)
  # calculate the percentage used
  perc <- sum(raster::values(clip1), na.rm= T)
  Max_prob <- max(raster::values(clip1), na.rm= T)
  n_pixel <- table(is.na(raster::values(clip1)))[1]
  
  if(return_clip != T){
    # Extract year from the "date" provided 
    Year = lubridate::year(date)
    # Extract DOY from the "date" provided 
    DOY = lubridate::yday(date)
    # layer name to match the date
    LAI_FP <- surface_cover_info[[paste0("LAI", Year, "_DOY", DOY)]]*clip1
    LAI_FP_values <- sum(raster::values(LAI_FP), na.rm=T)*(1/perc)
    # build the result data.table
    result <- data.table::data.table(
      "timestamp" = date,
      'Max_prob' = Max_prob,
      "n_pixel" = n_pixel,
      "LAI_FP"= LAI_FP_values
    )
    return(result)
  }else{
    return(clip1)  #sp_90
  }
}
#################################################################
#################################################################
# FP calculation
#################################################################
#################################################################
# create a FP size and test
zmROTH <- 39.75
fp_extent_ROTH <- create_footprint_raster(fetch = 1500,
                                         height = (zmROTH-ROTH19plus1$zd.filled[1989]),
                                         grid = 300,
                                         speed = ROTH19plus1$ws.filled[1989],
                                         direction = ROTH19plus1$wd.filled[1989], 
                                         uStar = ROTH19plus1$u.filled[1989],
                                         zol = (zmROTH-ROTH19plus1$zd.filled[1989])/ROTH19plus1$L.filled[1989],
                                         sigmaV = sqrt(ROTH19plus1$v_var.filled[1989]),
                                         date = as.POSIXct(ROTH19plus1$timestamp[1989], tz="UTC"),
                                         lon = coord$ROTHlonUTM,
                                         lat = coord$ROTHlatUTM)

plot(fp_extent_ROTH)
#################################################################
###### resample 300m images to 10m FP
plot(LAI300m.sites[[55]])
LAI_ROTH <- resamplef(LAI300m.sites,fp_extent_ROTH)
compareRaster(fp_extent_ROTH,LAI_ROTH)
plot(LAI_ROTH[[10]])
plot(LAI_ROTH[[36]]) # NULL
plot(LAI_ROTH[[81]]) # NAs

names(LAI_ROTH) <- ymd_hms(LAI_estimations$timestamp)
plot(LAI_ROTH[['X2018.11.10.12.00.00']])
plot(LAI_ROTH[["X2020.03.10.12.00.00"]])
# start and finish with a Non-NA image for 2019 + some months
LAI_ROTH <- LAI_ROTH[[34:82]]
names(LAI_ROTH) <- paste0("LAI", year(LAI_estimations$timestamp[34:82]),
                          "_DOY",yday(LAI_estimations$timestamp[34:82]))
plot(LAI_ROTH[[1]])
plot(LAI_ROTH[[2]])
plot(LAI_ROTH[[3]])
plot(LAI_ROTH[[36]])

# interpolate the maps
LAI_ROTH <- approxNA(LAI_ROTH)
plot(LAI_ROTH[[1]])
plot(LAI_ROTH[[2]])
plot(LAI_ROTH[[3]])
plot(LAI_ROTH[[36]])

# days between the two dates
difftime(as.POSIXct("2018-11-10", tz="UTC"), 
         as.POSIXct("2020-03-10", tz="UTC"), units="days")

ts_RO <- seq(as.POSIXct("2018-11-10", tz = "UTC"),
             as.POSIXct("2020-03-10", tz = "UTC"),
             by = "day") #"30 min"

LAI_ROTH_DOY <- NULL
# Include the closest doy image for the 486 selected doys 
for (i in 1:486) { 
  LAI_ROTH_DOY[[i]] <- LAI_ROTH[[
    min(which(abs(difftime(
    as.POSIXct(LAI_estimations$timestamp[34:82], tz="UTC"), 
    as.POSIXct(ts_RO[i], tz="UTC"), units="days")) == min(abs(difftime(
      as.POSIXct(LAI_estimations$timestamp[34:82], tz="UTC"), 
      as.POSIXct(ts_RO[i], tz="UTC"), units="days")))))
                               ]] 
}

plot(LAI_ROTH[[1]])
plot(LAI_ROTH_DOY[[1]])
plot(LAI_ROTH_DOY[[10]])
plot(LAI_ROTH_DOY[[11]])
plot(LAI_ROTH_DOY[[17]])
plot(LAI_ROTH_DOY[[18]])
plot(LAI_ROTH[[49]])
plot(LAI_ROTH_DOY[[486]])

# Create NDVI layer name to match the date
LAI_ROTH_DOY <- stack(LAI_ROTH_DOY)
names(LAI_ROTH_DOY) <- paste0("LAI", year(ts_RO),
                              "_DOY", yday(ts_RO))[1:486]

# Run the FP selecting the LAI by the DOY number/File name 
LAI_FP_ROTH <- NULL

index.run <- 1:length(ROTH19plus1$timestamp)
index.run <- index.run[c(-10171,-10726, -11245, -11782, -13551)]

for (i in index.run) {
  LAI_FP_ROTH[[i]] <- footprint_extraction(fetch = 1500,
                                           height = (zmROTH-ROTH19plus1$zd.filled[i]),
                                           grid = 300,
                                           speed = ROTH19plus1$ws.filled[i],
                                           direction = ROTH19plus1$wd.filled[i],  
                                           uStar = ROTH19plus1$u.filled[i],
                                           zol = (zmROTH - ROTH19plus1$zd.filled[i])/ROTH19plus1$L.filled[i],
                                           sigmaV = sqrt(ROTH19plus1$v_var.filled[i]),
                                           date = ROTH19plus1$timestamp[i],
                                           surface_cover_info = LAI_ROTH_DOY,
                                           lon = coord$ROTHlonUTM,
                                           lat = coord$ROTHlatUTM)
}

LAI_FP_ROTH[[46]]
LAI_ROTH_DOY[[1989/24]]

##### footprints that fails
unique(diff(as.integer(index(LAI_FP_ROTH))))
unlist(sapply(1:length(LAI_FP_ROTH),FUN=function(i) 
  ifelse(is.null(LAI_FP_ROTH[[i]]),print(i),NA)))
#10171, 10726, 11245, 11782, 13551

# fill the gaps with the footprints just before fail
LAI_FP_ROTH[[10171]] <- LAI_FP_ROTH[[10170]]
LAI_FP_ROTH[[10726]] <- LAI_FP_ROTH[[10725]]
LAI_FP_ROTH[[11245]] <- LAI_FP_ROTH[[11244]]
LAI_FP_ROTH[[11782]] <- LAI_FP_ROTH[[11781]]
LAI_FP_ROTH[[13551]] <- LAI_FP_ROTH[[13550]]

# transform list in df , for each variable
#########################################################
LAI_ROTH_FP = matrix(nrow=length(LAI_FP_ROTH), ncol=length(LAI_FP_ROTH[[1]]))
for(i in 1:dim(LAI_ROTH_FP)[1]) {
  for(j in 1:dim(LAI_ROTH_FP)[2]) {
    LAI_ROTH_FP[i,j] = LAI_FP_ROTH[[i]][[j]]
  }
}
#########################################################
LAI_ROTH_FP <- data.frame(LAI_ROTH_FP)
names(LAI_ROTH_FP) <- names(LAI_FP_ROTH[[1]])
LAI_ROTH_FP$timestamp <- ymd_hms(ROTH19plus1$timestamp)
LAI_ROTH_FP$Max_prob <- round(as.double(LAI_ROTH_FP$Max_prob),5)
LAI_ROTH_FP$LAI_FP <- round(as.double(LAI_ROTH_FP$LAI_FP),3)
LAI_ROTH_FP$n_pixel <- as.integer(LAI_ROTH_FP$n_pixel)
str(LAI_ROTH_FP)

summary(LAI_ROTH_FP)
plot(LAI_ROTH_FP$LAI_FP)
plot(LAI_ROTH_FP$Max_prob)
plot(LAI_ROTH_FP$n_pixel)
t(cor(ROTH19plus1$LE.dry.fsd, LAI_ROTH_FP[,-1], use="complete.obs"))

write.csv(LAI_ROTH_FP, file="LAI_ROTH_FP.csv", row.names = F)
########################################################
LAI_ROTH_FPh <- LAI_ROTH_FP

colnames(LAI_ROTH_FPh)[c(1)] <- c("date")
LAI_ROTH_FPh <- timeAverage(LAI_ROTH_FPh, avg.time="1 hour", fill=TRUE) #
str(LAI_ROTH_FPh)
summary(LAI_ROTH_FPh)

colnames(LAI_ROTH_FPh)[c(1)] <- c("timestamp")
plot(LAI_ROTH_FPh$LAI_FP)
#####################################################################
LAI_ROTH_FPh <- filter(LAI_ROTH_FPh, date(timestamp)>="2018-11-30" &
                                     date(timestamp)<="2020-02-01")
#####################################################################
range(LAI_ROTH_FPh$timestamp)

LAI_RS_ROTH <- data.frame(
"timestamp"=LAI_ROTH_FPh$timestamp,
"LAI_FP"=LAI_ROTH_FPh$LAI_FP,
"LAI_Buffer"=as.double(LAI_est_MA[,2]),
"LAI_point"=as.double(LAI_est_MA[,3]),
"LAI_DWD_Buffer"=as.double(LAI_est_MA[,4]),
"LAI_DWD_point"= as.double(LAI_est_MA[,5]),
"LAI_Green_Buffer"=as.double(LAI_est_MA[,6]),
"LAI_Green_point"=as.double(LAI_est_MA[,7]))

summary(LAI_RS_ROTH)

EC_ROTHh <- read_csv("C:/Users/Alby Rocha/Documents/EC/FPmaps/EC_ROTHh.csv",col_names = T,
                    cols(.default = col_double(),
                         timestamp = col_datetime(format = ""),
                         prec.window = col_integer()))

EC_ROTH19h <- filter(EC_ROTHh, date(timestamp)>="2018-11-30" &
                                   date(timestamp)<="2020-02-01")

t(cor(EC_ROTH19h$LE.dry.fsd, LAI_RS_ROTH[,-1], use="complete.obs"))
#####################################################################
#####################################################################
write.csv(LAI_RS_ROTH, file="LAI_RS_ROTH.csv", row.names = F)
#####################################################################
#####################################################################
