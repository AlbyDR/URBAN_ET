########################################################################
library(readr)
library(dplyr)
library(lubridate)
library(FREddyPro)
library(spatialEco)
library(sf)
library(raster)
library(sp)
########################################################################

########################################################################
########################################################################
# Functions
########################################################################
########################################################################
### Function 1 - generate FP raster with probability 0.90
create_footprint_raster <- function(fetch, height, grid, speed, direction, uStar, zol, sigmaV, percentage_in = F,date = NULL, lon, lat){
  # make the footprint calculation according to Kormann and Meixner (2001) from FREddyPro
  footprint <- FREddyPro::Calculate(fetch = fetch, height = height, grid = grid,
                                    speed = speed, direction = direction, uStar = uStar,
                                    zol = zol, sigmaV = sigmaV)
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
  foot_expo <- FREddyPro::exportFootprintPoints(footprint_1,xcoord = x_cor, ycoord = y_cor)
  foot_raster <- raster::rasterFromXYZ(xyz = foot_expo,
                                       crs = sp::CRS("+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  return(foot_raster)
}
########################################################################
### Function 2 - generate FP polygons 0.9
footprint_polygon <- function(fetch, height, grid,
                              speed, direction, uStar,
                              zol, sigmaV, date = NULL,
                              return_clip = FALSE,
                              lon,
                              lat){
  # calculate the footprint
  first_guess_fetch<- create_footprint_raster(fetch = fetch,
                                              height = height,
                                              grid = grid,
                                              speed = speed,
                                              direction = direction,
                                              uStar = uStar,
                                              zol = zol,
                                              sigmaV = sigmaV,
                                              lon = lon, 
                                              lat = lat)
  # generate the 90 percent area # try without raster.vol but export 0.9
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
  return(sp_90)  #clip TRUE
}

#########################################################################
### Function 3 - extract land surfaces values from the FP
footprint_weighted_SVV <- function(fetch, height, grid,
                                   speed, direction, uStar,
                                   zol, sigmaV, date,
                                   surface_cover_info = NULL,
                                   return_clip = FALSE,
                                   lon,
                                   lat){
# calculate the footprint
first_guess_fetch<- create_footprint_raster(fetch = fetch,
                                              height = height,
                                              grid = grid,
                                              speed = speed,
                                              direction = direction,
                                              uStar = uStar,
                                              zol = zol,
                                              sigmaV = sigmaV,
                                              lon = lon, 
                                              lat = lat)
  # in case the footprint is very small
  if(!max(raster::values(first_guess_fetch)) > 0.8 |
     length(raster::values(first_guess_fetch)[
       (which(raster::values(first_guess_fetch) > 0.2))]) > 1){ # I know, this is not nice, but ..
    
    # generate the 90 percent area
    fgf <- raster.vol(first_guess_fetch, p= 0.90)
    # raster::plot(fgf)
    fgf_polygon_90 <- raster::rasterToContour(fgf)
    
    # make a SpatialPolygonsDataFrame
    sf_90 <- sf::st_as_sf(fgf_polygon_90)
    sp_90 <- sf::st_polygonize(sf_90)
    if(!any(is.na(sf::st_dimension(sp_90)))){
      # as some (only few) might be several polygons
      sp_90 <- sf::st_collection_extract(sp_90, "POLYGON")
      shp_fp90 <- as(sp_90[2,], "Spatial")
    }else{
      fgf_polygon_90@lines[[1]]@Lines[[1]]@coords[] <- rbind(as.matrix(sp::coordinates(fgf_polygon_90[1,])[[1]][[1]])[1:(nrow(sp::coordinates(fgf_polygon_90[1,])[[1]][[1]])-1),],as.matrix(sp::coordinates(fgf_polygon_90[1,])[[1]][[1]])[1,])
      sf_90 <- sf::st_as_sf(fgf_polygon_90)
      sp_90 <- sf::st_polygonize(sf_90)
      shp_fp90 <- as(sp_90[1,], "Spatial")
    }
    
    # make the clip with a mask
    clip1 <- raster::mask(first_guess_fetch,mask = shp_fp90)
    # raster::plot(clip1)
    
    # calculate the percentage used
    perc <- sum(raster::values(clip1), na.rm= T)
    Max_prob <- max(raster::values(clip1), na.rm= T)
    n_pixel <- table(is.na(raster::values(clip1)))[1]
    if(return_clip != T){
      # layer
      layer1.1 <- surface_cover_info[["layer1"]]*clip1
      layer1 <- sum(raster::values(layer1.1),na.rm =T)*(1/perc)
      layer2.1 <- surface_cover_info[["layer2"]]*clip1
      layer2 <- sum(raster::values(layer2.1),na.rm =T)*(1/perc)
      layer3.1 <- surface_cover_info[["layer3"]]*clip1
      layer3 <- sum(raster::values(layer3.1),na.rm =T)*(1/perc)
      layer4.1 <- surface_cover_info[["layer4"]]*clip1
      layer4 <- sum(raster::values(layer4.1),na.rm =T)*(1/perc)
      layer5.1 <- surface_cover_info[["layer5"]]*clip1
      layer5 <- sum(raster::values(layer5.1),na.rm =T)*(1/perc)
      layer6.1 <- surface_cover_info[["layer6"]]*clip1
      layer6 <- sum(raster::values(layer6.1),na.rm =T)*(1/perc)
      layer7.1 <- surface_cover_info[["layer7"]]*clip1
      layer7 <- sum(raster::values(layer7.1),na.rm =T)*(1/perc)
      layer8.1 <- surface_cover_info[["layer8"]]*clip1
      layer8 <- sum(raster::values(layer8.1),na.rm =T)*(1/perc)
      layer9.1 <- surface_cover_info[["layer9"]]*clip1
      layer9 <- sum(raster::values(layer9.1),na.rm =T)*(1/perc)
      layer10.1 <- surface_cover_info[["layer10"]]*clip1
      layer10 <- sum(raster::values(layer10.1),na.rm =T)*(1/perc)
      layer11.1 <- surface_cover_info[["layer11"]]*clip1
      layer11 <- sum(raster::values(layer11.1),na.rm =T)*(1/perc)
      layer12.1 <- surface_cover_info[["layer12"]]*clip1
      layer12 <- sum(raster::values(layer12.1),na.rm =T)*(1/perc)
      layer13.1 <- surface_cover_info[["layer13"]]*clip1
      layer13 <- sum(raster::values(layer13.1),na.rm =T)*(1/perc)
      layer14.1 <- surface_cover_info[["layer14"]]*clip1
      layer14 <- sum(raster::values(layer14.1),na.rm =T)*(1/perc)
      layer15.1 <- surface_cover_info[["layer15"]]*clip1
      layer15 <- sum(raster::values(layer15.1),na.rm =T)*(1/perc)
      layer16.1 <- surface_cover_info[["layer16"]]*clip1
      layer16 <- sum(raster::values(layer16.1),na.rm =T)*(1/perc)
      layer17.1 <- surface_cover_info[["layer17"]]*clip1
      layer17 <- sum(raster::values(layer17.1),na.rm =T)*(1/perc)
      layer18.1 <- surface_cover_info[["layer18"]]*clip1
      layer18 <- sum(raster::values(layer18.1),na.rm =T)*(1/perc)
      layer19.1 <- surface_cover_info[["layer19"]]*clip1
      layer19 <- sum(raster::values(layer19.1),na.rm =T)*(1/perc)
      layer20.1 <- surface_cover_info[["layer20"]]*clip1
      layer20 <- sum(raster::values(layer20.1),na.rm =T)*(1/perc)
      layer21.1 <- surface_cover_info[["layer21"]]*clip1
      layer21 <- sum(raster::values(layer21.1),na.rm =T)*(1/perc)
      layer22.1 <- surface_cover_info[["layer22"]]*clip1
      layer22 <- sum(raster::values(layer22.1),na.rm =T)*(1/perc)
      layer23.1 <- surface_cover_info[["layer23"]]*clip1
      layer23 <- sum(raster::values(layer23.1),na.rm =T)*(1/perc)
      layer24.1 <- surface_cover_info[["layer24"]]*clip1
      layer24 <- sum(raster::values(layer24.1),na.rm =T)*(1/perc)
      layer25.1 <- surface_cover_info[["layer25"]]*clip1
      layer25 <- sum(raster::values(layer25.1),na.rm =T)*(1/perc)
      layer26.1 <- surface_cover_info[["layer26"]]*clip1
      layer26 <- sum(raster::values(layer26.1),na.rm =T)*(1/perc)
      layer27.1 <- surface_cover_info[["layer27"]]*clip1
      layer27 <- sum(raster::values(layer27.1),na.rm =T)*(1/perc)
      layer28.1 <- surface_cover_info[["layer28"]]*clip1
      layer28 <- sum(raster::values(layer28.1),na.rm =T)*(1/perc)
      layer29.1 <- surface_cover_info[["layer29"]]*clip1
      layer29 <- sum(raster::values(layer29.1),na.rm =T)*(1/perc)
      layer30.1 <- surface_cover_info[["layer30"]]*clip1    
      layer30 <- sum(raster::values(layer30.1),na.rm =T)*(1/perc)
      layer31.1 <- surface_cover_info[["layer31"]]*clip1
      layer31 <- sum(raster::values(layer31.1),na.rm =T)*(1/perc)
      layer32.1 <- surface_cover_info[["layer32"]]*clip1
      layer32 <- sum(raster::values(layer32.1),na.rm =T)*(1/perc)
      layer33.1 <- surface_cover_info[["layer33"]]*clip1
      layer33 <- sum(raster::values(layer33.1),na.rm =T)*(1/perc)
      layer34.1 <- surface_cover_info[["layer34"]]*clip1
      layer34 <- sum(raster::values(layer34.1),na.rm =T)*(1/perc)
      layer35.1 <- surface_cover_info[["layer35"]]*clip1
      layer35 <- sum(raster::values(layer35.1),na.rm =T)*(1/perc)
      layer36.1 <- surface_cover_info[["layer36"]]*clip1
      layer36 <- sum(raster::values(layer36.1),na.rm =T)*(1/perc)
      layer37.1 <- surface_cover_info[["layer37"]]*clip1
      layer37 <- sum(raster::values(layer37.1),na.rm =T)*(1/perc)
      layer38.1 <- surface_cover_info[["layer38"]]*clip1
      layer38 <- sum(raster::values(layer38.1),na.rm =T)*(1/perc)
      layer39.1 <- surface_cover_info[["layer39"]]*clip1
      layer39 <- sum(raster::values(layer39.1),na.rm =T)*(1/perc)
      layer40.1 <- surface_cover_info[["layer40"]]*clip1
      layer40 <- sum(raster::values(layer40.1),na.rm =T)*(1/perc)
      layer41.1 <- surface_cover_info[["layer41"]]*clip1
      layer41 <- sum(raster::values(layer41.1),na.rm =T)*(1/perc)
      layer42.1 <- surface_cover_info[["layer42"]]*clip1
      layer42 <- sum(raster::values(layer42.1),na.rm =T)*(1/perc)
      # build the result data.table
      result <- data.table::data.table(
        "timestamp" = date,
        'Max_prob' = Max_prob,
        "n_pixel" = n_pixel,
        "ET_r"= layer1,
        "Runoff_r"= layer2,
        "infiltration_r"= layer3,
        "discharge_r"= layer4,
        "Sealing_block_r"= layer5,
        "Sealing_str_r"= layer6,
        "Dist_underwater_r"= layer7,
        "sewer_presence_r"= layer8, 
        "sewerage_bult_r" = layer9,  
        "sewerage_undev_r"= layer10,
        "sewerage_str_r" = layer11,   
        "Prec_annual_r"= layer12,
        "Prec_summer_r" = layer13,   
        "Cap_ShallowRoot_r"= layer14, 
        "Cap_deepRoot_r" = layer15,  
        "soilwater_exchange_r"= layer16, 
        "soil_company_r" = layer17,  
        "Av_nFK_shallow_rootzone_r"= layer18,
        "Av_nFK_deep_rootzone_r" = layer19,  
        "Av_field_capacity_r"= layer20,
        "Topsoil_field_capacity_r"= layer21, 
        "subfloor_field_capacity_r"= layer22,
        "field_capacity_20dm_r" = layer23,  
        "topsoil_saturation_r"= layer24,      
        "topsoil_saturation_value_r"= layer25, 
        "topsoil_KAKeff_r"= layer26,          
        "Organic_carbon_stock_r"= layer27, 
        "Impervious_r"= layer28,              
        "Impervious_With_str.r"= layer29,    
        "Built_up_area_r"= layer30,           
        "Unbuilt_imprev_r"= layer31, 
        "Veg_cover_r"= layer32,               
        "Veg_height_vc_r"= layer33,   
        "Green_volume_r"= layer34,            
        "Veg_cover_str.r" = layer35,   
        "Veg_height_vc_str.r"= layer36,       
        "Green_volume_str.r"= layer37,
        "Veg_height_old_maps_r"= layer38,
        "water_maps_r"= layer39,              
        "impervious_old_r" = layer40,   
        "building_height_old_r"= layer41,
        "veg.cover_vh"= layer42
      )
      return(result)
    }else{
      return(clip1)
    }
  }
  # in case the footprint is very small
  if(max(raster::values(first_guess_fetch)) > 0.8){
    if(return_clip != T){
      # build the result data.table
      result <- data.table::data.table("timestamp" = as.POSIXct(date, tz="UTC"),
                                       'Max_prob' = as.numeric(NA),
                                       'n_pixel' = as.numeric(NA),
                                       "ET_r"= as.numeric(NA),
                                       "Runoff_r"= as.numeric(NA),
                                       "infiltration_r"= as.numeric(NA),
                                       "discharge_r"= as.numeric(NA),
                                       "Sealing_block_r"= as.numeric(NA),
                                       "Sealing_str_r"= as.numeric(NA),
                                       "Dist_underwater_r"= as.numeric(NA),
                                       "sewer_presence_r"= as.numeric(NA), 
                                       "sewerage_bult_r" = as.numeric(NA),  
                                       "sewerage_undev_r"= as.numeric(NA),
                                       "sewerage_str_r" = as.numeric(NA),
                                       "Prec_annual_r"= as.numeric(NA),
                                       "Prec_summer_r" = as.numeric(NA),
                                       "Cap_ShallowRoot_r"= as.numeric(NA),
                                       "Cap_deepRoot_r" = as.numeric(NA),
                                       "soilwater_exchange_r"= as.numeric(NA), 
                                       "soil_company_r" = as.numeric(NA),
                                       "Av_nFK_shallow_rootzone_r"= as.numeric(NA),
                                       "Av_nFK_deep_rootzone_r" = as.numeric(NA),
                                       "Av_field_capacity_r"= as.numeric(NA),
                                       "Topsoil_field_capacity_r"= as.numeric(NA),
                                       "subfloor_field_capacity_r"= as.numeric(NA),
                                       "field_capacity_20dm_r" = as.numeric(NA),
                                       "topsoil_saturation_r"= as.numeric(NA),      
                                       "topsoil_saturation_value_r"= as.numeric(NA),
                                       "topsoil_KAKeff_r"= as.numeric(NA),          
                                       "Organic_carbon_stock_r"= as.numeric(NA),
                                       "Impervious_r"= as.numeric(NA),              
                                       "Impervious_With_str.r"= as.numeric(NA),
                                       "Built_up_area_r"= as.numeric(NA),
                                       "Unbuilt_imprev_r"= as.numeric(NA),
                                       "Veg_cover_r"= as.numeric(NA),               
                                       "Veg_height_vc_r"= as.numeric(NA),
                                       "Green_volume_r"= as.numeric(NA),            
                                       "Veg_cover_str.r" = as.numeric(NA),
                                       "Veg_height_vc_str.r"= as.numeric(NA),       
                                       "Green_volume_str.r"= as.numeric(NA),
                                       "Veg_height_old_maps_r"= as.numeric(NA),
                                       "water_maps_r"= as.numeric(NA),              
                                       "impervious_old_r" = as.numeric(NA),   
                                       "building_height_old_r"= as.numeric(NA),
                                       "veg.cover_vh"= as.numeric(NA)
      )
      # convert result to dataframe 
      return(result)
    }else{
      return(clip1)
    }
  }
}

########################################################################  
### function 4 - resample FP
  resample_to_footprint = function(r, footprint_rast) {
    r_new = raster::crop(x=r, y=extent(footprint_rast))  # first crop 
    r_new = raster::projectRaster(r_new, footprint_rast) # reproject
    return(r_new)
  }

#####################################################################
#####################################################################
# data
#EC_ROTH <- read_csv("EC_ROTH.csv", col_names = T)
summary(EC_ROTH)
str(EC_ROTH)

# 2019 +/- 1 month
ROTH19plus1 <- filter(EC_ROTH, date(timestamp)>="2018-11-30" &
                               date(timestamp)<="2020-02-01")

#write.csv(ROTH19plus1, file="ROTH19plus1.csv", row.names = F)

# data from the tower
#####################################################################
zmROTH = 39.75
coordROTH <- data.frame(ROTHlon= 13.315827, ROTHlat= 52.457232)
coordinates(coordROTH) = c("ROTHlon", "ROTHlat") 
proj4string(coordROTH) <- CRS("+proj=longlat +datum=WGS84")
coordROTH_UTM = spTransform(coordROTH, CRS("+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
coordROTH_UTM = as.data.frame(coordROTH_UTM)
########################################################################

#######################################################################
# Generate one the footprints raster
#######################################################################
# one footprint to test and resample the layers (funcion 1)
fp_extent_ROTH <- create_footprint_raster(fetch = 1500,
                                        height = (zmROTH-ROTH19plus1$zd.filled[10]),
                                        grid = 300,
                                        speed = ROTH19plus1$ws.filled[10],
                                        direction = ROTH19plus1$wd.filled[10], 
                                        uStar = ROTH19plus1$u.filled[10],
                                        zol = (zmROTH-ROTH19plus1$zd.filled[10])/ROTH19plus1$L.filled[10],
                                        sigmaV = sqrt(ROTH19plus1$v_var.filled[10]),
                                        date = as.POSIXct(ROTH19plus1$timestamp[10], tz="UTC"),
                                        lon = coordROTH_UTM$ROTHlon,
                                        lat = coordROTH_UTM$ROTHlat)
plot(fp_extent_ROTH)

#######################################################################
# Generate all the footprints polygon
#######################################################################
## function footprint_polygon # clip=T (SP_90)
fp_polygon_ROTH <- NULL

for (i in 1:length(ROTH19plus1$timestamp)) {
  fp_polygon_ROTH[[i]] <- footprint_polygon(fetch = 1500,
                                                height = (zmROTH-ROTH19plus1$zd.filled[i]),
                                                grid = 300,
                                                speed = ROTH19plus1$ws.filled[i],
                                                direction = ROTH19plus1$wd.filled[i], 
                                                uStar = ROTH19plus1$u.filled[i],
                                                zol = (zmROTH-ROTH19plus1$zd.filled[i])/ROTH19plus1$L.filled[i],
                                                sigmaV = sqrt(ROTH19plus1$v_var.filled[i]), #*2.5 the FP get wider
                                                date = as.POSIXct(ROTH19plus1$timestamp[i], tz="UTC"),
                                                lon = coordROTH_UTM$ROTHlon,
                                                lat = coordROTH_UTM$ROTHlat,
                                                return_clip = T) #SP_90
}

plot(Atlas_r_GreenVolume[[1]])
plot(fp_polygon_ROTH[[2]],add=T)

##### footprints that fails
unique(diff(as.integer(index(fp_polygon_ROTH))))
unlist(sapply(1:length(fp_polygon_ROTH),FUN=function(i) 
  ifelse(is.null(fp_polygon_ROTH[[i]]), print(i),0)))
# 10171, 10726, 11245, 11782, 13551 

#replace by NA values
fp_polygon_ROTH[[10171]] <- fp_polygon_ROTH[[10170]]
fp_polygon_ROTH[[10726]] <- fp_polygon_ROTH[[10725]]
fp_polygon_ROTH[[11245]] <- fp_polygon_ROTH[[11244]]
fp_polygon_ROTH[[11782]] <- fp_polygon_ROTH[[11781]]
fp_polygon_ROTH[[13551]] <- fp_polygon_ROTH[[13550]]

#fp_polygon_ROTH[[10171]] <- NA
#fp_polygon_ROTH[[10726]] <- NA
#fp_polygon_ROTH[[11245]] <- NA
#fp_polygon_ROTH[[11782]] <- NA
#fp_polygon_ROTH[[13551]] <- NA

######################################################################
# Generate all the footprints as raster
#######################################################################
## function footprint_weighted_SVV # clip true (clip1)
fp_raster_ROTH <- NULL
for (i in 1:length(ROTH19plus1$timestamp)) { 
  fp_raster_ROTH[[i]] <- footprint_weighted_SVV(fetch = 1500,
                                              height = (zmROTH-ROTH19plus1$zd.filled[i]),
                                              grid = 300,
                                              speed = ROTH19plus1$ws.filled[i],
                                              direction = ROTH19plus1$wd.filled[i], 
                                              uStar = ROTH19plus1$u.filled[i],
                                              zol = (zmROTH-ROTH19plus1$zd.filled[i])/ROTH19plus1$L.filled[i],
                                              sigmaV = sqrt(ROTH19plus1$v_var.filled[i]),
                                              date = as.POSIXct(ROTH19plus1$timestamp[i], tz="UTC"),
                                              lon = coordROTH_UTM$ROTHlon,
                                              lat = coordROTH_UTM$ROTHlat,
                                              return_clip = T) #clip1
}

plot(Atlas_r_GreenVolume[[1]])
plot(fp_raster_ROTH[[2]],add=T)

##### footprints that fails
unique(diff(as.integer(index(fp_raster_ROTH))))
unlist(sapply(1:length(fp_raster_ROTH),FUN=function(i) 
  ifelse(is.null(fp_raster_ROTH[[i]]), print(i),0)))
# 10171, 10726, 11245, 11782, 13551 

#replace by NA values
fp_raster_ROTH[[10171]] <- fp_raster_ROTH[[10170]]
fp_raster_ROTH[[10726]] <- fp_raster_ROTH[[10725]]
fp_raster_ROTH[[11245]] <- fp_raster_ROTH[[11244]]
fp_raster_ROTH[[11782]] <- fp_raster_ROTH[[11781]]
fp_raster_ROTH[[13551]] <- fp_raster_ROTH[[13550]]

#values(fp_extent_ROTH_SP[[10171]]) <- NA
#values(fp_extent_ROTH_SP[[10726]]) <- NA
#values(fp_extent_ROTH_SP[[11245]]) <- NA
#values(fp_extent_ROTH_SP[[11782]]) <- NA
#values(fp_extent_ROTH_SP[[13551]]) <- NA

##### write the list as a stack raster
STACkclipROTH <- stack(fp_raster_ROTH)
names(STACkclipROTH) <- ymd_hms(ROTH19plus1$timestamp)
plot(STACkclipROTH[[10]])

###############################################################
#writeRaster(STACkclipROTH, 
#            filename = "FPclipROTH.tif",
#            overwrite=TRUE)
###############################################################
############ arguments to save separatly all rasters 
#paste0("L", as.character(index(fp_extent_ROTH))),# to save separately
#bylayer=TRUE, format='GTiff'                     # all layers
######### or 
#mapply(writeRaster, fp_extent_ROTH, 
#       as.character(index(fp_extent_ROTH)), 'GTiff')
###############################################################

###############################################################
#### load the stack 
#FPclipROTH <- stack("FPclipROTH.tif")
#names(FPclipROTH) <- ymd_hms(ROTH19plus1$timestamp)
#plot(FPclipROTH[[3000]])
#length(FPclipROTH)/90000
#tail(names(FPclipROTH))
#################################################################


#################################################################
#################################################################
# Extract layers from FP
#################################################################
#################################################################
# create the surface_cover_info file to extract land surfaces
maps_ROTH = resample_to_footprint(r=atlas_r_maps, 
                                  footprint_rast=fp_extent_ROTH)
plot(atlas_r_maps[[10]])
plot(maps_ROTH[[10]])

writeRaster(maps_ROTH,filename="maps_ROTH",overwrite=TRUE)

names(maps_ROTH) <- c("layer1","layer2","layer3","layer4",
                       "layer5","layer6","layer7","layer8","layer9",
                       "layer10","layer11","layer12","layer13",
                       "layer14","layer15","layer16", "layer17",
                       "layer18","layer19","layer20","layer21",
"layer22","layer23","layer24","layer25","layer26","layer27",
"layer28","layer29","layer30","layer31","layer32","layer33",
"layer34","layer35","layer36","layer37","layer38", "layer39",
"layer40","layer41","layer42")
######################################################################


######################################################################
######################################################################
# Extract land surface property from the layers with the FP
######################################################################
######################################################################
### function 3 - clip=F
fp_layers_ROTH <- NULL

for (i in 1:length(ROTH19plus1$timestamp)) { 
  fp_layers_ROTH[[i]] <- footprint_weighted_SVV(
                         fetch = 1500,
                         height = (zmROTH-ROTH19plus1$zd.filled[i]),
                         grid = 300,
                         speed = ROTH19plus1$ws.filled[i],
                         direction = ROTH19plus1$wd.filled[i],  
                         uStar = ROTH19plus1$u.filled[i],
                         zol = (zmROTH - ROTH19plus1$zd.filled[i])/ROTH19plus1$L.filled[i],
                         sigmaV = sqrt(ROTH19plus1$v_var.filled[i]),
                         date = as.POSIXct(ROTH19plus1$timestamp[i], tz="UTC"),
                         surface_cover_info = maps_ROTH,
                         lon = coordROTH_UTM$ROTHlon,
                         lat = coordROTH_UTM$ROTHlat)
}

fp_layers_ROTH[[2]]

##### footprints that fails
unique(diff(as.integer(index(fp_layers_ROTH))))
unlist(sapply(1:length(fp_layers_ROTH),FUN=function(i) 
       ifelse(is.null(fp_layers_ROTH[[i]]),print(i),NA)))
#10171, 10726, 11245, 11782, 13551

# fill the gaps with the footprints just before fail
fp_layers_ROTH[[10171]] <- fp_layers_ROTH[[10170]]
fp_layers_ROTH[[10726]] <- fp_layers_ROTH[[10725]]
fp_layers_ROTH[[11245]] <- fp_layers_ROTH[[11244]]
fp_layers_ROTH[[11782]] <- fp_layers_ROTH[[11781]]
fp_layers_ROTH[[13551]] <- fp_layers_ROTH[[13552]]

# transform list in df , for each variable
#########################################################
fp_ROTH = matrix(nrow=length(fp_layers_ROTH), ncol=45)
for(i in 1:dim(fp_ROTH)[1])
{
  for(j in 1:dim(fp_ROTH)[2])
  {
    fp_ROTH[i,j] = fp_layers_ROTH[[i]][[j]]
  }
}
#########################################################
fp_ROTH <- data.frame(fp_ROTH)

names(fp_ROTH) <- names(fp_layers_ROTH[[1]])
fp_ROTH$timestamp <- ROTH19plus1$timestamp
str(fp_ROTH)

cor.test(ROTH19plus1$LE.dry.fsd, fp_ROTH$impervious_old_r)
cor.test(ROTH19plus1$LE.dry.fsd, fp_ROTH$Impervious_r)
cor.test(ROTH19plus1$LE.dry.fsd, fp_ROTH$Impervious_With_str.r)

t(cor(ROTH19plus1$LE.dry.fsd, fp_ROTH[,-1], use="complete.obs"))

boxplot(fp_ROTH$n_pixel, horizontal = TRUE)
summary(fp_ROTH$n_pixel)
hist(fp_ROTH$n_pixel)


# Cleaning FP too big or too small
#########################################################
fp_ROTHh <- fp_ROTH

for (i in 4:45) {
fp_ROTHh[,i][fp_ROTHh$n_pixel<=20] <- NA
}
for (i in 4:45) {
  fp_ROTHh[,i][fp_ROTHh$n_pixel>=3000] <- NA
}

# convert to hourly FP
########################################################
colnames(fp_ROTHh)[c(1)] <- c("date")
fp_ROTHh <- timeAverage(fp_ROTHh,avg.time="1 hour",fill=TRUE) #
str(fp_ROTHh)
summary(fp_ROTHh)

colnames(fp_ROTHh)[c(1)] <- c("timestamp")

plot(fp_ROTHh$n_pixel)
plot(fp_ROTHh$veg.cover_vh)
plot(fp_ROTHh$Veg_cover_r)
cor.test(fp_ROTHh$veg.cover_vh,fp_ROTHh$Veg_cover_r)
cor.test(fp_ROTHh$veg.cover_vh,fp_ROTHh$Veg_cover_str.r)
cor.test(fp_ROTHh$impervious_old_r,fp_ROTHh$Impervious_r)
cor.test(fp_ROTHh$impervious_old_r,fp_ROTHh$Impervious_With_str.r)

