library(raster)
library(httr)
library(sf)
library(dplyr)
library(fasterize)
library(ggplot2)
###################################################################
############ functions to import maps from the atlas ##############
###################################################################
get_X_Y_coordinates <- function(x) {
  sftype <- as.character(sf::st_geometry_type(x, by_geometry = FALSE))
  if(sftype == "POINT") {
    xy <- as.data.frame(sf::st_coordinates(x))
    dplyr::bind_cols(x, xy)
  } else {
    x
  }
}

sf_fisbroker <- function(url) {
  typenames <- basename(url)
  url <- httr::parse_url(url)
  url$query <- list(service = "wfs",
                    version = "2.0.0",
                    request = "GetFeature",
                    srsName = "EPSG:25833",
                    TYPENAMES = typenames)
  request <- httr::build_url(url)
  print(request)
  out <- sf::read_sf(request)
  out <- sf::st_transform(out, 4326)
  out <- get_X_Y_coordinates(out)
  return(out)
}

export_format <- c(
  "geojson", 
  "sqlite"
)

sf_save <- function(z, fname) {
  ifelse(!dir.exists(fname), dir.create(fname), "Folder exists already")
  ff <- paste(file.path(fname, fname), export_format, sep = ".")
  purrr::walk(ff, ~{ sf::st_write(z, .x, delete_dsn = TRUE)})
  saveRDS(z, paste0(file.path(fname, fname), ".rds"))
  
}

##########################################################################
# funtion to resample
resample_to_footprint = function(r, footprint_rast) {
  r_new = raster::crop(x=r, y=extent(footprint_rast))  # first crop 
  r_new = raster::resample(r_new, footprint_rast) # reproject
  return(r_new)
}
###########################################################################
#########################################################################
#########################################################################
#########################################################################
# coordinates from towers and stations
coord <- data.frame(
  Rothlon= 13.315827, Rothlat= 52.457232,
  RothlonUTM=385566.5, RothlatUTM= 5813229,
  DWDRothlon= 13.3017, DWDRothlat= 52.4537,
  DWDRothlonUTM= 384597.4, DWDRothlatUTM= 5812858,
  TUCClon= 13.32785, TUCClat= 52.51228,
  TUCClonUTM=386525.1, TUCClatUTM= 5819332,
  DWDTUCClon= 13.3088, DWDTUCClat= 52.5644,
  DWDTUCClonUTM= 385368.3, DWDTUCClatUTM=5825159)

###################################################################
####### import the ET map - ekd213
###################################################################
# 02.13 Surface Runoff, Percolation, Total Runoff and Evaporation 
#       from Precipitation (2019 Edition)
###################################################################
#Water balance variables including new groundwater formation and input
#parameters from the ABIMO model (as of 2017) for approx. 25,000 block
#and block subareas based on the block map of the information system 
#for the city and the environment
####################################################################
ETmap <- sf_fisbroker("https://fbinter.stadt-berlin.de/fb/wfs/data/senstadt/s02_13whaus2017")

ETmap <- ETmap %>%
  dplyr::mutate(RAUMID = stringr::str_sub(gml_id, 12, 19)) %>%
  dplyr::select(gml_id, RAUMID, everything()) %>%
  dplyr::arrange(RAUMID)

dplyr::glimpse(ETmap)

sf_save(ETmap, "ETmap_atlas")

summary(ETmap)
names(ETmap)
str(filter(ETmap, schl5==0700231951000000)) #key TUCC

#$ r         : num 387  = Total discharge [mm]
#$ verdunstun: num 196  = Evaporation [mm]
#$ row       : num 243  = Surface runoff [mm]
#$ ri        : num 144  = Infiltration [mm]
#$ kor_fl_n  : num 1
#$ ri_k      : num 144
#$ flaeche   : num 198991 = Block area and proportional street area [m²]
#$ nutzung   : chr "Gemeinbedarfs- und Sondernutzung" = Land use	General and special use
#$ vg        : num 68.9  = Sealing of the block areas [%]
#$ vgstrasse : num 97.4  = Sealing of the streets [%]
#$ flur      : num 3.1   = Distance to groundwater level [m]
#$ kanal     : int 1     = rainwater sewer (1 = yes, 0 = no)
#$ kan_beb   : int 82    = Degree of sewerage built-up sealed
#$ kan_vgu   : int 66    = Degree of sewerage sealed undeveloped
#$ kan_str   : int 89    = Degree of sewerage street
#$ regenja   : int 535   = mean uncorrected annual precipitation [mm]
#$ regenso   : int 297   = mean uncorrected summer precipitation [mm]
#$ feld_30   : int 10    = Usable field capacity of the shallow roots [Vol%]
#$ feld_150  : int 12    = Usable field capacity of deep-rooters [Vol%]

# ET mm
summary(ETmap$verdunstun)

ggplot() +
  geom_sf(data = ETmap)

ggplot(ETmap) +
  geom_sf(color="transparent", size=0,aes(fill = feld_30 )) +
  coord_sf(xlim = c(13.26,13.36), ylim = c(52.44,52.53), expand = FALSE) +
  xlab("Longitude") + 
  ylab("Latitude")

# transform to UTM
ETmap <- st_transform(ETmap,crs("+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs"))

# create an empty raster of 10m resolution
mapr <- raster(xmn=370000,xmx=415741,ymn=5799519,ymx=5837199, 
            res=10,crs=crs("+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs"))

# rasterize the most important variables
Atlas_r_ETmap <- raster::stack(
  fasterize(ETmap, mapr, field="verdunstun",fun="max"),
  fasterize(ETmap, mapr, field="row",fun="max"),
  fasterize(ETmap, mapr, field="ri",fun="max"),
  fasterize(ETmap, mapr, field="r",fun="max"),
  fasterize(ETmap, mapr, field="vg",fun="max"),
  fasterize(ETmap, mapr, field="vgstrasse",fun="max"),
  fasterize(ETmap, mapr, field="flur",fun="max"),
  fasterize(ETmap, mapr, field="kanal",fun="max"),
  fasterize(ETmap, mapr, field="kan_beb",fun="max"),
  fasterize(ETmap, mapr, field="kan_vgu",fun="max"),
  fasterize(ETmap, mapr, field="kan_str",fun="max"),
  fasterize(ETmap, mapr, field="regenja",fun="max"),
  fasterize(ETmap, mapr, field="regenso",fun="max"),
  fasterize(ETmap, mapr, field="feld_30",fun="max"),
  fasterize(ETmap, mapr, field="feld_150",fun="max"))

names(Atlas_r_ETmap) <- c("ET","Runoff","infiltration","discharge","Sealing_block",
                    "Sealing_str","Dist_underwater", "sewer_presence",
                    "sewerage_bult","sewerage_undev","sewerage_str","
                    Prec_annual","Prec_summer","Cap_ShallowRoot","Cap_deepRoot")

Atlas_r_ETmap <- crop(Atlas_r_ETmap, 
                      extent(381673,388803,5811189,5821467))

plot(Atlas_r_ETmap[[1]], col=blues9)
points(coord$RothlonUTM, coord$RothlatUTM)
points(coord$TUCClonUTM, coord$TUCClatUTM)
###################################################################
###################################################################
# raster originaly from the Atlas
# Impreviousmapr <- raster("./ETmap_atlas/class_4.tif")
# plot(Impreviousmapr)
# summary(Impreviousmapr)
####################################################################
####################################################################
###################################################################
####### import the Imprevious map - ekd102
###################################################################
# 01.02 Impervious Soil Coverage (Sealing of Soil Surface) (Edition 2017)
###################################################################
#	The real use of the built-up areas, the existing green and open space,
# the urban structure, the sealing and the types of surface. 
# Spatial reference block / partial block map ISU5 (Information System
# City and Environment) as of December 31, 2015.
####################################################################
Impreviousmap <- sf_fisbroker("https://fbinter.stadt-berlin.de/fb/wfs/data/senstadt/sach_nutz2015_nutzsa")

Impreviousmap <- Impreviousmap %>%
  dplyr::mutate(RAUMID = stringr::str_sub(gml_id, 12, 19)) %>%
  dplyr::select(gml_id, RAUMID, everything()) %>%
  dplyr::arrange(RAUMID)

dplyr::glimpse(Impreviousmap)

sf_save(Impreviousmap, "Impreviousmap_atlas")

summary(Impreviousmap)
str(filter(Impreviousmap, FLALLE==160724)) #key TUCC

#$ VG        : num 68.9 = Sealing, track ballast is considered sealed [% of area size]
#$ VG_0      : num 68.9 = Sealing, track ballast is considered unsealed [% of area size]
#$ PROBAU    : num 47.5 = Built-up sealed area [% of area size]
#$ PROVGNEU  : num 21.4 = Sealed area undeveloped, track ballast is considered sealed [% of area size]
#$ PROVGNEU_0: num 21.4 = Unbuilt sealed area, track ballast is considered unsealed [% of area size]
#$ KL1       : int 15   = Surface type 1 [% of undeveloped sealed area]
#$ KL2       : int 70   = Surface type 2 [% of undeveloped sealed area]
#$ KL3       : int 12   = Surface type 3 [% of undeveloped sealed area]
#$ KL4       : int 3    = Surface type 4 [% of undeveloped sealed area]

Impreviousmap <- st_transform(Impreviousmap,crs(ETmap))

Atlas_r_Imprevious <- raster::stack(
  fasterize(Impreviousmap, mapr, field="VG",fun="max"),
  fasterize(Impreviousmap, mapr, field="VG",fun="max"),
  fasterize(Impreviousmap, mapr, field="PROBAU",fun="max"),
  fasterize(Impreviousmap, mapr, field="PROVGNEU_0",fun="max"))

Atlas_r_Imprevious[[2]]@data@values[which(is.na(Atlas_r_Imprevious[[2]]@data@values))] <- 100

names(Atlas_r_Imprevious) <- c("Impervious","Impervious_With_str",
                                 "Built_up_area","Unbuilt_imperv")

Atlas_r_Imprevious <- crop(Atlas_r_Imprevious, 
                      extent(381673,388803,5811189,5821467))

plot(Atlas_r_Imprevious[[1:2]], col=terrain.colors(10))

#green volume - streets
####################################################################
####################################################################
###################################################################
####### import the green volume map - ekd509
###################################################################
# 05.09 Green Volume (Edition 2017)
###################################################################
####################################################################
#green volume - blocks
Green.blocks <- sf_fisbroker("https://fbinter.stadt-berlin.de/fb/wfs/data/senstadt/s_05_09_gruenvol2010")

Green.blocks <- Green.blocks %>%
  mutate(RAUMID = stringr::str_sub(gml_id, 12, 19)) %>%
  select(gml_id, RAUMID, everything()) %>%
  arrange(RAUMID)

dplyr::glimpse(Green.blocks)

sf_save(Green.blocks, "Green.blocks_atlas")

summary(Green.blocks)
plot(Green.blocks["VEGPROZ"])

str(filter(Green.blocks, FLALLE==160724.4)) 
####################################################################
#$ gml_id    : chr "s_05_09_gruenvol2010.0700231951000000" #key TUCC
#$ FLALLE    : num 160724  = area
#$ VEGHOE    : num 9.4     = Mean height of vegetation in relation to the area covered with vegetation [m]
#$ VEGPROZ   : num 30.4    = Area covered with vegetation in relation to the total area [%]
#$ VEGVOLA   : int 461279  = Green volume [m³]
#$ VEGVOL    : num 2.87    = Green volume number [m³ / m²]
#$ FLUBEB    : num 87791   = Area size of the above-ground undeveloped area [m²]
#$ VEGHOEUBEB: num 9.4     = Mean height of the vegetation in relation to the area of the undeveloped area covered with vegetation [m]
#$ VEGPROUBEB: num 53.9    = Area covered with vegetation in relation to the undeveloped area [%]
#$ VEGVOLAUBE: int 446854  = Green volume of the undeveloped area [m³]
#$ VEGVOLUBEB: num 5.09    = Number of green volumes related to the area of the undeveloped area covered with vegetation [m³ / m²]
################################################################
Green.blocks <- st_transform(Green.blocks,crs(ETmap))

### green street
Green.Street <- sf_fisbroker("https://fbinter.stadt-berlin.de/fb/wfs/data/senstadt/wfs_05_09_gruenvol2010_str")

Green.Street <- Green.Street %>%
  dplyr::mutate(RAUMID = stringr::str_sub(gml_id, 12, 19)) %>%
  dplyr::select(gml_id, RAUMID, everything()) %>%
  dplyr::arrange(RAUMID)

dplyr::glimpse(Green.Street)
sf_save(Green.Street, "Green.Street_atlas")

summary(Green.Street)
#plot(Green.Street["VEGPROZ"])
###########################################
#$ VEGHOE    : = Mean height of vegetation in relation to the area covered with vegetation [m]
#$ VEGPROZ   : = Area covered with vegetation in relation to the total area [%]
#$ VEGVOLA   : = Green volume [m³]
#$ VEGVOL    : = Green volume number [m³ / m²]
#$ VEGHOEUBEB: = Mean height of the vegetation in relation to the area of the undeveloped area covered with vegetation [m]
#$ VEGPROUBEB: = Area covered with vegetation in relation to the undeveloped area [%]
#$ VEGVOLAUBE: = Green volume of the undeveloped area [m³]
#$ VEGVOLUBEB: = Number of green volumes related to the area of the undeveloped area covered with vegetation [m³ / m²]
###########################################
Green.Street <- st_transform(Green.Street,crs(ETmap))

Atlas_r_GreenVolume <- raster::stack(
  fasterize(Green.blocks, mapr, field="VEGPROZ",fun="max"),
  fasterize(Green.blocks, mapr, field="VEGHOE",fun="max"),
  fasterize(Green.blocks, mapr, field="VEGVOL",fun="max"),
  fasterize(Green.Street, mapr, field="VEGPROZ",fun="max"),
  fasterize(Green.Street, mapr, field="VEGHOE",fun="max"),
  fasterize(Green.Street, mapr, field="VEGVOL",fun="max"))

names(Atlas_r_GreenVolume) <- c("Veg_cover","Veg_height",
                               "Green_volume","Veg_cover_str",
                               "Veg_height_vc_str","Green_volume_str")

Atlas_r_GreenVolume <- crop(Atlas_r_GreenVolume, 
                           extent(381673,388803,5811189,5821467))

plot(Atlas_r_GreenVolume[[1]])

####################################################################
####################################################################
###################################################################
####### import the  - ekd
###################################################################
# Soil science characteristics 2015 (Environmental Atlas)
###################################################################
#Factual data on soil science characteristics, use and soil associations.
# Spatial reference block / partial block map ISU 5 
# (Information System City and Environment) as of 2015.
####################################################################
Soil2 <- sf_fisbroker("https://fbinter.stadt-berlin.de/fb/wfs/data/senstadt/s_boden_wfs2_2015")

Soil2 <- Soil2 %>%
  dplyr::mutate(RAUMID = stringr::str_sub(gml_id, 12, 19)) %>%
  dplyr::select(gml_id, RAUMID, everything()) %>%
  dplyr::arrange(RAUMID)

dplyr::glimpse(Soil2)

sf_save(Soil2, "Soil2_atlas")

summary(Soil2)
str(filter(Soil2, schl5==0700231951000000))

#$ nfkmin30     : num 10 = lowest nFK value of the shallow root zone [mm]
#$ nfkmax30     : num 80 = highest nFK value of the shallow root zone [mm]
#$ nfkdur30     : num 30 = average nFK value of the shallow root zone [mm]
#$ nfk30_dm     : num 10 = lowest nFK value of the shallow root zone [mm]
#$ nfkstu30     : num 2 = Level of the nFK of the shallow root zone
#$ nfkmin150    : num 60  = lowest nFK value of the deep root zone [mm]
#$ nfkmax150    : num 320 = highest nFK value of the deep root zone [mm]
#$ nfkdur150    : num 180 = average nFK value of the deep root zone [mm]
#$ nfk150_dm    : num 12 = average nFK value per decimeter of the deep root zone [mm / dm]
#$ nfkstu150    : num 4 = Level of the nFK of the deep root zone
#$ nfkmin       : num 35  = lowest usable field capacity of the effective root space [mm]
#$ nfkmax       : num 50  = highest usable field capacity of the effective root space [mm]
#$ nfkdur       : num 45  = average usable field capacity of the effective root space [mm]
#$ nfkdurstu    : num 1 = Level of the average usable field capacity of the effective root space
#$ fk_o_dm      : num 15  = Field capacity topsoil [mm / dm]
#$ fk_u_dm      : num 11  = Field capacity sub-floor [mm / dm]
#$ fk           : num 232 = Field capacity 0-20 dm [mm]
#$ fkstufe      : num 1   = Field capacity level 0-20dm [mm]
#$ ld           : num 1.57 = effective storage density of the humus layer [kg / dm³]
#$ phobermin    : num 5 = lowest pH value (CaCl2) for the topsoil (0-10cm)
#$ phobermax    : num 8 = highest pH value (CaCl2) for the topsoil (0 - 10 cm)
#$ phoberdur    : num 7 = typical pH value (CaCl2) for the topsoil (0 - 10 cm)
#$ phstufe_o    : num 6 = Level typical pH value (CaCl2), topsoil
#$ phuntermin   : num 6 = lowest pH value (CaCl2) for the subsoil (90 - 100 cm)
#$ phuntermax   : num 8 = highest pH value (CaCl2) for the sub-floor (90 - 100 cm)
#$ phunterdur   : num 7.5 = typical pH value (CaCl2) for the subsoil (90 - 100 cm)
#$ phstufe_u    : num 5 = Level typical pH value (CaCl2), subsoil
#$ bs           : num 97 = Base saturation, topsoil [%]
#$ bsstufe      : num 5 = Base saturation level, topsoil
#$ swert        : num 13.5 = S-value, topsoil [molc / cm²]
#$ swertstu     : num 6 = Level S-value topsoil
#$ kak_o        : num 3.9 = KAKeff, topsoil [cmolc / kg]
#$ kak_u        : num 2 = KAKeff, subsoil [cmolc / kg]
#$ kak          : num 3 = KAKeff, top and bottom soil [cmol / kg]
#$ kakstufe     : num 1 = KAKeff level, top and bottom floor
#$ humus        : num 3 = Humus content of the (mineral) topsoil [mass%]
#$ humus_dm     : num 1 = Thickness of the humus layer [dm]
#$ humus_real   : num 3 = Humus content of the humus layer taking into account the proportion of peat [mass%]
#$ humus_m      : num 4.7 = Amount of humus [kg / m²]
#$ humusmstu    : num 1  = Amount of humus level
#$ corg_kg_qm   : num 2.7  = Organic carbon stock [kg / m²]
#$ kf_u         : num 427 = Kf subfloor [cm / d]
#$ kfstufe_u    : num 6 = Level Kf sub-floor
#$ kf_o         : num 427 = Kf topsoil [cm / d]
#$ kfstufe_o    : num 6 = Level Kf topsoil
#$ kf           : num 427 = Kf top and bottom floor [cm / d
#$ kfstufe      : num 6 = Level Kf top and bottom

Soil2 <- st_transform(Soil2,crs(ETmap))

####################################################################
####################################################################
###################################################################
####### import the Soil Functions - ekd111
###################################################################
# 01.11 Criteria for the Evaluation of the Soil Functions (Edition 2018)
###################################################################
# Criteria for evaluating soil functions, use and soil associations. 
# Spatial reference block / partial block map ISU 5 
# (Information System City and Environment) as of 2015
####################################################################
#Regulatory Function of the Soil for the Water Balance 2015 soil water supply
Soil_functions <- sf_fisbroker("https://fbinter.stadt-berlin.de/fb/wfs/data/senstadt/s_boden_wfs3_2015")

Soil_functions <- Soil_functions %>%
  dplyr::mutate(RAUMID = stringr::str_sub(gml_id, 12, 19)) %>%
  dplyr::select(gml_id, RAUMID, everything()) %>%
  dplyr::arrange(RAUMID)

dplyr::glimpse(Soil_functions)

sf_save(Soil_functions, "Soil_functions_atlas")

summary(Soil_functions)
str(filter(Soil_functions, SCH5==0700231951000000)) #key TUCC

#$ AUSTAUSCH     : num 2.67 = Frequency of exchange of soil water [/ year]
#$ SELTENPROZ    : num 4.78 = Share of the soil company in the total area [%]

Soil_functions <- st_transform(Soil_functions,crs(ETmap))

Atlas_r_Soil <- raster::stack(
  fasterize(Soil2, mapr, field="nfkdur30",fun="max"),
  fasterize(Soil2, mapr, field="nfkdur150",fun="max"),
  fasterize(Soil2, mapr, field="nfkdur",fun="max"),
  fasterize(Soil2, mapr, field="fk_o_dm",fun="max"),
  fasterize(Soil2, mapr, field="fk_u_dm",fun="max"),
  fasterize(Soil2, mapr, field="fk",fun="max"),
  fasterize(Soil2, mapr, field="bs",fun="max"),
  fasterize(Soil2, mapr, field="swert",fun="max"),
  fasterize(Soil2, mapr, field="kak_o",fun="max"),
  fasterize(Soil2, mapr, field="corg_kg_qm",fun="max"),
  fasterize(Soil_functions, mapr, field="AUSTAUSCH",fun="max"),
  fasterize(Soil_functions, mapr, field="SELTENPROZ",fun="max"))

names(Atlas_r_Soil) <- c("Av_nFK_shallow_rootzone", "Av_nFK_deep_rootzone","Av_field_capacity_r",
                         "Topsoil_field_capacity","subfloor_field_capacity","field_capacity_20dm",
                         "topsoil_saturation","topsoil_saturation_value","topsoil_KAKeff",
                         "Organic_carbon_stock", "soilwater_exchange",
                         "soil_company")

Atlas_r_Soil <- crop(Atlas_r_Soil, 
                     extent(381673,388803,5811189,5821467))

plot(Atlas_r_Soil[[1]])


######################################################################
## raster data originally from the Atlas
######################################################################
#height.map <- sf_fisbroker("https://fbinter.stadt-berlin.de/fb/feed/senstadt/a_06_10gebveghoeh2010_geb1")
#plot(height.map)
###
#Imprevious.r <- raster("./ETmap_atlas/class_4.tif")
#plot(Impreviousmapr)
###
#Flura.r <- raster("./ETmap_atlas/Flurabstand_0905_EPSG25833.tif")
#plot(Flura.r)
###
# building and veg height
#https://fbinter.stadt-berlin.de/fb/feed/senstadt/a_06_10gebveghoeh2010_geb1
#############################################################

######################################################################
### vegetation height raster
######################################################################
Veg_height_old_r <- raster("./maps/VEG_DSM_Berlin_1m_noNA.tif")
Veg_height_old <- crop(Veg_height_old_r, extent(381673,388803, 
                                                5811189,5821467))
Veg_height_old_r <- resample_to_footprint(r=Veg_height_old,
                                         footprint_rast=Atlas_r_ETmap)
plot(Veg_height_old_r)
points(coord$RothlonUTM, coord$RothlatUTM)
points(coord$TUCClonUTM, coord$TUCClatUTM)

#summary(getValues(Veg_height_old_r, row=9000))

impervious_old_r <- raster("./maps/Berlin_impervious_fraction.tif")
impervious_old <- crop(impervious_old_r, extent(381673,388803, 
                                                5811189,5821467))
impervious_old_r <- resample_to_footprint(r=impervious_old,
                                          footprint_rast=Atlas_r_ETmap)
plot(impervious_old_r)

building.height_r <- raster("./maps/BUI_DSM_Berlin_1m_noNA.tif")
building.height <- crop(building.height_r, extent(381673,388803, 
                                                5811189,5821467))
building.height_r <- resample_to_footprint(r=building.height,
                                          footprint_rast=Atlas_r_ETmap)
plot(building.height_r)

water <- raster("./maps/Berlin_water_fraction.tif")
water.r <- crop(water, extent(381673,388803, 5811189,5821467))
water <- resample_to_footprint(r=water.r,footprint_rast=Atlas_r_ETmap)
                                  
plot(water)

# calculate veg. cover from veg.height
veg.cover_vh_old_r <- Veg_height_old_r
veg.cover_vh_old_r[] <- ifelse(Veg_height_old_r[] > 0.01, 1, 0)
plot(Veg_height_old_r)

#stack old files
Old_maps <- raster::stack(Veg_height_old_r,
                          impervious_old_r,
                          building.height_r,
                          water,
                          veg.cover_vh_old_r)

plot(Old_maps[[1]])

#stack all the stacks
atlas_r_maps <- raster::stack(Old_maps,
                              Atlas_r_Imprevious,
                              Atlas_r_GreenVolume,Atlas_r_Soil,
                              Atlas_r_ETmap)

writeRaster(atlas_r_maps, filename="atlas_r_maps")
plot(atlas_r_maps[[30]])
