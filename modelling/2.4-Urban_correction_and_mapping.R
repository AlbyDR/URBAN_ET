library(tidyverse)
library(lubridate)
library(REddyProc)
library(bigleaf)
library(summarytools)
library(sf)
library(raster)
library(exactextractr)
library(ggplot2)
library(ggspatial)
library(geobuffer)
library(rgeos)
library(RColorBrewer)

########################################################################
names(Berlin2020_pred) # list the name of all outputs
########################################################################

summary(Berlin2020_pred)

# Average ET per month (without urban correction)
Berlin2020_pred %>%
  mutate_if(is.numeric, pmax, 0) %>% # convert negative ET to 0
  mutate(month = month(timestamp)) %>%
  group_by(month, id_pixel) %>%
  summarise(ET_month = sum(ET)) %>%
  descr(stats = c("mean", "min", "max"), 
        transpose = T, headings = F, Data.frame=T)

# annual ET per pixel (no correction)
Berlin2020_pred %>%
  mutate_if(is.numeric, pmax, 0) %>%
  group_by(id_pixel) %>%
  summarise(ET = sum(ET)) -> ET_2020_noNA

summary(ET_2020_noNA)

# Heat flux in the hottest day of 2020
Berlin2020_pred %>%
  mutate(date = date(timestamp)) %>%
  filter(date =="2020-08-08") %>%
  group_by(id_pixel) %>%
  summarise(ETsum = sum(ET), ET_soilsum = sum(ET_soil), ET_canopysum = sum(ET_canopy),
            Tsavemax = max(Tsave), Tcavemax = max(Tcave),
            Tsavemean = mean(Tsave), Tcavemean = mean(Tcave)) %>%
  select(id_pixel, Tsavemax, Tcavemax, Tsavemean, Tcavemean,
         ETsum, ET_soilsum, ET_canopysum) -> Hottest_2020_noNA

summary(Hottest_2020_noNA)

# Urban correction and mapping
# create a dataframe with all pixels of the Berlin extent (even the NAs)
ET2020_df <- data.frame("ET_annual" = cellNA, # cellNA was created in 2.1 model inputs
                        "ET_hottest" = cellNA,
                        "Temp_soil_max" = cellNA,
                        "Temp_canopy_max" = cellNA)

# fill the non-NA predictions
ET2020_df$ET_annual[!is.na(ET2020_df$ET_annual)] <- ET_2020_noNA$ET
ET2020_df$ET_hottest[!is.na(ET2020_df$ET_hottest)] <- Hottest_2020_noNA$ETsum
ET2020_df$Temp_soil_max[!is.na(ET2020_df$Temp_soil_max)] <- Hottest_2020_noNA$Tsavemax
ET2020_df$Temp_canopy_max[!is.na(ET2020_df$Temp_canopy_max)] <- Hottest_2020_noNA$Tcavemax

summary(ET2020_df)

# convert the df into a raster
ET_raster <- krg_grid
ET_raster$ET_annual <- ET2020_df$ET_annual
ET_raster$ET_hottest <- ET2020_df$ET_hottest
ET_raster$Temp_soil_max <- ET2020_df$Temp_soil_max
ET_raster$Temp_canopy_max <- ET2020_df$Temp_canopy_max

summary(ET_raster[[]])
plot(ET_raster[[-1]])

# Extract the raster information into a vector map
plot(Green_vol["vegproz"]) # see 1.4

# extract the values of the raster with the SCOPE model outputs into a vector map (polygon) 
ET_vector <- exactextractr::exact_extract(ET_raster, Green_vol, 'max') # 'mean'
head(ET_vector)

# include the values into a vector map
ET_vector2020  <- Green_vol[2:3] # only the geometry of multipolygon object
# include Annual_ET values
ET_vector2020$ET_annual <- ET_vector$max.ET_annual
# correct the annual ET based on the vegetation fraction
ET_vector2020$Urban_ET_annual <- (ET_vector2020$ET_annual * (ET_vector2020$vegproz)/100)
# include daily ET (hottest day)
ET_vector2020$ET_hottest <- ET_vector$max.ET_hottest
# correct the daily ET (hottest day) based on the vegetation fraction
ET_vector2020$Urban_ET_hottest<- (ET_vector2020$ET_hottest * (ET_vector2020$vegproz)/100)

plot(ET_vector2020["ET_hottest"], border="transparent")
plot(ET_vector2020["Urban_ET_hottest"], border="transparent")

# include the max soil and canopy temperature in the hottest day
ET_vector2020$Ts_hottest <- ET_vector$max.Temp_soil_max
ET_vector2020$Tc_hottest <- ET_vector$max.Temp_canopy_max

# create an index for the soil temperature under vegetation in the hottest day (radiation cooling effect)
ET_vector2020$Ts_hottest_idx <- scales::rescale(1-(
  (ET_vector2020$Ts_hottest - min(ET_vector2020$Ts_hottest,na.rm=T))/
    (max(ET_vector2020$Ts_hottest,na.rm=T)-min(ET_vector2020$Ts_hottest,na.rm=T))), 
  to = c(0,1))

# correct the radiation cooling index (RCoS) to urban environment using vegetation fraction
ET_vector2020$Rcos20 <- (ET_vector2020$Ts_hottest_idx * (ET_vector2020$vegproz)/100)
summary(ET_vector2020$Rcos20)

# create the Evapotranspirative cooling index (ECoS) based on the hottest day
ET_vector2020$Ecos20 <- scales::rescale(
  (ET_vector2020$Urban_ET_hottest - min(ET_vector2020$Urban_ET_hottest, na.rm=T))/
(max(ET_vector2020$Urban_ET_hottest,na.rm=T) - min(ET_vector2020$Urban_ET_hottest,na.rm=T))
  , to = c(0,1))
summary(ET_vector2020$Ecos20)

# calculate the greening cooling index as the average of RCoS and ECoS
ET_vector2020$Gcos20 <- ((ET_vector2020$Rcos20 + ET_vector2020$Ecos20)/2)

# extract LAI in the hottest day 2020
LAI_Berlin_hottest <- raster(LAI_Berlin$date_2020.08.08_12)
LAI_Berlin_hottest$hottest2020 <- raster::values(LAI_Berlin$date_2020.08.08_12)

LAI_vector <- exactextractr::exact_extract(LAI_Berlin_hottest, 
                                           Green_vol, 'max')
summary(LAI_vector)

names(ET_vector2020)

# create the final map object with all important variables
Cooling_Maps <- ET_vector2020[3]
Cooling_Maps$Annual_URBAN_ET_2020 <- ET_vector2020$Urban_ET_annual
Cooling_Maps$ET_hottest_day_2020 <- ET_vector2020$Urban_ET_hottest
Cooling_Maps$ECoS_2020 <- ET_vector2020$Ecos20
Cooling_Maps$RCoS_2020 <- ET_vector2020$Rcos20
Cooling_Maps$GCoS_2020 <- ET_vector2020$Gcos20
Cooling_Maps$Vegetation_Height <- ET_vector2020$veghoe
Cooling_Maps$Vegetation_Fraction <- ET_vector2020$vegproz
Cooling_Maps$LAI_hottest_day_2020 <- LAI_vector

plot(Cooling_Maps[c(2,3)], border = "transparent", nbreaks=11, 
     pal=RColorBrewer::brewer.pal('RdYlBu', n = 11), reset=FALSE)

plot(Cooling_Maps[c(4,5,6)], border = "transparent", nbreaks=11, 
     pal=RColorBrewer::brewer.pal('RdYlBu', n = 11), reset=FALSE)

plot(Cooling_Maps[c(7,8,9)], border = "transparent", nbreaks=7, 
     pal=(RColorBrewer::brewer.pal(7,"Greens")), reset=FALSE)

plot(Berlin_border_utm4[c(1,2,3,4,5)])

class(Cooling_Maps)
sf::st_crs(Cooling_Maps) <- 

write_sf(Cooling_Maps, "D:/Research_topics/UWI/Data_UWI/Cooling_Maps.gpkg")
Cooling_Maps <- read_sf("Cooling_Maps.gpkg")

