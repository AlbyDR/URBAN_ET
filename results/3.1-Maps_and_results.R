library(tidyverse)
library(lubridate)
library(summarytools)
library(sf)
library(raster)
library(ggplot2)
library(ggspatial)
library(geobuffer) #devtools::install_github("valentinitnelav/geobuffer")
library(rgeos)
library(RColorBrewer)
library(mapview)

# cooling index maps
ggplot(Cooling_Maps) +
  geom_sf(aes(fill = GCoS_2020), colour = NA) + 
  geom_sf(data = water_polygons, fill = "white", size = 0, color = "transparent") +
  labs(title = "Greening cooling service index (Gcos) - 2020") +
  geom_sf(data = Berlin_border_utm, fill = "transparent", size = 1.2, color = "black") +
  guides(linetype = guide_legend(title = NULL, order = 2), color = guide_legend(order = 1)) +
  scale_fill_gradientn(breaks = seq(0, 1, 0.1),limits = c(0,1),
                       colors = RColorBrewer::brewer.pal('RdYlBu', n = 9), 
                       name = "GCoS", na.value = NA,
                       guide = guide_colorbar(direction = "vertical",
                                              label.position = "right",
                                              title.position = "top",
                                              title.vjust = 2.5, label.vjust = 0.5,
                                              frame.colour = "black",
                                              frame.linewidth = 0.5,
                                              frame.linetype = 1,
                                              barwidth = 1.2, barheight = 25, nbin = 30,
                                              label.theme = element_text(angle = 0, size = 14))) +
  annotation_scale(location = "bl", height = unit(0.4, "cm"),
                   pad_x = unit(1.5,"cm"), pad_y = unit(2,"cm"),
                   text_pad = unit(0.25, "cm"),
                   text_cex = 1.5) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         height = unit(1.5, "cm"), width = unit(1.5, "cm"),
                         pad_x=unit(1,"cm"), pad_y=unit(1.5,"cm")) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        title = element_text(size = 16),
        legend.spacing.y = unit(0, "lines"),
        legend.box.spacing = unit(0, "lines"),
        panel.grid.major = element_blank(), 
        panel.background = element_rect("white"))

# create a pallet for ET
ETcolor = rev(c(rep("#4575b4",1),rep("#74add1",1),rep("#abd9e9",1), rep("#e0f3f8",1),
            rep("#ffffbf",1),rep("#fee090",1),rep("#fdae61",1),rep("#f46d43",1), 
            rep("#d73027",1),  rep("#d73027",1), rep("#a50026",1), rep("#a50026",1)))

# EC tower TUCC map zoom
ggplot(Cooling_Maps) +
  geom_sf(aes(fill = Annual_URBAN_ET_2020), colour = NA) + 
  geom_sf(data = water_polygons, fill = "white", size = 0, color = "transparent") +
  scale_fill_gradientn(breaks = seq(0, 600, 50),limits = c(0,600),
                       colors = ETcolor, 
                       name = "ET", na.value = NA,
                       guide = guide_colorbar(direction = "horizontal",
                                              label.position = "bottom",
                                              title.vjust = 9, label.vjust = 9,
                                              frame.colour = "black",
                                              frame.linewidth = 0.5,
                                              frame.linetype = 1,
                                              title.position = "left",
                                              barwidth = 25, barheight = 1.2, nbin = 30,
                                              label.theme = element_text(angle = 0, size = 14))) +
  coord_sf(xlim = c(385525.6,387524.6), ylim = c(5818333,5820331), expand = FALSE) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="bottom",
        legend.spacing.y = unit(1.5, "lines"),
        legend.box.spacing = unit(-0.5, "lines"),
        panel.grid.major = element_blank(), 
        panel.background = element_rect("white"))

# areas with Vegetation_Fraction = 0 or NA
Cooling_Maps %>%
  filter(Vegetation_Fraction == 0 | is.na(Vegetation_Fraction)) %>%
  ggplot() +
  geom_sf(aes(fill = Vegetation_Fraction), colour = NA) + 
  geom_sf(data = water_polygons, fill = "white", size = 0, color = "transparent")

# average values considering the number of polygons/features
tibble(data.frame(Cooling_Maps)) %>%
  mutate_if(is.numeric, pmax, 0) %>%
  summarise_if(is.numeric, sum, na.rm = TRUE)/length(Cooling_Maps$geometry) %>%
  t() %>% round(2) %>% print()

# average values ponder by the area of the polygon/features
tibble(data.frame(Cooling_Maps, "area"=as.double(st_area(Cooling_Maps))))%>%
  mutate_if(is.numeric, pmax, 0) %>%
  select(c(-1)) %>%
  summarise_if(is.numeric, quantile, na.rm = TRUE) %>%
  t() %>% round(2) %>% print()

# maximum annual ET
tibble(data.frame(Cooling_Maps)) %>%
  select_if(is.numeric) %>%
  arrange(desc(Annual_URBAN_ET_2020)) %>%
  select(c(1,7,6,5,4,3)) %>%
  print(n = 20)

# maximum GCoS
tibble(data.frame(Cooling_Maps)) %>%
  select_if(is.numeric) %>%
  arrange(desc(GCoS_2020)) %>%
  select(c(5,3,4,6,7,8)) %>%
  print(n = 20)

# coordinates of the highest ETs
max(Cooling_Maps$Annual_URBAN_ET_2020, na.rm = TRUE)
st_centroid(filter(Cooling_Maps, Annual_URBAN_ET_2020 >= 615))

Cooling_Maps %>%
  filter(Annual_URBAN_ET_2020 >= 550, 
         Vegetation_Height <= 0.5, 
         Vegetation_Fraction >= 95) %>% 
  arrange(desc(Annual_URBAN_ET_2020)) %>% 
  select("Annual_URBAN_ET_2020", "GCoS_2020", "Vegetation_Fraction",
         "Vegetation_Height") %>%
  st_centroid()

# select some areas to have a zoom image
points_map_zoom <- data.frame(
  x = c(386476,     385574.5,  391546.1, 
        392786.3,   409259.2,  383499.8,
        398122.2 ,  405222.7),
  y = c(5819200,    5813272,   5814851,
        5831838,    5805898,   5830699,
        5812437,    5810506),
  site=c("EC_Tower_TUCC(2919)", "EC_Tower_ROTH(9217)", "Grassland(10589)",
         "crop(19212)", "Forest_max",  "Veg_height_max",
         "ET_545", "ET544"))

# convert to spatial object
coordinates(points_map_zoom) = c("x", "y") 
proj4string(points_map_zoom) <- raster::crs(krg_grid)
points_map_utm <- st_as_sf(points_map_zoom)
points_map_lat <- spTransform(points_map_zoom, CRSobj=CRS("+proj=longlat +datum=WGS84"))
#points_map_lat_sf <- st_as_sf(points_map_zoom)

points_map_zoom[[1]]

points_map_zoom@coords[1,1]
points_map_lat@coords[1,1][[1]]


#mapviewOptions(fgb = FALSE)

# show all areas
mapview(Cooling_Maps, zcol="Annual_URBAN_ET_2020", alpha.regions = 0.5, col.regions = RColorBrewer::brewer.pal('RdYlBu', n = 9),
        legend = F, legend.opacity = 0, layer.name = "ET", map.types = "Esri.WorldImagery",  
        na.color = "cyan", color = "white", lwd = 1.2 ) +
  mapview(points_map_utm, legend = FALSE, col.regions = "black", lwd = 5)

# show Annual_URBAN_ET_2020 at area 1
mapview(Cooling_Maps, zcol="Annual_URBAN_ET_2020", alpha.regions = 0.5, col.regions = RColorBrewer::brewer.pal('RdYlBu', n = 9),
        legend = F, legend.opacity = 0, layer.name = "ET", map.types = "Esri.WorldImagery",  
        na.color = "cyan", color = "white", lwd = 1.2 )@map %>% 
  leaflet::setView(lng = points_map_lat@coords[1,1][[1]], lat = points_map_lat@coords[1,2][[1]], zoom = 14)

# create a 1 km buffer around the selected points
buffer_points <- geobuffer::geobuffer_pts(points_map_lat, dist_m = 1000, 
                                           output = "sp")
plot(buffer_points)

# map with Topmap in the background
mapview_TopMap <-
  mapview(Cooling_Maps, zcol="GCoS_2020",legend = FALSE, layer.name = "ETcos_2019", 
          col.regions = RColorBrewer::brewer.pal('RdYlBu', n = 7), 
          alpha.regions = 0.5, cex = 3, map.types = "Esri.WorldImagery",
          na.color = "white") +
  mapview(berlin.sf, legend = FALSE, color = "black",alpha = 1, alpha.regions = 0, lwd = 3) +
  mapview(points_map_utm, legend = FALSE, col.regions = "black", lwd = 3) +
  mapview(buffer_points, legend = FALSE, color = "black", alpha = 1, alpha.regions = 0, lwd = 1.4) 

# zoom the map for each selected point
mapview_TopMap@map %>% leaflet::setView(lng = points_map_lat@coords[1,1][[1]], 
                                        lat = points_map_lat@coords[1,2][[1]], zoom = 14) #-> mapview_zoom_EC_tower_TUCC
mapview_TopMap@map %>% leaflet::setView(lng = points_map_lat@coords[2,1][[1]], 
                                        lat = points_map_lat@coords[2,2][[1]], zoom = 14) #-> mapview_zoom_EC_tower_ROTH
mapview_TopMap@map %>% leaflet::setView(lng = points_map_lat@coords[3,1][[1]], 
                                        lat = points_map_lat@coords[3,2][[1]], zoom = 14) #-> mapview_zoom_Grass
mapview_TopMap@map %>% leaflet::setView(lng = points_map_lat@coords[4,1][[1]], 
                                        lat = points_map_lat@coords[4,2][[1]], zoom = 14) #-> mapview_zoom_Cropland
mapview_TopMap@map %>% leaflet::setView(lng = points_map_lat@coords[5,1][[1]], 
                                        lat = points_map_lat@coords[5,2][[1]], zoom = 14) #-> mapview_zoom_Forest
mapview_TopMap@map %>% leaflet::setView(lng = points_map_lat@coords[6,1][[1]], 
                                        lat = points_map_lat@coords[6,2][[1]], zoom = 14) #-> mapview_zoom_ForestII
mapview_TopMap@map %>% leaflet::setView(lng = points_map_lat@coords[7,1][[1]], 
                                        lat = points_map_lat@coords[7,2][[1]], zoom = 14.5) -> mapview_zoom_ForestIII

mapviewOptions(fgb = FALSE)

mapshot(x = mapview_zoom_ForestIII,
        remove_url = TRUE,
        vwidth = 1000, 
        vheight = 1000,
        file = paste0(getwd(),"/mapview_zoom_ForestIII.png"),
        remove_controls =c("zoomControl", "scaleBar", "layersControl", 
                           "homeButton", "drawToolbar", "easyButton"))

# find areas according some criteria
Cooling_Maps %>%
  filter(Annual_URBAN_ET_2020 >= 550 & Vegetation_Fraction >= 95) %>%
  mapview(zcol = "ECoS_2020", alpha.regions = 0.5, 
          col.regions = RColorBrewer::brewer.pal('RdYlBu', n = 7),
          legend = T, legend.opacity = 1, layer.name = "ET", 
          map.types = "Esri.WorldImagery", color = "white", lwd = 1.2)+
  mapview(berlin.sf, legend = FALSE, color = "black", alpha = 1, alpha.regions = 0, lwd = 1.4)

# find coordinates according some criteria
Cooling_Maps %>%
  filter(Annual_URBAN_ET_2020 >= 575 & 
         Vegetation_Fraction >= 98 & 
         Vegetation_Height >= 15) %>%
  st_centroid()

st_intersects(points_map_utm, Cooling_Maps)

# Annual ET over a LCLU Urban fabric (50%-80%)
LCLU_b %>%
  filter(classII == "Urban fabric (50%-80%)") %>% 
  st_intersects( ., Cooling_Maps)

mean(Cooling_Maps[c(23967, 31295, 31296, 12120, 12121, 12122, 29402,
                    1668, 1670, 1671, 25835, 26301),]$Annual_URBAN_ET_2020)

#  Annual ET over a a pine forest (Biotypo)
st_intersects(Pine, Cooling_Maps)
mean(Cooling_Maps[c(3654,3655, 3656, 3657, 3658, 3663, 3664, 3697, 3698, 3699, 
                    3701, 3704,3714,  3723, 3727, 3737, 3750, 3755, 3758,
                    27433),]$Annual_URBAN_ET_2020)


# Average model outputs per pixel (non-corrected)
Berlin2020_pred %>%
  filter(id_pixel %in% c(161, 441, 1217, 930, 1169, 882)) %>%
  select(where(is.numeric)) %>%
  #select(Htot, Actot, Tcave, Tsave, Gtot, id_pixel) %>%
  group_by(id_pixel) %>%
  summarytools::descr(stats = c("mean", "min", "max"), 
        transpose = T, headings = F, Data.frame=T)
# Tsave ºC ‘average’ soil temperature
# Tcave ºC ‘average’ canopy temperature
# Actot umol m-2 s-1 net photosynthesis of canopy

# Average of model inputs per month for some selected pixels
Inputs_Berlin %>%
  filter(id_pixel %in% c(882, 1169)) %>%
  # select(where(is.numeric)) %>%
  mutate(month = month(REddyProc::BerkeleyJulianDateToPOSIXct(t))) %>%
  select(LAI, Ta, RH, Rin, id_pixel, month) %>%
  group_by(id_pixel, month) %>%
  summarytools::descr(stats = c("mean", "min", "max"), 
        transpose = T, headings = F, Data.frame=T)

# Average final results per pixels
ET_vector2020 %>%
  filter(id_pixel %in% c(161, 1217, 930, 1169, 882)) %>%
  # select(where(is.numeric)) %>%
  select(ET_annual, Urban_ET_annual, ET_hottest, Urban_ET_hottest,
         Ts_hottest, Tc_hottest, Ts_hottest_idx, Rcos20, Ecos20,
         Gcos20, id_pixel) %>%
  group_by(id_pixel) %>%
  summarytools::descr(stats = c("mean", "min", "max"), 
      transpose = T, headings = F, Data.frame=T)

# Average final results per polygon
ET_vector2020 %>%
  filter(id_polygon %in% c(2919, 3564, 4678, 9217, 10589, 19212, 20559, 21192)) %>% 
  select(ET_annual, Urban_ET_annual, ET_hottest, Urban_ET_hottest,
         Ts_hottest, Tc_hottest, Ts_hottest_idx, Rcos20, Ecos20,
         Gcos20, id_polygon) %>%
  group_by(id_polygon) %>%
  summarytools::descr(stats = c("mean", "min", "max"), 
        transpose = T, headings = F, Data.frame=T)

# sumary per hour
as_tibble(data.frame("timestamp" = REddyProc::BerkeleyJulianDateToPOSIXct(t)[8761:17544], 
                     "Build-up_area" = FP_Berlin2020$ROTH_ETpixel_VFbyFP,
                     "TUCC" = FP_Berlin2020$TUCC_ETpixel_VFbyFP,
                     "Forest" = filter(Berlin2020_pred, id_pixel == 1278)$ET)) %>% 
  group_by(year=year(timestamp),
           month=month(timestamp, label = TRUE), 
           date=date(timestamp)) %>%
  mutate_if(is.numeric, pmax, 0) %>%
  summarise(ROTH_daily = sum(ROTH, na.rm = TRUE),
            TUCC_daily = sum(TUCC, na.rm = TRUE),
            Forest_daily = sum(Forest, na.rm = TRUE),
            .groups='drop') -> ET_dailymean_2020

summary(ET_dailymean_2020)


# plot by hour and month
ggplot(ET_dailymean_2020, aes(x = date)) +
  geom_line(aes(y = TUCC_daily, colour = "1.TUCC"), size = 1) +
  geom_line(aes(y = ROTH_daily, colour = "2.ROTH"), size = 1) +
  geom_line(aes(y = Forest_daily, colour= "3.Forest"), size = 1) +
  scale_y_continuous(limits=c(0,5), 
                     breaks=c(0,0.5,1,1.5,2,2.5,3,3.5,4,4.5,5),
                     expand = c(0.0, 0.0)) +
  # coord_x_date(xlim = c("2019-01-01", "2020-12-31"), expand = F, #
  #              ylim = c(-0.035, 0.035)) +
  #scale_x_date(name = NULL, labels = NULL, breaks = NULL) +
  scale_x_date(date_labels = "%b",date_breaks = "month", breaks = pretty_breaks(), expand = c(0, 0)) + # %y
  labs(x=" Month (2020)", y='ET [mm/day]') +
  scale_color_manual(name = "Site:", 
                     values = c("1.TUCC" = "red", 
                                "2.ROTH" = "black" ,               
                                "3.Forest" = "#008C00")) +
  guides(color = guide_legend(override.aes = list(size = 3, fill = "transparent"))) + 
  #ggtitle("d) TUCC site: corrected predicted and observed ET") +
  theme(legend.position = c(0.80, 0.95),
        legend.direction = "horizontal",
        legend.background = element_rect("transparent"),
        legend.key = element_rect("transparent"),
        legend.text =  element_text(color="black", size = 12),
        legend.title = element_text(colour="black", size = 12, face="bold"),
        axis.text.y = element_text(color="grey25", hjust=.9, size = 12),
        axis.title.y = element_text(color = "grey25", face="bold", size = 12, vjust = 1.2),
        axis.text.x = element_text(color="grey25", hjust = -1.0, size = 12),
        axis.title.x = element_text(color = "grey25", face = "bold", size = 12, vjust = 1.2),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        panel.spacing.y = unit(0, "lines"), 
        panel.spacing.x = unit(0, "lines"), 
        panel.background = element_rect(fill = "white", colour = "lightgrey", size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.25, linetype = 'dotted', colour = "lightgrey"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'dotted', colour = "white"),
        plot.title = element_text(size=9, vjust=-1,hjust=0))

summary(EC_DWD_ROTH$prec_mm[1:17544])
summary(EC_DWD_TUCC$prec_mm[1:17544])

as_tibble(data.frame("timestamp" = ymd_hms(EC_DWD_TUCC$timestamp)[1:17544],
                     "dry_TUCC" = rain_cond_vec_TUCC[1:17544],
                     "Precmm_TUCC" = EC_DWD_TUCC$prec_mm[1:17544],
                     "dry_ROTH" = rain_cond_vec[1:17544],
                     "Precmm_ROTH" = EC_DWD_ROTH$prec_mm[1:17544],
                     "TUCC_obs" = EC_DWD_TUCC$ET_clean[1:17544], 
                     "TUCC_modeled" = c(Berlin2019_TUCC$ET_cVF,Berlin2020_TUCC$ET_cVF),
                     "ROTH_obs" = EC_DWD_ROTH$ET_clean[1:17544], 
                     "ROTH_modeled" = c(Berlin2019_ROTH$ET_cVF,Berlin2020_ROTH$ET_cVF)
)) %>% 
  ggplot() +
  geom_smooth(formula = y~splines::bs(x, 24), aes(x = date(timestamp), y = (Precmm_TUCC-0.04838)/5, color="Prec_TUCC", fill="Prec_TUCC")) +
  geom_smooth(formula = y~splines::bs(x, 24), aes(x = date(timestamp), y = (Precmm_ROTH-0.05078)/5, color="Prec_ROTH", fill="Prec_ROTH")) +
  stat_smooth(formula = y~splines::bs(x, 24), aes(x = date(timestamp), y = ROTH_obs-ROTH_modeled, color = "Error_ROTH", fill = "Error_ROTH")) +
  stat_smooth(formula = y~splines::bs(x, 24), aes(x = date(timestamp), y = TUCC_obs-TUCC_modeled, color = "Error_TUCC", fill = "Error_TUCC")) +
  geom_hline(yintercept = 0, color = "red", linetype = "dotted") +
  scale_y_continuous(breaks=c(-0.03, -0.02, -0.01, 0, 0.01, 0.02, 0.03), 
                     sec.axis = sec_axis(~ . * 5 + 0.05078, 
                                         breaks=c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3), name = "Precipitation [mm/h]")) +
  coord_x_date(xlim = c("2019-01-01", "2020-12-31"), expand = F, #
               ylim = c(-0.035, 0.035)) +
  scale_color_manual(name=NULL, #labels=NULL, breaks=NULL,
                     values = c("Prec_TUCC"="darkgrey", 
                                "Prec_ROTH" = "grey",
                                "Error_ROTH" = "blue",
                                "Error_TUCC" = "red"),        
                     aesthetics = c("color", "fill")) +
  scale_x_date(date_labels = "%b",date_breaks = "month", breaks = pretty_breaks(), 
               expand = c(0, 0)) + # %y
  labs(x = "Month (2019/2020)", y = 'Model error [mm/h]') +
  theme(legend.position = c(0.7, 0.10),
        legend.direction = "horizontal",
        legend.background = element_rect("transparent"),
        legend.key = element_rect("transparent"),
        legend.text =  element_text(color = "black", size = 12),
        axis.text.y = element_text(color = "grey25", hjust = .9, size = 12),
        axis.title.y = element_text(color = "grey25", face = "bold", size = 13, vjust = 1.2),
        axis.text.x = element_text(color = "grey25", hjust = -0, size = 13),
        axis.title.x = element_text(color = "grey25", face = "bold", size = 13, vjust = 1.2),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        panel.spacing.y = unit(0.5, "lines"), 
        panel.spacing.x = unit(0.5, "lines"), 
        panel.background = element_rect(fill = "grey90"),  
        panel.border = element_blank(), 
        panel.grid.major = element_line(colour = "grey90"),
        panel.grid.minor = element_line(colour = "grey90"),
        plot.title = element_text(size = 14, vjust = -1, hjust = 0, face = "bold"))

