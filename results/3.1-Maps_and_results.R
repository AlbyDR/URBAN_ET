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

mapview_TopMap <-
  mapview(Cooling_Maps, zcol="GCoS_2020",legend = FALSE, layer.name = "ETcos_2019", 
          col.regions = RColorBrewer::brewer.pal('RdYlBu', n = 7), 
          alpha.regions = 0.5, cex = 3, map.types = "Esri.WorldImagery",
          na.color = "white") +
  mapview(berlin.sf, legend = FALSE, color = "black",alpha = 1, alpha.regions = 0, lwd = 3) +
  mapview(points_map_utm, legend = FALSE, col.regions = "black", lwd = 3) +
  mapview(buffer_points, legend = FALSE, color = "black", alpha = 1, alpha.regions = 0, lwd = 1.4) 

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
                                        lat = points_map_lat@coords[7,2][[1]], zoom = 14) #-> mapview_zoom_ForestIII

Cooling_Maps %>%
  filter(Annual_URBAN_ET_2020 >= 550 & Vegetation_Fraction >= 95) %>%
  mapview(zcol = "ECoS_2020", alpha.regions = 0.5, 
          col.regions = RColorBrewer::brewer.pal('RdYlBu', n = 7),
          legend = T, legend.opacity = 1, layer.name = "ET", 
          map.types = "Esri.WorldImagery", color = "white", lwd = 1.2)+
  mapview(berlin.sf, legend = FALSE, color = "black", alpha = 1, alpha.regions = 0, lwd = 1.4)

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

