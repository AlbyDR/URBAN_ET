############# quality FP ROTH
library(viridisLite)
library(stars)
library(sf)
library(magrittr)
library(raster)
##########################
plot(fp_polygon_ROTH[[2]][[2]][1])

plot(atlas_r_maps[[10]])
for (i in 5857:7296) {
  plot(fp_polygon_ROTH[[i]][[2]][1], add=T, col=rgb(1,0,0,0.05), 
       border="transparent")
}
points(xy.coords(coord$RothlonUTM,coord$RothlatUTM))

for (i in 5857:7296) {
  plot(fp_polygon_TUCC[[i]][[2]][1], add=T, col=rgb(1,0,0,0.05), 
       border="transparent")
}
points(xy.coords(coord$TUCClonUTM,coord$TUCClatUTM))

#save(fp_polygon_TUCC, file = "fp_polygon_TUCC.RData")
#load("fp_polygon_TUCC")

summary(fp_ROTH[1:8])

FP_indicators_RO <- data.frame("timestamp"=ROTH19plus1$timestamp)

FP_indicators_RO$Area <- sapply(1:length(fp_polygon_ROTH), FUN=function(i)
  as.double(st_area(fp_polygon_ROTH[[i]][[2]][1])))

plot(maps_ROTH[[10]])
plot(fp_polygon_ROTH[[1]][[2]][1], add=T)
points(xy.coords(coord$RothlonUTM,coord$RothlatUTM))
points(xy.coords(st_bbox(fp_polygon_ROTH[[1]][[2]][1])[1],
                 st_bbox(fp_polygon_ROTH[[1]][[2]][1])[4]),col="red") #xmin[1] ymax[4] = center
points(xy.coords(st_bbox(fp_polygon_ROTH[[1]][[2]][1])[3],
                 st_bbox(fp_polygon_ROTH[[1]][[2]][1])[2]),col="blue") #xmax[3] ymin[2] = extreme

st_bbox(fp_polygon_ROTH[[79]][[2]][1])

xy_NA_ROTH <- unlist(sapply(1:length(fp_polygon_ROTH),FUN=function(i) 
  ifelse(is.na(st_bbox(fp_polygon_ROTH[[i]][[2]][1])[[1]]),i,0)))

xy_nonNA_RO <- which(unlist(xy_NA_ROTH)==0)

#combination 
FP_indicators_RO <- NULL
for (i in xy_nonNA_RO) {
  FP_indicators_RO[[i]] <- spDistsN1(pts=matrix(c(
st_centroid(fp_polygon_ROTH[[i]][[2]][1])[[1]][1],
st_centroid(fp_polygon_ROTH[[i]][[2]][1])[[1]][1],
st_centroid(fp_polygon_ROTH[[i]][[2]][1])[[1]][1],
st_bbox(fp_polygon_ROTH[[i]][[2]][1])[[1]],
st_bbox(fp_polygon_ROTH[[i]][[2]][1])[[1]],
st_bbox(fp_polygon_ROTH[[i]][[2]][1])[[1]],
st_bbox(fp_polygon_ROTH[[i]][[2]][1])[[3]], 
st_bbox(fp_polygon_ROTH[[i]][[2]][1])[[3]], 
st_bbox(fp_polygon_ROTH[[i]][[2]][1])[[3]], 
st_centroid(fp_polygon_ROTH[[i]][[2]][1])[[1]][2],
st_bbox(fp_polygon_ROTH[[i]][[2]][1])[[2]],
st_bbox(fp_polygon_ROTH[[i]][[2]][1])[[4]],
st_centroid(fp_polygon_ROTH[[i]][[2]][1])[[1]][2],
st_bbox(fp_polygon_ROTH[[i]][[2]][1])[[2]],
st_bbox(fp_polygon_ROTH[[i]][[2]][1])[[4]],
st_centroid(fp_polygon_ROTH[[i]][[2]][1])[[1]][2],
st_bbox(fp_polygon_ROTH[[i]][[2]][1])[[2]],
st_bbox(fp_polygon_ROTH[[i]][[2]][1])[[4]]),ncol=2),
  pt=c(cbind(coord$RothlonUTM, coord$RothlatUTM)),longlat=F)
}

for (i in which(unlist(xy_NA_ROTH)!=0)) {
  FP_indicators_RO[[i]] <- NA
}


FP_indicators_RO[[1]]

fp_ROTH$distcenter <- unlist(sapply(1:length(fp_polygon_ROTH), FUN=function(i) 
                                        min(FP_indicators_RO[[i]])))

fp_ROTH$distcentroid <- unlist(sapply(1:length(fp_polygon_ROTH), FUN=function(i) 
                                          (FP_indicators_RO[[i]][1])))

fp_ROTH$longdist <- unlist(sapply(1:length(fp_polygon_ROTH),FUN=function(i) 
                                        max(FP_indicators_RO[[i]])))
 
summary(fp_ROTH)
which(fp_ROTH$distcenter>=600)

plot(maps_ROTH$Impervious, axes=F, box=F)
for (i in which(fp_ROTH$distcenter>=150)) {
  plot(fp_polygon_ROTH[[i]][[2]][1], add=T, col=rgb(1,0,0,0.5), 
       border="transparent")
}

plot(maps_ROTH$Impervious, axes=F, box=F)
for (i in which(fp_ROTH$longdist>=1450)) {
  plot(fp_polygon_ROTH[[i]][[2]][1], add=T, col=rgb(1,0,0,0.1), 
       border="transparent")
}

plot(maps_ROTH$Impervious, axes=F, box=F)
for (i in which(fp_ROTH$distcentroid>=1000)) {
  plot(fp_polygon_ROTH[[i]][[2]][1], add=T, col=rgb(1,0,0,0.5), 
       border="transparent")
}

plot(maps_ROTH$Impervious, axes=F, box=F)
for (i in which(fp_ROTH$maxArea<=20000)) {
  plot(fp_polygon_ROTH[[i]][[2]][1], add=T, col=rgb(1,0,0,0.1), 
       border="transparent")
}

plot(maps_ROTH$Impervious, axes=F, box=F)
for (i in which(fp_ROTH$maxArea>=300000)) {
  plot(fp_polygon_ROTH[[i]][[2]][1], add=T, col=rgb(1,0,0,0.1), 
       border="transparent")
}

########################################################################
library(suncalc)
Night <- getSunlightTimes(date = date(fp_ROTH$timestamp), 
                          keep = c("nightEnd", "night"), 
                          lat = 52, lon = 13, tz = "UTC")

fp_ROTH$day_night <- if_else(fp_ROTH$timestamp>Night$night | fp_ROTH$timestamp<Night$nightEnd,
                             "night", "day")

plot(maps_ROTH$Veg_cover, axes=F, box=F)
for (i in which(fp_ROTH$day_night=="day" & month(fp_ROTH$timestamp)==9)) {
  plot(fp_polygon_ROTH[[i]][[2]][1], add=T, col=rgb(1,0,0,0.1), 
       border="transparent")
}

points(xy.coords(coord$RothlonUTM,coord$RothlatUTM))

plot(maps_ROTH$Veg_cover, axes=F, box=F)
for (i in which(fp_ROTH$day_night=="day" & month(fp_ROTH$timestamp)==4)) {
  plot(fp_polygon_ROTH[[i]][[2]][1], add=T, col=rgb(1,0,0,0.1), 
       border="transparent")
}
points(xy.coords(coord$RothlonUTM,coord$RothlatUTM))

plot(maps_ROTH$Veg_cover, axes=F, box=F)
for (i in which(fp_ROTH$day_night=="day" & month(fp_ROTH$timestamp)==12)) {
  plot(fp_polygon_ROTH[[i]][[2]][1], add=T, col=rgb(0,0,1,0.1), 
       border="transparent")
}
points(xy.coords(coord$RothlonUTM,coord$RothlatUTM))

plot(maps_ROTH$Veg_cover, axes=F, box=F)
for (i in which(fp_ROTH$day_night=="day" & month(fp_ROTH$timestamp)==12)) {
  plot(fp_polygon_ROTH[[i]][[2]][1], add=T, col=rgb(0,0,1,0.1), 
       border="transparent")
}
points(xy.coords(coord$RothlonUTM,coord$RothlatUTM))

plot(maps_ROTH$Veg_cover, axes=F, box=F)
for (i in which(fp_ROTH$day_night=="night" & month(fp_ROTH$timestamp)==12)) {
  plot(fp_polygon_ROTH[[i]][[2]][1], add=T, col=rgb(0,0,1,0.1), 
       border="transparent")
}
points(xy.coords(coord$RothlonUTM,coord$RothlatUTM))

plot(maps_ROTH$Impervious, axes=F, box=F)
for (i in 1537:19056) {
plot(fp_polygon_ROTH[[i]][[2]][1], add=T, col=rgb(1,0,0,0.01), 
     border="transparent")
}
points(xy.coords(coord$RothlonUTM,coord$RothlatUTM))

plot(fp_polygon_ROTH[[369]][[2]][1], add=T, col=, border="transparent")

# precipitation
fp_ROTH$hours_no_precip <- ROTH19plus1$prec.window
summary(fp_ROTH$hours_no_precip)

plot(maps_ROTH$Impervious, axes=F, box=F)
for (i in which(fp_ROTH$hours_no_precip==0)) {
  plot(fp_polygon_ROTH[[i]][[2]][1], add=T, col=rgb(1,0,0,0.1), 
       border="transparent")
}

plot(maps_ROTH$Impervious, axes=F, box=F)
for (i in which(fp_ROTH$hours_no_precip<=4)) {
  plot(fp_polygon_ROTH[[i]][[2]][1], add=T, col=rgb(1,0,0,0.1), 
       border="transparent")
}

plot(maps_ROTH$Impervious, axes=F, box=F)
for (i in which(fp_ROTH$hours_no_precip>=48)) {
  plot(fp_polygon_ROTH[[i]][[2]][1], add=T, col=rgb(1,0,0,0.1), 
       border="transparent")
}

fp_ROTH <- read.csv("fp_ROTH.csv")

