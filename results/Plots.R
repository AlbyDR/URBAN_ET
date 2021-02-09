#######################################################################
library(bigleaf)
library(xts)
library(scales)
library(tidyquant)
library(cowplot)
library(dplyr)
library(lubridate)
library(ggplot2)
library(rasterVis)
library(ggspatial)
library(sp)
library(sf)
library(raster)
library(RColorBrewer)
library(geobuffer) # geobuffer_pts
library(gridExtra)
#######################################################################
## Figures plot and map codes
#######################################################################

####################################################################
# Figure #1 maps
####################################################################

####################################################################
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
# create a buffer around the tower
pts_buf_1500m <- geobuffer_pts(xy=data.frame(lon=c(coord$Rothlon,coord$TUCClon),
                                             lat=c(coord$Rothlat,coord$TUCClat)), 
                               dist_m=1500, output = "sp")

summary(pts_buf_1500m)
buf_1500m <- spTransform(pts_buf_1500m, CRS("+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs"))

plot(atlas_r_maps$Veg_cover,axes=F,box=F) #,horizontal=TRUE,legend.args=list(text='impervious (%)')
plot(buf_1500m, add=T, lwd=.5, fg=2)  
###################################################################
## Tower and DWD points 
points <- data.frame(
  x = c(385566.5, 384597.4, 386525.1,385368.3),
  y = c(5813229, 5812858, 5819332,5825159),
  city = c("ROTH", "DWD_Dahlem", "TUCC", "DWD_Tegel"))
###################################################################
# green pallets 
myPal <- list(RColorBrewer::brewer.pal('Greens', n=9),
              c("#ffffe5",'#f7fcb9','#d9f0a3',"#addd8e",'#78c679',
                '#41ab5d','#238443',"#006837","#004529"),
              c("#f7fcfd",'#e5f5f9','#ccece6',"#99d8c9",'#66c2a4',
                '#41ae76','#238b45',"#006d2c","#00441b"),
              c("#f7fcf5",'#e5f5e0','#c7e9c0',"#a1d99b",'#74c476',
                '#41ab5d','#238b45',"#006d2c","#00441b"),
              c("#f7fcf0","#ccebc5", "#8BD000","#56C200", "#28B400",
                "#00A600","#006837", "#006d2c", "#00441b"))
###################################################################
# Vegetation Height Berlin
ggplot() +
  layer_spatial(Atlas_r_GreenVolume$Veg_height) + 
  layer_spatial(berlin.sf, fill="transparent", size=1.2) + 
  layer_spatial(buf_1500m, fill="transparent", col="red", size=1.2, linetype="dashed") + 
  geom_spatial_point(aes(x=points$x, y=points$y), 
                     crs =crs(Atlas_r_GreenVolume), size=2, 
                     col=c("red","black","red","black")) +
  geom_spatial_label(aes(x=points$x, y=points$y,label=points$city),
                     crs = crs(Atlas_r_GreenVolume),
                     fill="transparent", col=c("red","black","red","black"), 
                     vjust=c(-0.75,0.5,-0.75,-0.25),
                     hjust=c(0.5,1.05,0.5,0.5), label.size= 0, size=6) +
  guides(linetype = guide_legend(title=NULL, order = 2), 
         color = guide_legend(order=1)) +
  scale_fill_gradientn(breaks=seq(0,30,5),limits=c(0,30),
                       colors=myPal[[1]], name="",na.value = NA,
                       guide =guide_colorbar(direction = "horizontal",
                                             label.position = "bottom",
                                             title.vjust=9, label.vjust=9,
                                             frame.colour = "black",
                                             frame.linewidth = 0.5,
                                             frame.linetype = 1,
                                             title.position = "left",
                                             barwidth=30,barheight=1.2,nbin=30,
                                             label.theme=element_text(angle=0,size=20))) +
  annotation_scale(location = "bl", height = unit(0.4, "cm"),
                   pad_x=unit(1.75,"cm"), pad_y=unit(2.5,"cm"),
                   text_pad = unit(0.25, "cm"),
                   text_cex = 1.5) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         height = unit(2, "cm"),width = unit(2, "cm"),
                         pad_x=unit(3.5,"cm"), pad_y=unit(2.5,"cm")) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="bottom",
        legend.spacing.y = unit(-0.5, "lines"),
        legend.box.spacing = unit(-1.5, "lines"),
        panel.grid.major = element_blank(), 
        panel.background = element_rect("white"))
###################################################################
# vegetation cover maps
FP_ROTH_polygon <- as_Spatial(st_geometry(fp_polygon_ROTHeg[[2]]))
crs(FP_ROTH_polygon) <- crs(Atlas_r_GreenVolume)

FP_TUCC_polygon <- as_Spatial(st_geometry(fp_polygon_TUCCeg[[1]]))
crs(FP_TUCC_polygon) <- crs(Atlas_r_GreenVolume)

ggplot() +
  layer_spatial(atlas_r_maps$Veg_cover) + 
  layer_spatial(buf_1500m, fill="transparent", col="red", linetype="dashed", size=1.2) + 
  layer_spatial(FP_ROTH_polygon, fill="transparent", col="red", size=0.8)+
  layer_spatial(FP_TUCC_polygon, fill="transparent", col="red", size=0.8)+
  geom_spatial_point(aes(x=points[c(1,3),1], y=points[c(1,3),2]), 
                     crs =crs(Atlas_r_GreenVolume), size=2, 
                     col=c("red","red")) +
  geom_spatial_label(aes(x=points[c(1,3),1], y=points[c(1,3),2],
                         label=points[c(1,3),3]),
                     crs = crs(Atlas_r_GreenVolume),
                     fill="transparent", col=c("red","red"), 
                     vjust=c(-0.5,1.2),
                     hjust=c(0.3,0.8), label.size= 0, size=6) +
  guides(linetype = guide_legend(title=NULL, order = 2), 
         color = guide_legend(order=1)) +
  scale_fill_gradientn(breaks=seq(0,100,10),limits=c(0,100),
                       colors=myPal[[1]], name="", na.value = 0,
                       guide =guide_colorbar(direction = "horizontal",
                                             label.position = "bottom",
                                             label.vjust=-2,
                                             frame.colour = "black",
                                             frame.linewidth = 0.5,
                                             frame.linetype = 1,
                                             #title.position = "left",
                                             barwidth=21,barheight=1.2,nbin=10,
                                             label.theme=element_text(angle=0,size=18))) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="bottom",
        legend.spacing.y = unit(-0.4, "lines"),
        legend.box.spacing = unit(-1.5, "lines"),
        panel.grid.major = element_blank(), 
        panel.background = element_rect("white"))

###################################################################
# impervious map
ggplot() +
  layer_spatial(atlas_r_maps$Impervious_With_str) + 
  layer_spatial(buf_1500m, fill="transparent", col="red", linetype="dashed", size=1.2) + 
  layer_spatial(FP_ROTH_polygon, fill="transparent", col="red", size=0.8)+
  layer_spatial(FP_TUCC_polygon, fill="transparent", col="red", size=0.8)+
  geom_spatial_point(aes(x=points[c(1,3),1], y=points[c(1,3),2]), 
                     crs =crs(Atlas_r_GreenVolume), size=2, 
                     col=c("red","red")) +
  geom_spatial_label(aes(x=points[c(1,3),1], y=points[c(1,3),2],
                         label=points[c(1,3),3]),
                     crs = crs(Atlas_r_GreenVolume),
                     fill="transparent", col=c("red","red"), 
                     vjust=c(-0.5,1.2),
                     hjust=c(0.3,0.8), label.size= 0, size=6) +
  guides(linetype = guide_legend(title=NULL, order = 2), 
         color = guide_legend(order=1)) +
  scale_fill_gradientn(breaks=seq(0,100,10),limits=c(0,100),
                       colors=terrain.colors(10), name="", na.value = 0,
                       guide =guide_colorbar(direction = "horizontal",
                                             label.position = "bottom",
                                             label.vjust=-2,
                                             frame.colour = "black",
                                             frame.linewidth = 0.5,
                                             frame.linetype = 1,
                                             #title.position = "left",
                                             barwidth=21,barheight=1.2,nbin=10,
                                             label.theme=element_text(angle=0,size=18))) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="bottom",
        legend.spacing.y = unit(-0.4, "lines"),
        legend.box.spacing = unit(-1.5, "lines"),
        panel.grid.major = element_blank(), 
        panel.background = element_rect("white"))
###################################################################

#######################################################################
### variables for the remain plots
#######################################################################
plotVARs <- data.frame(timestamp=EC_ROTH19$timestamp)
plotVARs$ET.ROTH <- LE.to.ET(EC_ROTH19$LE,EC_ROTH19$Ta)*3600
plotVARs$ETc.ROTH <- LE.to.ET(EC_ROTH19$LE.dry.fsd,EC_ROTH19$Ta)*3600
plotVARs$ET.TUCC <- LE.to.ET(EC_TUCC19$LE,EC_TUCC19$Ta)*3600
plotVARs$ETc.TUCC <- LE.to.ET(EC_TUCC19$LE.dry.fsd,EC_TUCC19$Ta)*3600
plotVARs$ET.ROTH0 <- plotVARs$ET.ROTH 
plotVARs$ET.ROTH0[which(plotVARs$ET.ROTH<0)] <- 0
plotVARs$ETc.ROTH0 <- plotVARs$ETc.ROTH 
plotVARs$ETc.ROTH0[which(plotVARs$ETc.ROTH<0)] <- 0
plotVARs$ET.TUCC0 <- plotVARs$ET.TUCC 
plotVARs$ET.TUCC0[which(plotVARs$ET.TUCC<0)] <- 0
plotVARs$ETc.TUCC0 <- plotVARs$ETc.TUCC 
plotVARs$ETc.TUCC0[which(plotVARs$ETc.TUCC<0)] <- 0
plotVARs$ET.ROTH.filled <- na.approx(LE.to.ET(EC_ROTH19$LE.filled,EC_ROTH19$Ta)*3600)
plotVARs$ET.ROTH.filled0 <- plotVARs$ET.ROTH.filled
plotVARs$ET.ROTH.filled0[which(plotVARs$ET.ROTH.filled<0)] <- 0
plotVARs$ET.TUCC.filled <- na.approx(LE.to.ET(EC_TUCC19$LE.filled,EC_TUCC19$Ta)*3600)
plotVARs$ET.TUCC.filled0 <- plotVARs$ET.TUCC.filled
plotVARs$ET.TUCC.filled0[which(plotVARs$ET.TUCC.filled<0)] <- 0
plotVARs$PrecYN.ROTH <- na.approx(DWD_ROTH19$precip.no.yes)
plotVARs$PrecYN.TUCC <- na.approx(DWD_TUCC19$precip.no.yes)
plotVARs$Precmm.ROTH <- na.approx(DWD_ROTH19$precipitation.dwd)
plotVARs$Precmm.TUCC <- na.approx(DWD_TUCC19$precipitation.dwd)
plotVARs$Prec.W.ROTH <- na.approx(EC_ROTH19$prec.window)
plotVARs$Prec.W.TUCC <- na.approx(EC_TUCC19$prec.window)
plotVARs$Rin.ROTH <- na.approx(DWD_ROTH19$Rin)
plotVARs$Rin.TUCC <- na.approx(DWD_TUCC19$Rin)
plotVARs$Ta.ROTH <- na.approx(DWD_ROTH19$air_temperature.dwd)
plotVARs$Ta.TUCC <- na.approx(DWD_TUCC19$air_temperature.dwd)
plotVARs$LAI.ROTHGp <- LAI_ROTH19$LAI_Green_point
plotVARs$LAI.TUCCGp <- LAI_TUCC19$LAI_Green_point
plotVARs$LAI.ROTHp <- LAI_ROTH19$LAI_point
plotVARs$LAI.TUCCp <- LAI_TUCC19$LAI_point
plotVARs$LAI.ROTH_B <- LAI_ROTH19$LAI_Buffer
plotVARs$LAI.TUCC_B <- LAI_TUCC19$LAI_Buffer
plotVARs$LAI.ROTH_FP <- LAI_ROTH19$LAI_FP
plotVARs$LAI.TUCC_FP <- LAI_TUCC19$LAI_FP
plotVARs$ETo.DWD.ROTH <- ROTH_Results$ETo
plotVARs$ETo.EC.ROTH <- ROTH_Results$ETo.EC
plotVARs$ETo.DWD.TUCC <- TUCC_Results$ETo
plotVARs$ETo.EC.TUCC <- TUCC_Results$ETo.EC
plotVARs$ETo.DWD.ROTH.adj <- ROTH_Results$ETo_adjB
plotVARs$ETo.EC.ROTH.adj <- ROTH_Results$ETo.EC_adjB
plotVARs$ETo.DWD.TUCC.adj <- TUCC_Results$ETo_adjB
plotVARs$ETo.EC.TUCC.adj <- TUCC_Results$ETo.EC_adjB
plotVARs$SCOPE.ETo.ROTH <- SCOPE_ET_ROTH[[74]]$SCOPE_ET
plotVARs$SCOPE.ETo.ROTH.adjB <- SCOPE_ET_ROTH[[74]]$SCOPE_ET_adjB
plotVARs$SCOPE.DWD.ROTH <- SCOPE_ET_ROTH[[73]]$SCOPE_ET
plotVARs$SCOPE.DWD.ROTH.adjB <- SCOPE_ET_ROTH[[73]]$SCOPE_ET_adjB
plotVARs$SCOPE.RS.ROTH <- SCOPE_ET_ROTH[[70]]$SCOPE_ET
plotVARs$SCOPE.RS.ROTH.adjB <- SCOPE_ET_ROTH[[70]]$SCOPE_ET_adjB
plotVARs$SCOPE.ETo.TUCC <- SCOPE_ET_TUCC[[14]]$SCOPE_ET
plotVARs$SCOPE.ETo.TUCC.adjB <- SCOPE_ET_TUCC[[14]]$SCOPE_ET_adjB
plotVARs$SCOPE.DWD.TUCC <- SCOPE_ET_TUCC[[12]]$SCOPE_ET
plotVARs$SCOPE.DWD.TUCC.adjB <- SCOPE_ET_TUCC[[12]]$SCOPE_ET_adjB
plotVARs$SCOPE.RS.TUCC <- SCOPE_ET_TUCC[[13]]$SCOPE_ET
plotVARs$SCOPE.RS.TUCC.adjB <- SCOPE_ET_TUCC[[13]]$SCOPE_ET_adjB
plotVARs$veg.cover.ROTH <- na.approx(FP_ROTH$Veg_cover)
plotVARs$impervious.ROTH <- na.approx(FP_ROTH$Impervious_With_str)
plotVARs$veg.cover.TUCC <- na.approx(FP_TUCC$Veg_cover)
plotVARs$impervious.TUCC <- na.approx(FP_TUCC$Impervious_With_str)

summary(plotVARs$ETo.EC.ROTH)
summary(plotVARs)

##########################################################################
# Figure #2 - 5 plots combined
##########################################################################
#### plot ET # figure 2e
plotET <-  ggplot(plotVARs, aes(x=date(timestamp))) +
  geom_point(aes(y=ET.ROTH), colour="darkgreen", size = 0.5)+
  geom_point(aes(y=ET.TUCC), colour="darkblue", size = 0.5)+
  #geom_smooth(span = 1,aes(y=ET.ROTH),colour="lightgreen",
  #            fill = "darkgreen", formula = y ~ splines::bs(x, 30)) +
  #geom_smooth(span = 1,aes(y=ETc.ROTH),colour="green"
  #              ,fill = "green", formula = y ~ splines::bs(x, 30)) +
  #geom_smooth(span = 1,aes(y=ET.TUCC),colour="lightblue",
  #              fill = "darkblue", formula = y ~ splines::bs(x, 30)) +
  #geom_smooth(span = 1,aes(y=ETc.TUCC),colour="blue",
  #              fill = "blue", formula = y ~ splines::bs(x, 30)) +
  coord_x_date(xlim = c("2019-01-01","2019-12-31"), expand = F, 
               ylim = c(-0.05, 0.50))+ #, #"2018-07-01", "2020-03-31"#
  scale_x_date(date_labels = "%b",date_breaks="month", breaks=pretty_breaks(),
               expand = c(0.0, 0.0)) + # "%b %y"
  labs(x="month (2019)", y='ET (mm/h)') +
  theme(axis.text.x = element_text(colour="grey25", size=9, angle=0),
    axis.title.x = element_text(colour="grey25", vjust=5, face="bold",size=10, margin=margin(20,0,0,0)),
    axis.text.y = element_text(colour="grey25", size=8),
    axis.title.y = element_text(colour="grey25", face="bold",vjust=-4.5,size=9, margin=margin(0,20,0,0)),
    panel.grid.major = element_line(colour = "white"))

# summarise Ta, prec and Rin
as_tibble(plotVARs) %>% 
  group_by(year=year(plotVARs$timestamp),
           DOY=yday(plotVARs$timestamp)) %>%
  summarise(Ta.ROTH.av=mean(Ta.ROTH, na.rm=T),
            Ta.ROTH.max=max(Ta.ROTH, na.rm=T),
            Ta.ROTH.min=min(Ta.ROTH, na.rm=T),
            Ta.TUCC.av=mean(Ta.TUCC, na.rm=T),
            Ta.TUCC.max=max(Ta.TUCC, na.rm=T),
            Ta.TUCC.min=min(Ta.TUCC, na.rm=T),
            mm.ROTH.sum=sum(Precmm.ROTH, na.rm=T),
            mm.ROTH.av=mean(Precmm.ROTH, na.rm=T),
            mm.ROTH.max=max(Precmm.ROTH, na.rm=T),
            mm.TUCC.sum=sum(Precmm.TUCC, na.rm=T),
            mm.TUCC.av=mean(Precmm.TUCC, na.rm=T),
            mm.TUCC.max=max(Precmm.ROTH, na.rm=T),
            Rin.av=mean(Rin.TUCC, na.rm=T),
            Rin.max=max(Rin.TUCC, na.rm=T),
            Rin.min=min(Rin.TUCC, na.rm=T),
            PrecYN.ROTH.sum=sum(PrecYN.ROTH, na.rm=T),
            PrecYN.TUCC.sum=sum(PrecYN.TUCC, na.rm=T),
            .groups='drop') -> plotSumm

# Plot Ta average, max and min # figure 2a
plotTa <-ggplot(filter(plotSumm,year==2019), aes(x=DOY)) +
  geom_line(aes(y=Ta.ROTH.av), colour = "darkgreen")+
  geom_line(aes(y=Ta.ROTH.min), colour = "darkgreen", linetype="dotted")+
  geom_line(aes(y=Ta.ROTH.max), colour = "darkgreen", linetype="dotted")+
  geom_line(aes(y=Ta.TUCC.av), colour = "darkblue")+
  geom_line(aes(y=Ta.TUCC.min), colour = "darkblue", linetype="dotted")+
  geom_line(aes(y=Ta.TUCC.max), colour = "darkblue", linetype="dotted")+
  scale_x_discrete(name=NULL,limits=NULL, breaks = NULL) +
  labs(y = 'Ta (ºC)') +
  theme(axis.text.y = element_text(colour="grey25", size=8),
    axis.title.y = element_text(colour="grey30", vjust=-4.5, face="bold",size=9, margin=margin(0,20,0,0)),
    panel.grid.major = element_line(colour = "white"))

# Plot RIN average, max.  min=0 - # figure 2b
plotRin <-ggplot(filter(plotSumm,year==2019), aes(x=DOY)) +
  geom_line(aes(y=Rin.av), colour = "black")+
  #geom_line(aes(y=Rin.min), colour = "black", linetype="dotted")+
  geom_line(aes(y=Rin.max), colour = "black", linetype="dotted")+
  scale_x_discrete(name=NULL,limits=NULL, breaks = NULL) +
  labs(y = 'Rin (W m-2)') +
  theme(axis.text.y = element_text(colour="grey25", size=8),
        axis.title.y = element_text(colour="grey30", vjust=-4.5, face="bold",size=9, margin=margin(0,20,0,0)),
        panel.grid.major = element_line(colour = "white"))

# Plot prec mm # figure 2d
plotprecmm <-ggplot(filter(plotSumm,year==2019), aes(x=DOY)) +
  geom_bar(stat="identity", color="darkblue",aes(y=mm.ROTH.sum)) +
  geom_bar(stat="identity", color="darkgreen",alpha=0.05,aes(y=mm.TUCC.sum)) +
  scale_y_sqrt(limits=c(0,50), breaks = c(0,1,5,10,20,35,50))+ 
  scale_x_discrete(name=NULL,limits=NULL, breaks = NULL) +
  labs(y = 'Precipitation (mm)') +
  theme(axis.text.y = element_text(colour="grey25", size=8),
        axis.title.y = element_text(colour="grey30", vjust=-4.5, face="bold",size=9, margin=margin(0,20,0,0)),
        panel.grid.major = element_line(colour = "white"))

#Plot LAI # figure 2c
plotLAI <- ggplot(filter(plotVARs,year(timestamp)==2019), 
                  aes(x=yday(timestamp))) +
  geom_line(aes(y=LAI.ROTH_B), colour = "darkgreen")+
  geom_line(aes(y=LAI.TUCC_B), colour = "darkblue")+
  scale_x_discrete(name=NULL,limits=NULL, breaks = NULL) +
  labs(y = 'LAI') +
  theme(axis.text.y = element_text(colour="grey25", size=8),
    axis.title.y = element_text(colour="grey30", vjust=-4.5, face="bold",size=10, margin=margin(0,20,0,0)),
    panel.grid.major = element_line(colour = "white"))

# combina in a grid
grid_fig1 <- plot_grid(plotTa,plotRin,plotLAI,plotprecmm,plotET, align="v",ncol=1,
                       rel_heights=c(0.15,0.15,0.15,0.15,0.4), 
                       hjust=c(-6.5,-6.5,-7,-6.5,-6.5), vjust=1.6,
               labels = c("a)","b)","c)","d)","e)"), #labels="auto",
               label_size = 10)

# another Combo Plot method
#egg::ggarrange(plotTa,plotRin,plotLAI,plotprecmm,plotET,
#               heights = c(0.15,0.15,0.15,0.15,0.4))

##########################################################################
# Figure #3 - 6 scatterplots combined
##########################################################################
#Plot ETos 
cor(plotVARs$ETo.DWD.ROTH,plotVARs$ETo.EC.ROTH)
cor(plotVARs$ETo.DWD.TUCC,plotVARs$ETo.EC.TUCC)

# EC vs DWD 
scatt1 <- ggplot(filter(plotVARs, year(timestamp)==2019)) + 
  geom_point(aes(x=ETo.EC.ROTH,y=ETo.DWD.ROTH), color="green", alpha = 0.1) +  
  geom_point(aes(x=ETo.EC.TUCC,y=ETo.DWD.TUCC), color="blue", alpha = 0.1) +  
  geom_abline(intercept=0, slope=1, color="red", linetype="dotted") +
  scale_y_continuous(limits=c(0,0.8)) +
  scale_x_continuous(limits=c(0,0.8)) + 
#  ggtitle("a) ETo (DWD data) vs ETo (EC data)") +
  labs(x='ETo (EC data) [mm/h]',  y='ETo (DWD data) [mm/h]')+
  theme(axis.title.x= element_text(size=10),
    axis.title.y= element_text(size=10),
    axis.text.x = element_text(hjust=.9, angle=0, size=10),
    axis.text.y = element_text(hjust=.9, size=10),
    strip.text.x = element_text(size=8, face="bold"),
    strip.text.y = element_text(size=8, face="bold"),
    plot.title = element_text(size=9, vjust=-1,hjust=0.08, face="bold"))

# ETo vs observed
scatt2 <- ggplot(filter(plotVARs, year(timestamp)==2019)) + 
  geom_point(aes(x=ETc.ROTH0,y=ETo.DWD.ROTH), color= "green", alpha = 0.1) + 
  geom_point(aes(x=ETc.TUCC0,y=ETo.DWD.TUCC), color= "blue", alpha = 0.1) +  
  geom_abline(intercept=0, slope=1, color="red", linetype="dotted") +
  scale_y_continuous(limits=c(0,0.8)) +
  scale_x_continuous(limits=c(0,0.8)) + 
  #ggtitle("b) ETo (uncorrected) vs Observed ET") +
  labs(x = 'observed ET [mm/h]',  y = 'ETo (DWD) [mm/h]')+
  theme(axis.title.x= element_text(size = 10),
        axis.title.y= element_text(size = 10),
        axis.text.x = element_text(hjust = .9, angle = 0, size = 10),
        axis.text.y = element_text(hjust = .9, size = 10),
        strip.text.x = element_text(size=8, face="bold"),
        strip.text.y = element_text(size=8, face="bold"),
        plot.title = element_text(size=9, vjust=-1,hjust=0.08, face="bold"))

### plot ETo adj
scatt3 <- ggplot(filter(plotVARs, year(timestamp)==2019)) + 
  geom_point(aes(x=ETc.ROTH0,y=ETo.DWD.ROTH.adj), color= "green", alpha = 0.1) + 
  geom_point(aes(x=ETc.TUCC0,y=ETo.DWD.TUCC.adj), color= "blue", alpha = 0.1) + 
  geom_abline(intercept=0, slope=1, color="red", linetype="dotted") +
  scale_y_continuous(limits=c(0,0.4)) +
  scale_x_continuous(limits=c(0,0.4)) + 
  #ggtitle("c) ETo (corrected) vs Observed ET") +
  labs(x = 'observed ET [mm/h]',  y = 'corrected ETo (DWD) [mm/h]')+
  theme(axis.title.x= element_text(size = 10),
        axis.title.y= element_text(size = 10),
        axis.text.x = element_text(hjust = .9, angle = 0, size = 10),
        axis.text.y = element_text(hjust = .9, size = 10),
        strip.text.x = element_text(size=8, face="bold"),
        strip.text.y = element_text(size=8, face="bold"),
        plot.title = element_text(size=9, vjust=-1,hjust=0.08, face="bold"))
# plot SCOPE.ETo
scatt4 <- ggplot(filter(plotVARs, year(timestamp)==2019)) + 
  geom_point(aes(x=ETc.ROTH0,y=SCOPE.ETo.ROTH.adj), color= "green", alpha = 0.1) + 
  geom_point(aes(x=ETc.TUCC0,y=SCOPE.ETo.TUCC.adj), color= "blue", alpha = 0.1) +   
  geom_abline(intercept=0, slope=1, color="red", linetype="dotted") +
  scale_y_continuous(limits=c(0,0.4)) +
  scale_x_continuous(limits=c(0,0.4)) + 
  #ggtitle("d) SCOPE_ETo (corrected): predicted vs observed") +
  labs(x='Observed ET [mm/h]', y='predicted ET (SCOPE_ETo corrected) [mm/h]')+
  theme(axis.title.x= element_text(size=10),
        axis.title.y= element_text(size=10),
        axis.text.x = element_text(hjust=.9, angle=0, size=10),
        axis.text.y = element_text(hjust=.9, size=10),
        strip.text.x = element_text(size=8, face="bold"),
        strip.text.y = element_text(size=8, face="bold"),
        plot.title = element_text(size=9, vjust=-1,hjust=0.08, face="bold"))
# plot SCOPE.DWD
scatt5 <- ggplot(filter(plotVARs, year(timestamp)==2019)) + 
  geom_point(aes(x=ETc.ROTH0,y=SCOPE.DWD.ROTH.adj), color= "green", alpha = 0.1) + 
  geom_point(aes(x=ETc.TUCC0,y=SCOPE.DWD.TUCC.adj), color= "blue", alpha = 0.1) +   
  geom_abline(intercept=0, slope=1, color="red", linetype="dotted") +
  scale_y_continuous(limits=c(0,0.4)) +
  scale_x_continuous(limits=c(0,0.4)) + 
  #ggtitle("e) SCOPE_DWD (corrected): predicted vs observed") +
  labs(x='Observed ET [mm/h]', y='predicted ET (SCOPE_DWD corrected) [mm/h]')+
  theme(axis.title.x= element_text(size = 10),
        axis.title.y= element_text(size = 10),
        axis.text.x = element_text(hjust = .9, angle = 0, size = 10),
        axis.text.y = element_text(hjust = .9, size = 10),
        strip.text.x = element_text(size=8, face="bold"),
        strip.text.y = element_text(size=8, face="bold"),
        plot.title = element_text(size=9, vjust=-1,hjust=0.08, face="bold"))
### plot SCOPE RS
scatt6 <- ggplot(filter(plotVARs, year(timestamp)==2019)) + 
  geom_point(aes(x=ETc.ROTH0,y=SCOPE.RS.ROTH.adj), color= "green", alpha = 0.1) + 
  geom_point(aes(x=ETc.TUCC0,y=SCOPE.RS.TUCC.adj), color= "blue", alpha = 0.1) + 
  geom_abline(intercept=0, slope=1, color="red", linetype="dotted") +
  scale_y_continuous(limits=c(0,0.4)) +
  scale_x_continuous(limits=c(0,0.4)) + 
  #ggtitle("f) SCOPE_RS (corrected): predicted vs observed") +
  labs(x='Observed ET [mm/h]', y='predicted ET (SCOPE_RS corrected) [mm/h]')+
  theme(axis.title.x= element_text(size = 10),
        axis.title.y= element_text(size = 10),
        axis.text.x = element_text(hjust = .9, angle = 0, size = 10),
        axis.text.y = element_text(hjust = .9, size = 10),
        strip.text.x = element_text(size=8, face="bold"),
        strip.text.y = element_text(size=8, face="bold"),
        plot.title = element_text(size=9, vjust=-1,hjust=0.08, face="bold"))
# combine in a grid
plot_grid(scatt1,scatt2,scatt1,scatt1,scatt1,scatt1, align="v",ncol=3,
          #rel_heights=c(0.15,0.15,0.15,0.15,0.4), 
          #hjust=c(-6.5,-6.5,-7,-6.5,-6.5), vjust=1.6,
          labels = c("a)","b)","c)","d)","e)","f)"), #labels="auto",
          label_size = 10)

##########################################################################
# Figure #4 - 4 24 hours plots combined
##########################################################################
summary(plotVARs)
##############################################################
as_tibble(plotVARs) %>% 
  group_by(year=year(plotVARs$timestamp),
           month=month(plotVARs$timestamp, label=TRUE), 
           hour=hour(plotVARs$timestamp)) %>%
  summarise(obs_ET_ROTH=mean(ETc.ROTH0, na.rm=T),
            obs_ET_ROTH_orig=mean(ET.ROTH, na.rm=T),
            ETo_ROTH=mean(ETo.DWD.ROTH, na.rm=T),
            ETo_ROTH_adjB=mean(ETo.DWD.ROTH.adj, na.rm=T),
            SCOPE_ETo_ROTH=mean(SCOPE.ETo.ROTH, na.rm=T),
            SCOPE_ETo_ROTH_adjB=mean(SCOPE.ETo.ROTH.adjB, na.rm=T),
            SCOPE_DWD_ROTH=mean(SCOPE.DWD.ROTH, na.rm=T),
            SCOPE_DWD_ROTH_adjB=mean(SCOPE.DWD.ROTH.adjB, na.rm=T),
            SCOPE_RS_ROTH=mean(SCOPE.RS.ROTH, na.rm=T),
            SCOPE_RS_ROTH_adjB=mean(SCOPE.RS.ROTH.adjB, na.rm=T),
            prech_TUCC=sum(PrecYN.TUCC, na.rm=T),
            prech_ROTH=sum(PrecYN.ROTH, na.rm=T),
            precmm_ROTH=sum(Precmm.ROTH, na.rm=T),
            precmm_TUCC=sum(Precmm.TUCC, na.rm=T),
            veg_TUCC=mean(veg.cover.TUCC, na.rm=T),
            veg_ROTH=mean(veg.cover.ROTH, na.rm=T),
            impervious_ROTH=mean(impervious.ROTH, na.rm=T),
            impervious_TUCC=mean(impervious.TUCC, na.rm=T),
            obs_ET_TUCC=mean(ETc.TUCC0, na.rm=T),
            obs_ET_TUCC_orig=mean(ET.TUCC, na.rm=T),
            ETo_TUCC=mean(ETo.DWD.TUCC, na.rm=T),
            ETo_TUCC_adjB=mean(ETo.DWD.TUCC.adj, na.rm=T),
            SCOPE_ETo_TUCC=mean(SCOPE.ETo.TUCC, na.rm=T),
            SCOPE_ETo_TUCC_adjB=mean(SCOPE.ETo.TUCC.adjB, na.rm=T),
            SCOPE_DWD_TUCC=mean(SCOPE.DWD.TUCC, na.rm=T),
            SCOPE_DWD_TUCC_adjB=mean(SCOPE.DWD.TUCC.adjB, na.rm=T),
            SCOPE_RS_TUCC=mean(SCOPE.RS.TUCC, na.rm=T),
            SCOPE_RS_TUCC_adjB=mean(SCOPE.RS.TUCC.adjB, na.rm=T),
            .groups='drop') -> Mean_h_results_ET

Mean_h_results_ET

plotEThour_RO <- ggplot(filter(Mean_h_results_ET, year==2019), aes(x=hour)) +
  geom_line(aes(y=obs_ET_ROTH ),size=1.2, colour="black") +
  geom_line(aes(y=ETo_ROTH_adjB),size=.55, colour="orange") +
  geom_line(aes(y=SCOPE_ETo_ROTH_adjB),size=.75, colour="#b2df8a") +
  geom_line(aes(y=SCOPE_DWD_ROTH_adjB),size=.75, colour="#238b45") +
  geom_line(aes(y=SCOPE_RS_ROTH_adjB),size=.75, colour="#00441b") +
  scale_y_continuous(limits=c(0,0.35), 
                     breaks=c(0,0.05,0.1,0.15,0.2,0.25,0.3,0.35),
                     expand = c(0.0, 0.0)) +
  scale_x_discrete(name=NULL,limits=NULL, breaks = NULL) +
  labs( y='ET [mm/h]') + #x="hour",
  #ggtitle("c) ROTH site: observed ET (black), ETo corrected (orange) and SCOPE corrected (greens)") +
  facet_wrap(.~factor(month), ncol=12)+
  theme(legend.position = "bottom",
        legend.box.spacing = unit(-0.05,"lines"),
        legend.text = element_text(colour="black", size = 10),
        legend.title = element_text(colour="black", size=9, face="bold"),
        axis.text.y = element_text(color="grey25", hjust=.9, size=8),
        axis.title.y = element_text(color = "grey25", 
                                    face="bold",size=8,vjust=1.2),
        axis.text.x = element_text(color="grey25", hjust=.9, size=7),
        axis.title.x = element_text(color = "grey25", 
                                    face="bold",size=10,vjust=1.2),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        panel.spacing.y = unit(-0.5, "lines"), 
        panel.spacing.x = unit(0.075, "lines"), 
        panel.background = element_rect(fill = "white",
                                        colour = "lightgrey",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.25, linetype = 'dotted',
                                        colour = "lightgrey"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'dotted',
                                        colour = "white"),
        plot.title = element_text(size=9, vjust=-1,hjust=0))

plotEThour_TU <- ggplot(filter(Mean_h_results_ET, year==2019), aes(x=hour)) +
  geom_line(aes(y=obs_ET_TUCC , colour="1-ET_Observed"),size=1.2) +
  geom_line(aes(y=ETo_TUCC_adjB, colour="2-ETo"),size=.55) +
  geom_line(aes(y=SCOPE_ETo_TUCC_adjB, colour="3-SCOPE_ETo"),size=.75) +
  geom_line(aes(y=SCOPE_DWD_TUCC_adjB, colour="4-SCOPE_DWD"),size=.75) +
  geom_line(aes(y=SCOPE_RS_TUCC_adjB, colour="5-SCOPE_RS"),size=.75) +
  scale_y_continuous(limits=c(0,0.35), 
                     breaks=c(0,0.05,0.1,0.15,0.2,0.25,0.3,0.35),
                     expand = c(0.0, 0.0)) +
  scale_x_continuous(breaks=c(4,12,20)) +
  labs(x="hour", y='ET [mm/h]') +
  scale_color_manual(name = "ET:", 
                     values = c("1-ET_Observed"="black", 
                                "2-ETo"="orange",               
                                "3-SCOPE_ETo"="#a6cee3",
                                "4-SCOPE_DWD"="#1f78b4",
                                "5-SCOPE_RS"="#08306b"))+
  #ggtitle("d) TUCC site: corrected predicted and observed ET") +
  facet_wrap(.~factor(month), ncol=12)+
  theme(legend.position = "bottom",
        legend.box.spacing = unit(-0.05,"lines"),
        legend.text = element_text(colour="black", size = 9),
        legend.title = element_text(colour="black", size=10, face="bold"),
        axis.text.y = element_text(color="grey25", hjust=.9, size=9),
        axis.title.y = element_text(color = "grey25", 
                                    face="bold",size=8,vjust=1.2),
        axis.text.x = element_text(color="grey25", hjust=.9, size=7),
        axis.title.x = element_text(color = "grey25", 
                                    face="bold",size=10,vjust=1.2),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        panel.spacing.y = unit(-0.5, "lines"), 
        panel.spacing.x = unit(0.075, "lines"), 
        panel.background = element_rect(fill = "white",
                                        colour = "lightgrey",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.25, linetype = 'dotted',
                                        colour = "lightgrey"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'dotted',
                                        colour = "white"),
        plot.title = element_text(size=9, vjust=-1,hjust=0))

mean(filter(Mean_h_results_ET, year==2019)$prech_TUCC)
mean(filter(Mean_h_results_ET, year==2019)$prech_ROTH)

plotprech <- ggplot(filter(Mean_h_results_ET, year==2019), aes(x=hour)) +
  geom_line(aes(y=prech_ROTH, colour="prech.ROTH"),size=.5) +
  geom_hline(yintercept=6.14, color="green", linetype="dotted", size=0.4)+
  geom_line(aes(y=prech_TUCC, colour="prech.TUCC"),size=.5) +
  geom_hline(yintercept=5.88, color="blue", linetype="dotted", size=0.4)+
  scale_y_continuous(name = "precipitation [h]", 
                     limits = c(0,20),breaks=c(0,4,8,12,16,20))+
  scale_x_discrete(name=NULL,limits=NULL, breaks = NULL) +
  scale_color_manual(name=NULL,labels=NULL,breaks =NULL,
                     values = c("prech.ROTH"="green","prech.TUCC"="blue"))+ 
  #ggtitle("a) precipitation events (accumulated hours)") +
  facet_wrap(.~factor(month), ncol=12)+
  theme(legend.position = "bottom",
        panel.spacing.y = unit(-0.5, "lines"), 
        panel.spacing.x = unit(0.075, "lines"), 
        strip.background = element_rect(fill="grey80"),
        axis.text.y = element_text(color="grey20", hjust=.9, size=9),
        axis.title.y = element_text(color = "grey20", face="bold",
                                    size=8,vjust=1.2),
        panel.background = element_rect(fill = "white",
                                        colour = "lightgrey",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.25, linetype = 'dotted',
                                        colour = "lightgrey"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'dotted',
                                        colour = "white"),
        plot.title = element_text(size=9, vjust=-1,hjust=0))

plot.fraction <- ggplot(filter(Mean_h_results_ET, year==2019), aes(x=hour)) +
  geom_line(aes(y=veg_ROTH, colour="veg.ROTH.av"),size=.5) +
  geom_line(aes(y=veg_TUCC, colour="veg.TUCC.av"),size=.5) +
  geom_line(aes(y=impervious_ROTH, colour="impervious.ROTH.av"),
            size=.5, linetype="dashed")+
  geom_line(aes(y=impervious_TUCC, colour="impervious.TUCC.av"),
            size=.5, linetype="dashed")+
  scale_y_continuous(name = "fraction [%]", 
                     limits = c(20,80),breaks=c(0,20,30,40,50,60,70,80,100))+
  scale_x_discrete(name=NULL,limits=NULL, breaks = NULL) +
  scale_color_manual(name=NULL,labels=NULL,breaks =NULL,
                     values = c("impervious.ROTH.av"="darkgreen",
                                "veg.ROTH.av"="green",
                                "veg.TUCC.av"="blue",
                                "impervious.TUCC.av"="darkblue"))+ 
  #ggtitle("b) vegetation fraction (solid line) and impervious fraction (dashed line)") +
  facet_wrap(.~factor(month), ncol=12)+
  theme(legend.position = "bottom",
        axis.text.y = element_text(color="grey25", hjust=.9, size=9),
        axis.title.y = element_text(color = "grey25", 
                                    face="bold",size=8,vjust=1.2),
        axis.text.x = element_text(color="grey25", hjust=.9, size=7),
        axis.title.x = element_text(color = "grey25", 
                                    face="bold",size=10,vjust=1.2),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        panel.spacing.y = unit(-0.5, "lines"), 
        panel.spacing.x = unit(0.075, "lines"), 
        panel.background = element_rect(fill = "white",
                                        colour = "lightgrey",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.25, linetype = 'dotted',
                                        colour = "lightgrey"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'dotted',
                                        colour = "white"),
        plot.title = element_text(size=9, vjust=-1,hjust=0))

plot_grid(plotprech,plot.fraction,plotEThour_RO,plotEThour_TU, align="v",ncol=1,
          rel_heights=c(0.2,0.2,0.25,0.35), 
          labels = c("a)","b)","c)","d)","e)","f)"), #labels="auto",
          label_size = 10)

##########################################################################
# Figure #5 - 3 smoothly plots combined
##########################################################################
###########################################################################
summary(plotVARs)
###########################################################################
plot_smoothRO <- ggplot(plotVARs,aes(x=date(timestamp)))+
  stat_smooth(formula=y~splines::bs(x,24),aes(y=ETc.ROTH, color="ET", fill="ET"),size=1) +
  stat_smooth(formula=y~splines::bs(x,24),aes(y=ETo.DWD.ROTH.adj, color="ETo", fill="ETo"))+
  stat_smooth(formula=y~splines::bs(x,24),aes(y=SCOPE.ETo.ROTH.adjB, color="SCOPE_ETo", fill="SCOPE_ETo")) +
  stat_smooth(formula=y~splines::bs(x,24),aes(y=SCOPE.DWD.ROTH.adjB, color="SCOPE_DWD", fill="SCOPE_DWD")) +
  stat_smooth(formula=y~splines::bs(x,24),aes(y=SCOPE.RS.ROTH.adjB, color="SCOPE_RS", fill="SCOPE_RS")) +
  scale_color_manual(name=NULL,labels=NULL,breaks =NULL,
                     values = c("ET"="black",
                                "ETo"="orange", 
                                "SCOPE_ETo"="lightgreen",
                                "SCOPE_DWD"="green", 
                                "SCOPE_RS"="darkgreen"),                
                     aesthetics = c("color", "fill"))+
  coord_x_date(xlim = c("2019-01-01","2019-12-31"), expand = F, #"2018-07-01", "2020-03-31"
               ylim = c(0, 0.15)) +
  scale_y_continuous(breaks=c(0,0.05,0.10,0.15,0.20))+
  scale_x_date(name=NULL,labels=NULL,breaks =NULL) +
  # , date_labels = "%b %y",date_breaks = "month", breaks = pretty_breaks(), expand = c(0, 0) #
  labs(x="month (year)", y='ET(mm/h)') +
  theme(
    strip.text.x = element_text(colour="grey30", size=8, face="bold"),
    strip.text.y = element_text(colour="grey30", size=8, face="bold"),
    axis.text.x = element_text(colour="grey30", hjust = .5, size = 8, angle = 90),
    axis.title.x = element_text(colour="grey30", face="bold", size = 8, margin=margin(20,0,0,0)),
    axis.text.y = element_text(colour="grey25", hjust = .9, size = 8),
    axis.title.y = element_text(colour="grey30", face="bold", size = 8, margin=margin(0,20,0,0)),
    legend.position = "bottom",
    panel.grid.major = element_line(colour = "white"))

plot_errorRO <- ggplot(plotVARs,aes(x=date(timestamp)))+
  stat_smooth(formula=y~splines::bs(x,20),aes(y=ETc.ROTH0-SCOPE.RS.ROTH.adjB,color="3-SCOPE_RS",fill="3-SCOPE_RS")) +
  stat_smooth(formula=y~splines::bs(x,20),aes(y=ETc.ROTH0-SCOPE.ETo.ROTH.adjB,color="2-SCOPE_DWD",fill="2-SCOPE_DWD")) +
  stat_smooth(formula=y~splines::bs(x,20),aes(y=ETc.ROTH0-SCOPE.ETo.ROTH.adjB,color="1-SCOPE_ETo",fill="1-SCOPE_ETo")) +
  geom_hline(yintercept=0, color="red", linetype="dotted")+
  scale_y_continuous(breaks=c(-0.02,-0.01,0,0.01,0.02))+
  scale_color_manual(name="ROTH models:", #labels=T,breaks=NULL,
                     values = c("ETo"="orange", 
                                "1-SCOPE_ETo"="lightgreen",
                                "2-SCOPE_DWD"="green", 
                                "3-SCOPE_RS"="darkgreen"),        
                     aesthetics = c("color", "fill"))+
  coord_x_date(xlim = c("2019-01-01","2019-12-31"), expand = F, #
               ylim = c(-0.02, 0.02))+
  scale_x_date(name=NULL,labels=NULL,breaks =NULL) +
  #scale_x_date(date_labels = "%b %y",date_breaks = "month", breaks = pretty_breaks(),
               #expand = c(0, 0)) + #
  #ggtitle("a) ROTH model error (observed - predicted)") +
  labs(x = "month (2019)", y = 'error [mm/h]') +
  theme(legend.position = "bottom",
        legend.box.spacing = unit(-0.01, "lines"),
        axis.text.y = element_text(color="grey25", hjust=.9, size=9),
        axis.title.y = element_text(color = "grey25", 
                                    face="bold",size=10,vjust=1.2),
        axis.text.x = element_text(color="grey25", hjust=-1.3, size=9),
        axis.title.x = element_text(color = "grey25", 
                                    face="bold",size=10,vjust=1.2),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        panel.spacing.y = unit(-0.5, "lines"), 
        panel.spacing.x = unit(0.075, "lines"), 
        panel.background = element_rect(fill = "grey90"),  
        panel.border = element_blank(), 
        panel.grid.major = element_line(colour="grey90"),
        panel.grid.minor = element_line(colour="grey90"),
        plot.title = element_text(size=9, vjust=-1,hjust=0))

###########################################################################
plot_smoothTU <- ggplot(plotVARs,aes(x=date(timestamp)))+
  stat_smooth(formula=y~splines::bs(x,24),aes(y=ETc.TUCC, color="ET", fill="ET"),size=1) +
  stat_smooth(formula=y~splines::bs(x,24),aes(y=ETo.DWD.TUCC.adj, color="ETo", fill="ETo"))+
  stat_smooth(formula=y~splines::bs(x,24),aes(y=SCOPE.ETo.TUCC.adjB, color="SCOPE_ETo", fill="SCOPE_ETo")) +
  stat_smooth(formula=y~splines::bs(x,24),aes(y=SCOPE.DWD.TUCC.adjB, color="SCOPE_DWD", fill="SCOPE_DWD")) +
  stat_smooth(formula=y~splines::bs(x,24),aes(y=SCOPE.RS.TUCC.adjB, color="SCOPE_RS", fill="SCOPE_RS")) +
  scale_color_manual(name=NULL,labels=NULL,breaks =NULL,
                     values = c("ET"="black",
                                "ETo.adj1"="orange", 
                                "SCOPE_ETo"="lightblue",
                                "SCOPE_1"="blue", 
                                "SCOPE_2"="darkblue", 
                                "SCOPE_3"="brown"),                
                     aesthetics = c("color", "fill"))+
  coord_x_date(xlim = c("2019-01-01","2019-12-31"), expand = F, #"2018-07-01", "2020-03-31"
               ylim = c(0, 0.08)) +
  scale_y_continuous(breaks=c(0,0.02,0.04,0.06,0.08))+
  scale_x_date(name=NULL,labels=NULL,breaks =NULL) +
  # , date_labels = "%b %y",date_breaks = "month", breaks = pretty_breaks(), expand = c(0, 0) #
  labs(x="month (year)", y='ET(mm/h)') +
  theme(
    strip.text.x = element_text(colour="grey30", size=8, face="bold"),
    strip.text.y = element_text(colour="grey30", size=8, face="bold"),
    axis.text.x = element_text(colour="grey30", size = 8, angle = 90),
    axis.title.x = element_text(colour="grey30", face="bold", size = 10, margin=margin(10,0,0,0)),
    axis.text.y = element_text(colour="grey25", hjust = .9, size = 8),
    axis.title.y = element_text(colour="grey30", face="bold", size = 8, margin=margin(0,20,0,0)),
    legend.position = "bottom",
    panel.grid.major = element_line(colour = "white"))

plot_errorTU <- ggplot(plotVARs,aes(x=date(timestamp)))+
  stat_smooth(formula=y~splines::bs(x,20),aes(y=ETc.TUCC0-SCOPE.RS.TUCC.adjB,color="3-SCOPE_RS",fill="3-SCOPE_RS")) +
  stat_smooth(formula=y~splines::bs(x,20),aes(y=ETc.TUCC0-SCOPE.DWD.TUCC.adjB,color="2-SCOPE_DWD",fill="2-SCOPE_DWD")) +
  stat_smooth(formula=y~splines::bs(x,20),aes(y=ETc.TUCC0-SCOPE.ETo.TUCC.adjB,color="1-SCOPE_ETo",fill="1-SCOPE_ETo")) +
  geom_hline(yintercept=0, color="red", linetype="dotted")+
  scale_color_manual(name="TUCC models:", #labels=NULL,breaks=NULL,
                     values = c("ETo"="orange", 
                                "1-SCOPE_ETo"="lightblue",
                                "2-SCOPE_DWD"="blue", 
                                "3-SCOPE_RS"="darkblue"),              
                     aesthetics = c("color", "fill"))+
  scale_y_continuous(breaks=c(-0.02,-0.01,0,0.01,0.02))+
  coord_x_date(xlim = c("2019-01-01","2019-12-31"), expand = F, #
               ylim = c(-0.02, 0.02))+
  scale_x_date(date_labels = "%b",date_breaks = "month", breaks = pretty_breaks(),
               expand = c(0, 0)) + #
  labs(x = "month (2019)", y = 'error [mm/h]') +
  #ggtitle("C) TUCC model error (observed - predicted)") +
    theme(legend.position = "bottom",
          legend.box.spacing = unit(-0.05, "lines"),
          axis.text.y = element_text(color="grey25", hjust=.9, size=9),
          axis.title.y = element_text(color = "grey25", 
                                      face="bold",size=10,vjust=1.2),
          axis.text.x = element_text(color="grey25", hjust=-1.1, size=9),
          axis.title.x = element_text(color = "grey25", 
                                      face="bold",size=10,vjust=1.2),
          strip.background = element_blank(),
          strip.text.x = element_blank(),
          panel.spacing.y = unit(-0.5, "lines"), 
          panel.spacing.x = unit(0.075, "lines"), 
          panel.background = element_rect(fill = "grey90"),  
          panel.border = element_blank(), 
          panel.grid.major = element_line(colour="grey90"),
          panel.grid.minor = element_line(colour="grey90"),
          plot.title = element_text(size=9, vjust=-1,hjust=0))

# Plot precipitation(mm)
mean(filter(plotVARs, year(timestamp)==2019)$Precmm.ROTH)
mean(filter(plotVARs, year(timestamp)==2019)$Precmm.TUCC)

plotPrecSmooth <- ggplot(plotVARs, aes(x=date(timestamp))) +
  geom_smooth(formula=y~splines::bs(x,20),color="darkgreen",fill="green", aes(y=Precmm.ROTH)) +
  geom_smooth(formula=y~splines::bs(x,20),color="blue",fill="blue", aes(y=Precmm.TUCC)) +
  geom_hline(yintercept=0.0578,color="darkgreen", linetype="dotted")+
  geom_hline(yintercept=0.0458,color="darkblue", linetype="dotted")+
  scale_y_continuous(breaks=c(0,0.05,0.10,0.15,0.20))+
  coord_x_date(xlim = c("2019-01-01","2019-12-31"),ylim = c(-0.05,0.22), expand = F)+
  scale_x_date(name=NULL,limits=NULL, breaks = NULL) +
  labs(y = 'precipitation [mm/h]') +
  #ggtitle("b) Volume of precipitation (mm/h)") +
  theme(legend.position = "bottom",
        legend.box.spacing = unit(-0.05, "lines"),
        axis.text.y = element_text(color="grey25", hjust=.9, size=9),
        axis.title.y = element_text(color = "grey25", 
                                    face="bold",size=10,vjust=1.2),
        axis.text.x = element_text(color="grey25", hjust=-1.3, size=9),
        axis.title.x = element_text(color = "grey25", 
                                    face="bold",size=10,vjust=1.2),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        panel.spacing.y = unit(-0.5, "lines"), 
        panel.spacing.x = unit(0.075, "lines"), 
        panel.background = element_rect(fill = "grey90"),  
        panel.border = element_blank(), 
        panel.grid.major = element_line(colour="grey90"),
        panel.grid.minor = element_line(colour="grey90"),
        plot.title = element_text(size=9, vjust=-1,hjust=0))

plot_grid(plot_errorRO,plotPrecSmooth,plot_errorTU, align="v",ncol=1,
          rel_heights=c(0.32,0.32,0.36), 
          labels = c("a)","b)","c)"), #labels="auto",
          label_size = 10)

##########################################################################
# Figure #6 - 6 doy/24hours plots combined
##########################################################################
### Plots DOY/hour 
#####################################################################################
## pallets
### purple to gren
LEcolorG = c(rep("#40004b",1),rep("#762a83",1),rep("#9970ab",1), rep("#c2a5cf",1), rep("#e7d4e8",1),
             rep("#d9f0d3",1),rep("#a6dba0",1),rep("#5aae61",1),rep("#1b7837",1), rep("#00441b",1))
### blue to blue
LEcolorB = c(rep("#67001f",1),rep("#b2182b",1),rep("#d6604d",1), rep("#f4a582",1), rep("#fddbc7",1),
             rep("#d1e5f0",1),rep("#92c5de",1),rep("#4393c3",1),rep("#2166ac",1), rep("#053061",1))
ETcolor = c(rep("#4575b4",1),rep("#abd9e9",1),
            rep("#ffffbf",1),rep("#f46d43",1), rep("#d73027",1), rep("#d73027",1), rep("#a50026",1))
####################################################################################
summary(plotVARs$ETc.ROTH0)
summary(plotVARs$ETo.DWD.ROTH.adj)
summary(plotVARs$SCOPE.ETo.ROTH.adjB)
summary(plotVARs$SCOPE.DWD.ROTH.adjB)
summary(plotVARs$SCOPE.RS.ROTH.adjB)

Plot_ROTH_ET <- ggplot(filter(plotVARs,year(timestamp)==2019)) +
  geom_raster(aes(x=yday(timestamp),y=hour(timestamp),fill=ETc.ROTH0)) + 
  scale_x_continuous(name =NULL,expand=c(0,0),breaks=seq(0,350,25),limits=c(1,365)) +
  scale_y_continuous("hour",expand=c(0,0),breaks=seq(0,24,4),limits=c(0,24)) +
  scale_fill_gradientn(breaks=seq(0,0.5,0.05),limits=c(0,0.5),
                       colors=ETcolor, name=NULL,na.value = NA,
                       guide =NULL) + 
  #ggtitle("ROTH - observed ET (cleaned)") +
  theme(#legend.position = "bottom",
        #legend.box.spacing = unit(-0.03, "lines"),
        #legend.title = element_text(color="grey25", vjust=.9, size=9),
        axis.text.y = element_text(color="grey25", hjust=.9, size=9),
        axis.title.y=element_text(color="grey25",face="bold",size=11,vjust=1.2),
        axis.text.x = element_text(color="grey25", size=10),
        #axis.title.x = element_text(color = "grey25",face="bold",size=10,vjust=1.2),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        panel.spacing.y = unit(-0.5, "lines"), 
        panel.spacing.x = unit(0.075, "lines"), 
        panel.background = element_rect(fill = "white"),  
        panel.border = element_blank(), 
        panel.grid.major = element_line(colour="white"),
        panel.grid.minor = element_line(colour="white"),
        plot.title = element_text(size=9, vjust=-1,hjust=0))

Plot_ROTH_ET.fill <- ggplot(filter(plotVARs,year(timestamp)==2019)) +
  geom_raster(aes(x=yday(timestamp),y=hour(timestamp),fill=ET.ROTH.filled0)) + 
  scale_x_continuous(name =NULL,expand=c(0,0),breaks=seq(0,350,25),limits=c(1,365)) +
  scale_y_continuous("hour",expand=c(0,0),breaks=seq(0,24,4),limits=c(0,24)) +
  scale_fill_gradientn(breaks=seq(0,0.5,0.05),limits=c(0,0.5),
                       colors=ETcolor, name=NULL,na.value = NA,
                       guide =NULL) + 
  #ggtitle("ROTH - Observed ET (gap-filled with MDS)") +
  theme(#legend.position = "bottom",
        #legend.box.spacing = unit(-0.03, "lines"),
        #legend.title = element_text(color="grey25", vjust=.9, size=7),
        axis.text.y = element_text(color="grey25", hjust=.9, size=9),
        axis.title.y=element_text(color="grey25",face="bold",size=11,vjust=1.2),
        axis.text.x = element_text(color="grey25", size=10),
        #axis.title.x = element_text(color = "grey25",face="bold",size=8,vjust=1.2),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        panel.spacing.y = unit(-0.5, "lines"), 
        panel.spacing.x = unit(0.075, "lines"), 
        panel.background = element_rect(fill = "white"),  
        panel.border = element_blank(), 
        panel.grid.major = element_line(colour="white"),
        panel.grid.minor = element_line(colour="white"),
        plot.title = element_text(size=9, vjust=-1,hjust=0))

Plot_ROTH_ETo <- ggplot(filter(plotVARs,year(timestamp)==2019)) +
  geom_raster(aes(x=yday(timestamp),y=hour(timestamp),fill=ETo.DWD.ROTH.adj)) + 
  scale_x_continuous(name =NULL,expand=c(0,0),breaks=seq(0,350,25),limits=c(1,365)) +
  scale_y_continuous("hour",expand=c(0,0),breaks=seq(0,24,4),limits=c(0,24)) +
  scale_fill_gradientn(breaks=seq(0,0.5,0.05),limits=c(0,0.5),
                       colors=ETcolor, name=NULL,na.value = NA,
                       guide =NULL) + 
  #ggtitle("ROTH - ETo (corrected)") +
  theme(legend.position = "bottom",
        legend.box.spacing = unit(-0.03, "lines"),
        legend.title = element_text(color="grey25", vjust=.9, size=7),
        axis.text.y = element_text(color="grey25", hjust=.9, size=9),
        axis.title.y=element_text(color="grey25",face="bold",size=11,vjust=1.2),
        axis.text.x = element_text(color="grey25", size=10),
        axis.title.x = element_text(color = "grey25",face="bold",size=8,vjust=1.2),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        panel.spacing.y = unit(-0.5, "lines"), 
        panel.spacing.x = unit(0.075, "lines"), 
        panel.background = element_rect(fill = "white"),  
        panel.border = element_blank(), 
        panel.grid.major = element_line(colour="white"),
        panel.grid.minor = element_line(colour="white"),
        plot.title = element_text(size=9, vjust=-1,hjust=0))

Plot_ROTH_SCOPE.ETo <- ggplot(filter(plotVARs,year(timestamp)==2019)) +
  geom_raster(aes(x=yday(timestamp),y=hour(timestamp),fill=SCOPE.ETo.ROTH.adjB)) + 
  scale_x_continuous(name =NULL, expand=c(0,0),breaks=seq(0,350,25),limits=c(1,365)) +
  scale_y_continuous("hour",expand=c(0,0),breaks=seq(0,24,4),limits=c(0,24)) +
  scale_fill_gradientn(breaks=seq(0,0.5,0.05),limits=c(0,0.5),
                       colors=ETcolor, name=NULL,na.value = NA,
                       guide =NULL) + 
  #ggtitle("ROTH - SCOPE_ETo (corrected)") +
  theme(legend.position = "bottom",
        legend.box.spacing = unit(-0.03, "lines"),
        legend.title = element_text(color="grey25", vjust=.9, size=7),
        axis.text.y = element_text(color="grey25", hjust=.9, size=9),
        axis.title.y=element_text(color="grey25",face="bold",size=11,vjust=1.2),
        axis.text.x = element_text(color="grey25", size=10),
        axis.title.x = element_text(color = "grey25",face="bold",size=8,vjust=1.2),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        panel.spacing.y = unit(-0.5, "lines"), 
        panel.spacing.x = unit(0.075, "lines"), 
        panel.background = element_rect(fill = "white"),  
        panel.border = element_blank(), 
        panel.grid.major = element_line(colour="white"),
        panel.grid.minor = element_line(colour="white"),
        plot.title = element_text(size=9, vjust=-1,hjust=0))

Plot_ROTH_SCOPE.DWD <- ggplot(filter(plotVARs,year(timestamp)==2019)) +
  geom_raster(aes(x=yday(timestamp),y=hour(timestamp),fill=SCOPE.DWD.ROTH.adjB)) + 
  scale_x_continuous("day of year (2019)",expand=c(0,0),breaks=seq(0,350,25),limits=c(1,365)) +
  scale_y_continuous("hour",expand=c(0,0),breaks=seq(0,24,4),limits=c(0,24)) +
  scale_fill_gradientn(breaks=seq(0,0.5,0.1),limits=c(0,0.5),colors=ETcolor, 
                       name=NULL,na.value=NA, guide=NULL) + 
                    #   name="ET [mm/hour]",na.value = NA,
                    #   guide =guide_colorbar(direction = "horizontal",
                    #                         title.position = "left",label.position = "bottom",
                    #                         barwidth = 8, barheight = 0.8,nbin = 100,
                    #                         label.theme = element_text(angle = 0,size=8))) + 
  #ggtitle("ROTH - SCOPE_DWD (corrected)") +
  theme(legend.position = "bottom",
        legend.box.spacing = unit(-0.03, "lines"),
        legend.title = element_text(color="grey25", vjust=.9, size=7),
        axis.text.y = element_text(color="grey25", hjust=.9, size=9),
        axis.title.y=element_text(color="grey25",face="bold",size=11,vjust=1.2),
        axis.text.x = element_text(color="grey25", size=10),
        axis.title.x = element_text(color = "grey25",face="bold",size=10,vjust=1.2),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        panel.spacing.y = unit(-0.5, "lines"), 
        panel.spacing.x = unit(0.075, "lines"), 
        panel.background = element_rect(fill = "white"),  
        panel.border = element_blank(), 
        panel.grid.major = element_line(colour="white"),
        panel.grid.minor = element_line(colour="white"),
        plot.title = element_text(size=9, vjust=-1,hjust=0))

Plot_ROTH_SCOPE.RS <- ggplot(filter(plotVARs,year(timestamp)==2019)) +
  geom_raster(aes(x=yday(timestamp),y=hour(timestamp),fill=SCOPE.RS.ROTH.adjB)) + 
  scale_x_continuous("day of year (2019)",expand=c(0,0),breaks=seq(0,350,25),limits=c(1,365)) +
  scale_y_continuous("hour",expand=c(0,0),breaks=seq(0,24,4),limits=c(0,24)) +
  scale_fill_gradientn(breaks=seq(0,0.5,0.1),limits=c(0,0.5),
                       colors=ETcolor, name=NULL,na.value=NA, guide=NULL) + 
  #                       name="ET [mm/hour]",na.value = NA,
  #                       guide =guide_colorbar(direction = "horizontal",
  #                                             title.position = "left",label.position = "bottom",
  #                                             barwidth = 8, barheight = 0.8,nbin = 100,
  #                                             label.theme = element_text(angle = 0,size=8))) + 
  #ggtitle("ROTH - SCOPE_RS (corrected)") +
  theme(legend.position = "bottom",
        legend.box.spacing = unit(-0.03, "lines"),
        legend.title = element_text(color="grey25", vjust=.9, size=7),
        axis.text.y = element_text(color="grey25", hjust=.9, size=9),
        axis.title.y=element_text(color="grey25",face="bold",size=11,vjust=1.2),
        axis.text.x = element_text(color="grey25", size=10),
        axis.title.x=element_text(color="grey25",face="bold",size=10,vjust=1.2),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        panel.spacing.y = unit(-0.5, "lines"), 
        panel.spacing.x = unit(0.075, "lines"), 
        panel.background = element_rect(fill = "white"),  
        panel.border = element_blank(), 
        panel.grid.major = element_line(colour="white"),
        panel.grid.minor = element_line(colour="white"),
        plot.title = element_text(size=9, vjust=-1,hjust=0))

legend_ET <- ggplot(filter(plotVARs,year(timestamp)==2019)) +
  geom_raster(aes(x=yday(timestamp),y=hour(timestamp),fill=SCOPE.RS.ROTH.adjB)) + 
  #scale_x_continuous("day of year (2019)",expand=c(0,0),breaks=seq(0,350,25),limits=c(1,365)) +
  #scale_y_continuous("hour",expand=c(0,0),breaks=seq(0,24,4),limits=c(0,24)) +
  scale_fill_gradientn(breaks=seq(0,0.5,0.1),limits=c(0,0.5),colors=ETcolor, 
     name="ET [mm/hour]",na.value = NA,
     guide =guide_colorbar(direction = "horizontal",
                           title.position = "left",
                           label.position = "bottom",
                           barwidth =18, barheight =0.9,nbin = 20,
                           label.theme = element_text(angle = 0,size=11))) + 
  theme(legend.position = "bottom",
        legend.box.spacing = unit(-0.03, "lines"),
        legend.title = element_text(color="grey25", vjust=.9, size=12),
        axis.text.y = element_text(color="grey25", hjust=.9, size=9),
        axis.title.y=element_text(color="grey25",face="bold",size=11,vjust=1.2),
        axis.text.x = element_text(color="grey25", size=10),
        axis.title.x=element_text(color="grey25",face="bold",size=11,vjust=1.2),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        panel.spacing.y = unit(-0.5, "lines"), 
        panel.spacing.x = unit(0.075, "lines"), 
        panel.background = element_rect(fill = "white"),  
        panel.border = element_blank(), 
        panel.grid.major = element_line(colour="white"),
        panel.grid.minor = element_line(colour="white"),
        plot.title = element_text(size=9, vjust=-1,hjust=0))

legend <- cowplot::get_legend(legend_ET)

grid::grid.newpage()
grid::grid.draw(legend)

ggdraw(plot_grid(
  plot_grid(Plot_ROTH_ET, Plot_ROTH_ET.fill, Plot_ROTH_ETo,
           Plot_ROTH_ETo, Plot_ROTH_SCOPE.DWD, Plot_ROTH_SCOPE.RS,
           align="v",ncol=2, rel_heights=c(0.33,0.32,0.35,0.33,0.32,0.35), 
          labels = c("a)","b)","c)","d)","e)","f)"), label_size = 12),
  plot_grid(NULL, legend, ncol=2, rel_widths=c(0.05,0.95)), 
  nrow=2, rel_heights=c(0.90,0.1)))

#### TUCC
summary(plotVARs$ETc.TUCC0)
summary(plotVARs$ETo.DWD.TUCC.adj)
summary(plotVARs$SCOPE.ETo.TUCC.adjB)
summary(plotVARs$SCOPE.DWD.TUCC.adjB)
summary(plotVARs$SCOPE.RS.TUCC.adjB)

Plot_TUCC_ET <- ggplot(filter(plotVARs,year(timestamp)==2019)) +
  geom_raster(aes(x=yday(timestamp),y=hour(timestamp),fill=ETc.TUCC0)) + 
  scale_x_continuous(name =NULL,expand=c(0,0),breaks=seq(0,350,25),limits=c(1,365)) +
  scale_y_continuous("hour",expand=c(0,0),breaks=seq(0,24,4),limits=c(0,24)) +
  scale_fill_gradientn(breaks=seq(0,0.5,0.05),limits=c(0,0.5),
                       colors=ETcolor, name=NULL,na.value = NA,
                       guide =NULL) + 
  #ggtitle("TUCC - observed ET (cleaned)") +
  theme(#legend.position = "bottom",
    #legend.box.spacing = unit(-0.03, "lines"),
    #legend.title = element_text(color="grey25", vjust=.9, size=9),
    axis.text.y = element_text(color="grey25", hjust=.9, size=9),
    axis.title.y=element_text(color="grey25",face="bold",size=11,vjust=1.2),
    axis.text.x = element_text(color="grey25", size=10),
    #axis.title.x = element_text(color = "grey25",face="bold",size=10,vjust=1.2),
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    panel.spacing.y = unit(-0.5, "lines"), 
    panel.spacing.x = unit(0.075, "lines"), 
    panel.background = element_rect(fill = "white"),  
    panel.border = element_blank(), 
    panel.grid.major = element_line(colour="white"),
    panel.grid.minor = element_line(colour="white"),
    plot.title = element_text(size=9, vjust=-1,hjust=0))

Plot_TUCC_ET.fill <- ggplot(filter(plotVARs,year(timestamp)==2019)) +
  geom_raster(aes(x=yday(timestamp),y=hour(timestamp),fill=ET.TUCC.filled0)) + 
  scale_x_continuous(name =NULL,expand=c(0,0),breaks=seq(0,350,25),limits=c(1,365)) +
  scale_y_continuous("hour",expand=c(0,0),breaks=seq(0,24,4),limits=c(0,24)) +
  scale_fill_gradientn(breaks=seq(0,0.5,0.05),limits=c(0,0.5),
                       colors=ETcolor, name=NULL,na.value = NA,
                       guide =NULL) + 
  #ggtitle("TUCC - Observed ET (gap-filled with MDS)") +
  theme(#legend.position = "bottom",
    #legend.box.spacing = unit(-0.03, "lines"),
    #legend.title = element_text(color="grey25", vjust=.9, size=7),
    axis.text.y = element_text(color="grey25", hjust=.9, size=9),
    axis.title.y=element_text(color="grey25",face="bold",size=11,vjust=1.2),
    axis.text.x = element_text(color="grey25", size=10),
    #axis.title.x = element_text(color = "grey25",face="bold",size=8,vjust=1.2),
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    panel.spacing.y = unit(-0.5, "lines"), 
    panel.spacing.x = unit(0.075, "lines"), 
    panel.background = element_rect(fill = "white"),  
    panel.border = element_blank(), 
    panel.grid.major = element_line(colour="white"),
    panel.grid.minor = element_line(colour="white"),
    plot.title = element_text(size=9, vjust=-1,hjust=0))

Plot_TUCC_ETo <- ggplot(filter(plotVARs,year(timestamp)==2019)) +
  geom_raster(aes(x=yday(timestamp),y=hour(timestamp),fill=ETo.DWD.TUCC.adj)) + 
  scale_x_continuous(name =NULL,expand=c(0,0),breaks=seq(0,350,25),limits=c(1,365)) +
  scale_y_continuous("hour",expand=c(0,0),breaks=seq(0,24,4),limits=c(0,24)) +
  scale_fill_gradientn(breaks=seq(0,0.5,0.05),limits=c(0,0.5),
                       colors=ETcolor, name=NULL,na.value = NA,
                       guide =NULL) + 
  #ggtitle("TUCC - ETo (corrected)") +
  theme(legend.position = "bottom",
        legend.box.spacing = unit(-0.03, "lines"),
        legend.title = element_text(color="grey25", vjust=.9, size=7),
        axis.text.y = element_text(color="grey25", hjust=.9, size=9),
        axis.title.y=element_text(color="grey25",face="bold",size=11,vjust=1.2),
        axis.text.x = element_text(color="grey25", size=10),
        axis.title.x = element_text(color = "grey25",face="bold",size=8,vjust=1.2),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        panel.spacing.y = unit(-0.5, "lines"), 
        panel.spacing.x = unit(0.075, "lines"), 
        panel.background = element_rect(fill = "white"),  
        panel.border = element_blank(), 
        panel.grid.major = element_line(colour="white"),
        panel.grid.minor = element_line(colour="white"),
        plot.title = element_text(size=9, vjust=-1,hjust=0))

Plot_TUCC_SCOPE.ETo <- ggplot(filter(plotVARs,year(timestamp)==2019)) +
  geom_raster(aes(x=yday(timestamp),y=hour(timestamp),fill=SCOPE.ETo.TUCC.adjB)) + 
  scale_x_continuous(name =NULL, expand=c(0,0),breaks=seq(0,350,25),limits=c(1,365)) +
  scale_y_continuous("hour",expand=c(0,0),breaks=seq(0,24,4),limits=c(0,24)) +
  scale_fill_gradientn(breaks=seq(0,0.5,0.05),limits=c(0,0.5),
                       colors=ETcolor, name=NULL,na.value = NA,
                       guide =NULL) + 
  #ggtitle("TUCC - SCOPE_ETo (corrected)") +
  theme(legend.position = "bottom",
        legend.box.spacing = unit(-0.03, "lines"),
        legend.title = element_text(color="grey25", vjust=.9, size=7),
        axis.text.y = element_text(color="grey25", hjust=.9, size=9),
        axis.title.y=element_text(color="grey25",face="bold",size=11,vjust=1.2),
        axis.text.x = element_text(color="grey25", size=10),
        axis.title.x = element_text(color = "grey25",face="bold",size=8,vjust=1.2),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        panel.spacing.y = unit(-0.5, "lines"), 
        panel.spacing.x = unit(0.075, "lines"), 
        panel.background = element_rect(fill = "white"),  
        panel.border = element_blank(), 
        panel.grid.major = element_line(colour="white"),
        panel.grid.minor = element_line(colour="white"),
        plot.title = element_text(size=9, vjust=-1,hjust=0))

Plot_TUCC_SCOPE.DWD <- ggplot(filter(plotVARs,year(timestamp)==2019)) +
  geom_raster(aes(x=yday(timestamp),y=hour(timestamp),fill=SCOPE.DWD.TUCC.adjB)) + 
  scale_x_continuous("day of year (2019)",expand=c(0,0),breaks=seq(0,350,25),limits=c(1,365)) +
  scale_y_continuous("hour",expand=c(0,0),breaks=seq(0,24,4),limits=c(0,24)) +
  scale_fill_gradientn(breaks=seq(0,0.5,0.1),limits=c(0,0.5),colors=ETcolor, 
                       name=NULL,na.value=NA, guide=NULL) + 
  #   name="ET [mm/hour]",na.value = NA,
  #   guide =guide_colorbar(direction = "horizontal",
  #                         title.position = "left",label.position = "bottom",
  #                         barwidth = 8, barheight = 0.8,nbin = 100,
  #                         label.theme = element_text(angle = 0,size=8))) + 
  #ggtitle("TUCC - SCOPE_DWD (corrected)") +
  theme(legend.position = "bottom",
        legend.box.spacing = unit(-0.03, "lines"),
        legend.title = element_text(color="grey25", vjust=.9, size=7),
        axis.text.y = element_text(color="grey25", hjust=.9, size=9),
        axis.title.y=element_text(color="grey25",face="bold",size=11,vjust=1.2),
        axis.text.x = element_text(color="grey25", size=10),
        axis.title.x = element_text(color = "grey25",face="bold",size=10,vjust=1.2),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        panel.spacing.y = unit(-0.5, "lines"), 
        panel.spacing.x = unit(0.075, "lines"), 
        panel.background = element_rect(fill = "white"),  
        panel.border = element_blank(), 
        panel.grid.major = element_line(colour="white"),
        panel.grid.minor = element_line(colour="white"),
        plot.title = element_text(size=9, vjust=-1,hjust=0))

Plot_TUCC_SCOPE.RS <- ggplot(filter(plotVARs,year(timestamp)==2019)) +
  geom_raster(aes(x=yday(timestamp),y=hour(timestamp),fill=SCOPE.RS.TUCC.adjB)) + 
  scale_x_continuous("day of year (2019)",expand=c(0,0),breaks=seq(0,350,25),limits=c(1,365)) +
  scale_y_continuous("hour",expand=c(0,0),breaks=seq(0,24,4),limits=c(0,24)) +
  scale_fill_gradientn(breaks=seq(0,0.5,0.1),limits=c(0,0.5),
                       colors=ETcolor, name=NULL,na.value=NA, guide=NULL) + 
  #                       name="ET [mm/hour]",na.value = NA,
  #                       guide =guide_colorbar(direction = "horizontal",
  #                                             title.position = "left",label.position = "bottom",
  #                                             barwidth = 8, barheight = 0.8,nbin = 100,
  #                                             label.theme = element_text(angle = 0,size=8))) + 
  #ggtitle("TUCC - SCOPE_RS (corrected)") +
  theme(legend.position = "bottom",
        legend.box.spacing = unit(-0.03, "lines"),
        legend.title = element_text(color="grey25", vjust=.9, size=7),
        axis.text.y = element_text(color="grey25", hjust=.9, size=9),
        axis.title.y=element_text(color="grey25",face="bold",size=11,vjust=1.2),
        axis.text.x = element_text(color="grey25", size=10),
        axis.title.x=element_text(color="grey25",face="bold",size=10,vjust=1.2),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        panel.spacing.y = unit(-0.5, "lines"), 
        panel.spacing.x = unit(0.075, "lines"), 
        panel.background = element_rect(fill = "white"),  
        panel.border = element_blank(), 
        panel.grid.major = element_line(colour="white"),
        panel.grid.minor = element_line(colour="white"),
        plot.title = element_text(size=9, vjust=-1,hjust=0))

legend_ET <- ggplot(filter(plotVARs,year(timestamp)==2019)) +
  geom_raster(aes(x=yday(timestamp),y=hour(timestamp),fill=SCOPE.RS.TUCC.adjB)) + 
  #scale_x_continuous("day of year (2019)",expand=c(0,0),breaks=seq(0,350,25),limits=c(1,365)) +
  #scale_y_continuous("hour",expand=c(0,0),breaks=seq(0,24,4),limits=c(0,24)) +
  scale_fill_gradientn(breaks=seq(0,0.5,0.1),limits=c(0,0.5),colors=ETcolor, 
                       name="ET [mm/hour]",na.value = NA,
                       guide =guide_colorbar(direction = "horizontal",
                                             title.position = "left",
                                             label.position = "bottom",
                                             barwidth =18, barheight =0.9,nbin = 20,
                                             label.theme = element_text(angle = 0,size=11))) + 
  theme(legend.position = "bottom",
        legend.box.spacing = unit(-0.03, "lines"),
        legend.title = element_text(color="grey25", vjust=.9, size=12),
        axis.text.y = element_text(color="grey25", hjust=.9, size=9),
        axis.title.y=element_text(color="grey25",face="bold",size=11,vjust=1.2),
        axis.text.x = element_text(color="grey25", size=10),
        axis.title.x=element_text(color="grey25",face="bold",size=11,vjust=1.2),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        panel.spacing.y = unit(-0.5, "lines"), 
        panel.spacing.x = unit(0.075, "lines"), 
        panel.background = element_rect(fill = "white"),  
        panel.border = element_blank(), 
        panel.grid.major = element_line(colour="white"),
        panel.grid.minor = element_line(colour="white"),
        plot.title = element_text(size=9, vjust=-1,hjust=0))

legend <- cowplot::get_legend(legend_ET)

grid::grid.newpage()
grid::grid.draw(legend)

ggdraw(plot_grid(
  plot_grid(Plot_TUCC_ET, Plot_TUCC_ET.fill, Plot_TUCC_ETo, 
            Plot_TUCC_ETo, Plot_TUCC_SCOPE.DWD, Plot_TUCC_SCOPE.RS,
            align="v",ncol=2, rel_heights=c(0.33,0.32,0.35,0.33,0.32,0.35), 
            labels = c("a)","b)","c)","d)","e)","f)"), label_size = 12),
  plot_grid(NULL, legend, ncol=2, rel_widths=c(0.05,0.95)), 
  nrow=2, rel_heights=c(0.90,0.1)))

##########################################################################
# some statistics
# missing data
summary(filter(plotVARs,year(timestamp)==2019))
nrow(filter(plotVARs,year(timestamp)==2019))
3767/8760 # NAs ETc.ROTH cleaned
3685/8760 # NAs ETc.TUCC cleaned
889/8760 # NAs ET.ROTH
698/8760 # NAs ET.TUCC

# annually precipitation
round(sum(filter(plotVARs,year(timestamp)==2019)$ETo.DWD.ROTH.adj)/
        sum(filter(plotVARs,year(timestamp)==2019)$Precmm.ROTH),2)*100
#95%
round(sum(filter(plotVARs,year(timestamp)==2019)$SCOPE.ETo.ROTH.adj)/
        sum(filter(plotVARs,year(timestamp)==2019)$Precmm.ROTH),2)*100
#63%
round(sum(filter(plotVARs,year(timestamp)==2019)$SCOPE.DWD.ROTH.adj)/
        sum(filter(plotVARs,year(timestamp)==2019)$Precmm.ROTH),2)*100
#62%
round(sum(filter(plotVARs,year(timestamp)==2019)$SCOPE.RS.ROTH.adj)/
        sum(filter(plotVARs,year(timestamp)==2019)$Precmm.ROTH),2)*100
#65%
## gap-filling
round(sum(filter(plotVARs,year(timestamp)==2019 & ET.ROTH.filled>=0)$ET.ROTH.filled)/
        sum(filter(plotVARs,year(timestamp)==2019)$Precmm.ROTH),2)*100
#66%
round(sum(filter(plotVARs,year(timestamp)==2019)$ETo.DWD.TUCC.adj)/
        sum(filter(plotVARs,year(timestamp)==2019)$Precmm.TUCC),2)*100
#59%
round(sum(filter(plotVARs,year(timestamp)==2019)$SCOPE.ETo.TUCC.adj)/
        sum(filter(plotVARs,year(timestamp)==2019)$Precmm.TUCC),2)*100
#37%
round(sum(filter(plotVARs,year(timestamp)==2019)$SCOPE.DWD.TUCC.adj)/
        sum(filter(plotVARs,year(timestamp)==2019)$Precmm.TUCC),2)*100
#37%
round(sum(filter(plotVARs,year(timestamp)==2019)$SCOPE.RS.TUCC.adj)/
        sum(filter(plotVARs,year(timestamp)==2019)$Precmm.TUCC),2)*100
#38%
## gap-filling
round(sum(filter(plotVARs,year(timestamp)==2019 & ET.TUCC.filled>=0)$ET.TUCC.filled)/
        sum(filter(plotVARs,year(timestamp)==2019)$Precmm.TUCC),2)*100
#47%

plotVARs$month <- month(plotVARs$timestamp)
plotVARs$year <- year(plotVARs$timestamp)

sapply(1:12, FUN = function(i) round(sum(filter(plotVARs, month==i 
                                                & year==2019)$ETo.DWD.ROTH.adj, na.rm=T),2))
sapply(1:12, FUN = function(i) round(sum(filter(plotVARs, month==i 
                                                & year==2019)$SCOPE.ETo.ROTH.adjB, na.rm=T),2))
sapply(1:12, FUN = function(i) round(sum(filter(plotVARs, month==i 
                                                & year==2019)$SCOPE.DWD.ROTH.adjB, na.rm=T),2))
sapply(1:12, FUN = function(i) round(sum(filter(plotVARs, month==i 
                                                & year==2019)$SCOPE.RS.ROTH.adjB, na.rm=T),2))
sapply(1:12, FUN = function(i) round(sum(filter(plotVARs, month==i 
                                                & year==2019)$Precmm.ROTH, na.rm=T),2))
#          1    2     3     4     5     6     7     8     9     10    11    12
#[ETo]   5.48 14.22 21.15 53.32 53.19 87.53 67.22 63.66 36.59 18.04  5.69  5.83
#[SCOPE] 5.06 10.15 14.24 34.01 36.94 67.17 50.97 47.27 26.34 13.34  5.02  5.22
#[prec] 48.00 18.80 61.80  6.00 39.50 75.10 54.90 28.20 55.25 55.10 34.90 28.90

sapply(1:12, FUN = function(i) round(sum(filter(plotVARs, month==i 
                                                & year==2019)$ETo.DWD.TUCC.adj, na.rm=T),2))
sapply(1:12, FUN = function(i) round(sum(filter(plotVARs, month==i 
                                                & year==2019)$SCOPE.ETo.TUCC.adjB, na.rm=T),2))
sapply(1:12, FUN = function(i) round(sum(filter(plotVARs, month==i 
                                                & year==2019)$SCOPE.DWD.TUCC.adjB, na.rm=T),2))
sapply(1:12, FUN = function(i) round(sum(filter(plotVARs, month==i 
                                                & year==2019)$SCOPE.RS.TUCC.adjB, na.rm=T),2))
sapply(1:12, FUN = function(i) round(sum(filter(plotVARs, month==i 
                                                & year==2019)$Precmm.TUCC, na.rm=T),2))
#          1    2     3     4     5     6     7     8     9     10    11    12
#[ETo]   3.33  6.75  9.86 38.55 29.14 48.76 27.95 31.76 17.16 10.30  4.20  3.61
#[SCOPE] 2.70  5.28  6.42 24.75 19.18 35.32 22.98 23.89 12.42  7.20  2.91  2.51
#[prec] 40.80 21.40 62.10  7.50 39.50 26.10 25.20 10.30 55.20 47.80 37.85 27.20

#####################################################################
#####################################################################
