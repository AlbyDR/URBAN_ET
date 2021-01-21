########################################################################
library(REddyProc)  # gap-filling # see REddyProc typical workflow (help) or REddyProc::useCase
library(lubridate)  # year
library(dplyr)      # filter
library(MeTo)       # satVP
library(bigleaf)    # LE.to.ET
library(zoo)        # na.aprox
library(ggplot2)    # plot
############################################################################
# get a database to copy the structure
############################################################################
#+++ Load data with 1 header and 1 unit row from (tab-delimited) text file
fileName <- getExamplePath('Example_DETha98.txt', isTryDownload = TRUE)
EddyData <- if (length(fileName)) fLoadTXTIntoDataframe(fileName) else
  # or use example dataset in RData format provided with REddyProc
  Example_DETha98
#+++ Replace long runs of equal NEE values by NA
EddyData <- filterLongRuns(EddyData, "NEE")
#+++ Add time stamp in POSIX time format
EddyDataWithPosix <- fConvertTimeToPosix(
  EddyData, 'YDH',Year = 'Year',Day = 'DoY', Hour = 'Hour') %>% 
  filterLongRuns("NEE")
############################################################################
############################################################################
# create a data.frame with the required variables as the example above
Gap_ROTH <- data.frame(DataTime=EC_ROTH$timestamp,
                       Year=EC_ROTH$year,
                       DoY=EC_ROTH$DOY,
                       Hour=EC_ROTH$hour,
                       NEE=EC_ROTH$CO2_sig_strgth_mean,
                       LE=EC_ROTH$LE.dry6.fsd, # cleaned LE
                       H=EC_ROTH$H,
                       Rg=EC_ROTH$Rin,
                       Tair=EC_ROTH$Ta,
                       Tsoil=EC_ROTH$Ta-5,
                       rH=EC_ROTH$RH,
                       VPD=satVP(Tmean=EC_ROTH$Ta,interval='hour')*10*(1-EC_ROTH$RH/100),
                       Ustar=sdClean(EC_ROTH$u.,3)) # excluding extreme values

colnames(Gap_ROTH) <- colnames(EddyDataWithPosix) 

#including data require to footprint 
Gap_ROTH$L=sdClean(EC_ROTH$L,3) # excluding extreme values
Gap_ROTH$wd=EC_ROTH$wd
Gap_ROTH$ws=sdClean(EC_ROTH$ws,5) # excluding extreme values
Gap_ROTH$v_var=sdClean(EC_ROTH$v_var,3) # excluding extreme values

# Variables for gapfilling
G_Roth <- sEddyProc$new('ROTH', Gap_ROTH, c('NEE','LE','Rg','Tair','VPD','Ustar',
                                             'v_var','ws','wd','L','rH'))
# set location                                             
G_Roth$sSetLocationInfo(LatDeg = 52.45, LongDeg = 13.32, TimeZoneHour = 0)  #Location of Gebesee

#++ Fill NEE gaps with MDS gap filling algorithm (without prior ustar filtering)
G_Roth$sMDSGapFill('LE', FillAll = F)
G_Roth$sMDSGapFill('L', FillAll = FALSE)
G_Roth$sMDSGapFill('ws', FillAll = FALSE)
G_Roth$sMDSGapFill('wd', FillAll = FALSE)
G_Roth$sMDSGapFill('v_var', FillAll = FALSE)
G_Roth$sMDSGapFill('Ustar', FillAll = FALSE)

#++ Export gap filled and partitioned data to standard data frame
FilledLE_Roth <- G_Roth$sExportResults()
#
#++ Example plots of filled data to screen or to directory \plots
G_Roth$sPlotFingerprintY('LE_orig', Year = 2019)
G_Roth$sPlotFingerprintY('LE_f', Year = 2019)

# including date variables
FilledLE_Roth$timestamp <- EC_ROTH$timestamp
FilledLE_Roth <- data.frame(FilledLE_Roth)
# including filled variables to EC_data
EC_ROTH$LE.filled <- FilledLE_Roth$LE_f
EC_ROTH$u.filled <- FilledLE_Roth$Ustar_f
EC_ROTH$L.filled <- FilledLE_Roth$L_f
EC_ROTH$ws.filled <- FilledLE_Roth$ws_f
EC_ROTH$wd.filled <- FilledLE_Roth$wd_f
EC_ROTH$v_var.filled <- FilledLE_Roth$v_var_f

summary(EC_ROTH)

# convert LE to ET
EC_ROTH$ET.filled <- LE.to.ET(EC_ROTH$LE.filled, EC_ROTH$Ta)*3600
EC_ROTH$ET.filled[which(EC_ROTH$ET.filled<=0)] <- 0

EC_ROTH$ET.filled[360:39400] <- na.approx(EC_ROTH$ET.filled[360:39400])
summary(filter(EC_ROTH, year==2019)$ET.filled)

# create a pallet for ET
ETcolor = c(rep("#4575b4",1),rep("#74add1",1),rep("#abd9e9",1), rep("#e0f3f8",1),
            rep("#ffffbf",1),rep("#fee090",1),rep("#fdae61",1),rep("#f46d43",1), 
            rep("#d73027",1),  rep("#d73027",1), rep("#a50026",1), rep("#a50026",1))

# plot 
ggplot(filter(EC_ROTH,year==2019)) +
  geom_raster(aes(x=DOY,y=hour,fill=ET.filled)) +
  scale_x_continuous("day of year (2019)",expand=c(0,0),breaks=seq(5,365,15),limits=c(1,365)) +
  scale_y_continuous("hour",expand=c(0,0),breaks=seq(0,24,2),limits=c(0,24)) +
  scale_fill_gradientn(breaks=seq(0,0.45,0.05),limits=c(0,0.45),
                       colors=ETcolor, name="ET [mm/hour]",na.value = NA,
                       guide =guide_colorbar(direction = "horizontal",
                                             title.vjust=1,
                                             label.vjust=16,
                                             title.position = "left",label.position = "bottom",
                                             barwidth = 20, barheight = 1,nbin = 20,
                                             label.theme = element_text(angle = 0))) + 
  theme(legend.position="bottom",
        panel.background = element_rect(fill = "white"),  
        panel.border = element_blank(), 
        panel.grid.major = element_line(colour="white"),
        panel.grid.minor = element_line(colour="white"))
###################################################################
write.csv(EC_ROTH,
          #file="C:/Users/Alby Rocha/Documents/EC/FPmaps/data/EC_ROTH.csv", 
          row.names = F)
