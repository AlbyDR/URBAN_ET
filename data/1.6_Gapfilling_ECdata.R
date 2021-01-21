########################################################################
library(REddyProc)  # gap-filling # see REddyProc typical workflow (help) or REddyProc::useCase
library(lubridate)  # year
library(dplyr)      # filter
library(MeTo)       # satVP
library(bigleaf)    # LE.to.ET
library(zoo)        # na.aprox
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
ET_ROTH.Filled <- na.approx(LE.to.ET(filter(EC_ROTH,year(timestamp)==2019)$LE.filled,
                                     filter(EC_ROTH,year(timestamp)==2019)$Ta)*3600)

ET_ROTH.Filled[which(ET_ROTH.Filled<=0)] <- 0
sum(ET_ROTH.Filled)
###################################################################
write.csv(EC_ROTH,
          #file="C:/Users/Alby Rocha/Documents/EC/FPmaps/data/EC_ROTH.csv", 
          row.names = F)
