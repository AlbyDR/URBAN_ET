##############################################################################
library(readr)      # read file
library(dplyr)      # left_join / group_by
library(lubridate)  # time variable
library(MeTo)       # VP function
library(FREddyPro)  # cleanFluxes with quality flag - remove outlier and clean SD
library(suncalc)    # getSunlightTimes
library(ggplot2)    # plot
library(openair)    # timeAverage
##############################################################################
#library(xts)       # time series
#library(TigR)      # time series apply.hourly function
#library(expss)    # apply_labels
#library(tidyverse)  # separate function
##############################################################################
##############################################################################
# read Eddy flux tower data from ROTH-Steglitz
##############################################################################
##############################################################################
EC_ROTH <- read_csv("./data/UCO_ROTH_for_UWI_2018-06-01_2020-08-31.txt",
                        col_names = T)

originalnamesROTH <- colnames(EC_ROTH)

colnames(EC_ROTH) <- list("timestamp","H","qc_H","LE","qc_LE",
                         "H2O_sig_strgth_mean","ET", "co2_flux", "qc_co2_flux",
                         "CO2_sig_strgth_mean","CO2",
                         "co2_mole_fraction_30","co2_mole_fraction_20",
                         "ws", "wd", "u.","L",
                         "monin_obukhov", "bowen_ratio","v_var","w_unrot",
                        "Ta","RH","p.EC", "V_precip",
                        "soil_temp_.05m","soil_temp_.1m","soil_temp_.2m",
                        "soil_temp_.3m","soil_temp_.5m","soil_temp_1m",
                        "smv_.1m","smv_.2m","smv_.3m", "smv_.4m",
                        "smv_.6m","smv_1m",
                         "SPN_TOTAL_Avg","diffuse_radiation",
                         "Rin","radiation.longwave.in",                 
                         "radiation.shortwave.out","Rli",
                         "net.downward.radiative")

str(EC_ROTH)
head(filter(EC_ROTH, year(timestamp)>=2018)[,1:5])
tail(EC_ROTH[,1:5])
tz(EC_ROTH$timestamp)

##################################################################
#### clean fluxes
##################################################################
EC_ROTH$DOY <- yday(EC_ROTH$timestamp)
EC_ROTH$year <- year(EC_ROTH$timestamp)
EC_ROTH$month <- month(EC_ROTH$timestamp)
EC_ROTH$hour <- hour(EC_ROTH$timestamp)
EC_ROTH$VPD <-  (satVP(Tmean=EC_ROTH$Ta,interval='hour')*10)*(1-EC_ROTH$RH/100) 
########################################################################
### variable for the clen function
EC_ROTH$date <- as.Date(EC_ROTH$timestamp)
EC_ROTH$time <- strftime(EC_ROTH$timestamp, format="%H:%M:%S", tz = "UTC")
EC_ROTH$Tau <- 0.226  
EC_ROTH$qc_Tau <- 0
EC_ROTH$h2o_flux <- 0.6118  
EC_ROTH$qc_h2o_flux <- 0  
##########################################################
##########################################################
##########################################################
############## cleaning by three methods
##########################################################
boxplot(EC_ROTH$LE, ylim=c(-100,1000),horizontal =T)
summary(EC_ROTH$LE)
# flag 2
EC_ROTH_clean1 <- cleanFluxes(EC_ROTH, gas="LE", qcFlag=2,
                              thresholdList=list(LE=c(-100,1000)),
                              plot=FALSE, na.value="NA")
summary(EC_ROTH_clean1$LE)

# standard deviation
EC_ROTH_clean2=cleanFluxes(EC_ROTH, gas="LE", sdCor=TRUE, 
                               sdTimes=6, timesList=6,
                               distCor=TRUE, plot=FALSE, na.value="NA")

summary(EC_ROTH_clean2$LE)

EC_ROTH_clean3=cleanFluxes(EC_ROTH_clean1, gas="LE", sdCor=TRUE, 
                           sdTimes=6, timesList=6,
                           distCor=TRUE, plot=FALSE, na.value="NA")

summary(EC_ROTH_clean1$LE)
summary(EC_ROTH_clean2$LE)
summary(EC_ROTH_clean3$LE)

### include the cleaned variables to the data
str(EC_ROTH)
EC_ROTH <- data.frame(EC_ROTH[,1:48])
str(EC_ROTH)

# LE clean results  
EC_ROTH$LE.flag <- EC_ROTH_clean1$LE
EC_ROTH$LE.6sd <- EC_ROTH_clean2$LE
EC_ROTH$LE.flag.6sd <- EC_ROTH_clean3$LE

#####################################################################
#plotMonthly(EC_ROTH, "ET",legend=TRUE,legendSide='bottomleft',type='o',lty=2,
#            ylim=c(-0.1,0.5),col=1,yaxt.in='n',yaxt.out='n',xaxt.in='n',xaxt.out='n',
#            at2.out=seq(-0.05,0.05,0.05),axis2.out=TRUE,at1.out=seq(0,23,2),axis1.out=TRUE,
#            at1.in=seq(0,23,2),axis1.in=TRUE)

#plotTimeseries(EC_ROTH,limList=list(DOY=c(1,365)),step=2,type='o',
#               cex=0.6,pch=1,lwd=0.5,lty=2)
#####################################################################
#summary(EC_ROTH$u.)
#EC_ROTH$u.3sd <- sdClean(EC_ROTH$u.,3)
#boxplot(EC_ROTH$u.3sd, horizontal=TRUE)

#summary(EC_ROTH$L)
#EC_ROTH$L.3sd <- sdClean(EC_ROTH$L,3)
#boxplot(EC_ROTH$L.3sd, horizontal=TRUE)
#EC_ROTH$L.out <- removeOutliers(EC_ROTH$L, na.rm = TRUE)
#boxplot(EC_ROTH$L.out, horizontal=TRUE)
#plot(EC_ROTH$L.out)

#summary(EC_ROTH$v_var)
#EC_ROTH$v_var.3sd <- sdClean(EC_ROTH$v_var,3)
#boxplot(EC_ROTH$v_var.3sd, horizontal=TRUE)

#summary(EC_ROTH$ws)
#EC_ROTH$ws.5sd <- sdClean(EC_ROTH$ws,5)
#boxplot(EC_ROTH$ws.5sd, horizontal=TRUE)
#summary(EC_ROTH$ws.5sd)

#summary(EC_ROTH$CO2)
#EC_ROTH$CO2.sd <- sdClean(EC_ROTH$CO2,3)
#summary(EC_ROTH$CO2.sd)

#str(EC_ROTH)
#summary(EC_ROTH)

###########################################################################33
# Calculated (VP function MeTo) - kPa 
#EC_ROTH$ea <- VP(Tmea=EC_ROTH$air_temperature, 
#                 Rhmean =EC_ROTH$RH, 
#                 P=EC_ROTH$air_pressure,
#                 interval = "hour")*10 # *10 to convert kPa to hPa

#EC_ROTH$SVP <- satVP(Tmean=EC_ROTH$air_temperature, interval = 'hour')*10
#EC_ROTH$VPD <- EC_ROTH$SVP*(1-EC_ROTH$RH/100)
#############################################################

####################################################################
# DWD interpolation to 30 min 
####################################################################
DWD_ROTH <- read_csv("C:/Users/Alby Rocha/Documents/EC/DWDdata/DWD_ROTH.csv",col_names = T)
str(DWD_ROTH)
# timestamp should be named as "date" and wind direction as "wd"
colnames(DWD_ROTH)[c(1)] <- c("date") 
DWD_ROTH <- timeAverage(DWD_ROTH, vector.ws=TRUE,
                        avg.time="30 min",fill=TRUE) #

summary(DWD_ROTH$prec.window)
summary(DWD_ROTH$wd)

str(DWD_ROTH)
colnames(DWD_ROTH)[c(1)] <- c("timestamp")

head(DWD_ROTH[1])
head(EC_ROTH[1])

tail(DWD_ROTH[1])
tail(EC_ROTH[1])
                  
EC_ROTH$prec.window <- as.double(unlist(DWD_ROTH$prec.window[2:39505]))

##### cleaning based on precipitation events
EC_ROTH$LE.dry <- EC_ROTH$LE
EC_ROTH$LE.dry[EC_ROTH$prec.window<=4 & EC_ROTH$qc_LE!=2] <- NA

EC_ROTH$LE.dry6.f <- EC_ROTH$LE.flag
EC_ROTH$LE.dry6.f[EC_ROTH$prec.window<=6] <- NA

EC_ROTH$LE.dry.fsd <- EC_ROTH$LE.flag.6sd
EC_ROTH$LE.dry.fsd[EC_ROTH$prec.window<=4] <- NA

EC_ROTH$LE.dry6.fsd <- EC_ROTH$LE.flag.6sd
EC_ROTH$LE.dry6.fsd[EC_ROTH$prec.window<=6] <- NA
######################################
summary(EC_ROTH)

write.csv(EC_ROTH, file="EC_ROTH.csv", row.names = F)

write.csv(EC_ROTH,
          file="C:/Users/Alby Rocha/Documents/EC/FPmaps/data/EC_ROTH.csv", 
          row.names = F)

#EC_ROTH19 <- filter(EC_ROTH, year(timestamp)==2019)
#write.csv(EC_ROTH19,
#          file="C:/Users/Alby Rocha/Documents/EC/FPmaps/data/EC_ROTH19.csv", 
#          row.names = F)
#########################################################################
#########################################################################
#### convert to 1 hour time series #########################################
#########################################################################
#########################################################################
EC_ROTHh <- EC_ROTH
colnames(EC_ROTHh)[c(1)] <- c("date")
EC_ROTHh <- timeAverage(EC_ROTHh, vector.ws=TRUE, avg.time="1 hour",fill=TRUE) #

colnames(EC_ROTHh)[c(1)] <- c("timestamp")
str(EC_ROTHh)

write.csv(EC_ROTHh,
          file="C:/Users/Alby Rocha/Documents/EC/SCOPEresults/data/EC_ROTHh.csv", 
          row.names = F)

EC_ROTH19h <- filter(EC_ROTHh, year(timestamp)==2019)
write.csv(EC_ROTH19h,
          file="C:/Users/Alby Rocha/Documents/EC/SCOPEresults/data/EC_ROTH19h.csv", 
          row.names = F)
###########################################################################33
############################################################################
#### Weather variables #####################################################   
############################################################################
##### colnames(EC_ROTH)                            Unit   Acroname
############################################################################
#[1]  "timestamp                                   (UTC)"                                                            
#[4]  "upward latent heat flux in air            (W m-2) (LE)"                                
#[7]  "evapotranspiration rate                  (mm h-1) (ET)"                                      
#[12] "wind speed                                (m s-1) (wind_speed)" 
#[13] "wind from direction                      (degree) (wind_dir)"                                    
#[20] "air temperature                              (°C) (T_40m_Avg)"                                     
#[21] "relative humidity                             (%) (RH_40m_Avg)"                                         
#[22] "air pressure                                (hPa) (air_pressure)"   
#[23] "lwe precipitation rate                   (mm h-1) (NS_Int_total)"  
#[25] "diffuse downwelling shortwave flux in air (W m-2) (SPN_DIFFUSE_Avg)" 
#[26] "downwelling shortwave flux in air         (W m-2) (Rs_downwell_Avg)"                
#########################################################################

