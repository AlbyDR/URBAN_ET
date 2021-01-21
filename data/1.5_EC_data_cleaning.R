##############################################################################
library(readr)      # read file
library(dplyr)      # left_join / group_by
library(lubridate)  # time variable
library(MeTo)       # VP function
library(FREddyPro)  # cleanFluxes with quality flag - remove outlier and clean SD
library(openair)    # timeAverage
##############################################################################

##############################################################################
##############################################################################
# read Eddy flux tower data from ROTH-Steglitz
EC_ROTH <- read_csv("./data/UCO_ROTH_for_UWI_2018-06-01_2020-08-31.txt",
                        col_names = T)

originalnamesROTH <- colnames(EC_ROTH)

# rename the columns
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
tail(EC_ROTH[,1:5])
tz(EC_ROTH$timestamp)

##################################################################
#### clean fluxes
################################################################################
### variable required for the clen function
EC_ROTH$DOY <- yday(EC_ROTH$timestamp)
EC_ROTH$year <- year(EC_ROTH$timestamp)
EC_ROTH$month <- month(EC_ROTH$timestamp)
EC_ROTH$hour <- hour(EC_ROTH$timestamp)
EC_ROTH$VPD <-  (satVP(Tmean=EC_ROTH$Ta,interval='hour')*10)*(1-EC_ROTH$RH/100) 
EC_ROTH$date <- as.Date(EC_ROTH$timestamp)
EC_ROTH$time <- strftime(EC_ROTH$timestamp, format="%H:%M:%S", tz = "UTC")
EC_ROTH$Tau <- 0.226  
EC_ROTH$qc_Tau <- 0
EC_ROTH$h2o_flux <- 0.6118  
EC_ROTH$qc_h2o_flux <- 0  
################################################################################

##########################################################
############## cleaning by three methods
##########################################################
boxplot(EC_ROTH$LE, ylim=c(-100,1000),horizontal =T)
summary(EC_ROTH$LE)
# flag 2 and thresholdList from -100 to 1000
EC_ROTH_clean1 <- cleanFluxes(EC_ROTH, gas="LE", qcFlag=2,
                              thresholdList=list(LE=c(-100,1000)),
                              plot=FALSE, na.value="NA")
# standard deviation - 6sd
EC_ROTH_clean2=cleanFluxes(EC_ROTH, gas="LE", sdCor=TRUE, 
                               sdTimes=6, timesList=6,
                               distCor=TRUE, plot=FALSE, na.value="NA")
# first and second combined
EC_ROTH_clean3=cleanFluxes(EC_ROTH_clean1, gas="LE", sdCor=TRUE, 
                           sdTimes=6, timesList=6,
                           distCor=TRUE, plot=FALSE, na.value="NA")

summary(EC_ROTH_clean1$LE)
summary(EC_ROTH_clean2$LE)
summary(EC_ROTH_clean3$LE)

### include the cleaned variables to the data
str(EC_ROTH)
EC_ROTH <- data.frame(EC_ROTH[,1:48]) # excluding the fake variables
str(EC_ROTH)

# LE clean results  
EC_ROTH$LE.flag <- EC_ROTH_clean1$LE
EC_ROTH$LE.6sd <- EC_ROTH_clean2$LE
EC_ROTH$LE.flag.6sd <- EC_ROTH_clean3$LE

####################################################################
# DWD data - interpolation to 30 min - precipitation window
####################################################################
# read DWD data (result of the code 1.3)
DWD_ROTH <- read_csv("C:/Users/Alby Rocha/Documents/EC/DWDdata/DWD_ROTH.csv",col_names = T)
str(DWD_ROTH)

# timestamp should be named as "date" and wind direction as "wd"
colnames(DWD_ROTH)[c(1)] <- c("date") 
DWD_ROTH <- timeAverage(DWD_ROTH, vector.ws=TRUE,
                        avg.time="30 min",fill=TRUE) #

summary(DWD_ROTH$prec.window)
summary(DWD_ROTH$wd)

# remane date to timestamp again
colnames(DWD_ROTH)[c(1)] <- c("timestamp")

head(DWD_ROTH[1])
head(EC_ROTH[1])
tail(DWD_ROTH[1])
tail(EC_ROTH[1])
# include prec windom variable                
EC_ROTH$prec.window <- as.double(unlist(DWD_ROTH$prec.window[2:39505]))

##### cleaning the data based on precipitation events
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
