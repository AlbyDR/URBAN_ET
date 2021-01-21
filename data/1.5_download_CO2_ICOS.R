############################################################################
############################################################################
### atmospheric CO2 concentration 	              (Ca)	[ppm]
############################################################################
############################################################################
#### ICOS network ##########################################################
############################################################################
#library(sp)
#spDistsN1(pts=matrix(c(14.07,13.315827,52.10,52.457232), ncol=2),
#          pt=c(13.315827,52.457232),longlat=TRUE)  # long lat ROTH
# distance to ROTh 65km
# CO2 mixing ratio (dry mole fraction) [µmol mol-1] - height 40m
############################################################################

# downloaded from https://data.icos-cp.eu/portal/ - Lindenberg (LIN), Germany
# read file #1
ICOS_LIN_40.1 <- read_delim("./data/ICOS_CO2data/ICOS_ATC_L2_L2-2020.1_LIN_40.0_CTS.CO2", 
                          ";", escape_double = FALSE, trim_ws = TRUE,  skip = 40)
str(ICOS_LIN_40.1)

# read file #2
ICOS_LIN_40.2 <- read_delim("./data/ICOS_CO2data/ICOS_ATC_NRT_LIN_2020-06-01_2020-10-05_40.0_399.CO2", 
                          ";", escape_double = FALSE, trim_ws = TRUE, skip = 37)
str(ICOS_LIN_40.2)

# merge both
ICOS_LIN_40 <- rbind(ICOS_LIN_40.1[,1:10],ICOS_LIN_40.2[,1:10])
# merge date
ICOS_LIN_40$ymd <- as.POSIXct(paste(ICOS_LIN_40$Year, 
                                    ICOS_LIN_40$Month, 
                                    ICOS_LIN_40$Day, 
                                    sep = '-'))
# merge hour/minute/seconds
ICOS_LIN_40$hms <- paste(ICOS_LIN_40$Hour, 
                         ICOS_LIN_40$Minute, 
                         "00", 
                         sep = ':')
# create timestamp ymd_hms
ICOS_LIN_40$timestamp <- ymd_hms(paste(ICOS_LIN_40$ymd, 
                                       ICOS_LIN_40$hms, 
                                       sep = ' '))

tz(ICOS_LIN_40$timestamp) # see timezone
ICOS_LIN_40 <- data.frame(na_if(ICOS_LIN_40, -999.990)) # convert -999.990 to NA

head(ICOS_LIN_40$timestamp)
tail(ICOS_LIN_40$timestamp)
str(ICOS_LIN_40)

write.csv(ICOS_LIN_40, file="DWD_ROTH.csv", row.names = F)

#########################################################################
DWD_ROTH <- left_join(DWD_ROTH, ICOS_LIN_40[,c(13,9)], by = "timestamp")
colnames(DWD_ROTH)[18] <- "CO2.ICOS"
summary(DWD_ROTH$CO2.ICOS)
write.csv(DWD_ROTH, file="DWD_ROTH.csv", row.names = F)
#########################################################################
