############################################################################
############################################################################

############################################################################
### atmospheric CO2 concentration 	              (Ca)	[ppm]
######################################################################
#### ICOS network ####################################################
#library(sp)
#spDistsN1(pts=matrix(c(14.07,13.315827,52.10,52.457232), ncol=2),
#          pt=c(13.315827,52.457232),longlat=TRUE)  # long lat ROTH
# 65km
#CO2 mixing ratio (dry mole fraction) [µmol mol-1] - height 40m
######################################################################
ICOS_LIN_40.1 <- read_delim("./data/ICOS_CO2data/ICOS_ATC_L2_L2-2020.1_LIN_40.0_CTS.CO2", 
                          ";", escape_double = FALSE, trim_ws = TRUE, 
                          skip = 40)
str(ICOS_LIN_40.1)

ICOS_LIN_40.2 <- read_delim("./data/ICOS_CO2data/ICOS_ATC_NRT_LIN_2020-06-01_2020-10-05_40.0_399.CO2", 
                          ";", escape_double = FALSE, trim_ws = TRUE, 
                          skip = 37)
str(ICOS_LIN_40.2)

ICOS_LIN_40 <- rbind(ICOS_LIN_40.1[,1:10],ICOS_LIN_40.2[,1:10])

ICOS_LIN_40$ymd <- as.POSIXct(paste(ICOS_LIN_40$Year, 
                                    ICOS_LIN_40$Month, 
                                    ICOS_LIN_40$Day, 
                                    sep = '-'))

ICOS_LIN_40$hms <- paste(ICOS_LIN_40$Hour, 
                         ICOS_LIN_40$Minute, 
                         "00", 
                         sep = ':')

ICOS_LIN_40$timestamp <- ymd_hms(paste(ICOS_LIN_40$ymd, 
                                       ICOS_LIN_40$hms, 
                                       sep = ' '))

tz(ICOS_LIN_40$timestamp)
ICOS_LIN_40 <- data.frame(na_if(ICOS_LIN_40, -999.990))
head(ICOS_LIN_40$timestamp)
tail(ICOS_LIN_40$timestamp)
ICOS_LIN_40$timestamp[40750:40755]
str(ICOS_LIN_40)

################################################################
DWD_ROTH <- left_join(DWD_ROTH, ICOS_LIN_40[,c(13,9)], by = "timestamp")
colnames(DWD_ROTH)[18] <- "CO2.ICOS"
summary(DWD_ROTH$CO2.ICOS)

DWD_ROTH19 <- left_join(DWD_ROTH19, ICOS_LIN_40[,c(13,9)], by = "timestamp")
colnames(DWD_ROTH19)[18] <- "CO2.ICOS"

DWD_TUCC <- left_join(DWD_TUCC, ICOS_LIN_40[,c(13,9)], by = "timestamp")
colnames(DWD_TUCC)[18] <- "CO2.ICOS"

DWD_TUCC19 <- left_join(DWD_TUCC19, ICOS_LIN_40[,c(13,9)], by = "timestamp")
colnames(DWD_TUCC19)[18] <- "CO2.ICOS"
summary(DWD_TUCC19$CO2.ICOS)

#################################################################
write.csv(DWD_ROTH, file="DWD_ROTH.csv", row.names = F)
write.csv(DWD_TUCC, file="DWD_TUCC.csv", row.names = F)

write.csv(DWD_TUCC19, file="DWD_TUCC19.csv", row.names = F)
write.csv(DWD_ROTH19, file="DWD_ROTH19.csv", row.names = F)
