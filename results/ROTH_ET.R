#####################################################################
#SCOPE 2.0
################# analyses SCOPE results #######################################
############################################################################
############################################################################
library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(bigleaf) # LE to ET
library(water)
library(xts)
library(caret)
library(suncalc)
########################################################################
night <- getSunlightTimes(date = date(EC_ROTH19$timestamp[1:10225]), 
                          keep = c("nightEnd", "night"), 
                          lat = 52, lon = 13, tz = "UTC")
########################################################################
#### ROTH
ROTH_Results <- NULL
ROTH_Results$timestamp <- EC_ROTH19$timestamp[1:10225]
ROTH_Results$month <- month(EC_ROTH19$timestamp[1:10225], label=T)
ROTH_Results$day_night <- if_else(EC_ROTH19$timestamp[1:10225]>night$night | EC_ROTH19$timestamp[1:10225]<night$nightEnd,
                             "night", "day")
ROTH_Results$ET.obsO <- LE.to.ET(EC_ROTH19$LE[1:10225],EC_ROTH19$Ta[1:10225])*3600
ROTH_Results$ET.obsC <- LE.to.ET(EC_ROTH19$LE.dry.fsd[1:10225],EC_ROTH19$Ta[1:10225])*3600
ROTH_Results$ET.obsC[ROTH_Results$ET.obsC<=-0.0000000001] <- 0

ROTH_Results <- data.frame(ROTH_Results)

##### ETO
ROTH_Results$ETo  <- hourlyET(data.frame(wind=na.approx(Input_ROTH$u[1:10225]),
                                           RH=na.approx(Input_ROTH$RH[1:10225]), 
                                         temp=na.approx(Input_ROTH$Ta[1:10225]),
                                    radiation=na.approx(Input_ROTH$Rin[1:10225]), 
                                height=39.75, 
                                   lat=52.4537, 
                                  long=13.3017, 
                                   tz = "UTC",
                                  elev=50), 
                              hours=hour(ymd_hms(Input_ROTH$timestamp[1:10225], tz="UTC")) , 
                              DOY=yday(ymd_hms(Input_ROTH$timestamp[1:10225], tz="UTC")), 
                              ET="ETo")

ROTH_Results$ETo[ROTH_Results$ETo<=-0.0000000001] <- 0

ROTH_Results$ETo.EC  <- hourlyET(data.frame(wind=na.approx(EC_ROTH19$ws[1:10225]),
                                              RH=na.approx(EC_ROTH19$RH[1:10225]), 
                                            temp=na.approx(EC_ROTH19$Ta[1:10225]),
                                       radiation=na.approx(EC_ROTH19$Rin[1:10225]), 
                                    height=39.75, 
                                    lat=52.4537, 
                                    long=13.3017, 
                                    tz = "UTC",
                                    elev=50), 
                         hours=hour(ymd_hms(EC_ROTH19$timestamp[1:10225], tz="UTC")) , 
                         DOY=yday(ymd_hms(EC_ROTH19$timestamp[1:10225], tz="UTC")), 
                         ET="ETo")

ROTH_Results$ETo.EC[ROTH_Results$ETo.EC<=-0.0000000001] <- 0

##################################################################
##### correction fraction ########################################
##################################################################
#summary(FP_ROTH)
#ROTH_Results$day_night <- FP_ROTH$day_night[1:10225]
##################################################################
#veg.cover  =   "Veg_cover"/100, "Veg_cover_str.r"/100,
#                "Green_volume_r", "veg_cover_vh_old"
#impervious = "Impervious_r"/100 , "Impervious_With_str"/100,
#             "Built_up_area_r"/100 , "Unbuilt_imprev_r"/100,
#             "impervious_old"
##################################################################
summary(na.approx(filter(FP_ROTH, year(timestamp)==2019)$veg_cover_vh_old))
summary(na.approx(filter(FP_ROTH, year(timestamp)==2019)$Veg_cover))
summary(na.approx(filter(FP_ROTH, year(timestamp)==2019)$Veg_height_old))
summary(na.approx(filter(FP_ROTH, year(timestamp)==2019)$Impervious_With_str))

#FP_ROTH$veg.cover_vh[which(FP_ROTH$n_pixel<=200)] <- NA
#FP_ROTH$Veg_cover[which(FP_ROTH$n_pixel<=200)] <- NA
#FP_ROTH$impervious_old[which(FP_ROTH$n_pixel<=200)] <- NA
#FP_ROTH$Impervious_With_str[which(FP_ROTH$n_pixel<=200)] <- NA
#FP_ROTH$veg.cover_vh[which(FP_ROTH$distcentroid<=100)] <- NA
#FP_ROTH$Veg_cover[which(FP_ROTH$distcentroid<=100)] <- NA
#FP_ROTH$impervious_old[which(FP_ROTH$distcentroid<=100)] <- NA
#FP_ROTH$Impervious_With_str[which(FP_ROTH$distcentroid<=100)] <- NA

#hist(FP_ROTH$veg_cover_vh_old)
#FP_ROTH$veg.cover_vh[which(FP_ROTH$veg_cover_vh_old<=0.25)] <- NA
#FP_ROTH$veg.cover_vh[which(FP_ROTH$veg_cover_vh_old>=0.70)] <- NA

#hist(FP_ROTH$Veg_cover)
#FP_ROTH$Veg_cover[which(FP_ROTH$Veg_cover<=25)] <- NA
#FP_ROTH$Veg_cover[which(FP_ROTH$Veg_cover>=70)] <- NA

#hist(FP_ROTH$impervious_old)
#FP_ROTH$impervious_old[which(FP_ROTH$impervious_old<=0.25)] <- NA
#FP_ROTH$impervious_old[which(FP_ROTH$impervious_old>=0.75)] <- NA

#hist(FP_ROTH$Impervious_With_str)
#FP_ROTH$Impervious_With_str[which(FP_ROTH$Impervious_With_str<=25)] <- NA
#FP_ROTH$Impervious_With_str[which(FP_ROTH$Impervious_With_str>=75)] <- NA

#################################

ROTH_Results$ETo_adjA <- ROTH_Results$ETo*na.approx(FP_ROTH$veg_cover_vh_old[1:10225])
ROTH_Results$ETo_adjB <- ROTH_Results$ETo*na.approx(FP_ROTH$Veg_cover[1:10225]/100)
ROTH_Results$ETo_adjC <- ROTH_Results$ETo*na.approx(1-(FP_ROTH$impervious_old[1:10225]))
ROTH_Results$ETo_adjD <- ROTH_Results$ETo*na.approx(1-(FP_ROTH$Impervious_With_str[1:10225]/100))
ROTH_Results$ETo_adjAN <- ifelse(ROTH_Results$day_night=="night",
                            ROTH_Results$ETo,
                            ROTH_Results$ETo*na.approx(FP_ROTH$veg_cover_vh_old[1:10225]))
ROTH_Results$ETo_adjBN <- ifelse(ROTH_Results$day_night=="night",
                           ROTH_Results$ETo,
                           ROTH_Results$ETo*na.approx(FP_ROTH$Veg_cover[1:10225]/100))
ROTH_Results$ETo_adjCN <- ifelse(ROTH_Results$day_night=="night",
                           ROTH_Results$ETo,
                           ROTH_Results$ETo*na.approx(1-(FP_ROTH$impervious_old[1:10225])))
ROTH_Results$ETo_adjDN <- ifelse(ROTH_Results$day_night=="night",
                           ROTH_Results$ETo,
                          ROTH_Results$ETo*na.approx(1-(FP_ROTH$Impervious_With_str[1:10225]/100)))

summary(ROTH_Results)

ROTH_Results$ETo.EC_adjB <- ROTH_Results$ETo.EC*na.approx(FP_ROTH$Veg_cover[1:10225]/100)

Metric_ROTH <- list(
  data.frame(
    "R2"= defaultSummary(na.omit(data.frame(obs=ROTH_Results$ET.obsC,pred=ROTH_Results$ETo)))["Rsquared"],
    "RMSE"= defaultSummary(na.omit(data.frame(obs=ROTH_Results$ET.obsC,pred=ROTH_Results$ETo)))["RMSE"],
    "rbias"= sum(ROTH_Results$ETo-ROTH_Results$ET.obsC,na.rm=T)/sum(ROTH_Results$ET.obsC,na.rm=T),
    row.names = "ETo"),
  data.frame(
    "R2"= defaultSummary(na.omit(data.frame(obs=ROTH_Results$ET.obsC,pred=ROTH_Results$ETo_adjA)))["Rsquared"],
    "RMSE"= defaultSummary(na.omit(data.frame(obs=ROTH_Results$ET.obsC,pred=ROTH_Results$ETo_adjA)))["RMSE"],
    "rbias"= sum(ROTH_Results$ETo_adjA-ROTH_Results$ET.obsC,na.rm=T)/sum(ROTH_Results$ET.obsC,na.rm=T),
    row.names = "ETo_adjA"),
  data.frame(
    "R2"= defaultSummary(na.omit(data.frame(obs=ROTH_Results$ET.obsC,pred=ROTH_Results$ETo_adjB)))["Rsquared"],
    "RMSE"= defaultSummary(na.omit(data.frame(obs=ROTH_Results$ET.obsC,pred=ROTH_Results$ETo_adjB)))["RMSE"],
    "rbias"= sum(ROTH_Results$ETo_adjB-ROTH_Results$ET.obsC,na.rm=T)/sum(ROTH_Results$ET.obsC,na.rm=T),
    row.names = "ETo_adjB"),
  data.frame(
    "R2"= defaultSummary(na.omit(data.frame(obs=ROTH_Results$ET.obsC,pred=ROTH_Results$ETo.EC_adjB)))["Rsquared"],
    "RMSE"= defaultSummary(na.omit(data.frame(obs=ROTH_Results$ET.obsC,pred=ROTH_Results$ETo.EC_adjB)))["RMSE"],
    "rbias"= sum(ROTH_Results$ETo.EC_adjB-ROTH_Results$ET.obsC,na.rm=T)/sum(ROTH_Results$ET.obsC,na.rm=T),
    row.names = "ETo.EC_adjB"),
  data.frame(
    "R2"= defaultSummary(na.omit(data.frame(obs=ROTH_Results$ET.obsC,pred=ROTH_Results$ETo_adjC)))["Rsquared"],
    "RMSE"= defaultSummary(na.omit(data.frame(obs=ROTH_Results$ET.obsC,pred=ROTH_Results$ETo_adjC)))["RMSE"],
    "rbias"= sum(ROTH_Results$ETo_adjC-ROTH_Results$ET.obsC,na.rm=T)/sum(ROTH_Results$ET.obsC,na.rm=T),
    row.names = "ETo_adjC"),
  data.frame(
    "R2"= defaultSummary(na.omit(data.frame(obs=ROTH_Results$ET.obsC,pred=ROTH_Results$ETo_adjD)))["Rsquared"],
    "RMSE"= defaultSummary(na.omit(data.frame(obs=ROTH_Results$ET.obsC,pred=ROTH_Results$ETo_adjD)))["RMSE"],
    "rbias"= sum(ROTH_Results$ETo_adjD-ROTH_Results$ET.obsC,na.rm=T)/sum(ROTH_Results$ET.obsC,na.rm=T),
    row.names = "ETo_adjD"),
  data.frame(
    "R2"= defaultSummary(na.omit(data.frame(obs=ROTH_Results$ET.obsC,pred=ROTH_Results$ETo_adjAN)))["Rsquared"],
    "RMSE"= defaultSummary(na.omit(data.frame(obs=ROTH_Results$ET.obsC,pred=ROTH_Results$ETo_adjAN)))["RMSE"],
    "rbias"= sum(ROTH_Results$ETo_adjAN-ROTH_Results$ET.obsC,na.rm=T)/sum(ROTH_Results$ET.obsC,na.rm=T),
    row.names = "ETo_adjAN"),
  data.frame(
    "R2"= defaultSummary(na.omit(data.frame(obs=ROTH_Results$ET.obsC,pred=ROTH_Results$ETo_adjBN)))["Rsquared"],
    "RMSE"= defaultSummary(na.omit(data.frame(obs=ROTH_Results$ET.obsC,pred=ROTH_Results$ETo_adjBN)))["RMSE"],
    "rbias"= sum(ROTH_Results$ETo_adjBN-ROTH_Results$ET.obsC,na.rm=T)/sum(ROTH_Results$ET.obsC,na.rm=T),
    row.names = "ETo_adjBN"),
  data.frame(
    "R2"= defaultSummary(na.omit(data.frame(obs=ROTH_Results$ET.obsC,pred=ROTH_Results$ETo_adjCN)))["Rsquared"],
    "RMSE"= defaultSummary(na.omit(data.frame(obs=ROTH_Results$ET.obsC,pred=ROTH_Results$ETo_adjCN)))["RMSE"],
    "rbias"= sum(ROTH_Results$ETo_adjCN-ROTH_Results$ET.obsC,na.rm=T)/sum(ROTH_Results$ET.obsC,na.rm=T),
    row.names = "ETo_adjCN"),
  data.frame(
    "R2"= defaultSummary(na.omit(data.frame(obs=ROTH_Results$ET.obsC,pred=ROTH_Results$ETo_adjDN)))["Rsquared"],
    "RMSE"= defaultSummary(na.omit(data.frame(obs=ROTH_Results$ET.obsC,pred=ROTH_Results$ETo_adjDN)))["RMSE"],
    "rbias"= sum(ROTH_Results$ETo_adjDN-ROTH_Results$ET.obsC,na.rm=T)/sum(ROTH_Results$ET.obsC,na.rm=T),
    row.names = "ETo_adjDN")
)

ETo_ROTH <- matrix(unlist(Metric_ROTH), nrow=3)

colnames(ETo_ROTH) <- c("ETo_orig","ETo_AdjA","ETo_AdjB",
                        "ETo.EC_AdjB","ETo_AdjC","ETo_AdjD",
                        "AdjAN","AdjBN","AdjCN","AdjDN")
                        
rownames(ETo_ROTH) <- c("R2", "RMSE","Rbias")

t(round(ETo_ROTH,4))

##########################################################################
######## SCOPE results ###################################################
##########################################################################
SCOPE_ET_ROTH <- NULL

for (i in 1:length(SCOPE_ROTH)) {
SCOPE_ET_ROTH[[i]] <- data.frame(
 "timestamp"=Input_ROTH$timestamp[1:10225],
 "SCOPE_ET"=LE.to.ET(SCOPE_ROTH[[i]]$lEtot,Input_ROTH$Ta[1:10225])*3600,
 "SCOPE_ETsoil"=LE.to.ET(SCOPE_ROTH[[i]]$lEstot,Input_ROTH$Ta[1:10225])*3600,
 "SCOPE_ETcanopy"=LE.to.ET(SCOPE_ROTH[[i]]$lEctot,Input_ROTH$Ta[1:10225])*3600,
 "SCOPE_ET_adjA"=NA,
 "SCOPE_ET_adjB"=NA,
 "SCOPE_ET_adjC"=NA,
 "SCOPE_ET_adjD"=NA,
 "SCOPE_ET_adjAC"=NA,
 "SCOPE_ET_adjAD"=NA,
 "SCOPE_ET_adjBC"=NA,
 "SCOPE_ET_adjBD"=NA
 )
SCOPE_ET_ROTH[[i]]$SCOPE_ET[SCOPE_ET_ROTH[[i]]$SCOPE_ET<=-0.0000000001] <- 0
SCOPE_ET_ROTH[[i]]$SCOPE_ETsoil[SCOPE_ET_ROTH[[i]]$SCOPE_ETsoil<=-0.0000000001] <- 0
SCOPE_ET_ROTH[[i]]$SCOPE_ETcanopy[SCOPE_ET_ROTH[[i]]$SCOPE_ETcanopy<=-0.0000000001] <- 0
}

summary(SCOPE_ET_ROTH[[4]])

for (i in 1:length(SCOPE_ROTH)) {
SCOPE_ET_ROTH[[i]]$SCOPE_ET_adjA <- SCOPE_ET_ROTH[[i]]$SCOPE_ET*na.approx(
                                                  FP_ROTH$veg_cover_vh_old[1:10225])
SCOPE_ET_ROTH[[i]]$SCOPE_ET_adjB <- SCOPE_ET_ROTH[[i]]$SCOPE_ET*na.approx(
                                                  FP_ROTH$Veg_cover[1:10225]/100)
SCOPE_ET_ROTH[[i]]$SCOPE_ET_adjC <- SCOPE_ET_ROTH[[i]]$SCOPE_ET*na.approx(
                                               1-(FP_ROTH$impervious_old[1:10225]))
SCOPE_ET_ROTH[[i]]$SCOPE_ET_adjD <- SCOPE_ET_ROTH[[i]]$SCOPE_ET*na.approx(
                                               1-(FP_ROTH$Impervious_With_str[1:10225]/100))
SCOPE_ET_ROTH[[i]]$SCOPE_ET_adjAC <- SCOPE_ET_ROTH[[i]]$SCOPE_ETsoil*na.approx(1-(FP_ROTH$impervious_old[1:10225]))+
                                     SCOPE_ET_ROTH[[i]]$SCOPE_ETcanopy*na.approx(FP_ROTH$veg_cover_vh_old[1:10225])
SCOPE_ET_ROTH[[i]]$SCOPE_ET_adjAD <- SCOPE_ET_ROTH[[i]]$SCOPE_ETsoil*na.approx(1-(FP_ROTH$Impervious_With_str[1:10225]/100))+
                                     SCOPE_ET_ROTH[[i]]$SCOPE_ETcanopy*na.approx(FP_ROTH$veg_cover_vh_old[1:10225])
SCOPE_ET_ROTH[[i]]$SCOPE_ET_adjBC <- SCOPE_ET_ROTH[[i]]$SCOPE_ETsoil*na.approx(1-(FP_ROTH$impervious_old[1:10225]))+
                                     SCOPE_ET_ROTH[[i]]$SCOPE_ETcanopy*na.approx(FP_ROTH$Veg_cover[1:10225]/100)
SCOPE_ET_ROTH[[i]]$SCOPE_ET_adjBD <- SCOPE_ET_ROTH[[i]]$SCOPE_ETsoil*na.approx(1-(FP_ROTH$Impervious_With_str[1:10225]/100))+
                                     SCOPE_ET_ROTH[[i]]$SCOPE_ETcanopy*na.approx(FP_ROTH$Veg_cover[1:10225]/100)
}

summary(SCOPE_ET_ROTH[[50]])

##########################################################################
#### calculate the metric ################################################
##########################################################################
SCOPE_Metric_ROTH <- NULL

for (i in 1:length(SCOPE_ET_ROTH)) {
  SCOPE_Metric_ROTH[[i]] <- list(
  matrix(c(defaultSummary(na.omit(data.frame(
                  obs=filter(ROTH_Results, year(timestamp)==2019)$ET.obsC,
                  pred=filter(SCOPE_ET_ROTH[[i]],year(timestamp)==2019)[[2]])))["Rsquared"],
           defaultSummary(na.omit(data.frame(
                  obs=filter(ROTH_Results, year(timestamp)==2019)$ET.obsC,
                  pred=filter(SCOPE_ET_ROTH[[i]],year(timestamp)==2019)[[2]])))["RMSE"],
           sum(filter(SCOPE_ET_ROTH[[i]], year(timestamp)==2019)[[2]]-
                 filter(ROTH_Results, year(timestamp)==2019)$ET.obsC,na.rm=T)/
             sum(filter(ROTH_Results, year(timestamp)==2019)$ET.obsC,na.rm=T),
           
           defaultSummary(na.omit(data.frame(
             obs=filter(ROTH_Results, year(timestamp)==2019)$ET.obsC,
             pred=filter(SCOPE_ET_ROTH[[i]], year(timestamp)==2019)[[5]])))["Rsquared"],
           defaultSummary(na.omit(data.frame(
             obs=filter(ROTH_Results, year(timestamp)==2019)$ET.obsC,
             pred=filter(SCOPE_ET_ROTH[[i]], year(timestamp)==2019)[[5]])))["RMSE"],
           sum(filter(SCOPE_ET_ROTH[[i]], year(timestamp)==2019)[[5]]-
                 filter(ROTH_Results, year(timestamp)==2019)$ET.obsC,na.rm=T)/
             sum(filter(ROTH_Results, year(timestamp)==2019)$ET.obsC,na.rm=T),
           
           defaultSummary(na.omit(data.frame(
             obs=filter(ROTH_Results, year(timestamp)==2019)$ET.obsC,
             pred=filter(SCOPE_ET_ROTH[[i]], year(timestamp)==2019)[[6]])))["Rsquared"],
           defaultSummary(na.omit(data.frame(
             obs=filter(ROTH_Results, year(timestamp)==2019)$ET.obsC,
             pred=filter(SCOPE_ET_ROTH[[i]], year(timestamp)==2019)[[6]])))["RMSE"],
           sum(filter(SCOPE_ET_ROTH[[i]], year(timestamp)==2019)[[6]]-
                 filter(ROTH_Results, year(timestamp)==2019)$ET.obsC,na.rm=T)/
             sum(filter(ROTH_Results, year(timestamp)==2019)$ET.obsC,na.rm=T),
           
           defaultSummary(na.omit(data.frame(
             obs=filter(ROTH_Results, year(timestamp)==2019)$ET.obsC,
             pred=filter(SCOPE_ET_ROTH[[i]], year(timestamp)==2019)[[7]])))["Rsquared"],
           defaultSummary(na.omit(data.frame(
             obs=filter(ROTH_Results, year(timestamp)==2019)$ET.obsC,
             pred=filter(SCOPE_ET_ROTH[[i]], year(timestamp)==2019)[[7]])))["RMSE"],
           sum(filter(SCOPE_ET_ROTH[[i]], year(timestamp)==2019)[[7]]-
                 filter(ROTH_Results, year(timestamp)==2019)$ET.obsC,na.rm=T)/
             sum(filter(ROTH_Results, year(timestamp)==2019)$ET.obsC,na.rm=T),
           
           defaultSummary(na.omit(data.frame(
             obs=filter(ROTH_Results, year(timestamp)==2019)$ET.obsC,
             pred=filter(SCOPE_ET_ROTH[[i]], year(timestamp)==2019)[[8]])))["Rsquared"],
           defaultSummary(na.omit(data.frame(
             obs=filter(ROTH_Results, year(timestamp)==2019)$ET.obsC,
             pred=filter(SCOPE_ET_ROTH[[i]], year(timestamp)==2019)[[8]])))["RMSE"],
           sum(filter(SCOPE_ET_ROTH[[i]], year(timestamp)==2019)[[8]]-
                 filter(ROTH_Results, year(timestamp)==2019)$ET.obsC,na.rm=T)/
             sum(filter(ROTH_Results, year(timestamp)==2019)$ET.obsC,na.rm=T),
           
           defaultSummary(na.omit(data.frame(
             obs=filter(ROTH_Results, year(timestamp)==2019)$ET.obsC,
             pred=filter(SCOPE_ET_ROTH[[i]], year(timestamp)==2019)[[9]])))["Rsquared"],
           defaultSummary(na.omit(data.frame(
             obs=filter(ROTH_Results, year(timestamp)==2019)$ET.obsC,
             pred=filter(SCOPE_ET_ROTH[[i]], year(timestamp)==2019)[[9]])))["RMSE"],
           sum(filter(SCOPE_ET_ROTH[[i]], year(timestamp)==2019)[[9]]-
                 filter(ROTH_Results, year(timestamp)==2019)$ET.obsC,na.rm=T)/
             sum(filter(ROTH_Results, year(timestamp)==2019)$ET.obsC,na.rm=T),
           
           defaultSummary(na.omit(data.frame(
             obs=filter(ROTH_Results, year(timestamp)==2019)$ET.obsC,
             pred=filter(SCOPE_ET_ROTH[[i]], year(timestamp)==2019)[[10]])))["Rsquared"],
           defaultSummary(na.omit(data.frame(
             obs=filter(ROTH_Results, year(timestamp)==2019)$ET.obsC,
             pred=filter(SCOPE_ET_ROTH[[i]], year(timestamp)==2019)[[10]])))["RMSE"],
           sum(filter(SCOPE_ET_ROTH[[i]], year(timestamp)==2019)[[10]]-
                 filter(ROTH_Results, year(timestamp)==2019)$ET.obsC,na.rm=T)/
             sum(filter(ROTH_Results, year(timestamp)==2019)$ET.obsC,na.rm=T),
           
           defaultSummary(na.omit(data.frame(
             obs=filter(ROTH_Results, year(timestamp)==2019)$ET.obsC,
             pred=filter(SCOPE_ET_ROTH[[i]], year(timestamp)==2019)[[11]])))["Rsquared"],
           defaultSummary(na.omit(data.frame(
             obs=filter(ROTH_Results, year(timestamp)==2019)$ET.obsC,
             pred=filter(SCOPE_ET_ROTH[[i]], year(timestamp)==2019)[[11]])))["RMSE"],
           sum(filter(SCOPE_ET_ROTH[[i]], year(timestamp)==2019)[[11]]-
                 filter(ROTH_Results, year(timestamp)==2019)$ET.obsC,na.rm=T)/
             sum(filter(ROTH_Results, year(timestamp)==2019)$ET.obsC,na.rm=T),
           
           defaultSummary(na.omit(data.frame(
             obs=filter(ROTH_Results, year(timestamp)==2019)$ET.obsC,
             pred=filter(SCOPE_ET_ROTH[[i]], year(timestamp)==2019)[[12]])))["Rsquared"],
           defaultSummary(na.omit(data.frame(
             obs=filter(ROTH_Results, year(timestamp)==2019)$ET.obsC,
             pred=filter(SCOPE_ET_ROTH[[i]], year(timestamp)==2019)[[12]])))["RMSE"],
           sum(filter(SCOPE_ET_ROTH[[i]], year(timestamp)==2019)[[12]]-
                 filter(ROTH_Results, year(timestamp)==2019)$ET.obsC,na.rm=T)/
             sum(filter(ROTH_Results, year(timestamp)==2019)$ET.obsC,na.rm=T)
  ), nrow=3))

colnames(SCOPE_Metric_ROTH[[i]][[1]]) <- c("SCOPE_orig","AdjA","AdjB","AdjC","AdjD",
                                           "AdjAC","AdjAD","AdjBC","AdjBD")
rownames(SCOPE_Metric_ROTH[[i]][[1]]) <- c("R2", "RMSE","Rbias")
}

SCOPE_Metric_ROTH[[70]]

#######################################################
#######################################################
#######################################################
for (i in 1:length(SCOPE_ET_ROTH)) {
  ifelse(which(SCOPE_Metric_ROTH[[i]][[1]][1,]>=0.82),
print(i),0)
}

for (i in 1:length(SCOPE_ET_ROTH)) {
  ifelse(which(SCOPE_Metric_ROTH[[i]][[1]][2,]<=0.027),
         print(i),0)
}

for (i in 1:length(SCOPE_ET_ROTH)) {
  ifelse(which(abs(SCOPE_Metric_ROTH[[i]][[1]][3,])<=0.05),
         print(i),0)
}
#######################################################
SCOPE_Metric_ROTH[c(-2,-12,-19,-20,-23,-37,-40,-42,-49,-50,-56,-57,-58,-59,-60)]

SCOPE_Metric_ROTH[c(47,65,74)]
Parameters_ROTH[c(47,65)]
#######################################################
# R2 per month
month_R2_ROTH <- matrix(nrow=length(SCOPE_ET_ROTH),ncol=12)

for(i in 1:length(SCOPE_ET_ROTH)) {
  for(j in 1:12) {
    month_R2_ROTH[i,j] <- 
    defaultSummary(na.omit(data.frame(
      obs=filter(ROTH_Results, year(timestamp)==2019 & month(timestamp)==j)$ET.obsC,
      pred=filter(SCOPE_ET_ROTH[[i]], year(timestamp)==2019 & month(timestamp)==j)[[5]]
                  )))["Rsquared"]
  }
}

plot(month_R2_ROTH[1,], ylim = c(0,1))
for(i in 1:length(SCOPE_ET_ROTH)) {
  lines(month_R2_ROTH[i,])
}

month_R2_ROTH <- data.frame(t(month_R2_ROTH))
month_R2_ROTH$month <- 1:12

ggplot(month_R2_ROTH, aes(x=month))+
  geom_line(aes(y=X3), colour="red")+
  geom_line(aes(y=X70), colour="blue")+
  geom_line(aes(y=X71), colour="green") +
  scale_y_continuous(name="Rsquared" ,limits=c(0,1))+
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12),
                     labels=c("Jan","Fev","Mar","Apr","May","Jun",
                              "Jul","Ago","Set","Oct", "Nov","Dec"))
  
# RMSE per month
month_RMSE_ROTH <- matrix(nrow=length(SCOPE_ET_ROTH),ncol=12)

for(i in 1:length(SCOPE_ET_ROTH)) {
  for(j in 1:12) {
    month_RMSE_ROTH[i,j] <- 
      defaultSummary(na.omit(data.frame(
        obs=filter(ROTH_Results, year(timestamp)==2019 & month(timestamp)==j)$ET.obsC,
        pred=filter(SCOPE_ET_ROTH[[i]], year(timestamp)==2019 & month(timestamp)==j)[[5]]
      )))["RMSE"]
  }
}

plot(month_RMSE_ROTH[1,], ylim = c(0,0.12))
for(i in 1:length(SCOPE_ET_ROTH)) {
  lines(month_RMSE_ROTH[i,])
}

for(i in 1:12) {
print(
which.min(month_RMSE_ROTH[,i]))
}

month_RMSE_ROTH <- data.frame(t(month_RMSE_ROTH))
month_RMSE_ROTH$month <- 1:12

ggplot(month_RMSE_ROTH, aes(x=month))+
  geom_line(aes(y=X3), colour="red")+
  geom_line(aes(y=X70), colour="blue")+
  geom_line(aes(y=X71), colour="green") +
  scale_y_continuous(name="RMSE" ,limits=c(0,0.05))+
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12),
                     labels=c("Jan","Fev","Mar","Apr","May","Jun",
                              "Jul","Ago","Set","Oct", "Nov","Dec"))

#metricETo_RMSE_ROTH <- matrix(nrow=1,ncol=12)

#for(i in 1:1) {
#  for(j in 1:12) {
#    metricETo_RMSE_ROTH[i,j] <- 
#      defaultSummary(na.omit(data.frame(
#        obs=filter(ROTH_Results, year(timestamp)==2019 & month(timestamp)==j)$ET.obsC,
#        pred=filter(ROTH_Results, year(timestamp)==2019 & month(timestamp)==j)$ETo_adjB
#      )))["RMSE"]
#  }
#}

#metricETo_RMSE_ROTH <- data.frame(t(metricETo_RMSE_ROTH))


# Rbias per month
month_Rbias_ROTH <- matrix(nrow=length(SCOPE_ET_ROTH),ncol=12)

for(i in 1:length(SCOPE_ET_ROTH)) {
  for(j in 1:12) {
    month_Rbias_ROTH[i,j] <- 
      sum(filter(SCOPE_ET_ROTH[[i]], year(timestamp)==2019 & month(timestamp)==j)[[5]]-
          filter(ROTH_Results, year(timestamp)==2019 & month(timestamp)==j)$ET.obsC,na.rm=T)/
      sum(filter(ROTH_Results, year(timestamp)==2019 & month(timestamp)==j)$ET.obsC,na.rm=T)
  }
}

plot(month_Rbias_ROTH[1,], ylim = c(-1,1))
for(i in 1:length(SCOPE_ET_ROTH)) {
  lines(month_Rbias_ROTH[i,])
}

for(i in 1:12) {
  print(i)
  print(
    which(month_Rbias_ROTH[,i]>-0.05 & month_Rbias_ROTH[,i]<0.05))
}

month_Rbias_ROTH <- data.frame(t(month_Rbias_ROTH))
month_Rbias_ROTH$month <- 1:12

ggplot(month_Rbias_ROTH, aes(x=month))+
  geom_line(aes(y=X3), colour="black")+
  geom_line(aes(y=X70), colour="blue")+
  geom_line(aes(y=X71), colour="green") +
  geom_hline(yintercept = 0, colour="red", linetype="dotted")+
  scale_y_continuous(name="Rbias" ,limits=c(-1.0,1))+
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12),
                     labels=c("Jan","Fev","Mar","Apr","May","Jun",
                              "Jul","Ago","Set","Oct", "Nov","Dec"))

# night and day
for (i in 1:length(SCOPE_ET_ROTH)) {
  SCOPE_ET_ROTH[[i]]$day_night <- ROTH_Results$day_night[1:10225]
}

SCOPE_ET_ROTH[[72]]

day_night_ROTH <- matrix(nrow=length(SCOPE_ET_ROTH),ncol=6)

for(i in 1:length(SCOPE_ET_ROTH)) {
  day_night_ROTH[i,1] <- 
      defaultSummary(na.omit(data.frame(
        obs=filter(ROTH_Results, year(timestamp)==2019 & day_night=="night")$ET.obsC,
        pred=filter(SCOPE_ET_ROTH[[i]], year(timestamp)==2019 & day_night=="night")[[5]]
      )))["RMSE"]
  day_night_ROTH[i,2] <- 
      defaultSummary(na.omit(data.frame(
        obs=filter(ROTH_Results, year(timestamp)==2019 & day_night=="day")$ET.obsC,
        pred=filter(SCOPE_ET_ROTH[[i]], year(timestamp)==2019 & day_night=="day")[[5]]
      )))["RMSE"]
#  day_night_ROTH[i,3] <- 
#      defaultSummary(na.omit(data.frame(
#        obs=filter(ROTH_Results, year(timestamp)==2019 & day_night=="Dawn_Dusk")$ET.obsC,
#        pred=filter(SCOPE_ET_ROTH[[i]], year(timestamp)==2019 & day_night=="Dawn_Dusk")[[5]]
#      )))["RMSE"]
  day_night_ROTH[i,3] <- 
      defaultSummary(na.omit(data.frame(
        obs=filter(ROTH_Results, year(timestamp)==2019 & day_night=="night")$ET.obsC,
        pred=filter(SCOPE_ET_ROTH[[i]], year(timestamp)==2019 & day_night=="night")[[5]]
      )))["Rsquared"]
  day_night_ROTH[i,4] <- 
      defaultSummary(na.omit(data.frame(
        obs=filter(ROTH_Results, year(timestamp)==2019 & day_night=="day")$ET.obsC,
        pred=filter(SCOPE_ET_ROTH[[i]], year(timestamp)==2019 & day_night=="day")[[5]]
      )))["Rsquared"]
#  day_night_ROTH[i,6] <- 
#      defaultSummary(na.omit(data.frame(
#        obs=filter(ROTH_Results, year(timestamp)==2019 & day_night=="Dawn_Dusk")$ET.obsC,
#        pred=filter(SCOPE_ET_ROTH[[i]], year(timestamp)==2019 & day_night=="Dawn_Dusk")[[5]]
#      )))["Rsquared"]
  day_night_ROTH[i,5] <- 
      sum(filter(SCOPE_ET_ROTH[[i]], year(timestamp)==2019 & day_night=="night")[[5]]-
          filter(ROTH_Results, year(timestamp)==2019 & day_night=="night")$ET.obsC,na.rm=T)/
      sum(filter(ROTH_Results, year(timestamp)==2019 & day_night=="night")$ET.obsC,na.rm=T)
  day_night_ROTH[i,6] <- 
      sum(filter(SCOPE_ET_ROTH[[i]], year(timestamp)==2019 & day_night=="day")[[5]]-
            filter(ROTH_Results, year(timestamp)==2019 & day_night=="day")$ET.obsC,na.rm=T)/
      sum(filter(ROTH_Results, year(timestamp)==2019 & day_night=="day")$ET.obsC,na.rm=T)
#  day_night_ROTH[i,9] <- 
#      sum(filter(SCOPE_ET_ROTH[[i]], year(timestamp)==2019 & day_night=="Dawn_Dusk")[[5]]-
#            filter(ROTH_Results, year(timestamp)==2019 & day_night=="Dawn_Dusk")$ET.obsC,na.rm=T)/
#      sum(filter(ROTH_Results, year(timestamp)==2019 & day_night=="Dawn_Dusk")$ET.obsC,na.rm=T)
}

colnames(day_night_ROTH) <- c("RMSE_night","RMSE_day", #"RMSE_dusk",
                               "R2_night","R2_day", #"R2_dusk",
                               "Rbias_night","Rbias_day") #,"Rbias_dusk"
                                         
t(round(day_night_ROTH,3))

# n pixel and dist to the centroid
hist(FP_ROTH$n_pixel)
hist(filter(FP_ROTH,n_pixel<500)$n_pixel)
hist(filter(FP_ROTH,n_pixel>2000)$n_pixel)

for (i in 1:length(SCOPE_ET_ROTH)) {
  SCOPE_ET_ROTH[[i]]$distcentroid <- FP_ROTH$distcentroid[1:10225]
  SCOPE_ET_ROTH[[i]]$n_pixel <- FP_ROTH$n_pixel[1:10225]
}

ROTH_Results$n_pixel <- FP_ROTH$n_pixel[1:10225]
ROTH_Results$distcentroid <- FP_ROTH$distcentroid[1:10225]

# n pixel
metric_FP_ROTH <- matrix(nrow=length(SCOPE_ET_ROTH), ncol=9)

for(i in 1:length(SCOPE_ET_ROTH)) {
  metric_FP_ROTH[i,1] <- 
    defaultSummary(na.omit(data.frame(
      obs=filter(ROTH_Results, year(timestamp)==2019 & n_pixel<500)$ET.obsC,
      pred=filter(SCOPE_ET_ROTH[[i]], year(timestamp)==2019 & n_pixel<500)[[5]]
    )))["RMSE"]
  metric_FP_ROTH[i,2] <- 
    defaultSummary(na.omit(data.frame(
      obs=filter(ROTH_Results, year(timestamp)==2019 & n_pixel>500 & n_pixel<2000)$ET.obsC,
      pred=filter(SCOPE_ET_ROTH[[i]], year(timestamp)==2019 & n_pixel>500 & n_pixel<2000)[[5]]
    )))["RMSE"]
  metric_FP_ROTH[i,3] <- 
    defaultSummary(na.omit(data.frame(
      obs=filter(ROTH_Results, year(timestamp)==2019 & n_pixel>2000)$ET.obsC,
      pred=filter(SCOPE_ET_ROTH[[i]], year(timestamp)==2019 & n_pixel>2000)[[5]]
    )))["RMSE"]
  metric_FP_ROTH[i,4] <- 
    defaultSummary(na.omit(data.frame(
      obs=filter(ROTH_Results, year(timestamp)==2019 & n_pixel<500)$ET.obsC,
      pred=filter(SCOPE_ET_ROTH[[i]], year(timestamp)==2019 & n_pixel<500)[[5]]
    )))["Rsquared"]
  metric_FP_ROTH[i,5] <- 
    defaultSummary(na.omit(data.frame(
      obs=filter(ROTH_Results, year(timestamp)==2019 & n_pixel>500 & n_pixel<2000)$ET.obsC,
      pred=filter(SCOPE_ET_ROTH[[i]], year(timestamp)==2019 & n_pixel>500 & n_pixel<2000)[[5]]
    )))["Rsquared"]
  metric_FP_ROTH[i,6] <- 
    defaultSummary(na.omit(data.frame(
      obs=filter(ROTH_Results, year(timestamp)==2019 & n_pixel>2000)$ET.obsC,
      pred=filter(SCOPE_ET_ROTH[[i]], year(timestamp)==2019 & n_pixel>2000)[[5]]
    )))["Rsquared"]
  metric_FP_ROTH[i,7] <- 
    sum(filter(SCOPE_ET_ROTH[[i]], year(timestamp)==2019 & n_pixel<500)[[5]]-
          filter(ROTH_Results, year(timestamp)==2019 & n_pixel<500)$ET.obsC,na.rm=T)/
    sum(filter(ROTH_Results, year(timestamp)==2019 & n_pixel<500)$ET.obsC,na.rm=T)
  metric_FP_ROTH[i,8] <- 
    sum(filter(SCOPE_ET_ROTH[[i]], year(timestamp)==2019 & n_pixel>500 & n_pixel<2000)[[5]]-
          filter(ROTH_Results, year(timestamp)==2019 & n_pixel>500 & n_pixel<2000)$ET.obsC,na.rm=T)/
    sum(filter(ROTH_Results, year(timestamp)==2019 & n_pixel>500 & n_pixel<2000)$ET.obsC,na.rm=T)
  metric_FP_ROTH[i,9] <- 
    sum(filter(SCOPE_ET_ROTH[[i]], year(timestamp)==2019 & n_pixel>2000)[[5]]-
          filter(ROTH_Results, year(timestamp)==2019 & n_pixel>2000)$ET.obsC,na.rm=T)/
    sum(filter(ROTH_Results, year(timestamp)==2019 & n_pixel>2000)$ET.obsC,na.rm=T)
  
  
}

colnames(metric_FP_ROTH) <- c("RMSE<500p","RMSE_b","RMSE>2000p",
                               "R2<500p","R2_b","R2>2000p",
                               "Rbias<500p","Rbias_b","Rbias>2000p")

t(round(metric_FP_ROTH,3))

# distcentroid
hist(FP_ROTH$distcentroid)
hist(filter(FP_ROTH,distcentroid<250)$distcentroid)
hist(filter(FP_ROTH,distcentroid>750)$distcentroid)

metric_FP2_ROTH <- matrix(nrow=length(SCOPE_ET_ROTH), ncol=9)

for(i in 1:length(SCOPE_ET_ROTH)) {
  metric_FP2_ROTH[i,1] <- 
    defaultSummary(na.omit(data.frame(
      obs=filter(ROTH_Results, year(timestamp)==2019 & distcentroid<250)$ET.obsC,
      pred=filter(SCOPE_ET_ROTH[[i]], year(timestamp)==2019 & distcentroid<250)[[5]]
    )))["RMSE"]
  metric_FP2_ROTH[i,2] <- 
    defaultSummary(na.omit(data.frame(
      obs=filter(ROTH_Results, year(timestamp)==2019 & distcentroid>250 & distcentroid<750)$ET.obsC,
      pred=filter(SCOPE_ET_ROTH[[i]], year(timestamp)==2019 & distcentroid>250 & distcentroid<750)[[5]]
    )))["RMSE"]
  metric_FP2_ROTH[i,3] <- 
    defaultSummary(na.omit(data.frame(
      obs=filter(ROTH_Results, year(timestamp)==2019 & distcentroid>750)$ET.obsC,
      pred=filter(SCOPE_ET_ROTH[[i]], year(timestamp)==2019 & distcentroid>750)[[5]]
    )))["RMSE"]
  metric_FP2_ROTH[i,4] <- 
    defaultSummary(na.omit(data.frame(
      obs=filter(ROTH_Results, year(timestamp)==2019 & distcentroid<250)$ET.obsC,
      pred=filter(SCOPE_ET_ROTH[[i]], year(timestamp)==2019 & distcentroid<250)[[5]]
    )))["Rsquared"]
  metric_FP2_ROTH[i,5] <- 
    defaultSummary(na.omit(data.frame(
      obs=filter(ROTH_Results, year(timestamp)==2019 & distcentroid>250 & distcentroid<750)$ET.obsC,
      pred=filter(SCOPE_ET_ROTH[[i]], year(timestamp)==2019 & distcentroid>250 & distcentroid<750)[[5]]
    )))["Rsquared"]
  metric_FP2_ROTH[i,6] <- 
    defaultSummary(na.omit(data.frame(
      obs=filter(ROTH_Results, year(timestamp)==2019 & distcentroid>750)$ET.obsC,
      pred=filter(SCOPE_ET_ROTH[[i]], year(timestamp)==2019 & distcentroid>750)[[5]]
    )))["Rsquared"]
  metric_FP2_ROTH[i,7] <- 
    sum(filter(SCOPE_ET_ROTH[[i]], year(timestamp)==2019 & distcentroid<250)[[5]]-
          filter(ROTH_Results, year(timestamp)==2019 & distcentroid<250)$ET.obsC,na.rm=T)/
    sum(filter(ROTH_Results, year(timestamp)==2019 & distcentroid<250)$ET.obsC,na.rm=T)
  metric_FP2_ROTH[i,8] <- 
    sum(filter(SCOPE_ET_ROTH[[i]], year(timestamp)==2019 & distcentroid>250 & distcentroid<750)[[5]]-
          filter(ROTH_Results, year(timestamp)==2019 & distcentroid>250 & distcentroid<750)$ET.obsC,na.rm=T)/
    sum(filter(ROTH_Results, year(timestamp)==2019 & distcentroid>250 & distcentroid<750)$ET.obsC,na.rm=T)
  metric_FP2_ROTH[i,9] <- 
    sum(filter(SCOPE_ET_ROTH[[i]], year(timestamp)==2019 & distcentroid>750)[[5]]-
          filter(ROTH_Results, year(timestamp)==2019 & distcentroid>750)$ET.obsC,na.rm=T)/
    sum(filter(ROTH_Results, year(timestamp)==2019 & distcentroid>750)$ET.obsC,na.rm=T)
}

colnames(metric_FP2_ROTH) <- c("RMSE<250m","RMSE_b","RMSE>750m",
                              "R2<250m","R2_b","R2>750m",
                              "Rbias<250m","Rbias_b","Rbias>750m")

t(round(metric_FP2_ROTH,3))


summary(ROTH_Results$distcentroid)
which(ROTH_Results$distcentroid<200)
which(ROTH_Results$distcentroid>1000)
summary(ROTH_Results$n_pixel)
which(ROTH_Results$n_pixel<100)
which(ROTH_Results$n_pixel>3000)


metric_FP3_ROTH <- matrix(nrow=length(SCOPE_ET_ROTH), ncol=3)

for(i in 1:length(SCOPE_ET_ROTH)) {
  metric_FP3_ROTH[i,1] <- 
    defaultSummary(na.omit(data.frame(
      obs=filter(ROTH_Results, year(timestamp)==2019 & n_pixel>100 & n_pixel<3000
                 & distcentroid>250 & distcentroid<1000 & is.na(distcentroid)==F)$ET.obsC,
      pred=filter(SCOPE_ET_ROTH[[i]], year(timestamp)==2019 & n_pixel>100 & n_pixel<3000
                  & distcentroid>250 & distcentroid<1000 & is.na(distcentroid)==F)[[5]]
    )))["RMSE"]
  metric_FP3_ROTH[i,2] <- 
    defaultSummary(na.omit(data.frame(
      obs=filter(ROTH_Results, year(timestamp)==2019 & n_pixel>100 & n_pixel<3000
                 & distcentroid>250 & distcentroid<1000 & is.na(distcentroid)==F)$ET.obsC,
      pred=filter(SCOPE_ET_ROTH[[i]], year(timestamp)==2019 & n_pixel>100 & n_pixel<3000
                  & distcentroid>250 & distcentroid<1000 & is.na(distcentroid)==F)[[5]]
    )))["Rsquared"]
  metric_FP3_ROTH[i,3] <- 
    sum(filter(SCOPE_ET_ROTH[[i]], year(timestamp)==2019 & n_pixel>100 & n_pixel<3000
               & distcentroid>250 & distcentroid<1000 & is.na(distcentroid)==F)[[5]]-
          filter(ROTH_Results, year(timestamp)==2019 & n_pixel>100 & n_pixel<3000
                 & distcentroid>250 & distcentroid<1000 & is.na(distcentroid)==F)$ET.obsC,na.rm=T)/
    sum(filter(ROTH_Results, year(timestamp)==2019 & n_pixel>100 & n_pixel<3000
               & distcentroid>250 & distcentroid<1000 & is.na(distcentroid)==F)$ET.obsC,na.rm=T)
}

colnames(metric_FP3_ROTH) <- c("RMSE","R2","Rbias")

t(round(metric_FP3_ROTH,3))

table(is.na(filter(ROTH_Results, year(timestamp)==2019 & n_pixel>200 & n_pixel<2500
       & distcentroid>250 & distcentroid<1000 & is.na(distcentroid)==F)$ET.obsC))
table(is.na(filter(ROTH_Results, year(timestamp)==2019)$ET.obsC))




###################################################################################
###################################################################################
SCOPE_DN_ROTH <- NULL

for (i in 1:length(SCOPE_ROTH)) {
  SCOPE_DN_ROTH[[i]] <- data.frame(
    "timestamp"=Input_ROTH$timestamp[1:10225],
    "SCOPE_ET"=LE.to.ET(SCOPE_ROTH[[i]]$lEtot,Input_ROTH$Ta[1:10225])*3600,
    "SCOPE_ETsoil"=LE.to.ET(SCOPE_ROTH[[i]]$lEstot,Input_ROTH$Ta[1:10225])*3600,
    "SCOPE_ETcanopy"=LE.to.ET(SCOPE_ROTH[[i]]$lEctot,Input_ROTH$Ta[1:10225])*3600,
    "SCOPE_ET_adjA"=NA,
    "SCOPE_ET_adjB"=NA,
    "SCOPE_ET_adjC"=NA,
    "SCOPE_ET_adjD"=NA,
    "SCOPE_ET_adjAC"=NA,
    "SCOPE_ET_adjAD"=NA,
    "SCOPE_ET_adjBC"=NA,
    "SCOPE_ET_adjBD"=NA
  )
  SCOPE_DN_ROTH[[i]]$SCOPE_ET[SCOPE_DN_ROTH[[i]]$SCOPE_ET<=-0.0000000001] <- 0
  SCOPE_DN_ROTH[[i]]$SCOPE_ETsoil[SCOPE_DN_ROTH[[i]]$SCOPE_ETsoil<=-0.0000000001] <- 0
  SCOPE_DN_ROTH[[i]]$SCOPE_ETcanopy[SCOPE_DN_ROTH[[i]]$SCOPE_ETcanopy<=-0.0000000001] <- 0
}

for (i in 1:length(SCOPE_DN_ROTH)) {
  SCOPE_DN_ROTH[[i]]$day_night <- ROTH_Results$day_night[1:10225]
}

summary(SCOPE_DN_ROTH[[1]])

for (i in 1:length(SCOPE_ROTH)) {
  SCOPE_DN_ROTH[[i]]$SCOPE_ET_adjA <- ifelse(SCOPE_DN_ROTH[[i]]$day_night=="night",
                                                SCOPE_DN_ROTH[[i]]$SCOPE_ET,
                                             SCOPE_DN_ROTH[[i]]$SCOPE_ET*
                                               na.approx(FP_ROTH$veg_cover_vh_old[1:10225]))
  SCOPE_DN_ROTH[[i]]$SCOPE_ET_adjB <- ifelse(SCOPE_DN_ROTH[[i]]$day_night=="night",
                                                SCOPE_DN_ROTH[[i]]$SCOPE_ET,
                                             SCOPE_DN_ROTH[[i]]$SCOPE_ET*
                                               na.approx(FP_ROTH$Veg_cover[1:10225]/100))
  SCOPE_DN_ROTH[[i]]$SCOPE_ET_adjC <- ifelse(SCOPE_DN_ROTH[[i]]$day_night=="night",
                                                SCOPE_DN_ROTH[[i]]$SCOPE_ET,
                                             SCOPE_DN_ROTH[[i]]$SCOPE_ET*
                                               na.approx(1-(FP_ROTH$impervious_old[1:10225])))
  SCOPE_DN_ROTH[[i]]$SCOPE_ET_adjD <- ifelse(SCOPE_DN_ROTH[[i]]$day_night=="night",
                                                SCOPE_DN_ROTH[[i]]$SCOPE_ET,
                                             SCOPE_DN_ROTH[[i]]$SCOPE_ET*
                                               na.approx(1-(FP_ROTH$Impervious_With_str[1:10225]/100)))
  SCOPE_DN_ROTH[[i]]$SCOPE_ET_adjAC <- ifelse(SCOPE_DN_ROTH[[i]]$day_night=="night",
                                                 SCOPE_DN_ROTH[[i]]$SCOPE_ET,
                                              SCOPE_DN_ROTH[[i]]$SCOPE_ETsoil*
                                                na.approx(1-(FP_ROTH$impervious_old[1:10225]))+
                                                SCOPE_DN_ROTH[[i]]$SCOPE_ETcanopy*
                                                na.approx(FP_ROTH$veg_cover_vh_old[1:10225]))
  SCOPE_DN_ROTH[[i]]$SCOPE_ET_adjAD <- ifelse(SCOPE_DN_ROTH[[i]]$day_night=="night",
                                                 SCOPE_DN_ROTH[[i]]$SCOPE_ET,
                                              SCOPE_DN_ROTH[[i]]$SCOPE_ETsoil*
                                                na.approx(1-(FP_ROTH$Impervious_With_str[1:10225]/100))+
                                                SCOPE_DN_ROTH[[i]]$SCOPE_ETcanopy*
                                                na.approx(FP_ROTH$veg_cover_vh_old[1:10225]))
  SCOPE_DN_ROTH[[i]]$SCOPE_ET_adjBC <- ifelse(SCOPE_DN_ROTH[[i]]$day_night=="night",
                                                 SCOPE_DN_ROTH[[i]]$SCOPE_ET,
                                              SCOPE_DN_ROTH[[i]]$SCOPE_ETsoil*
                                                na.approx(1-(FP_ROTH$impervious_old[1:10225]))+
                                                SCOPE_DN_ROTH[[i]]$SCOPE_ETcanopy*
                                                na.approx(FP_ROTH$Veg_cover[1:10225]/100))
  SCOPE_DN_ROTH[[i]]$SCOPE_ET_adjBD <- ifelse(SCOPE_DN_ROTH[[i]]$day_night=="night",
                                                 SCOPE_DN_ROTH[[i]]$SCOPE_ET,
                                              SCOPE_DN_ROTH[[i]]$SCOPE_ETsoil*
                                                na.approx(1-(FP_ROTH$Impervious_With_str[1:10225]/100))+
                                                SCOPE_DN_ROTH[[i]]$SCOPE_ETcanopy*
                                                na.approx(FP_ROTH$Veg_cover[1:10225]/100))
}

summary(SCOPE_DN_ROTH[[1]])


SCOPE_MetricDN_ROTH <- NULL

for (i in 1:length(SCOPE_DN_ROTH)) {
  SCOPE_MetricDN_ROTH[[i]] <- list(
    matrix(c(defaultSummary(na.omit(data.frame(
      obs=filter(ROTH_Results, year(timestamp)==2019)$ET.obsC,
      pred=filter(SCOPE_DN_ROTH[[i]],year(timestamp)==2019)[[2]])))["Rsquared"],
      defaultSummary(na.omit(data.frame(
        obs=filter(ROTH_Results, year(timestamp)==2019)$ET.obsC,
        pred=filter(SCOPE_DN_ROTH[[i]],year(timestamp)==2019)[[2]])))["RMSE"],
      sum(filter(SCOPE_DN_ROTH[[i]], year(timestamp)==2019)[[2]]-
            filter(ROTH_Results, year(timestamp)==2019)$ET.obsC,na.rm=T)/
        sum(filter(ROTH_Results, year(timestamp)==2019)$ET.obsC,na.rm=T),
      
      defaultSummary(na.omit(data.frame(
        obs=filter(ROTH_Results, year(timestamp)==2019)$ET.obsC,
        pred=filter(SCOPE_DN_ROTH[[i]], year(timestamp)==2019)[[5]])))["Rsquared"],
      defaultSummary(na.omit(data.frame(
        obs=filter(ROTH_Results, year(timestamp)==2019)$ET.obsC,
        pred=filter(SCOPE_DN_ROTH[[i]], year(timestamp)==2019)[[5]])))["RMSE"],
      sum(filter(SCOPE_DN_ROTH[[i]], year(timestamp)==2019)[[5]]-
            filter(ROTH_Results, year(timestamp)==2019)$ET.obsC,na.rm=T)/
        sum(filter(ROTH_Results, year(timestamp)==2019)$ET.obsC,na.rm=T),
      
      defaultSummary(na.omit(data.frame(
        obs=filter(ROTH_Results, year(timestamp)==2019)$ET.obsC,
        pred=filter(SCOPE_DN_ROTH[[i]], year(timestamp)==2019)[[6]])))["Rsquared"],
      defaultSummary(na.omit(data.frame(
        obs=filter(ROTH_Results, year(timestamp)==2019)$ET.obsC,
        pred=filter(SCOPE_DN_ROTH[[i]], year(timestamp)==2019)[[6]])))["RMSE"],
      sum(filter(SCOPE_DN_ROTH[[i]], year(timestamp)==2019)[[6]]-
            filter(ROTH_Results, year(timestamp)==2019)$ET.obsC,na.rm=T)/
        sum(filter(ROTH_Results, year(timestamp)==2019)$ET.obsC,na.rm=T),
      
      defaultSummary(na.omit(data.frame(
        obs=filter(ROTH_Results, year(timestamp)==2019)$ET.obsC,
        pred=filter(SCOPE_DN_ROTH[[i]], year(timestamp)==2019)[[7]])))["Rsquared"],
      defaultSummary(na.omit(data.frame(
        obs=filter(ROTH_Results, year(timestamp)==2019)$ET.obsC,
        pred=filter(SCOPE_DN_ROTH[[i]], year(timestamp)==2019)[[7]])))["RMSE"],
      sum(filter(SCOPE_DN_ROTH[[i]], year(timestamp)==2019)[[7]]-
            filter(ROTH_Results, year(timestamp)==2019)$ET.obsC,na.rm=T)/
        sum(filter(ROTH_Results, year(timestamp)==2019)$ET.obsC,na.rm=T),
      
      defaultSummary(na.omit(data.frame(
        obs=filter(ROTH_Results, year(timestamp)==2019)$ET.obsC,
        pred=filter(SCOPE_DN_ROTH[[i]], year(timestamp)==2019)[[8]])))["Rsquared"],
      defaultSummary(na.omit(data.frame(
        obs=filter(ROTH_Results, year(timestamp)==2019)$ET.obsC,
        pred=filter(SCOPE_DN_ROTH[[i]], year(timestamp)==2019)[[8]])))["RMSE"],
      sum(filter(SCOPE_DN_ROTH[[i]], year(timestamp)==2019)[[8]]-
            filter(ROTH_Results, year(timestamp)==2019)$ET.obsC,na.rm=T)/
        sum(filter(ROTH_Results, year(timestamp)==2019)$ET.obsC,na.rm=T),
      
      defaultSummary(na.omit(data.frame(
        obs=filter(ROTH_Results, year(timestamp)==2019)$ET.obsC,
        pred=filter(SCOPE_DN_ROTH[[i]], year(timestamp)==2019)[[9]])))["Rsquared"],
      defaultSummary(na.omit(data.frame(
        obs=filter(ROTH_Results, year(timestamp)==2019)$ET.obsC,
        pred=filter(SCOPE_DN_ROTH[[i]], year(timestamp)==2019)[[9]])))["RMSE"],
      sum(filter(SCOPE_DN_ROTH[[i]], year(timestamp)==2019)[[9]]-
            filter(ROTH_Results, year(timestamp)==2019)$ET.obsC,na.rm=T)/
        sum(filter(ROTH_Results, year(timestamp)==2019)$ET.obsC,na.rm=T),
      
      defaultSummary(na.omit(data.frame(
        obs=filter(ROTH_Results, year(timestamp)==2019)$ET.obsC,
        pred=filter(SCOPE_DN_ROTH[[i]], year(timestamp)==2019)[[10]])))["Rsquared"],
      defaultSummary(na.omit(data.frame(
        obs=filter(ROTH_Results, year(timestamp)==2019)$ET.obsC,
        pred=filter(SCOPE_DN_ROTH[[i]], year(timestamp)==2019)[[10]])))["RMSE"],
      sum(filter(SCOPE_DN_ROTH[[i]], year(timestamp)==2019)[[10]]-
            filter(ROTH_Results, year(timestamp)==2019)$ET.obsC,na.rm=T)/
        sum(filter(ROTH_Results, year(timestamp)==2019)$ET.obsC,na.rm=T),
      
      defaultSummary(na.omit(data.frame(
        obs=filter(ROTH_Results, year(timestamp)==2019)$ET.obsC,
        pred=filter(SCOPE_DN_ROTH[[i]], year(timestamp)==2019)[[11]])))["Rsquared"],
      defaultSummary(na.omit(data.frame(
        obs=filter(ROTH_Results, year(timestamp)==2019)$ET.obsC,
        pred=filter(SCOPE_DN_ROTH[[i]], year(timestamp)==2019)[[11]])))["RMSE"],
      sum(filter(SCOPE_DN_ROTH[[i]], year(timestamp)==2019)[[11]]-
            filter(ROTH_Results, year(timestamp)==2019)$ET.obsC,na.rm=T)/
        sum(filter(ROTH_Results, year(timestamp)==2019)$ET.obsC,na.rm=T),
      
      defaultSummary(na.omit(data.frame(
        obs=filter(ROTH_Results, year(timestamp)==2019)$ET.obsC,
        pred=filter(SCOPE_DN_ROTH[[i]], year(timestamp)==2019)[[12]])))["Rsquared"],
      defaultSummary(na.omit(data.frame(
        obs=filter(ROTH_Results, year(timestamp)==2019)$ET.obsC,
        pred=filter(SCOPE_DN_ROTH[[i]], year(timestamp)==2019)[[12]])))["RMSE"],
      sum(filter(SCOPE_DN_ROTH[[i]], year(timestamp)==2019)[[12]]-
            filter(ROTH_Results, year(timestamp)==2019)$ET.obsC,na.rm=T)/
        sum(filter(ROTH_Results, year(timestamp)==2019)$ET.obsC,na.rm=T)
    ), nrow=3))
  
  colnames(SCOPE_MetricDN_ROTH[[i]][[1]]) <- c("SCOPE_orig","AdjA","AdjB","AdjC","AdjD",
                                             "AdjAC","AdjAD","AdjBC","AdjBD")
  rownames(SCOPE_MetricDN_ROTH[[i]][[1]]) <- c("R2", "RMSE","Rbias")
}

#######################################################
#######################################################
#######################################################
for (i in 1:length(SCOPE_DN_ROTH)) {
  ifelse(which(SCOPE_MetricDN_ROTH[[i]][[1]][1,]>=0.82),
         print(i),0)
}
#c(11, 19, 20, 23, 29, 38, 39, 40, 43, 45, 47, 48)

for (i in 1:length(SCOPE_DN_ROTH)) {
  ifelse(which(SCOPE_MetricDN_ROTH[[i]][[1]][2,]<=0.0265),
         print(i),0)
}
#c(4, 10, 11, 19, 20, 23, 29, 31, 33, 37, 38, 39, 43, 45, 46, 47, 48)

for (i in 1:length(SCOPE_DN_ROTH)) {
  ifelse(which(abs(SCOPE_MetricDN_ROTH[[i]][[1]][3,])<=0.05),
         print(i),0)
}
# c(-9,-12,-15,-16,-19,-20,-23,-37,-40,-49,-50,-53,-53,-56,-57,-58,-59,-60)
#######################################################
#######################################################
SCOPE_MetricDN_ROTH[c(11,19,20,23,29,38,39,40,43,45,47,48)]
SCOPE_Metric_ROTH[c(11,19,20,23,29,38,39,40,43,45,47,48)]

SCOPE_MetricDN_ROTH[c(4,10,11,19,20,23,29,31,33,37,38,39,43,45,46,47,48)]
SCOPE_Metric_ROTH[c(4,10,11,19,20,23,29,31,33,37,38,39,43,45,46,47,48)]

SCOPE_MetricDN_ROTH[c(-9,-12,-15,-16,-19,-20,-23,-37,-40,-49,-50,-53,-53,-56,-57,-58,-59,-60)]
SCOPE_Metric_ROTH[c(-2,-12,-19,-20,-23,-37,-40,-42,-49,-50,-56,-57,-58,-59,-60)]

SCOPE_MetricDN_ROTH[c(11,29,38,39,43,45,47,48)]
Settings_ROTH[c(11,29,38,39,43,45,47,48)]
Parameters_ROTH[c(11,29,38,39,43,45,47,48)]
Constants_ROTH[c(11,29,38,39,43,45,47,48)]

SCOPE_MetricDN_ROTH[c(5,38,39,45,47,61)]
Parameters_ROTH[c(5,38,39,45,47,61)]
Settings_ROTH[c(5,38,39,45,47,61)]
SCOPE_Metric_ROTH[c(5,39,45,47,61)]

print(as_tibble(Constants_ROTH[[47]]), n=84)

SCOPE_Metric_ROTH[c(47,62,72,73)]
SCOPE_MetricDN_ROTH[c(47,62,72,73)]
Settings_ROTH[c(47,62,72,73)]
Parameters_ROTH[c(47,62,72,73)]


#######################################################
# R2 per month

for(i in 1:length(SCOPE_DN_ROTH)) {
  for(j in 1:12) {
    metric_monthDN_ROTH[i,j] <- 
      defaultSummary(na.omit(data.frame(
        obs=filter(ROTH_Results, year(timestamp)==2019 & month(timestamp)==j)$ET.obsC,
        pred=filter(SCOPE_DN_ROTH[[i]], year(timestamp)==2019 & month(timestamp)==j)[[5]]
      )))["RMSE"]
  }
}

plot(metric_monthDN_ROTH[2,], ylim = c(0,0.09))
for(i in 1:length(SCOPE_DN_ROTH)) {
  lines(metric_monthDN_ROTH[i,])
}

for(i in 1:12) {
  print(
    which.min(metric_monthDN_ROTH[,i]))
}


for (i in 1:length(SCOPE_DN_ROTH)) {
  SCOPE_DN_ROTH[[i]]$distcentroid <- FP_ROTH$distcentroid[1:10225]
  SCOPE_DN_ROTH[[i]]$n_pixel <- FP_ROTH$n_pixel[1:10225]
}

metric_FPDN_ROTH <- matrix(nrow=length(SCOPE_DN_ROTH), ncol=3)

for(i in 1:length(SCOPE_DN_ROTH)) {
  metric_FPDN_ROTH[i,1] <- 
    defaultSummary(na.omit(data.frame(
      obs=filter(ROTH_Results, year(timestamp)==2019 & n_pixel>150 & n_pixel<3000
                 & distcentroid>250 & distcentroid<1000)$ET.obsC,
      pred=filter(SCOPE_DN_ROTH[[i]], year(timestamp)==2019 & n_pixel>150 & n_pixel<3000
                  & distcentroid>250 & distcentroid<1000)[[5]]
    )))["RMSE"]
  metric_FPDN_ROTH[i,2] <- 
    defaultSummary(na.omit(data.frame(
      obs=filter(ROTH_Results, year(timestamp)==2019 & n_pixel>150 & n_pixel<3000
                 & distcentroid>250 & distcentroid<1000)$ET.obsC,
      pred=filter(SCOPE_DN_ROTH[[i]], year(timestamp)==2019 & n_pixel>150 & n_pixel<3000
                  & distcentroid>250 & distcentroid<1000)[[5]]
    )))["Rsquared"]
  metric_FPDN_ROTH[i,3] <- 
    sum(filter(SCOPE_DN_ROTH[[i]], year(timestamp)==2019 & n_pixel>150 & n_pixel<3000
               & distcentroid>250 & distcentroid<1000)[[5]]-
          filter(ROTH_Results, year(timestamp)==2019 & n_pixel>150 & n_pixel<3000
                 & distcentroid>250 & distcentroid<1000)$ET.obsC,na.rm=T)/
    sum(filter(ROTH_Results, year(timestamp)==2019 & n_pixel>150 & n_pixel<3000
               & distcentroid>250 & distcentroid<1000)$ET.obsC,na.rm=T)
}

colnames(metric_FPDN_ROTH) <- c("RMSE","R2","Rbias")

t(round(metric_FPDN_ROTH,3))

table(is.na(filter(ROTH_Results, year(timestamp)==2019 & n_pixel>200 & n_pixel<2500
                   & distcentroid>250 & distcentroid<1000)$ET.obsC))
table(is.na(filter(ROTH_Results, year(timestamp)==2019)$ET.obsC))


#ETo
t(round(ETo_ROTH,3))

SCOPE_ET_ROTH[[74]]$SCOPE_ET_adjB[which(SCOPE_ROTH[[74]]$nu_iterations>100)]
Parameters_ROTH[[74]][9:length(Parameters_ROTH[[74]])]
Settings_ROTH[[74]][1:2][c(5,7,11,14,15),]
round(SCOPE_Metric_ROTH[[74]][[1]],3)
round(SCOPE_Metric_ROTH[[74]][[1]][1:3,c(1,3,9)],3) 
round(SCOPE_MetricDN_ROTH[[74]][[1]],3) # one *better
round(SCOPE_MetricDN_ROTH[[74]][[1]][1:3,c(1,3,9)],3) # night - day
print(as_tibble(filter(Constants_ROTH[[74]][1:2],!is.na(X2))),n=84)

#SCOPE DWD
SCOPE_ET_ROTH[[73]]$SCOPE_ET_adjB[which(SCOPE_ROTH[[73]]$nu_iterations>100)]
Parameters_ROTH[[73]][9:length(Parameters_ROTH[[73]])]
Settings_ROTH[[73]][1:2][c(5,7,11,14,15),]
round(SCOPE_Metric_ROTH[[73]][[1]],3)   # [1:3,c(1,3,9)] 
round(SCOPE_MetricDN_ROTH[[73]][[1]],3) #[1:3,c(1,3,9)] night - day *better rbias
print(as_tibble(filter(Constants_ROTH[[61]][1:2],!is.na(X2))),n=84)

#SCOPE DWD + RS
SCOPE_ET_ROTH[[72]]$SCOPE_ET_adjB[which(SCOPE_ROTH[[72]]$nu_iterations>100)]
Parameters_ROTH[[72]][9:length(Parameters_ROTH[[72]])]
Settings_ROTH[[72]][1:2][c(5,7,11,14,15),]
round(SCOPE_Metric_ROTH[[72]][[1]],3)   #[1:3,c(1,3,9)] one
round(SCOPE_MetricDN_ROTH[[72]][[1]],3) #[1:3,c(1,3,9)] night - day *better rbias
print(as_tibble(filter(Constants_ROTH[[72]][1:2],!is.na(X2))),n=84)

SCOPE_ET_ROTH[[70]]$SCOPE_ET_adjB[which(SCOPE_ROTH[[70]]$nu_iterations>100)]
Parameters_ROTH[[70]][9:length(Parameters_ROTH[[70]])]
Settings_ROTH[[70]][1:2][c(5,7,11,14,15),]
round(SCOPE_Metric_ROTH[[70]][[1]],4)   # [1:3,c(1,3,9)]one
round(SCOPE_MetricDN_ROTH[[70]][[1]],3) #[1:3,c(1,3,9)] night - day *better rbias
print(as_tibble(filter(Constants_ROTH[[70]][1:2],!is.na(X2))),n=84)

#SCOPE DWD + RS + SMC
Parameters_ROTH[[48]][9:length(Parameters_ROTH[[48]])]
Settings_ROTH[[48]][1:2][c(5,7,11,14,15),]
round(SCOPE_Metric_ROTH[[48]][[1]][1:3,c(1,3,9)],3)   # one
round(SCOPE_MetricDN_ROTH[[48]][[1]][1:3,c(1,3,9)],3) # night - day *better rbias

Parameters_ROTH[[45]][9:length(Parameters_ROTH[[45]])]
Settings_ROTH[[45]][1:2][c(5,7,11,14,15),]
round(SCOPE_Metric_ROTH[[45]][[1]][1:3,c(1,3,9)],3)   # one
round(SCOPE_MetricDN_ROTH[[45]][[1]][1:3,c(1,3,9)],3) # night - day *better rbias


