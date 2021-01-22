######################################################################
library(tibble)     # glimpse
library(readr)      # read
library(xts)        # na.aprox
######################################################################

######################################################################
######################################################################
### Z0 and Zd function 
######################################################################
######################################################################
#planar_area_index
planar_area_index <- function(lambda_building, lambda_vegetaion, P3D){
  # calculate the plane area index for the vegetation
  Lambda_V <- (1-P3D)*lambda_vegetaion
  # combine Lambda_building and lambda vegetation
  Lambda_p <- (Lambda_V+lambda_building)
  return(Lambda_p)
}

#frontal_area_index
frontal_area_index <- function(fai_v, fai_b, P3D,Cdb){
  Pv <- ((-1.251*(P3D^2)+0.489*(P3D)+0.803)/1*Cdb)
  lambda_output <- fai_b + (Pv*fai_v)
  return(lambda_output)
}

#Kanda_zd
Kanda_zd <- function(pai, fai, zH, zstd, zHmax, Cd = 1.2){
  Alph <- 4.43
  Beet <- 1.0
  k <- 0.4 
  Ao <- 1.29
  Bo <- 0.36
  Co <- -0.17
  z_d_output <- (1+ (Alph^(-pai)) * (pai-1))*zH
  X <- (zstd+zH)/zHmax
  if(!is.na(X)){
    if(X >0 & X <= 1){
      z_d_output <- ((Co*(X^2))+((Ao*(pai^Bo)) -Co)*X)*zHmax
    }else{
      z_d_output <- (Ao*(pai^Bo))*zH
    }
  }else{
    z_d_output <- (Ao*(pai^Bo))*zH
  }
  
  return(z_d_output)
}

#Kanda_z0
Kanda_z0 <- function(fai, pai, zstd, zH, zHmax, Cd = 1.2){
  # Basic params needed:
  Alph <- 4.43
  Beet <- 1.0
  k <- 0.4 # Karman Konstante
  A1 <- 0.71
  B1 <- 20.21
  C1 <- -0.77
  # THe basic zd from McDonald is needed for the calculation of the z0Mac
  z_d_output <- (1+ (Alph^(-pai)) * (pai-1))*zH
  z0Mac <- (zH*(1-z_d_output/zH)*exp(-(0.5*(Cd/k^2)*(1-(z_d_output/zH))*fai)^-0.5))
  # This z0Mac is than adjusted by the Kanda method
  Y <- (pai*zstd/zH)
  if(!is.na(Y)){
    if(Y >= 0){
      z_0_output <- ((B1*(Y^2))+(C1*Y)+A1)*z0Mac
    }else{
      z_0_output <- A1*z0Mac
    }
  }else{
    z_0_output <- A1*z0Mac
  }
  return(z_0_output)
}

#Running mean code
running_mean <- function(data){
  if(nrow(data) != 360){
    stop("The data should have 360 rows, one for each degree!")
  }
  output_mean <- data.table::data.table(matrix(as.numeric(), ncol = ncol(data), nrow = 360))
  names(output_mean) <- names(data)
  for(i in 1:360){
    for(a in 2:ncol(data)){
      if(i <5){
        output_mean[i,a] <- mean(unlist(data[c(((360+i-5):360),(0:(i+5))),..a]))
      }else if(i > 355 & i < 360){
        output_mean[i,a] <- mean(unlist(data[c(((i-5):i),(i+1):360,(1:(5+i-360))),..a]))
      }else if(i == 360){
        output_mean[i,a] <- mean(unlist(data[c((355:360),(1:5)),..a]))
      }else{
        output_mean[i,a] <- mean(unlist(data[c((i-5):(i+5)),..a]))
      }
    }
  }
  output_mean[,1] <- c(1:360)
  return(output_mean)
}

#Running_mean
running_mean <- function(data){
  if(nrow(data) != 360){
    stop("The data should have 360 rows, one for each degree!")
  }
  output_mean <- data.table::data.table(matrix(as.numeric(), ncol = ncol(data), nrow = 360))
  names(output_mean) <- names(data)
  for(i in 1:360){
    for(a in 2:ncol(data)){
      if(i <5){
        output_mean[i,a] <- mean(unlist(data[c(((360+i-5):360),(0:(i+5))),..a]))
      }else if(i > 355 & i < 360){
        output_mean[i,a] <- mean(unlist(data[c(((i-5):i),(i+1):360,(1:(5+i-360))),..a]))
      }else if(i == 360){
        output_mean[i,a] <- mean(unlist(data[c((355:360),(1:5)),..a]))
      }else{
        output_mean[i,a] <- mean(unlist(data[c((i-5):(i+5)),..a]))
      }
    }
  }
  output_mean[,1] <- c(1:360)
  return(output_mean)
}

#average_z_by wd
average_z_by_wd <- function(wd, v_sd= NULL, z_data, deg_column, Kotthaus = F){
  # create the winddirection range where to average over
  if(Kotthaus == F){
    # in case that the wd+v_sd is above 360 degree
    if(round(wd,0) >360){
      max_wd <- round(round((wd)-360,0),0)
      min_wd <- round(wd,0)
    }else if(round(wd,0) < 1){ # and in case that the wd+v_sd is below 1 deg
      min_wd <- round(round((wd)+360,0))
      max_wd <- round(wd,0)
    }else{
      min_wd <- round(wd,0)
      max_wd <- round(wd,0)
    }
  }else{
    # in case that the wd+v_sd is above 360 degree
    if(round(wd+v_sd,0) >360){
      max_wd <- round(round((wd+v_sd)-360,0),0)
      min_wd <- round(wd-v_sd,0)
    }else if(round(wd-v_sd,0) < 1){ # and in case that the wd+v_sd is below 1 deg
      min_wd <- round(round((wd-v_sd)+360,0))
      max_wd <- round(wd+v_sd,0)
    }else{
      min_wd <- round(wd-v_sd,0)
      max_wd <- round(wd+v_sd,0)
    }
  }
  
  # select the z_data according to max_wd and min_wd
  output_1 <- mean(z_data$zd[which(z_data$deg <= max_wd &
                                     z_data$deg >= min_wd)])
  output_2 <- mean(z_data$z0[which(z_data$deg <= max_wd &
                                     z_data$deg >= min_wd)])
  return(c(output_1,output_2))
}

#order_zd_z0 funtion
order_zd_z0 <- function(Zd_winter, Z0_winter, Zd_intermediate, Z0_intermediate,
                        Zd_summer, Z0_summer, Timestamp, Kotthaus = F,
                        wind_direction, wind_v = NULL){
  # check if the length of wind_direction and Timestamp are equal
  if(length(Timestamp) != length(wind_direction) ){
    stop("The length of Timestamp and wind_direction differ!")
  }
  # generate look-up data.table
  z_winter <- data.table::data.table("deg"= c(1:360),"zd"= Zd_winter,
                                     "z0" = Z0_winter)
  z_intermediate <- data.table::data.table("deg"= c(1:360),"zd"= Zd_intermediate,
                                           "z0" = Z0_intermediate)
  z_summer <- data.table::data.table("deg"= c(1:360),"zd"= Zd_summer,
                                     "z0" = Z0_summer)
  # create an empty output data.table
  z_result <- data.table::data.table(TIMESTAMPE = Timestamp, zd= rep(as.numeric(NA),length(wind_direction)),
                                     z0= rep(as.numeric(NA),length(wind_direction)))
  
  # in case that the average is made with the running mean
  if(Kotthaus == F){
    for(i in 1:nrow(z_result)){
      if(is.na(wind_direction[i])){
        z_result$zd[i] <- NA
        z_result$z0[i] <- NA
      }else{
        if(substr(Timestamp[i],6,7) %in% c("12","01","02")){  # THIS IS WINTER
          res <- average_z_by_wd(wd = wind_direction[i],
                                 z_data = z_winter, deg_column = "deg")
          # res includes two different data 1 = zd and 2 = z0
          z_result$zd[i] <- res[1]
          z_result$z0[i] <- res[2]
        }else if(substr(Timestamp[i],6,7) %in% c("06","07","08")){ # THIS IS SUMMER
          res <- average_z_by_wd(wd = wind_direction[i],
                                 z_data = z_summer, deg_column = "deg")
          z_result$zd[i] <- res[1]
          z_result$z0[i] <- res[2]
        }else{
          res <- average_z_by_wd(wd = wind_direction[i],
                                 z_data = z_intermediate, deg_column = "deg")
          z_result$zd[i] <- res[1]
          z_result$z0[i] <- res[2]
        }
      }
    }
  }else{
    for(i in 1:nrow(z_result)){
      if(is.na(wind_direction[i])){
        z_result$zd[i] <- NA
        z_result$z0[i] <- NA
      }else{
        if(substr(Timestamp[i],6,7) %in% c("12","01","02")){  # THIS IS WINTER
          res <- average_z_by_wd(wd = wind_direction[i],v_sd = wind_v[i],
                                 z_data = z_winter, deg_column = "deg", Kotthaus = T)
          # res includes two different data 1 = zd and 2 = z0
          z_result$zd[i] <- res[1]
          z_result$z0[i] <- res[2]
        }else if(substr(Timestamp[i],6,7) %in% c("06","07","08")){ # THIS IS SUMMER
          res <- average_z_by_wd(wd = wind_direction[i],v_sd = wind_v[i],
                                 z_data = z_summer, deg_column = "deg", Kotthaus = T)
          z_result$zd[i] <- res[1]
          z_result$z0[i] <- res[2]
        }else{
          res <- average_z_by_wd(wd = wind_direction[i],v_sd = wind_v[i],
                                 z_data = z_intermediate, deg_column = "deg", Kotthaus = T)
          z_result$zd[i] <- res[1]
          z_result$z0[i] <- res[2]
        }
      }
    }
  }
  return(z_result)
}

############################################################################
############################################################################
# calculate Z0 and Zd
############################################################################
############################################################################
##### read 1000m  360 by 1 degree
## ROTH
Lambda_B_1deg <- read.csv("./data/ROTH_BH_1000m__IMPPoint_anisotropic.csv", header=TRUE, sep=",")
Lambda_V_1deg <- read.csv("./data/ROTH_VH_1000m__IMPPoint_anisotropic.csv", header = TRUE, sep=",")
Lambda_C_1deg <- read.csv("./data/ROTH_comb_1000m__IMPPoint_anisotropic.csv", header = TRUE, sep=",")

#pai_season
pai_deg_summer <- planar_area_index(lambda_building = Lambda_B_1deg$pai, 
                                    lambda_vegetaion = Lambda_V_1deg$pai,P3D = 0.2)
pai_deg_winter <- planar_area_index(lambda_building = Lambda_B_1deg$pai, 
                                    lambda_vegetaion = Lambda_V_1deg$pai,P3D = 0.6)
pai_deg_intermediate <- planar_area_index(lambda_building = Lambda_B_1deg$pai, 
                                          lambda_vegetaion = Lambda_V_1deg$pai,P3D = 0.4)

#fai_season
fai_deg_summer <- frontal_area_index(fai_v = Lambda_V_1deg$fai,
                                     fai_b = Lambda_B_1deg$fai, 
                                     P3D = 0.2, Cdb = 1.2)
fai_deg_winter <- frontal_area_index(fai_v = Lambda_V_1deg$fai,
                                     fai_b = Lambda_B_1deg$fai, 
                                     P3D = 0.6, Cdb = 1.2)
fai_deg_intermediate <- frontal_area_index(fai_v = Lambda_V_1deg$fai,
                                           fai_b = Lambda_B_1deg$fai, 
                                           P3D = 0.4, Cdb = 1.2)
Zd_deg_summer <- c()
Zd_deg_winter <- c()
Zd_deg_intermediate <- c()

for(i in 1:360){
  Zd_deg_summer[i] <- Kanda_zd(pai = pai_deg_summer[i], fai = fai_deg_summer[i],
                               zH = Lambda_C_1deg$zH[i], zstd = Lambda_C_1deg$zHstd[i],
                               zHmax = Lambda_C_1deg$zHmax[i])
  Zd_deg_winter[i] <- Kanda_zd(pai = pai_deg_winter[i], fai = fai_deg_winter[i],
                               zH = Lambda_C_1deg$zH[i], zstd = Lambda_C_1deg$zHstd[i],
                               zHmax = Lambda_C_1deg$zHmax[i])
  Zd_deg_intermediate[i] <- Kanda_zd(pai = pai_deg_intermediate[i], 
                                     fai = fai_deg_intermediate[i],
                                     zH = Lambda_C_1deg$zH[i], 
                                     zstd = Lambda_C_1deg$zHstd[i],
                                     zHmax = Lambda_C_1deg$zHmax[i])
}

head(Zd_deg_winter)
head(Zd_deg_intermediate)
head(Zd_deg_summer)

Z0_deg_summer <- c()
Z0_deg_winter <- c()
Z0_deg_intermediate <- c()

for(i in 1:360){
  Z0_deg_summer[i] <- Kanda_z0(fai = fai_deg_summer[i], pai = pai_deg_summer[i], 
                               zstd = Lambda_C_1deg$zHstd[i], zH = Lambda_C_1deg$zH[i],
                               zHmax = Lambda_C_1deg$zHmax[i])
  Z0_deg_winter[i] <- Kanda_z0(fai = fai_deg_winter[i], pai = pai_deg_winter[i], 
                               zstd = Lambda_C_1deg$zHstd[i], zH = Lambda_C_1deg$zH[i],
                               zHmax = Lambda_C_1deg$zHmax[i])
  Z0_deg_intermediate[i] <- Kanda_z0(fai = fai_deg_intermediate[i], pai = pai_deg_intermediate[i], 
                                     zstd = Lambda_C_1deg$zHstd[i], zH = Lambda_C_1deg$zH[i],
                                     zHmax = Lambda_C_1deg$zHmax[i])
}

head(Z0_deg_winter)
head(Z0_deg_intermediate)
head(Z0_deg_summer)

#exporting data
#for summer
summer_1deg <- data.table::data.table(cbind(c(0:359),pai_deg_summer, fai_deg_summer, 
                                            Zd_deg_summer, Z0_deg_summer))
names(summer_1deg)[1] <- "Wd"
head(summer_1deg)
glimpse(summer_1deg)

# and for winter
winter_1deg <- data.table::data.table(cbind(c(0:359),pai_deg_winter, fai_deg_winter, 
                                            Zd_deg_winter, Z0_deg_winter))
names(winter_1deg)[1] <- "Wd"
head(winter_1deg)

# and for intermediate
intermediate_1deg <- data.table::data.table(cbind(c(0:359),pai_deg_intermediate, fai_deg_intermediate, 
                                                  Zd_deg_intermediate, Z0_deg_intermediate))
names(intermediate_1deg)[1] <- "Wd"
head(intermediate_1deg)

#Calculate running mean
summer_1deg_running = running_mean(data = summer_1deg)
winter_1_deg_running = running_mean(data = winter_1deg)
intermediate_1_deg_running = running_mean(data = intermediate_1deg)

summer_1deg_running = running_mean(data = summer_1deg)
head(summer_1deg_running)
winter_1_deg_running = running_mean(data = winter_1deg)
intermediate_1_deg_running = running_mean(data = intermediate_1deg)

#############################################################
#############################################################
# read ECdata
EC_ROTH <- read_csv("./data/EC_ROTH.csv",col_names = T,
                    cols(
                      .default = col_double(),
                      timestamp = col_datetime(format = ""),
                      prec.window = col_integer()))
summary(EC_ROTH)

# zo and zd calculation
zd_z0_Roth = order_zd_z0(Zd_winter = winter_1_deg_running$Zd_deg_winter,
                    Z0_winter = winter_1_deg_running$Z0_deg_winter,
                    Zd_intermediate = intermediate_1_deg_running$Zd_deg_intermediate,
                    Z0_intermediate = intermediate_1_deg_running$Z0_deg_intermediate,
                    Zd_summer = summer_1deg_running$Zd_deg_summer,
                    Z0_summer = summer_1deg_running$Z0_deg_summer,
                    Timestamp = EC_ROTH$timestamp,
                    wind_direction = EC_ROTH$wd.filled
)

#write.csv(zd_z0_Roth, file="zd_z0_Roth.csv", row.names = F)
#zd_z0_Roth <- read.csv("zd_z0_Roth.csv")

#combining data
EC_ROTH$zd <- zd_z0_Roth$zd
EC_ROTH$z0 <- zd_z0_Roth$z0

EC_ROTH$zd.filled <- na.approx(EC_ROTH$zd)

summary(EC_ROTH$zd)
summary(EC_ROTH$zd.filled)

write.csv(EC_ROTH, file="EC_ROTH.csv", row.names = F)
