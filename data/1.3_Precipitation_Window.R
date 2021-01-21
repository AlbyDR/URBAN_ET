##################################################
library(dplyr)  # arrange/left_join
##################################################
# calculate the raining window
##################################################
window.prec <- NULL

# from 0 to 400 hours ithout rain
for (i in 1:400) {
window.prec[[i]] <- data.frame(
  arrange(data.frame(row=unique(c(which(DWD_ROTH$H_precip==1)+i))),row),
  hour=i )
colnames(window.prec[[i]]) <- c("row",paste("hour",i, sep=""))
}

window.prec.0 <- data.frame(
  arrange(data.frame(row=unique(c(which(DWD_ROTH$H_precip==1)+0))),row),
  "hour0" = 0)

row_timestamp <- data.frame(timestamp=DWD_ROTH$timestamp)
row_timestamp$row <- row_number(DWD_ROTH$timestamp)
row_timestamp <- left_join(row_timestamp, window.prec.0, by="row")

for (i in 1:400) {
  row_timestamp <- left_join(row_timestamp, window.prec[[i]], by="row")
}

row_timestamp$prec.window <- 400

for (i in 399:0) {
  row_timestamp$prec.window[row_timestamp[i+3]==i] <- i 
}

DWD_ROTH$prec.window <- row_timestamp$prec.window
write.csv(DWD_ROTH, file="DWD_ROTH.csv", row.names = F)
