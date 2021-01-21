##################################################
# calculate the raining window
##################################################
window.prec <- NULL

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

row_timestamp <- filter(row_timestamp, year(timestamp)==2019)
table(row_timestamp$prec.window)
summary(row_timestamp$prec.window)

DWD_ROTH19$prec.window <- row_timestamp$prec.window

write.csv(DWD_ROTH19, file="DWD_ROTH19.csv", row.names = F)
write.csv(DWD_ROTH19,
          file="C:/Users/Alby Rocha/Documents/EC/SCOPEresults/DWD_ROTH19.csv", 
          row.names = F)
