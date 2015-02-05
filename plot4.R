##########################################################
# PLOT 4
## Exploratory Data Analysys
### Project 1 
### Author: Cassio
##########################################################

### Loading libraries
library(lubridate)
library(dplyr)

### Constants
leg_sym <- 1 # legend symbol

### Loading the Data (avoiding factors)
## put the file on a subfolder named 'data' under your working directory
epcData <- read.table("./data/household_power_consumption.txt", sep = ";",
                      header = TRUE, stringsAsFactors=FALSE)

### filtering data
epcData_ft <- 
    epcData %>%
    ## creating column DateTime
    mutate(DateTime = paste(Date,Time)) %>%
    ## changing Date and Time
    mutate(Date = dmy(Date)) %>%
    ## filtering from 2007/02/01 to 2007/02/01 
    filter(year(Date) == 2007 & month(Date) == 2 & (day(Date) == 1 | day(Date) == 2))   

### chekcing for '?'
tt_na <- 0
for (i in 1:length(epcData_ft)) {
    tt_na <- tt_na + sum(grepl("^[?]",epcData_ft[,i]))
}
print (tt_na)

### setting parameters
par(mfrow = c(2,2), mar =c(5,4,2,1))

### plotting data (1,0) ########################################
plot(strptime(epcData_ft$DateTime,'%d/%m/%Y %H:%M:%S'), as.numeric(epcData_ft$Global_active_power), 
     ylab= "Global Active Power", xlab = "", type='l', col='black') 

### plotting data (0,1) ########################################
plot(strptime(epcData_ft$DateTime,'%d/%m/%Y %H:%M:%S'), as.numeric(epcData_ft$Voltage), 
     ylab= "Voltage", xlab = "datetime", type='l', col='black') 

### plotting data (2,0) ########################################
with(epcData_ft, plot(strptime(epcData_ft$DateTime,'%d/%m/%Y %H:%M:%S'), 
                      as.numeric(epcData_ft$Sub_metering_1), 
                      ylab= "Energy sub metering", # y label
                      xlab = "",  # whitout x label
                      type = "n")) # dont plot
points(strptime(epcData_ft$DateTime,'%d/%m/%Y %H:%M:%S'), as.numeric(epcData_ft$Sub_metering_1), 
       type='l', # line
       col='black')
points(strptime(epcData_ft$DateTime,'%d/%m/%Y %H:%M:%S'), as.numeric(epcData_ft$Sub_metering_2), 
       type='l', # line
       col='red')
points(strptime(epcData_ft$DateTime,'%d/%m/%Y %H:%M:%S'), as.numeric(epcData_ft$Sub_metering_3), 
       type='l', # line
       col='blue')

### putting the legends
legend("topright", col = c("black", "red", "blue"), 
       legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
       lty = leg_sym, # legend symbol
       bty = "n", # no borders
       x.intersp = 7.0, # line displacement
       cex=0.8, # text size
       adj = c(0.5, 0.2)) # text displacement

### plotting data (0,2) ###################################
plot(strptime(epcData_ft$DateTime,'%d/%m/%Y %H:%M:%S'), as.numeric(epcData_ft$Global_reactive_power), 
     ylab= "Global_reactive_power", xlab = "datetime", type='l', col='black') 

## copying plot to PNG
dev.copy(png, file = "plot4.png", width = 480, height = 480)
dev.off()