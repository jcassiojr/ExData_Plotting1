##########################################################
# PLOT 3
## Exploratory Data Analysys
### Project 1 
### Author: Cassio
##########################################################

### Loading libraries
library(lubridate)
library(dplyr)

### constants
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

### plotting the 3 graphics
#par(mar = c(4,4,2,2))
#par(cex=.64) # adjusting the text size for labels and legends
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

### putting the legends FALTA ACERTAR PNG COM LEGENDA!!!!
legend("topright", col = c("black", "red", "blue"), 
       legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
       lty = leg_sym, # legend symbol
       x.intersp = 7.0, # line displacement
       cex=0.8,  # text size
       adj = c(0.5, 0.2)) # text displacement

## copying plot to PNG
# obs. I have to play with cex, adj and x.intersp in legend to 
# fix the legend appearence in the PNG file
dev.copy(png, file = "plot3.png", width = 480, height = 480)
dev.off()