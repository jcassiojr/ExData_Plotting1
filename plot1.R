##########################################################
# PLOT 1
## Exploratory Data Analysys
### Project 1 
### Author: Cassio
##########################################################

### Loading libraries
library(lubridate)
library(dplyr)

### Loading the Data (avoiding factors)
## put the file on a subfolder named 'data' under your working directory
epcData <- read.table("./data/household_power_consumption.txt", sep = ";",
                      header = TRUE, stringsAsFactors=FALSE)
## filtering data
epcData_ft <- 
    epcData %>%
    ## changing Date from chr to date
    mutate(Date = dmy(Date)) %>%
    ## filtering from 2007/02/01 to 2007/02/01 
    filter(year(Date) == 2007 & month(Date) == 2 & (day(Date) == 1 | day(Date) == 2))  

### chekcing for '?'
tt_na <- 0
for (i in 1:length(epcData_ft)) {
    tt_na <- tt_na + sum(grepl("^[?]",epcData_ft[,i]))
}
print (tt_na)

## building plot 1 (histogram)
hist(as.numeric(epcData_ft$Global_active_power), 
     main = "Global Active Power",col = "red", breaks = 20, ylim = c(0, 1200), 
     xlab = "Global Active Power (kilowatts)")

## copying to PNG file
dev.copy(png, file = "plot1.png", width = 480, height = 480)
dev.off()



