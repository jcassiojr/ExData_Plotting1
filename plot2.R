##########################################################
# PLOT 2
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

### filtering data
epcData_ft <- 
    epcData %>%
    ## creating column DateTime
    mutate(DateTime = paste(Date,Time)) %>%
    ## changing Date and Time
    mutate(Date = dmy(Date)) %>%
    ## filtering from 2007/02/01 to 2007/02/01 
    filter(year(Date) == 2007 & month(Date) == 2 & (day(Date) == 1 | day(Date) == 2))   

### plotting data
plot(strptime(epcData_ft$DateTime,'%d/%m/%Y %H:%M:%S'), as.numeric(epcData_ft$Global_active_power), 
     ylab= "Global Active Power (kilowatts)", xlab = "", type='l', col='black') 

## copying plot to PNG
dev.copy(png, file = "plot2.png", width = 480, height = 480)
dev.off()
