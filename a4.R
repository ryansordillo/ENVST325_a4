#Assignment 4
#Ryan Sordillo
#2/24/2026

weather <- read.csv("/cloud/project/activity04/campus_weather.csv",
                    na.strings = "#N/A")

metaDat <- read.csv("/cloud/project/activity04/meter_weather_metadata.csv",
                    na.strings = "#N/A")

install.packages(c("dplyr","ggplot2","lubridate"))
library(ggplot2)
library(dplyr)
library(lubridate)

# parse date
weather$dateF <- mdy_hm(weather$Date)
# create a month column
weather$doy <- yday(weather$dateF)
# create a year column
weather$year <- year(weather$dateF)

# examine precipitation using a bar plot
ggplot(data=weather[weather$doy > 121 & weather$doy < 274 ,],
       aes(x=dateF,
           y=Precip))+
  geom_col(color="royalblue4")+
  theme_classic()

# add a column to weather:
weather$precip.QC <- ifelse(weather$doy >= 121 & weather$doy <= 188 & weather$year == 2021, 
                            # evaluate if the doy is between May 1 and July 7 2021
                            NA, # value if true
                            weather$Precip) # value if false: uses original precipitation observation


weather$FreezeFlag <- ifelse(weather$AirTemp <= 0, # check if at or below zero
                             1, # if true: set flag to 1
                             0) # if false: set flag to zero

# set up intervals
intervals <- weather$dateF[-length(weather$dateF)] %--% weather$dateF[-1] # start date %--% end date
# interval starts with row 1, and row 2
# and ends on second to last row and final row
# calculate interval times
interval_times <- int_length(intervals)
# check interval times
intervals[interval_times != 900]

#create function for checking for irregular intervals that
# deviate from 900 seconds
# only argument is x, a vector of POSIXct formatted dates to be checked
# with an expected interval of 900 seconds
# the output is all observations that deviate from 900 seconds
timeCheck900 <- function(x){
  intervals <- x[-length(x)] %--% x[-1]
  interval_times <- int_length(intervals)
  intervals[interval_times != 900]
  
}
# run on weather data
timeCheck900(weather$dateF)

#For loops
soilFiles <- list.files("/cloud/project/activity04/soil")
print(soilFiles)

# start an empty list
soilList <- list()
# start an empty list
for(i in 1:length(soilFiles)){
  soilList[[i]] <- read.csv(paste0("/cloud/project/activity04/soil/", soilFiles[i]))
  
}
# inspect the begining of the first file
head(soilList[[1]])
# get info about your list
str(soilList)

soilData <- do.call("rbind", soilList)
str(soilData)

#####In Class Prompts

#1. Calculate a rolling average of air temperatures over eight 15 min measurements (2 hours) for January
#of 2022 using a for loop. Make a plot of the 15 minute air temperature and the rolling average.

january <- weather %>%
  filter(year == 2022 & month(dateF) == 1)

rolling_avg <- rep(NA, nrow(january))

for(i in 8:nrow(january)){
  rolling_avg[i] <- mean(january$AirTemp[(i-7):i], na.rm = TRUE)
}

january$rolling_average <- rolling_avg

#Plot rolling average
ggplot(january, aes(x=dateF)) +
  geom_line(aes(y=AirTemp), color = "red")+
  geom_line(aes(y=rolling_average), color="darkblue")+
  theme_classic()+
  labs(title = "January 2022 Air Temp with Rolling Average",
       x="Date",
       y="Air Temperature(C)")

#Prompt 2
##You want to see if the solar radiation measurements experienced any issues with build up or
#accumulation on the sensor in May and June of 2021. Make an assessment with your group.

may_june <- weather %>%
  filter(year == 2021 & month(dateF) == 5 | month(dateF) == 6)


ggplot(may_june, aes(x=dateF))+
  geom_line(aes(y=SolRad), color="red")+
  theme_classic()





