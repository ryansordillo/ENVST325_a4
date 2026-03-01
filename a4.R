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
  rolling_avg[i] <- mean(january$AirTemp[(i-7):i])
}

january$rolling_average <- rolling_avg

#Plot rolling average
ggplot(january, aes(x=dateF)) +
  geom_line(aes(y=AirTemp), color = "red")+
  geom_line(aes(y=january$rolling_average), color="darkblue")+
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


#Prompt 3.
#Check for any date time issues using the function created in the tutorial. Investigate instances of date
#time issues. What happens around daylight savings? Are there issues with the time zone assumption?

#Looking at output from the function, we can see a few time issues with the data. The first
#is due to daylight savings time where on March 14 2021, the interval is 75 minutes instead of 
#15 minutes which is because the clock jumped forward an hour on daylight savings.
#This comes back into effect in November 2021-11-07 03:30:00 UTC--2021-11-07 02:45:00 UTC,
# the time goes backwards due to daylight savings and gaining an extra hour.
#There are also a couple instances of intervals with 0 time as well as some with much larger
#intervals.


##Question 1
"As the weather station data manager, you been asked to share precipitation data with the village of
Clinton. You want to ensure that there are no issues with the bird excrement or frozen precipitation.
You want to exclude any precipitation that occurs when the air temperature is below zero. You also
want to check that no precipitation measurements are used if the X and Y level observations are more
than 2 degrees.
Do you think there might be any additional issues with the precipitation data to consider before
sending the data to the village? Describe what additional data and types of quality control tests (do
not need to implement) would help you take additional steps to remove problematic precipitation
data value.
Indicate how many missing precipitation values are in your data."

sum(is.na(weather$Precip))
#1158 missing precipitation data values


##Question 2
"Create a data flag that warns a user if the battery voltage falls below 8.5 Volts. Explain how you set up
the flag."

weather$battery_flag <- ifelse(weather$BatVolt <= 8500, 1, 0)


##Question 3
"You should also create a function that checks for observations that are in unrealistic data ranges in air
temperature and solar radiation. Explain how your function works and your reasoning."

quality_check <- function(airTemp, SolRad){
  air_temp_flag <- ifelse(airTemp < -40 | airTemp > 50, 1, 0)
  sol_rad_flag <- ifelse(SolRad < 0  | SolRad > 1400, 1, 0)
  return (data.frame(
    AirTempFlag <- air_temp_flag,
    SolRadFlag <- sol_rad_flag
  ))
}

qc_flags <- quality_check(weather$AirTemp, weather$SolRad)


#Question 4.
"Make a plot of winter air temperatures in Jan - Mar of 2021. Check for persistence issues that might
indicate snow accumulation on the sensor. Describe whether you think this might be an issue.
"

winter_2021 <- weather %>%
  filter(year == 2021 & month(dateF) %in% c(1,2,3))

ggplot(winter_2021, aes(x=dateF, y=AirTemp))+
  geom_line(color="blue")+
  theme_classic()+
  labs(
    title = "Winter Air Temperature (Jan–Mar 2021)",
    x = "Date",
    y = "Air Temperature (°C)"
  )

##Question 5.
"You are asked for total daily precipitation in March and April of 2021. 
Use a for loop to exclude (convert to NA) any days that include temperatures 
less than 35 degrees F on that day or the day prior to ensure that measurements 
are not likely to be affected by snow accumulation on the sensor. How many daily 
observations have precipitation observations (not a NA) in your final data table? "

spring_2021 <- weather %>%
  filter(year == 2021 & month(dateF) %in% c(3,4))

spring_2021 <- spring_2021 %>%
  select(dateF, Precip, AirTemp)

daily <- spring_2021 %>%
  group_by(month(dateF), day(dateF)) %>%
  summarize(
    daily_precip = sum(Precip),
    min_temp = min(AirTemp))

daily$precipQC <- daily$daily_precip

for(i in 2:nrow(daily)){
  if(daily$min_temp[i] < 1.67 |
     daily$min_temp[i-1] < 1.67){
    daily$precipQC[i] <- NA
  }
}

sum(is.na(daily$precipQC))
#19


#Question 6
"Read in the soil temperature data using the for loop. Alter your time interval function to include a user specified time interval (in seconds) as an argument in the 
function. Check for any clock/time issues associated with the soil data and 
include the results in your output. Include a brief description of any 
issues with the clock/data availability. "

soilFiles <- list.files("/cloud/project/activity04/soil")
# start an empty list
soilList <- list()
# start an empty list
for(i in 1:length(soilFiles)){
  soilList[[i]] <- read.csv(paste0("/cloud/project/activity04/soil/", soilFiles[i]))
  
}
soilData <- do.call("rbind", soilList)
soilData$dateF <- ymd_hm(soilData$Timestamp)

timeCheck <- function(x, interval){
  intervals <- x[-length(x)] %--% x[-1]
  interval_times <- int_length(intervals)
  data.frame(
    start_time = x[-length(x)],
    end_time = x[-1],
    interval_sec = interval_times
  ) %>%
    filter(interval_sec != interval)
}

#Checking on 15 minute intervals

soil_time_check <- timeCheck(soilData$dateF,3600)


