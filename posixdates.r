
library(lubridate)
Sys.setenv(TZ="Europe/London") #needing to convert TZs later on

start_datetime <- as.POSIXct("2016-01-13 08:00:00", tz = "Europe/London")
end_datetime   <- as.POSIXct("2016-01-16 15:00:00", tz = "Europe/London")

# using R's internal setting of 1970-01-01, gets 2016-01-14 23:30:00
midpoint <- as.POSIXct((as.numeric(end_datetime) + 
                          as.numeric(start_datetime)) / 2, origin = '1970-01-01') 

#gets 2016-01-15 only
midpoint_nearesthour_fail1 <- round_date(midpoint, "hours") #lubridate function

#gets 2016-01-15 only
midpoint_nearesthour_fail2 <- round(midpoint, "hours")  #base R function

#gets 2016-01-14 23:30:00 as above
#or, if all three "Europe/London" replaced with "America/Los_Angeles": 2016-01-15 07:30:00
midpoint_nearesthour_fail3 <- as_datetime(midpoint) 


as.POSIXct("2018-12-02 00:00:00")

format(as.POSIXct("2016-01-15 00:00:00"), "%Y-%m-%d %H:%M:%S")

format(as.POSIXct(midpoint_nearesthour_fail2), "%Y-%m-%d %H:%M:%S")
