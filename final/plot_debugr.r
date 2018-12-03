#Sys.setenv(TZ="America/Los_Angeles")
rm(list = ls()) 

setwd("~/Documents/Rscripts/shiny/vanduzenridgeapp")
Sys.setenv(TZ="America/Los_Angeles")
source("libs.r")
source("fun_defs.r")
source("vanduzenridges_df_create.r")

#eigh19thanomaly <- hefs_dtrmnstc %>% filter(fcast_t_pdt >= ymd("2016-01-18"), fcast_t_pdt <- ymd("2016-01-19"))
am_fcast <- ymd_hms(fcasts_firstam)
#fcasts_firstam_date <- date(fcasts_firstam)

## plot inputs
am_fcast <- c("2016-01-13 07:37:00 PST") #fcasts[4]
fcast_start_date <- mdy(c("01-17-2016"))
fcast_end_date <- mdy(c("01-18-2016"))
vol_ridge_scale <- c(0.0001)
flo_ridge_scale <- c(0.0001)
sta_ridge_scale <- c(0.1)



plotridgesandpdfs_cumvol(hefs_dtrmnstc, am_fcast, fcast_start_date, fcast_end_date, vol_ridge_scale)
plotridgesandpdfs_flow(hefs_dtrmnstc, am_fcast, fcast_start_date, fcast_end_date, flo_ridge_scale)
plotridgesandpdfs_stage(hefs_dtrmnstc, am_fcast, fcast_start_date, fcast_end_date, sta_ridge_scale)


