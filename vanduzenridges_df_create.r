
#rm(list = ls()) 

#Sys.setenv(TZ="America/Los_Angeles")
#Sys.getenv("TZ")
#source("fun_defs_redo.r")

dtrmnstc <- read_csv("data/brgc1_20160113_tidy_dmstc.csv") %>%  #all deterministics 13th to 17th (some have 2/day, some 3/day)
  transmute(fcast_i_gmt = forecast_issuance, fcast_t_gmt = forecast_time, kcfs) %>% 
  mutate(fcast_i_gmt = mdy_hm(fcast_i_gmt, tz = 
         "Europe/London"), fcast_t_gmt = mdy_hm(fcast_t_gmt, tz = "Europe/London")) %>% 
         mutate(kcfs = ifelse(kcfs == -9999, NA, kcfs), 
         fcast_t_pdt = with_tz(fcast_t_gmt, "America/Los_Angeles"),
         fcast_i_pdt = with_tz(fcast_i_gmt, "America/Los_Angeles"),
         cfs = 1000*kcfs)
fcasts <- unique(dtrmnstc$fcast_i_pdt)
fcasts_firstam <- fcasts[c(1,4,7,10,12)]
dtrmnstc <- dtrmnstc %>% filter(fcast_i_pdt %in% fcasts_firstam) %>% mutate(fcast_i_pdt_date = date(fcast_i_pdt))
dtrmnstc <- dtrmnstc %>% na.omit()
rating <- read_csv("data/brgc1_rating_1.6.2016_table410024.csv")

dtrmnstc$feet <- approx(x = rating$brgc1_cfs_1.6.16, y = rating$brgc1_ft_1.6.16, xout = dtrmnstc$cfs)$y
dtrmnstc <- dtrmnstc %>% mutate(mefp_yr = "dtrmnstc")

#usgs_obs <- read_csv("data/vanduz_obs.csv") %>% mutate(pdt  = ymd_hms(pdt, tz="America/Los_Angeles")) %>%
#  mutate(gmt = with_tz(pdt, "Europe/London"), cfs = as.numeric(cfs), kcfs = cfs/1000, feet = as.numeric(stage_ft)) %>% select(-"stage_ft")

#usgs_obs_hrly <- usgs_obs %>% filter(minute(pdt) == 0 )


csv13jan16.12z_hefs <- read_csv("data/2016011312_brgc1.csv") %>% mutate(fcast_t_gmt = mdy_hm(GMT)) %>% select(-"GMT")  %>% mutate(fcast_i_pdt = fcasts[1], fcast_i_pdt_date = date(fcast_i_pdt) ) #%>% filter(fcast_t_gmt <= "2016-01-25 08:00")
csv14jan16.12z_hefs <- read_csv("data/2016011412_brgc1.csv") %>% mutate(fcast_t_gmt = mdy_hm(GMT)) %>% select(-"GMT")  %>% mutate(fcast_i_pdt = fcasts[4], fcast_i_pdt_date = date(fcast_i_pdt) ) #%>% filter(fcast_t_gmt <= "2016-01-25 08:00")
csv15jan16.12z_hefs <- read_csv("data/2016011512_brgc1.csv") %>% mutate(fcast_t_gmt = mdy_hm(GMT)) %>% select(-"GMT")  %>% mutate(fcast_i_pdt = fcasts[7], fcast_i_pdt_date = date(fcast_i_pdt) ) #%>% filter(fcast_t_gmt <= "2016-01-25 08:00")
csv16jan16.12z_hefs <- read_csv("data/2016011612_brgc1.csv") %>% mutate(fcast_t_gmt = mdy_hm(GMT)) %>% select(-"GMT")  %>% mutate(fcast_i_pdt = fcasts[10],fcast_i_pdt_date = date(fcast_i_pdt) ) #%>% filter(fcast_t_gmt <= "2016-01-25 08:00")
csv17jan16.12z_hefs <- read_csv("data/2016011712_brgc1.csv") %>% mutate(fcast_t_gmt = mdy_hm(GMT)) %>% select(-"GMT")  %>% mutate(fcast_i_pdt = fcasts[12],fcast_i_pdt_date = date(fcast_i_pdt) ) #%>% filter(fcast_t_gmt <= "2016-01-25 08:00")

csv13jan16.12z_hefs %<>%  gather(key = mefp_yr, value = "kcfs", 1:59) %>% mutate(mefp_yr = as.integer(substring(mefp_yr,2)),cfs = 1000*kcfs) 
csv14jan16.12z_hefs %<>%  gather(key = mefp_yr, value = "kcfs", 1:59) %>% mutate(mefp_yr = as.integer(substring(mefp_yr,2)),cfs = 1000*kcfs)
csv15jan16.12z_hefs %<>%  gather(key = mefp_yr, value = "kcfs", 1:59) %>% mutate(mefp_yr = as.integer(substring(mefp_yr,2)),cfs = 1000*kcfs)
csv16jan16.12z_hefs %<>%  gather(key = mefp_yr, value = "kcfs", 1:59) %>% mutate(mefp_yr = as.integer(substring(mefp_yr,2)),cfs = 1000*kcfs)
csv17jan16.12z_hefs %<>%  gather(key = mefp_yr, value = "kcfs", 1:59) %>% mutate(mefp_yr = as.integer(substring(mefp_yr,2)),cfs = 1000*kcfs)

csv13jan16.12z_hefs$feet <- approx(x = rating$brgc1_cfs_1.6.16, y = rating$brgc1_ft_1.6.16, xout = csv13jan16.12z_hefs$cfs)$y
csv14jan16.12z_hefs$feet <- approx(x = rating$brgc1_cfs_1.6.16, y = rating$brgc1_ft_1.6.16, xout = csv14jan16.12z_hefs$cfs)$y 
csv15jan16.12z_hefs$feet <- approx(x = rating$brgc1_cfs_1.6.16, y = rating$brgc1_ft_1.6.16, xout = csv15jan16.12z_hefs$cfs)$y 
csv16jan16.12z_hefs$feet <- approx(x = rating$brgc1_cfs_1.6.16, y = rating$brgc1_ft_1.6.16, xout = csv16jan16.12z_hefs$cfs)$y 
csv17jan16.12z_hefs$feet <- approx(x = rating$brgc1_cfs_1.6.16, y = rating$brgc1_ft_1.6.16, xout = csv17jan16.12z_hefs$cfs)$y 

df_hefs <- rbind(csv13jan16.12z_hefs, csv14jan16.12z_hefs, csv15jan16.12z_hefs, csv16jan16.12z_hefs, csv17jan16.12z_hefs) 
df_hefs <- df_hefs %>% mutate(fcast_t_pdt = with_tz(fcast_t_gmt, "America/Los_Angeles"), 
                              fcast_i_gmt = with_tz(fcast_i_pdt, "Europe/London"))           

df_hefs$feet <- approx(x = rating$brgc1_cfs_1.6.16, y = rating$brgc1_ft_1.6.16, xout = df_hefs$cfs)$y



hefs_dtrmnstc <- rbind( dtrmnstc, df_hefs) 
#removes all dates (and duplicate dates) if any in either dtrmst or hefs have NAs








rm(list = ls()[grep("^csv", ls())])  #releases single csvs from memory - deletes everything starting with `csv`!)


##########################################################################################################
################# flow  ##################################################################################
##########################################################################################################


## hourly #############
#### type 4 quants
#                              create_hrly_quants: f(df, hourcolumn, value, qtype, metadata_to_add) 
#csv13jan16.12z_hefs_f_qntls_h_t4 <- create_hrly_quants(csv13jan16.12z_hefs, "fcast_t_gmt", "kcfs", 4, fcasts[1]) 
#csv14jan16.12z_hefs_f_qntls_h_t4 <- create_hrly_quants(csv14jan16.12z_hefs, "fcast_t_gmt", "kcfs", 4, fcasts[4]) 
#csv15jan16.12z_hefs_f_qntls_h_t4 <- create_hrly_quants(csv15jan16.12z_hefs, "fcast_t_gmt", "kcfs", 4, fcasts[7]) 
#csv16jan16.12z_hefs_f_qntls_h_t4 <- create_hrly_quants(csv16jan16.12z_hefs, "fcast_t_gmt", "kcfs", 4, fcasts[10]) 
#csv17jan16.12z_hefs_f_qntls_h_t4 <- create_hrly_quants(csv17jan16.12z_hefs, "fcast_t_gmt", "kcfs", 4, fcasts[12])
#
#
##### type 5 quants
##                              create_hrly_quants: f(df, hourcolumn, value, qtype, metadata_to_add) 
#csv13jan16.12z_hefs_f_qntls_h_t5 <- create_hrly_quants(csv13jan16.12z_hefs, "fcast_t_gmt", "kcfs", 5, fcasts[1]) 
#csv14jan16.12z_hefs_f_qntls_h_t5 <- create_hrly_quants(csv14jan16.12z_hefs, "fcast_t_gmt", "kcfs", 5, fcasts[4]) 
#csv15jan16.12z_hefs_f_qntls_h_t5 <- create_hrly_quants(csv15jan16.12z_hefs, "fcast_t_gmt", "kcfs", 5, fcasts[7]) 
#csv16jan16.12z_hefs_f_qntls_h_t5 <- create_hrly_quants(csv16jan16.12z_hefs, "fcast_t_gmt", "kcfs", 5, fcasts[10]) 
#csv17jan16.12z_hefs_f_qntls_h_t5 <- create_hrly_quants(csv17jan16.12z_hefs, "fcast_t_gmt", "kcfs", 5, fcasts[12]) 
#
##### type 6 quants
##                              create_hrly_quants: f(df, hourcolumn, value, qtype, metadata_to_add) 
#csv13jan16.12z_hefs_f_qntls_h_t6 <- create_hrly_quants(csv13jan16.12z_hefs, "fcast_t_gmt", "kcfs", 6, fcasts[1]) 
#csv14jan16.12z_hefs_f_qntls_h_t6 <- create_hrly_quants(csv14jan16.12z_hefs, "fcast_t_gmt", "kcfs", 6, fcasts[4]) 
#csv15jan16.12z_hefs_f_qntls_h_t6 <- create_hrly_quants(csv15jan16.12z_hefs, "fcast_t_gmt", "kcfs", 6, fcasts[7]) 
#csv16jan16.12z_hefs_f_qntls_h_t6 <- create_hrly_quants(csv16jan16.12z_hefs, "fcast_t_gmt", "kcfs", 6, fcasts[10]) 
#csv17jan16.12z_hefs_f_qntls_h_t6 <- create_hrly_quants(csv17jan16.12z_hefs, "fcast_t_gmt", "kcfs", 6, fcasts[12]) 
#
##### type 7 quants
##                              create_hrly_quants: f(df, hourcolumn, value, qtype, metadata_to_add) 
#csv13jan16.12z_hefs_f_qntls_h_t7 <- create_hrly_quants(csv13jan16.12z_hefs, "fcast_t_gmt", "kcfs", 7, fcasts[1]) 
#csv14jan16.12z_hefs_f_qntls_h_t7 <- create_hrly_quants(csv14jan16.12z_hefs, "fcast_t_gmt", "kcfs", 7, fcasts[4]) 
#csv15jan16.12z_hefs_f_qntls_h_t7 <- create_hrly_quants(csv15jan16.12z_hefs, "fcast_t_gmt", "kcfs", 7, fcasts[7]) 
#csv16jan16.12z_hefs_f_qntls_h_t7 <- create_hrly_quants(csv16jan16.12z_hefs, "fcast_t_gmt", "kcfs", 7, fcasts[10]) 
#csv17jan16.12z_hefs_f_qntls_h_t7 <- create_hrly_quants(csv17jan16.12z_hefs, "fcast_t_gmt", "kcfs", 7, fcasts[12]) 
#
##### type 8 quants
##                              create_hrly_quants: f(df, hourcolumn, value, qtype, metadata_to_add) 
#csv13jan16.12z_hefs_f_qntls_h_t8 <- create_hrly_quants(csv13jan16.12z_hefs, "fcast_t_gmt", "kcfs", 8, fcasts[1]) 
#csv14jan16.12z_hefs_f_qntls_h_t8 <- create_hrly_quants(csv14jan16.12z_hefs, "fcast_t_gmt", "kcfs", 8, fcasts[4]) 
#csv15jan16.12z_hefs_f_qntls_h_t8 <- create_hrly_quants(csv15jan16.12z_hefs, "fcast_t_gmt", "kcfs", 8, fcasts[7]) 
#csv16jan16.12z_hefs_f_qntls_h_t8 <- create_hrly_quants(csv16jan16.12z_hefs, "fcast_t_gmt", "kcfs", 8, fcasts[10]) 
#csv17jan16.12z_hefs_f_qntls_h_t8 <- create_hrly_quants(csv17jan16.12z_hefs, "fcast_t_gmt", "kcfs", 8, fcasts[12]) 
#
##### type 9 quants
##                              create_hrly_quants: f(df, hourcolumn, value, qtype, metadata_to_add) 
#csv13jan16.12z_hefs_f_qntls_h_t9 <- create_hrly_quants(csv13jan16.12z_hefs, "fcast_t_gmt", "kcfs", 9, fcasts[1]) 
#csv14jan16.12z_hefs_f_qntls_h_t9 <- create_hrly_quants(csv14jan16.12z_hefs, "fcast_t_gmt", "kcfs", 9, fcasts[4]) 
#csv15jan16.12z_hefs_f_qntls_h_t9 <- create_hrly_quants(csv15jan16.12z_hefs, "fcast_t_gmt", "kcfs", 9, fcasts[7]) 
#csv16jan16.12z_hefs_f_qntls_h_t9 <- create_hrly_quants(csv16jan16.12z_hefs, "fcast_t_gmt", "kcfs", 9, fcasts[10]) 
#csv17jan16.12z_hefs_f_qntls_h_t9 <- create_hrly_quants(csv17jan16.12z_hefs, "fcast_t_gmt", "kcfs", 9, fcasts[12]) 
#
#df_f_qntls_h <- rbind(csv13jan16.12z_hefs_f_qntls_h_t4,
#                      csv14jan16.12z_hefs_f_qntls_h_t4,
#                      csv15jan16.12z_hefs_f_qntls_h_t4,
#                      csv16jan16.12z_hefs_f_qntls_h_t4,
#                      csv17jan16.12z_hefs_f_qntls_h_t4,
#                      csv13jan16.12z_hefs_f_qntls_h_t5,
#                      csv14jan16.12z_hefs_f_qntls_h_t5,
#                      csv15jan16.12z_hefs_f_qntls_h_t5,
#                      csv16jan16.12z_hefs_f_qntls_h_t5,
#                      csv17jan16.12z_hefs_f_qntls_h_t5,
#                      csv13jan16.12z_hefs_f_qntls_h_t6,
#                      csv14jan16.12z_hefs_f_qntls_h_t6,
#                      csv15jan16.12z_hefs_f_qntls_h_t6,
#                      csv16jan16.12z_hefs_f_qntls_h_t6,
#                      csv17jan16.12z_hefs_f_qntls_h_t6,
#                      csv13jan16.12z_hefs_f_qntls_h_t7,
#                      csv14jan16.12z_hefs_f_qntls_h_t7,
#                      csv15jan16.12z_hefs_f_qntls_h_t7,
#                      csv16jan16.12z_hefs_f_qntls_h_t7,
#                      csv17jan16.12z_hefs_f_qntls_h_t7,
#                      csv13jan16.12z_hefs_f_qntls_h_t8,
#                      csv14jan16.12z_hefs_f_qntls_h_t8,
#                      csv15jan16.12z_hefs_f_qntls_h_t8,
#                      csv16jan16.12z_hefs_f_qntls_h_t8,
#                      csv17jan16.12z_hefs_f_qntls_h_t8,
#                      csv13jan16.12z_hefs_f_qntls_h_t9,
#                      csv14jan16.12z_hefs_f_qntls_h_t9,
#                      csv15jan16.12z_hefs_f_qntls_h_t9,
#                      csv16jan16.12z_hefs_f_qntls_h_t9,
#                      csv17jan16.12z_hefs_f_qntls_h_t9
#) %>% mutate(fcast_t_pdt = with_tz(fcast_t_gmt, "America/Los_Angeles"),
#             qfacet = paste0("q type ", qtype))




######### whole forecast exceedance #############
#### type 4 quants
##     create_5dyfc_quants <- function(df, modelrun, value, qtype, fcast_i_pdt)   {
#csv13jan16.12z_hefs_f_qntls_fc_t4 <- create_5dyfc_quants(csv13jan16.12z_hefs, "mefp_yr", "kcfs", 4, fcasts[1]) 
#csv14jan16.12z_hefs_f_qntls_fc_t4 <- create_5dyfc_quants(csv14jan16.12z_hefs, "mefp_yr", "kcfs", 4, fcasts[4]) 
#csv15jan16.12z_hefs_f_qntls_fc_t4 <- create_5dyfc_quants(csv15jan16.12z_hefs, "mefp_yr", "kcfs", 4, fcasts[7]) 
#csv16jan16.12z_hefs_f_qntls_fc_t4 <- create_5dyfc_quants(csv16jan16.12z_hefs, "mefp_yr", "kcfs", 4, fcasts[10]) 
#csv17jan16.12z_hefs_f_qntls_fc_t4 <- create_5dyfc_quants(csv17jan16.12z_hefs, "mefp_yr", "kcfs", 4, fcasts[12])
#
##### type 5 quants
###     create_5dyfc_quants <- function(df, modelrun, value, qtype, fcast_i_pdt)   {
#csv13jan16.12z_hefs_f_qntls_fc_t5 <- create_5dyfc_quants(csv13jan16.12z_hefs, "mefp_yr", "kcfs", 5, fcasts[1]) 
#csv14jan16.12z_hefs_f_qntls_fc_t5 <- create_5dyfc_quants(csv14jan16.12z_hefs, "mefp_yr", "kcfs", 5, fcasts[4]) 
#csv15jan16.12z_hefs_f_qntls_fc_t5 <- create_5dyfc_quants(csv15jan16.12z_hefs, "mefp_yr", "kcfs", 5, fcasts[7]) 
#csv16jan16.12z_hefs_f_qntls_fc_t5 <- create_5dyfc_quants(csv16jan16.12z_hefs, "mefp_yr", "kcfs", 5, fcasts[10]) 
#csv17jan16.12z_hefs_f_qntls_fc_t5 <- create_5dyfc_quants(csv17jan16.12z_hefs, "mefp_yr", "kcfs", 5, fcasts[12])
#
##### type 6 quants
###     create_5dyfc_quants <- function(df, modelrun, value, qtype, fcast_i_pdt)   {
#csv13jan16.12z_hefs_f_qntls_fc_t6 <- create_5dyfc_quants(csv13jan16.12z_hefs, "mefp_yr", "kcfs", 6, fcasts[1]) 
#csv14jan16.12z_hefs_f_qntls_fc_t6 <- create_5dyfc_quants(csv14jan16.12z_hefs, "mefp_yr", "kcfs", 6, fcasts[4]) 
#csv15jan16.12z_hefs_f_qntls_fc_t6 <- create_5dyfc_quants(csv15jan16.12z_hefs, "mefp_yr", "kcfs", 6, fcasts[7]) 
#csv16jan16.12z_hefs_f_qntls_fc_t6 <- create_5dyfc_quants(csv16jan16.12z_hefs, "mefp_yr", "kcfs", 6, fcasts[10]) 
#csv17jan16.12z_hefs_f_qntls_fc_t6 <- create_5dyfc_quants(csv17jan16.12z_hefs, "mefp_yr", "kcfs", 6, fcasts[12])
#
##### type 7 quants
###     create_5dyfc_quants <- function(df, modelrun, value, qtype, fcast_i_pdt)   {
#csv13jan16.12z_hefs_f_qntls_fc_t7 <- create_5dyfc_quants(csv13jan16.12z_hefs, "mefp_yr", "kcfs", 7, fcasts[1]) 
#csv14jan16.12z_hefs_f_qntls_fc_t7 <- create_5dyfc_quants(csv14jan16.12z_hefs, "mefp_yr", "kcfs", 7, fcasts[4]) 
#csv15jan16.12z_hefs_f_qntls_fc_t7 <- create_5dyfc_quants(csv15jan16.12z_hefs, "mefp_yr", "kcfs", 7, fcasts[7]) 
#csv16jan16.12z_hefs_f_qntls_fc_t7 <- create_5dyfc_quants(csv16jan16.12z_hefs, "mefp_yr", "kcfs", 7, fcasts[10]) 
#csv17jan16.12z_hefs_f_qntls_fc_t7 <- create_5dyfc_quants(csv17jan16.12z_hefs, "mefp_yr", "kcfs", 7, fcasts[12])
#
##### type 8 quants
###     create_5dyfc_quants <- function(df, modelrun, value, qtype, fcast_i_pdt)   {
#csv13jan16.12z_hefs_f_qntls_fc_t8 <- create_5dyfc_quants(csv13jan16.12z_hefs, "mefp_yr", "kcfs", 8, fcasts[1]) 
#csv14jan16.12z_hefs_f_qntls_fc_t8 <- create_5dyfc_quants(csv14jan16.12z_hefs, "mefp_yr", "kcfs", 8, fcasts[4]) 
#csv15jan16.12z_hefs_f_qntls_fc_t8 <- create_5dyfc_quants(csv15jan16.12z_hefs, "mefp_yr", "kcfs", 8, fcasts[7]) 
#csv16jan16.12z_hefs_f_qntls_fc_t8 <- create_5dyfc_quants(csv16jan16.12z_hefs, "mefp_yr", "kcfs", 8, fcasts[10]) 
#csv17jan16.12z_hefs_f_qntls_fc_t8 <- create_5dyfc_quants(csv17jan16.12z_hefs, "mefp_yr", "kcfs", 8, fcasts[12])
#
##### type 9 quants
###     create_5dyfc_quants <- function(df, modelrun, value, qtype, fcast_i_pdt)   {
#csv13jan16.12z_hefs_f_qntls_fc_t9 <- create_5dyfc_quants(csv13jan16.12z_hefs, "mefp_yr", "kcfs", 9, fcasts[1]) 
#csv14jan16.12z_hefs_f_qntls_fc_t9 <- create_5dyfc_quants(csv14jan16.12z_hefs, "mefp_yr", "kcfs", 9, fcasts[4]) 
#csv15jan16.12z_hefs_f_qntls_fc_t9 <- create_5dyfc_quants(csv15jan16.12z_hefs, "mefp_yr", "kcfs", 9, fcasts[7]) 
#csv16jan16.12z_hefs_f_qntls_fc_t9 <- create_5dyfc_quants(csv16jan16.12z_hefs, "mefp_yr", "kcfs", 9, fcasts[10]) 
#csv17jan16.12z_hefs_f_qntls_fc_t9 <- create_5dyfc_quants(csv17jan16.12z_hefs, "mefp_yr", "kcfs", 9, fcasts[12])
#
#df_f_qntls_p <- rbind(csv13jan16.12z_hefs_f_qntls_fc_t4,
#                      csv14jan16.12z_hefs_f_qntls_fc_t4,
#                      csv15jan16.12z_hefs_f_qntls_fc_t4,
#                      csv16jan16.12z_hefs_f_qntls_fc_t4,
#                      csv17jan16.12z_hefs_f_qntls_fc_t4,
#                      csv13jan16.12z_hefs_f_qntls_fc_t5,
#                      csv14jan16.12z_hefs_f_qntls_fc_t5,
#                      csv15jan16.12z_hefs_f_qntls_fc_t5,
#                      csv16jan16.12z_hefs_f_qntls_fc_t5,
#                      csv17jan16.12z_hefs_f_qntls_fc_t5,
#                      csv13jan16.12z_hefs_f_qntls_fc_t6,
#                      csv14jan16.12z_hefs_f_qntls_fc_t6,
#                      csv15jan16.12z_hefs_f_qntls_fc_t6,
#                      csv16jan16.12z_hefs_f_qntls_fc_t6,
#                      csv17jan16.12z_hefs_f_qntls_fc_t6,
#                      csv13jan16.12z_hefs_f_qntls_fc_t7,
#                      csv14jan16.12z_hefs_f_qntls_fc_t7,
#                      csv15jan16.12z_hefs_f_qntls_fc_t7,
#                      csv16jan16.12z_hefs_f_qntls_fc_t7,
#                      csv17jan16.12z_hefs_f_qntls_fc_t7,
#                      csv13jan16.12z_hefs_f_qntls_fc_t8,
#                      csv14jan16.12z_hefs_f_qntls_fc_t8,
#                      csv15jan16.12z_hefs_f_qntls_fc_t8,
#                      csv16jan16.12z_hefs_f_qntls_fc_t8,
#                      csv17jan16.12z_hefs_f_qntls_fc_t8,
#                      csv13jan16.12z_hefs_f_qntls_fc_t9,
#                      csv14jan16.12z_hefs_f_qntls_fc_t9,
#                      csv15jan16.12z_hefs_f_qntls_fc_t9,
#                      csv16jan16.12z_hefs_f_qntls_fc_t9,
#                      csv17jan16.12z_hefs_f_qntls_fc_t9
#) 

##########################################################################################################
################# stage  #################################################################################
##########################################################################################################


## hourly #############
#### type 4 quants
#                              create_hrly_quants: f(df, hourcolumn, value, qtype, metadata_to_add) 
#csv13jan16.12z_hefs_s_qntls_h_t4 <- create_hrly_quants(csv13jan16.12z_hefs, "fcast_t_gmt", "feet", 4, fcasts[1]) 
#csv14jan16.12z_hefs_s_qntls_h_t4 <- create_hrly_quants(csv14jan16.12z_hefs, "fcast_t_gmt", "feet", 4, fcasts[4]) 
#csv15jan16.12z_hefs_s_qntls_h_t4 <- create_hrly_quants(csv15jan16.12z_hefs, "fcast_t_gmt", "feet", 4, fcasts[7]) 
#csv16jan16.12z_hefs_s_qntls_h_t4 <- create_hrly_quants(csv16jan16.12z_hefs, "fcast_t_gmt", "feet", 4, fcasts[10]) 
#csv17jan16.12z_hefs_s_qntls_h_t4 <- create_hrly_quants(csv17jan16.12z_hefs, "fcast_t_gmt", "feet", 4, fcasts[12])
#
#
##### type 5 quants
##                              create_hrly_quants: f(df, hourcolumn, value, qtype, metadata_to_add) 
#csv13jan16.12z_hefs_s_qntls_h_t5 <- create_hrly_quants(csv13jan16.12z_hefs, "fcast_t_gmt", "feet", 5, fcasts[1]) 
#csv14jan16.12z_hefs_s_qntls_h_t5 <- create_hrly_quants(csv14jan16.12z_hefs, "fcast_t_gmt", "feet", 5, fcasts[4]) 
#csv15jan16.12z_hefs_s_qntls_h_t5 <- create_hrly_quants(csv15jan16.12z_hefs, "fcast_t_gmt", "feet", 5, fcasts[7]) 
#csv16jan16.12z_hefs_s_qntls_h_t5 <- create_hrly_quants(csv16jan16.12z_hefs, "fcast_t_gmt", "feet", 5, fcasts[10]) 
#csv17jan16.12z_hefs_s_qntls_h_t5 <- create_hrly_quants(csv17jan16.12z_hefs, "fcast_t_gmt", "feet", 5, fcasts[12]) 
#
##### type 6 quants
##                              create_hrly_quants: f(df, hourcolumn, value, qtype, metadata_to_add) 
#csv13jan16.12z_hefs_s_qntls_h_t6 <- create_hrly_quants(csv13jan16.12z_hefs, "fcast_t_gmt", "feet", 6, fcasts[1]) 
#csv14jan16.12z_hefs_s_qntls_h_t6 <- create_hrly_quants(csv14jan16.12z_hefs, "fcast_t_gmt", "feet", 6, fcasts[4]) 
#csv15jan16.12z_hefs_s_qntls_h_t6 <- create_hrly_quants(csv15jan16.12z_hefs, "fcast_t_gmt", "feet", 6, fcasts[7]) 
#csv16jan16.12z_hefs_s_qntls_h_t6 <- create_hrly_quants(csv16jan16.12z_hefs, "fcast_t_gmt", "feet", 6, fcasts[10]) 
#csv17jan16.12z_hefs_s_qntls_h_t6 <- create_hrly_quants(csv17jan16.12z_hefs, "fcast_t_gmt", "feet", 6, fcasts[12]) 
#
##### type 7 quants
##                              create_hrly_quants: f(df, hourcolumn, value, qtype, metadata_to_add) 
#csv13jan16.12z_hefs_s_qntls_h_t7 <- create_hrly_quants(csv13jan16.12z_hefs, "fcast_t_gmt", "feet", 7, fcasts[1]) 
#csv14jan16.12z_hefs_s_qntls_h_t7 <- create_hrly_quants(csv14jan16.12z_hefs, "fcast_t_gmt", "feet", 7, fcasts[4]) 
#csv15jan16.12z_hefs_s_qntls_h_t7 <- create_hrly_quants(csv15jan16.12z_hefs, "fcast_t_gmt", "feet", 7, fcasts[7]) 
#csv16jan16.12z_hefs_s_qntls_h_t7 <- create_hrly_quants(csv16jan16.12z_hefs, "fcast_t_gmt", "feet", 7, fcasts[10]) 
#csv17jan16.12z_hefs_s_qntls_h_t7 <- create_hrly_quants(csv17jan16.12z_hefs, "fcast_t_gmt", "feet", 7, fcasts[12]) 
#
##### type 8 quants
##                              create_hrly_quants: f(df, hourcolumn, value, qtype, metadata_to_add) 
#csv13jan16.12z_hefs_s_qntls_h_t8 <- create_hrly_quants(csv13jan16.12z_hefs, "fcast_t_gmt", "feet", 8, fcasts[1]) 
#csv14jan16.12z_hefs_s_qntls_h_t8 <- create_hrly_quants(csv14jan16.12z_hefs, "fcast_t_gmt", "feet", 8, fcasts[4]) 
#csv15jan16.12z_hefs_s_qntls_h_t8 <- create_hrly_quants(csv15jan16.12z_hefs, "fcast_t_gmt", "feet", 8, fcasts[7]) 
#csv16jan16.12z_hefs_s_qntls_h_t8 <- create_hrly_quants(csv16jan16.12z_hefs, "fcast_t_gmt", "feet", 8, fcasts[10]) 
#csv17jan16.12z_hefs_s_qntls_h_t8 <- create_hrly_quants(csv17jan16.12z_hefs, "fcast_t_gmt", "feet", 8, fcasts[12]) 
#
##### type 9 quants
##                              create_hrly_quants: f(df, hourcolumn, value, qtype, metadata_to_add) 
#csv13jan16.12z_hefs_s_qntls_h_t9 <- create_hrly_quants(csv13jan16.12z_hefs, "fcast_t_gmt", "feet", 9, fcasts[1]) 
#csv14jan16.12z_hefs_s_qntls_h_t9 <- create_hrly_quants(csv14jan16.12z_hefs, "fcast_t_gmt", "feet", 9, fcasts[4]) 
#csv15jan16.12z_hefs_s_qntls_h_t9 <- create_hrly_quants(csv15jan16.12z_hefs, "fcast_t_gmt", "feet", 9, fcasts[7]) 
#csv16jan16.12z_hefs_s_qntls_h_t9 <- create_hrly_quants(csv16jan16.12z_hefs, "fcast_t_gmt", "feet", 9, fcasts[10]) 
#csv17jan16.12z_hefs_s_qntls_h_t9 <- create_hrly_quants(csv17jan16.12z_hefs, "fcast_t_gmt", "feet", 9, fcasts[12]) 
#
#df_s_qntls_h <- rbind(csv13jan16.12z_hefs_s_qntls_h_t4,
#                      csv14jan16.12z_hefs_s_qntls_h_t4,
#                      csv15jan16.12z_hefs_s_qntls_h_t4,
#                      csv16jan16.12z_hefs_s_qntls_h_t4,
#                      csv17jan16.12z_hefs_s_qntls_h_t4,
#                      csv13jan16.12z_hefs_s_qntls_h_t5,
#                      csv14jan16.12z_hefs_s_qntls_h_t5,
#                      csv15jan16.12z_hefs_s_qntls_h_t5,
#                      csv16jan16.12z_hefs_s_qntls_h_t5,
#                      csv17jan16.12z_hefs_s_qntls_h_t5,
#                      csv13jan16.12z_hefs_s_qntls_h_t6,
#                      csv14jan16.12z_hefs_s_qntls_h_t6,
#                      csv15jan16.12z_hefs_s_qntls_h_t6,
#                      csv16jan16.12z_hefs_s_qntls_h_t6,
#                      csv17jan16.12z_hefs_s_qntls_h_t6,
#                      csv13jan16.12z_hefs_s_qntls_h_t7,
#                      csv14jan16.12z_hefs_s_qntls_h_t7,
#                      csv15jan16.12z_hefs_s_qntls_h_t7,
#                      csv16jan16.12z_hefs_s_qntls_h_t7,
#                      csv17jan16.12z_hefs_s_qntls_h_t7,
#                      csv13jan16.12z_hefs_s_qntls_h_t8,
#                      csv14jan16.12z_hefs_s_qntls_h_t8,
#                      csv15jan16.12z_hefs_s_qntls_h_t8,
#                      csv16jan16.12z_hefs_s_qntls_h_t8,
#                      csv17jan16.12z_hefs_s_qntls_h_t8,
#                      csv13jan16.12z_hefs_s_qntls_h_t9,
#                      csv14jan16.12z_hefs_s_qntls_h_t9,
#                      csv15jan16.12z_hefs_s_qntls_h_t9,
#                      csv16jan16.12z_hefs_s_qntls_h_t9,
#                      csv17jan16.12z_hefs_s_qntls_h_t9
#) %>% mutate(fcast_t_pdt = with_tz(fcast_t_gmt, "America/Los_Angeles"),
#             qfacet = paste0("q type ", qtype))


######### whole forecast exceedance #############
#### type 4 quants
##     create_5dyfc_quants <- function(df, modelrun, value, qtype, fcast_i_pdt)   {
#csv13jan16.12z_hefs_s_qntls_fc_t4 <- create_5dyfc_quants(csv13jan16.12z_hefs, "mefp_yr", "feet", 4, fcasts[1]) 
#csv14jan16.12z_hefs_s_qntls_fc_t4 <- create_5dyfc_quants(csv14jan16.12z_hefs, "mefp_yr", "feet", 4, fcasts[4]) 
#csv15jan16.12z_hefs_s_qntls_fc_t4 <- create_5dyfc_quants(csv15jan16.12z_hefs, "mefp_yr", "feet", 4, fcasts[7]) 
#csv16jan16.12z_hefs_s_qntls_fc_t4 <- create_5dyfc_quants(csv16jan16.12z_hefs, "mefp_yr", "feet", 4, fcasts[10]) 
#csv17jan16.12z_hefs_s_qntls_fc_t4 <- create_5dyfc_quants(csv17jan16.12z_hefs, "mefp_yr", "feet", 4, fcasts[12])
#
##### type 5 quants
###     create_5dyfc_quants <- function(df, modelrun, value, qtype, fcast_i_pdt)   {
#csv13jan16.12z_hefs_s_qntls_fc_t5 <- create_5dyfc_quants(csv13jan16.12z_hefs, "mefp_yr", "feet", 5, fcasts[1]) 
#csv14jan16.12z_hefs_s_qntls_fc_t5 <- create_5dyfc_quants(csv14jan16.12z_hefs, "mefp_yr", "feet", 5, fcasts[4]) 
#csv15jan16.12z_hefs_s_qntls_fc_t5 <- create_5dyfc_quants(csv15jan16.12z_hefs, "mefp_yr", "feet", 5, fcasts[7]) 
#csv16jan16.12z_hefs_s_qntls_fc_t5 <- create_5dyfc_quants(csv16jan16.12z_hefs, "mefp_yr", "feet", 5, fcasts[10]) 
#csv17jan16.12z_hefs_s_qntls_fc_t5 <- create_5dyfc_quants(csv17jan16.12z_hefs, "mefp_yr", "feet", 5, fcasts[12])
#
##### type 6 quants
###     create_5dyfc_quants <- function(df, modelrun, value, qtype, fcast_i_pdt)   {
#csv13jan16.12z_hefs_s_qntls_fc_t6 <- create_5dyfc_quants(csv13jan16.12z_hefs, "mefp_yr", "feet", 6, fcasts[1]) 
#csv14jan16.12z_hefs_s_qntls_fc_t6 <- create_5dyfc_quants(csv14jan16.12z_hefs, "mefp_yr", "feet", 6, fcasts[4]) 
#csv15jan16.12z_hefs_s_qntls_fc_t6 <- create_5dyfc_quants(csv15jan16.12z_hefs, "mefp_yr", "feet", 6, fcasts[7]) 
#csv16jan16.12z_hefs_s_qntls_fc_t6 <- create_5dyfc_quants(csv16jan16.12z_hefs, "mefp_yr", "feet", 6, fcasts[10]) 
#csv17jan16.12z_hefs_s_qntls_fc_t6 <- create_5dyfc_quants(csv17jan16.12z_hefs, "mefp_yr", "feet", 6, fcasts[12])
#
##### type 7 quants
###     create_5dyfc_quants <- function(df, modelrun, value, qtype, fcast_i_pdt)   {
#csv13jan16.12z_hefs_s_qntls_fc_t7 <- create_5dyfc_quants(csv13jan16.12z_hefs, "mefp_yr", "feet", 7, fcasts[1]) 
#csv14jan16.12z_hefs_s_qntls_fc_t7 <- create_5dyfc_quants(csv14jan16.12z_hefs, "mefp_yr", "feet", 7, fcasts[4]) 
#csv15jan16.12z_hefs_s_qntls_fc_t7 <- create_5dyfc_quants(csv15jan16.12z_hefs, "mefp_yr", "feet", 7, fcasts[7]) 
#csv16jan16.12z_hefs_s_qntls_fc_t7 <- create_5dyfc_quants(csv16jan16.12z_hefs, "mefp_yr", "feet", 7, fcasts[10]) 
#csv17jan16.12z_hefs_s_qntls_fc_t7 <- create_5dyfc_quants(csv17jan16.12z_hefs, "mefp_yr", "feet", 7, fcasts[12])
#
##### type 8 quants
###     create_5dyfc_quants <- function(df, modelrun, value, qtype, fcast_i_pdt)   {
#csv13jan16.12z_hefs_s_qntls_fc_t8 <- create_5dyfc_quants(csv13jan16.12z_hefs, "mefp_yr", "feet", 8, fcasts[1]) 
#csv14jan16.12z_hefs_s_qntls_fc_t8 <- create_5dyfc_quants(csv14jan16.12z_hefs, "mefp_yr", "feet", 8, fcasts[4]) 
#csv15jan16.12z_hefs_s_qntls_fc_t8 <- create_5dyfc_quants(csv15jan16.12z_hefs, "mefp_yr", "feet", 8, fcasts[7]) 
#csv16jan16.12z_hefs_s_qntls_fc_t8 <- create_5dyfc_quants(csv16jan16.12z_hefs, "mefp_yr", "feet", 8, fcasts[10]) 
#csv17jan16.12z_hefs_s_qntls_fc_t8 <- create_5dyfc_quants(csv17jan16.12z_hefs, "mefp_yr", "feet", 8, fcasts[12])
#
##### type 9 quants
###     create_5dyfc_quants <- function(df, modelrun, value, qtype, fcast_i_pdt)   {
#csv13jan16.12z_hefs_s_qntls_fc_t9 <- create_5dyfc_quants(csv13jan16.12z_hefs, "mefp_yr", "feet", 9, fcasts[1]) 
#csv14jan16.12z_hefs_s_qntls_fc_t9 <- create_5dyfc_quants(csv14jan16.12z_hefs, "mefp_yr", "feet", 9, fcasts[4]) 
#csv15jan16.12z_hefs_s_qntls_fc_t9 <- create_5dyfc_quants(csv15jan16.12z_hefs, "mefp_yr", "feet", 9, fcasts[7]) 
#csv16jan16.12z_hefs_s_qntls_fc_t9 <- create_5dyfc_quants(csv16jan16.12z_hefs, "mefp_yr", "feet", 9, fcasts[10]) 
#csv17jan16.12z_hefs_s_qntls_fc_t9 <- create_5dyfc_quants(csv17jan16.12z_hefs, "mefp_yr", "feet", 9, fcasts[12])
#
#df_s_qntls_p <- rbind(csv13jan16.12z_hefs_s_qntls_fc_t4,
#                      csv14jan16.12z_hefs_s_qntls_fc_t4,
#                      csv15jan16.12z_hefs_s_qntls_fc_t4,
#                      csv16jan16.12z_hefs_s_qntls_fc_t4,
#                      csv17jan16.12z_hefs_s_qntls_fc_t4,
#                      csv13jan16.12z_hefs_s_qntls_fc_t5,
#                      csv14jan16.12z_hefs_s_qntls_fc_t5,
#                      csv15jan16.12z_hefs_s_qntls_fc_t5,
#                      csv16jan16.12z_hefs_s_qntls_fc_t5,
#                      csv17jan16.12z_hefs_s_qntls_fc_t5,
#                      csv13jan16.12z_hefs_s_qntls_fc_t6,
#                      csv14jan16.12z_hefs_s_qntls_fc_t6,
#                      csv15jan16.12z_hefs_s_qntls_fc_t6,
#                      csv16jan16.12z_hefs_s_qntls_fc_t6,
#                      csv17jan16.12z_hefs_s_qntls_fc_t6,
#                      csv13jan16.12z_hefs_s_qntls_fc_t7,
#                      csv14jan16.12z_hefs_s_qntls_fc_t7,
#                      csv15jan16.12z_hefs_s_qntls_fc_t7,
#                      csv16jan16.12z_hefs_s_qntls_fc_t7,
#                      csv17jan16.12z_hefs_s_qntls_fc_t7,
#                      csv13jan16.12z_hefs_s_qntls_fc_t8,
#                      csv14jan16.12z_hefs_s_qntls_fc_t8,
#                      csv15jan16.12z_hefs_s_qntls_fc_t8,
#                      csv16jan16.12z_hefs_s_qntls_fc_t8,
#                      csv17jan16.12z_hefs_s_qntls_fc_t8,
#                      csv13jan16.12z_hefs_s_qntls_fc_t9,
#                      csv14jan16.12z_hefs_s_qntls_fc_t9,
#                      csv15jan16.12z_hefs_s_qntls_fc_t9,
#                      csv16jan16.12z_hefs_s_qntls_fc_t9,
#                      csv17jan16.12z_hefs_s_qntls_fc_t9
#) 


#df_mefpyr_max <- df  %>% group_by(fcast, mefp_yr) %>% filter(kcfs == max(kcfs))

#hefs_dtrmnstc_wstats <- hefs_dtrmnstc %>% mutate(taf = cfs*1.98347/24/1000) %>% group_by(mefp_yr) %>% mutate(taf_5day = sum(taf), cfs_5day = max(cfs),
#                                                                                                             ft_5day = max(feet)) %>% ungroup(mefp_yr)
#
#hefs_dtrmnstc_wstats <- hefs_dtrmnstc_wstats %>% mutate(taf_5day_label = ifelse(fcast_t_pdt == dt_midpoint, taf_5day, NA), 
#                                                        facet_ridge_taf = "deterministic & mefp-based scenarios, cumulative volume (60 total)", facet_text_taf = "cum. taf",
#                                                        cfs_5day_label = ifelse(fcast_t_pdt == dt_midpoint, cfs_5day, NA), 
#                                                        facet_ridge_cfs = "deterministic & mefp-based scenarios, hourly flow (60 total)", facet_text_cfs = "max. kcfs",
#                                                        ft_5day_label = ifelse(fcast_t_pdt == dt_midpoint, ft_5day, NA), 
#                                                        facet_ridge_ft = "deterministic & mefp-based scenarios,  hourly stage (60 total)", facet_text_ft = "max. height (ft)")
