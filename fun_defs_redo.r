library(tidyverse)
Sys.setenv(TZ="America/Los_Angeles")

create_hrly_quants <- function(df, hourcolumn, value, qtype, fcast_i_pdt)   {
    
    value <- rlang::sym(value)
    
    df  %>% group_by_at(vars(hourcolumn)) %>%
      summarize(
      `5%`=quantile(!!value, probs=0.05, type = qtype), 
     `10%`=quantile(!!value, probs=0.10, type = qtype),
     `25%`=quantile(!!value, probs=0.25, type = qtype),
     `50%`=quantile(!!value, probs=0.5,  type = qtype),
     `75%`=quantile(!!value, probs=0.75, type = qtype),
     `90%`=quantile(!!value, probs=0.9,  type = qtype),
     `95%`=quantile(!!value, probs=0.95, type = qtype),
     `min`=min(!!value), `max`=max(!!value), mean=mean(!!value), median=median(!!value), 
      n=n()) %>% mutate(qtype = qtype, fcast_i_pdt_hms = fcast_i_pdt, fcast_i_pdt_date = date(fcast_i_pdt_hms))
  }
  
create_per_quants <- function(df, fcaststartdate, fcastenddate, yeartrace, value, qtype, fcast_i_pdt)    {
    
    value <- rlang::sym(value)
  
    df  %>% filter(fcast_t_pdt >= fcaststartdate, fcast_t_pdt <= fcastenddate) %>%
      group_by_at(vars(yeartrace)) %>%
      summarize(maxtraceval = max(!!value))  %>%
      summarize(
      `5%`=quantile(maxtraceval, probs=0.05, type = qtype), 
     `10%`=quantile(maxtraceval, probs=0.10, type = qtype),
     `25%`=quantile(maxtraceval, probs=0.25, type = qtype),
     `50%`=quantile(maxtraceval, probs=0.5, type = qtype),
     `75%`=quantile(maxtraceval, probs=0.75, type = qtype),
     `90%`=quantile(maxtraceval, probs=0.9, type = qtype),
     `95%`=quantile(maxtraceval, probs=0.95, type = qtype),
     `min`=min(maxtraceval), `max`=max(maxtraceval), mean=mean(maxtraceval), median=median(maxtraceval), 
     n=n()) %>% mutate(qtype = qtype, fcast_i_pdt_hms = fcast_i_pdt, fcast_i_pdt_date = date(fcast_i_pdt_hms))
  }

########################################################################################################
########################## plotting funs  ##############################################################
########################################################################################################



plot_f_ahps <- function(df_qntls_h, df_hefs, dtrmnstc, fcaststartdate, fcastenddate,  fcast_i_pddate, q_type) {
df_qntls_h <- df_qntls_h %>% filter(fcast_t_pdt >= fcaststartdate, fcast_t_pdt <= fcastenddate, fcast_i_pdt_date == fcast_i_pddate, qtype == q_type )
df_hefs <- df_hefs %>% filter(fcast_t_pdt >= fcaststartdate, fcast_t_pdt <= fcastenddate, fcast_i_pdt_date == fcast_i_pddate)
dtrmnstc <- dtrmnstc %>% filter(fcast_i_pdt_date == fcast_i_pddate)

NonNAindex <- which(!is.na(dtrmnstc$cfs))
firstNonNAdate <- min(NonNAindex)
firstNonNAdate <- dtrmnstc$fcast_t_pdt[firstNonNAdate]
df_qntls_h <- df_qntls_h %>% filter(fcast_t_pdt >= firstNonNAdate)
df_hefs <- df_hefs %>% filter(fcast_t_pdt >= firstNonNAdate)

ggplot() + 
geom_line(data = df_hefs, aes(x = fcast_t_pdt, y = kcfs, group = mefp_yr, color = "mefp_yr") , size = 0.2) +
geom_line  (data = df_qntls_h,  aes(x=fcast_t_pdt, y=median, color = "mefp-bsd med excd lev")) +
geom_line  (data = dtrmnstc,     aes(x=fcast_t_pdt, y=kcfs, color = "deterministic fcast")) +
geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pdt, ymin= `95%`, ymax = max,  fill="<5% (top)"),  alpha=0.2) +
geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pdt, ymin= `90%`, ymax = `95%`,fill="10 -  5%"),   alpha=0.2) +
geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pdt, ymin= `75%`, ymax = `90%`,fill="25 - 10%"),   alpha=0.2) +
geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pdt, ymin= `50%`, ymax = `75%`,fill="50 - 25%"),   alpha=0.2) +
#  geom_ribbon(data = filter(df_qntls_h, qtype =4),  aes(x=fcast_t_pdt, ymin= median, ymax = median,fill="median"),   alpha=0.9) +
geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pdt, ymin= `25%`, ymax = `50%`,fill="75 - 50%"),   alpha=0.2) +
geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pdt, ymin= `10%`, ymax = `25%`,fill="90 - 75%"),   alpha=0.2) +
geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pdt, ymin= `5%`, ymax = `10%`,fill="95 - 90%"),   alpha=0.2) +
geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pdt, ymin= `min`, ymax = `5%`,fill=">95% (btm)"), alpha=0.2) +
# annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 20, fill = "orange", alpha = 0.5, color = NA)+
#geom_rect(data = NULL, aes(xmin = as.POSIXct(-Inf, orig = o), xmax = as.POSIXct(Inf, orig = o), ymin = -Inf,
#ymax = 25, fill = "test"), alpha = .01, color = NA) +

#geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 1, fill = "fill"), alpha = .5, color = NA) +

#facet_wrap(~fcast_i_pdt, ncol = 1) + 
scale_x_datetime(expand = c(0.02, 0.02),
                 date_breaks = "1 day", date_minor_breaks = "12 hours", date_labels = "%b %d\n%H:%M pst") +
scale_fill_manual(values = c(
  '<5% (top)' = 'snow4',
  '10 -  5%' = 'yellow',
  '25 - 10%' = 'green', 
  '50 - 25%' = 'blue',
  'median' = 'blue4',
  '75 - 50%' = 'blue',
  '90 - 75%' = 'green',
  '95 - 90%' = 'yellow',
  '>95% (btm)' = 'snow4'),
  breaks = c(
    '<5% (top)' ,
    '10 -  5%' ,
    '25 - 10%' , 
    '50 - 25%' ,
    'median',
    '75 - 50%' ,
    '90 - 75%' ,
    '95 - 90%',
    '>95% (btm)' )) +

  scale_color_manual(values= c(
  'deterministic fcast' = 'black',
  'mefp_yr' = 'gray',
  'mefp-bsd med excd lev' = 'blue')) + labs(y = "kcfs", x = NULL) +
  guides(fill=guide_legend(title="quantiles")) +
  guides(colour=guide_legend(title="series")) + 
  scale_y_continuous(sec.axis = dup_axis(name = NULL))
}


plot_f_ahps_qtypefacet <- function(df_qntls_h, df_hefs, dtrmnstc, fcaststartdate, fcastenddate,  fcast_i_pddate) {
  df_qntls_h <- df_qntls_h %>% filter(fcast_t_pdt >= fcaststartdate, fcast_t_pdt <= fcastenddate, fcast_i_pdt_date == fcast_i_pddate)
  df_hefs <- df_hefs %>% filter(fcast_t_pdt >= fcaststartdate, fcast_t_pdt <= fcastenddate, fcast_i_pdt_date == fcast_i_pddate)
  dtrmnstc <- dtrmnstc %>% filter(fcast_i_pdt_date == fcast_i_pddate)
  
  NonNAindex <- which(!is.na(dtrmnstc$cfs))
  firstNonNAdate <- min(NonNAindex)
  firstNonNAdate <- dtrmnstc$fcast_t_pdt[firstNonNAdate]
  df_qntls_h <- df_qntls_h %>% filter(fcast_t_pdt >= firstNonNAdate)
  df_hefs <- df_hefs %>% filter(fcast_t_pdt >= firstNonNAdate)
  
  ggplot() + 
    geom_line(data = df_hefs, aes(x = fcast_t_pdt, y = kcfs, group = mefp_yr, color = "mefp_yr") , size = 0.2) +
    geom_line  (data = df_qntls_h,  aes(x=fcast_t_pdt, y=median, color = "mefp-bsd med excd lev")) +
    geom_line  (data = dtrmnstc,     aes(x=fcast_t_pdt, y=kcfs, color = "deterministic fcast")) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pdt, ymin= `95%`, ymax = max,  fill="<5% (top)"),  alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pdt, ymin= `90%`, ymax = `95%`,fill="10 -  5%"),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pdt, ymin= `75%`, ymax = `90%`,fill="25 - 10%"),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pdt, ymin= `50%`, ymax = `75%`,fill="50 - 25%"),   alpha=0.2) +
    #  geom_ribbon(data = filter(df_qntls_h, qtype =4),  aes(x=fcast_t_pdt, ymin= median, ymax = median,fill="median"),   alpha=0.9) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pdt, ymin= `25%`, ymax = `50%`,fill="75 - 50%"),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pdt, ymin= `10%`, ymax = `25%`,fill="90 - 75%"),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pdt, ymin= `5%`, ymax = `10%`,fill="95 - 90%"),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pdt, ymin= `min`, ymax = `5%`,fill=">95% (btm)"), alpha=0.2) +
    # annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 20, fill = "orange", alpha = 0.5, color = NA)+
    #geom_rect(data = NULL, aes(xmin = as.POSIXct(-Inf, orig = o), xmax = as.POSIXct(Inf, orig = o), ymin = -Inf,
    #ymax = 25, fill = "test"), alpha = .01, color = NA) +
    
    #geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 1, fill = "fill"), alpha = .5, color = NA) +
    
    facet_wrap(~qfacet) + 
    scale_x_datetime(expand = c(0.02, 0.02),
                     date_breaks = "1 day", date_minor_breaks = "12 hours", date_labels = "%m/%d") +
    scale_fill_manual(values = c(
      '<5% (top)' = 'snow4',
      '10 -  5%' = 'yellow',
      '25 - 10%' = 'green', 
      '50 - 25%' = 'blue',
      'median' = 'blue4',
      '75 - 50%' = 'blue',
      '90 - 75%' = 'green',
      '95 - 90%' = 'yellow',
      '>95% (btm)' = 'snow4'),
      breaks = c(
        '<5% (top)' ,
        '10 -  5%' ,
        '25 - 10%' , 
        '50 - 25%' ,
        'median',
        '75 - 50%' ,
        '90 - 75%' ,
        '95 - 90%',
        '>95% (btm)' )) +
    
    scale_color_manual(values= c(
      'deterministic fcast' = 'black',
      'mefp_yr' = 'gray',
      'mefp-bsd med excd lev' = 'blue')) + labs(y = "kcfs", x = NULL) +
    guides(fill=guide_legend(title="quantiles")) +
    guides(colour=guide_legend(title="series")) +
    scale_y_continuous(sec.axis = dup_axis(name = NULL))
}
#fcastenddate <- max(df_hefs$fcast_t_gmt)
#plot_s_ahps <- function(df_qntls_h, df_hefs, dtrmnstc, fcaststartdate, fcastenddate,  fcast_i_pddate, q_type) {
plot_f_per <- function(df_hefs, dtrmnstc, fcaststartdate, fcastenddate, fcast_i_pddate, q_type) {
  df_hefs    <- df_hefs %>% filter(fcast_i_pdt == fcast_i_pddate, fcast_t_pdt >= fcaststartdate, fcast_t_pdt <= fcastenddate) 
  #create_per_quants <- function(df, fcaststartdate, fcastenddate, yeartrace, value, qtype, fcast_i_pdt) 
  fcastenddate <- max(df_hefs$fcast_t_gmt)
df_qntls_p <- create_per_quants(df_hefs, fcaststartdate, fcastenddate, "mefp_yr", "kcfs", q_type, fcast_i_pddate) 
dtrmnstc   <- dtrmnstc %>% filter(fcast_i_pdt == fcast_i_pddate, fcast_t_pdt >= fcaststartdate, fcast_t_pdt <= fcastenddate) 

df_mefpyr_max <- df_hefs  %>% group_by(mefp_yr) %>% filter(kcfs == max(kcfs))

NonNAindex <- which(!is.na(dtrmnstc$cfs))
firstNonNAdate <- min(NonNAindex)
firstNonNAdate <- dtrmnstc$fcast_t_pdt[firstNonNAdate]
df_hefs <- df_hefs %>% filter(fcast_t_pdt >= firstNonNAdate)
df_mefpyr_max <- df_mefpyr_max %>% filter(fcast_t_pdt >= firstNonNAdate)
#firstNonNAdate <- min()
#df_qntls_p <- df_qntls_p %>% filter(fcast_t_pdt >= firstNonNAdate)
#df_hefs <- df_hefs %>% filter(fcast_t_pdt >= firstNonNAdate)
o <- "1899-01-15 12:00:00"
  ggplot() + 
  
  
  geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(firstNonNAdate), xmax= as.POSIXct(fcastenddate), ymin= `95%`, ymax = max,  fill="<5% (top)"),  alpha=0.2) +
  geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(firstNonNAdate), xmax= as.POSIXct(fcastenddate), ymin= `90%`, ymax = `95%`,fill="10 -  5%"),   alpha=0.2) +
  geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(firstNonNAdate), xmax= as.POSIXct(fcastenddate), ymin= `75%`, ymax = `90%`,fill="25 - 10%"),   alpha=0.2) +
  geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(firstNonNAdate), xmax= as.POSIXct(fcastenddate), ymin= `50%`, ymax = `75%`,fill="50 - 25%"),   alpha=0.2) +
  #  geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(firstNonNAdate), xmax= as.POSIXct(fcastenddate), ymin= median, ymax = median,fill="median"),   alpha=0.9) +
  geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(firstNonNAdate), xmax= as.POSIXct(fcastenddate), ymin= `25%`, ymax = `50%`,fill="75 - 50%"),   alpha=0.2) +
  geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(firstNonNAdate), xmax= as.POSIXct(fcastenddate), ymin= `10%`, ymax = `25%`,fill="90 - 75%"),   alpha=0.2) +
  geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(firstNonNAdate), xmax= as.POSIXct(fcastenddate), ymin= `5%`, ymax = `10%`,fill="95 - 90%"),   alpha=0.2) +
  geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(firstNonNAdate), xmax= as.POSIXct(fcastenddate), ymin= `min`, ymax = `5%`,fill=">95% (btm)"), alpha=0.2) +

  geom_line(data = df_hefs, aes(x = fcast_t_pdt, y = kcfs, group = mefp_yr, color = "mefp_yr") , size = 0.2) +
  geom_point(data= df_mefpyr_max, aes(x = fcast_t_pdt, y = kcfs, color = "per. max(mefp_yr)"), size = 2) +
    geom_segment(data = df_qntls_p, 
                 aes(x=as.POSIXct(firstNonNAdate),xend=as.POSIXct(fcastenddate),
                     y=median,yend=median, linetype = "median"), color = "blue") +
  geom_line(data = dtrmnstc, aes( x = fcast_t_pdt, y = kcfs,color = "dtrmnstc fcast" ), size = 1) +

  
    
  
  
  #facet_wrap(~fcast, ncol = 1) + labs(y = "kcfs", x = NULL) +
  scale_x_datetime(expand = c(0.01, 0.01),
                   date_breaks = "1 day", date_minor_breaks = "12 hours", date_labels = "%b %d\n%H:%M pst") +
  scale_fill_manual(name = "period\nmax(mefp_yr)\nquantile",
    
    values = c(
    '<5% (top)' = 'snow3',
    '10 -  5%' = 'yellow',
    '25 - 10%' = 'green', 
    '50 - 25%' = 'blue',
    #  'median' = 'blue4',
    '75 - 50%' = 'blue',
    '90 - 75%' = 'green',
    '95 - 90%' = 'yellow',
    '>95% (btm)' = 'snow3'),
    breaks = c(
      '<5% (top)' ,
      '10 -  5%' ,
      '25 - 10%' , 
      '50 - 25%' ,
      #'median',
      '75 - 50%' ,
      '90 - 75%' ,
      '95 - 90%',
      '>95% (btm)' )) +
  
    scale_color_manual(name = "series",
      
      values= c(
      'dtrmnstc fcast' = 'black',
      'mefp_yr' = 'pink',
      'per. max(mefp_yr)' = 'pink3'),
      guide = guide_legend(override.aes = list(
        linetype = c("solid",    "solid", "blank"),
        shape = c(NA, NA, 16)))) + labs(x = NULL) +
    scale_linetype_manual(name = "period\nmax(mefp_yr)\nstat",values = c(1, 1), 
                          guide = guide_legend(override.aes = list(color = c("blue"))))
 
}
  
#######################################
############## stage ##################
#######################################


 
plot_s_ahps <- function(df_qntls_h, df_hefs, dtrmnstc, fcaststartdate, fcastenddate,  fcast_i_pddate, q_type) {
  df_qntls_h <- df_qntls_h %>% filter(fcast_t_pdt >= fcaststartdate, fcast_t_pdt <= fcastenddate, fcast_i_pdt_date == fcast_i_pddate, qtype == q_type )
  df_hefs <- df_hefs %>% filter(fcast_t_pdt >= fcaststartdate, fcast_t_pdt <= fcastenddate, fcast_i_pdt_date == fcast_i_pddate)
  dtrmnstc <- dtrmnstc %>% filter(fcast_i_pdt_date == fcast_i_pddate)
  
  NonNAindex <- which(!is.na(dtrmnstc$cfs))
  firstNonNAdate <- min(NonNAindex)
  firstNonNAdate <- dtrmnstc$fcast_t_pdt[firstNonNAdate]
  df_qntls_h <- df_qntls_h %>% filter(fcast_t_pdt >= firstNonNAdate)
  df_hefs <- df_hefs %>% filter(fcast_t_pdt >= firstNonNAdate)
  
  ggplot() + 
    geom_line(data = df_hefs, aes(x = fcast_t_pdt, y = stage, group = mefp_yr, color = "mefp_yr") , size = 0.2) +
    geom_line  (data = df_qntls_h,  aes(x=fcast_t_pdt, y=median, color = "mefp-bsd med excd lev")) +
    geom_line  (data = dtrmnstc,     aes(x=fcast_t_pdt, y=stage, color = "deterministic fcast")) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pdt, ymin= `95%`, ymax = max,  fill="<5% (top)"),  alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pdt, ymin= `90%`, ymax = `95%`,fill="10 -  5%"),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pdt, ymin= `75%`, ymax = `90%`,fill="25 - 10%"),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pdt, ymin= `50%`, ymax = `75%`,fill="50 - 25%"),   alpha=0.2) +
    #  geom_ribbon(data = filter(df_qntls_h, qtype =4),  aes(x=fcast_t_pdt, ymin= median, ymax = median,fill="median"),   alpha=0.9) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pdt, ymin= `25%`, ymax = `50%`,fill="75 - 50%"),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pdt, ymin= `10%`, ymax = `25%`,fill="90 - 75%"),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pdt, ymin= `5%`, ymax = `10%`,fill="95 - 90%"),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pdt, ymin= `min`, ymax = `5%`,fill=">95% (btm)"), alpha=0.2) +
    # annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 20, fill = "orange", alpha = 0.5, color = NA)+
    #geom_rect(data = NULL, aes(xmin = as.POSIXct(-Inf, orig = o), xmax = as.POSIXct(Inf, orig = o), ymin = -Inf,
    #ymax = 25, fill = "test"), alpha = .01, color = NA) +
    
    #geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 1, fill = "fill"), alpha = .5, color = NA) +
    
    #facet_wrap(~fcast_i_pdt, ncol = 1) + 
    scale_x_datetime(expand = c(0.02, 0.02),
                     date_breaks = "1 day", date_minor_breaks = "12 hours", date_labels = "%b %d\n%H:%M pst") +
    scale_fill_manual(values = c(
      '<5% (top)' = 'snow4',
      '10 -  5%' = 'yellow',
      '25 - 10%' = 'green', 
      '50 - 25%' = 'blue',
      'median' = 'blue4',
      '75 - 50%' = 'blue',
      '90 - 75%' = 'green',
      '95 - 90%' = 'yellow',
      '>95% (btm)' = 'snow4'),
      breaks = c(
        '<5% (top)' ,
        '10 -  5%' ,
        '25 - 10%' , 
        '50 - 25%' ,
        'median',
        '75 - 50%' ,
        '90 - 75%' ,
        '95 - 90%',
        '>95% (btm)' )) +
    
    scale_color_manual(values= c(
      'deterministic fcast' = 'black',
      'mefp_yr' = 'gray',
      'mefp-bsd med excd lev' = 'blue')) + labs(y = "stage", x = NULL) +
    guides(fill=guide_legend(title="quantiles")) +
    guides(colour=guide_legend(title="series")) + 
    scale_y_continuous(sec.axis = dup_axis(name = NULL))
}

plot_f_ahps <- function(df_qntls_h, df_hefs, dtrmnstc, fcaststartdate, fcastenddate,  fcast_i_pddate, q_type) {
  df_qntls_h <- df_qntls_h %>% filter(fcast_t_pdt >= fcaststartdate, fcast_t_pdt <= fcastenddate, fcast_i_pdt_date == fcast_i_pddate, qtype == q_type )
  df_hefs <- df_hefs %>% filter(fcast_t_pdt >= fcaststartdate, fcast_t_pdt <= fcastenddate, fcast_i_pdt_date == fcast_i_pddate)
  dtrmnstc <- dtrmnstc %>% filter(fcast_i_pdt_date == fcast_i_pddate)
  
  NonNAindex <- which(!is.na(dtrmnstc$cfs))
  firstNonNAdate <- min(NonNAindex)
  firstNonNAdate <- dtrmnstc$fcast_t_pdt[firstNonNAdate]
  df_qntls_h <- df_qntls_h %>% filter(fcast_t_pdt >= firstNonNAdate)
  df_hefs <- df_hefs %>% filter(fcast_t_pdt >= firstNonNAdate)
  
  ggplot() + 
    geom_line(data = df_hefs, aes(x = fcast_t_pdt, y = kcfs, group = mefp_yr, color = "mefp_yr") , size = 0.2) +
    geom_line  (data = df_qntls_h,  aes(x=fcast_t_pdt, y=median, color = "mefp-bsd med excd lev")) +
    geom_line  (data = dtrmnstc,     aes(x=fcast_t_pdt, y=kcfs, color = "deterministic fcast")) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pdt, ymin= `95%`, ymax = max,  fill="<5% (top)"),  alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pdt, ymin= `90%`, ymax = `95%`,fill="10 -  5%"),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pdt, ymin= `75%`, ymax = `90%`,fill="25 - 10%"),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pdt, ymin= `50%`, ymax = `75%`,fill="50 - 25%"),   alpha=0.2) +
    #  geom_ribbon(data = filter(df_qntls_h, qtype =4),  aes(x=fcast_t_pdt, ymin= median, ymax = median,fill="median"),   alpha=0.9) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pdt, ymin= `25%`, ymax = `50%`,fill="75 - 50%"),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pdt, ymin= `10%`, ymax = `25%`,fill="90 - 75%"),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pdt, ymin= `5%`, ymax = `10%`,fill="95 - 90%"),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pdt, ymin= `min`, ymax = `5%`,fill=">95% (btm)"), alpha=0.2) +
    # annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 20, fill = "orange", alpha = 0.5, color = NA)+
    #geom_rect(data = NULL, aes(xmin = as.POSIXct(-Inf, orig = o), xmax = as.POSIXct(Inf, orig = o), ymin = -Inf,
    #ymax = 25, fill = "test"), alpha = .01, color = NA) +
    
    #geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 1, fill = "fill"), alpha = .5, color = NA) +
    
    #facet_wrap(~fcast_i_pdt, ncol = 1) + 
    scale_x_datetime(expand = c(0.02, 0.02),
                     date_breaks = "1 day", date_minor_breaks = "12 hours", date_labels = "%b %d\n%H:%M pst") +
    scale_fill_manual(values = c(
      '<5% (top)' = 'snow4',
      '10 -  5%' = 'yellow',
      '25 - 10%' = 'green', 
      '50 - 25%' = 'blue',
      'median' = 'blue4',
      '75 - 50%' = 'blue',
      '90 - 75%' = 'green',
      '95 - 90%' = 'yellow',
      '>95% (btm)' = 'snow4'),
      breaks = c(
        '<5% (top)' ,
        '10 -  5%' ,
        '25 - 10%' , 
        '50 - 25%' ,
        'median',
        '75 - 50%' ,
        '90 - 75%' ,
        '95 - 90%',
        '>95% (btm)' )) +
    
    scale_color_manual(values= c(
      'deterministic fcast' = 'black',
      'mefp_yr' = 'gray',
      'mefp-bsd med excd lev' = 'blue')) + labs(y = "kcfs", x = NULL) +
    guides(fill=guide_legend(title="quantiles")) +
    guides(colour=guide_legend(title="series")) + 
    scale_y_continuous(sec.axis = dup_axis(name = NULL))
}

plot_s_ahps_qtypefacet <- function(df_qntls_h, df_hefs, dtrmnstc, fcaststartdate, fcastenddate,  fcast_i_pddate) {
  df_qntls_h <- df_qntls_h %>% filter(fcast_t_pdt >= fcaststartdate, fcast_t_pdt <= fcastenddate, fcast_i_pdt_date == fcast_i_pddate)
  df_hefs <- df_hefs %>% filter(fcast_t_pdt >= fcaststartdate, fcast_t_pdt <= fcastenddate, fcast_i_pdt_date == fcast_i_pddate)
  dtrmnstc <- dtrmnstc %>% filter(fcast_i_pdt_date == fcast_i_pddate)
  
  NonNAindex <- which(!is.na(dtrmnstc$cfs))
  firstNonNAdate <- min(NonNAindex)
  firstNonNAdate <- dtrmnstc$fcast_t_pdt[firstNonNAdate]
  df_qntls_h <- df_qntls_h %>% filter(fcast_t_pdt >= firstNonNAdate)
  df_hefs <- df_hefs %>% filter(fcast_t_pdt >= firstNonNAdate)
  
  ggplot() + 
    geom_line(data = df_hefs, aes(x = fcast_t_pdt, y = stage, group = mefp_yr, color = "mefp_yr") , size = 0.2) +
    geom_line  (data = df_qntls_h,  aes(x=fcast_t_pdt, y=median, color = "mefp-bsd med excd lev")) +
    geom_line  (data = dtrmnstc,     aes(x=fcast_t_pdt, y=stage, color = "deterministic fcast")) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pdt, ymin= `95%`, ymax = max,  fill="<5% (top)"),  alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pdt, ymin= `90%`, ymax = `95%`,fill="10 -  5%"),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pdt, ymin= `75%`, ymax = `90%`,fill="25 - 10%"),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pdt, ymin= `50%`, ymax = `75%`,fill="50 - 25%"),   alpha=0.2) +
    #  geom_ribbon(data = filter(df_qntls_h, qtype =4),  aes(x=fcast_t_pdt, ymin= median, ymax = median,fill="median"),   alpha=0.9) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pdt, ymin= `25%`, ymax = `50%`,fill="75 - 50%"),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pdt, ymin= `10%`, ymax = `25%`,fill="90 - 75%"),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pdt, ymin= `5%`, ymax = `10%`,fill="95 - 90%"),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pdt, ymin= `min`, ymax = `5%`,fill=">95% (btm)"), alpha=0.2) +
    # annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 20, fill = "orange", alpha = 0.5, color = NA)+
    #geom_rect(data = NULL, aes(xmin = as.POSIXct(-Inf, orig = o), xmax = as.POSIXct(Inf, orig = o), ymin = -Inf,
    #ymax = 25, fill = "test"), alpha = .01, color = NA) +
    
    #geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 1, fill = "fill"), alpha = .5, color = NA) +
    
    facet_wrap(~qfacet) + 
    scale_x_datetime(expand = c(0.02, 0.02),
                     date_breaks = "1 day", date_minor_breaks = "12 hours", date_labels = "%m/%d") +
    scale_fill_manual(values = c(
      '<5% (top)' = 'snow4',
      '10 -  5%' = 'yellow',
      '25 - 10%' = 'green', 
      '50 - 25%' = 'blue',
      'median' = 'blue4',
      '75 - 50%' = 'blue',
      '90 - 75%' = 'green',
      '95 - 90%' = 'yellow',
      '>95% (btm)' = 'snow4'),
      breaks = c(
        '<5% (top)' ,
        '10 -  5%' ,
        '25 - 10%' , 
        '50 - 25%' ,
        'median',
        '75 - 50%' ,
        '90 - 75%' ,
        '95 - 90%',
        '>95% (btm)' )) +
    
    scale_color_manual(values= c(
      'deterministic fcast' = 'black',
      'mefp_yr' = 'gray',
      'mefp-bsd med excd lev' = 'blue')) + labs(y = "stage", x = NULL) +
    guides(fill=guide_legend(title="quantiles")) +
    guides(colour=guide_legend(title="series")) +
    scale_y_continuous(sec.axis = dup_axis(name = NULL))
}

plot_s_per <- function(df_hefs, dtrmnstc, fcaststartdate, fcastenddate, fcast_i_pddate, q_type) {
  df_hefs    <- df_hefs %>% filter(fcast_i_pdt == fcast_i_pddate, fcast_t_pdt >= fcaststartdate, fcast_t_pdt <= fcastenddate) 
  #create_per_quants <- function(df, fcaststartdate, fcastenddate, yeartrace, value, qtype, fcast_i_pdt) 
  fcastenddate <- max(df_hefs$fcast_t_gmt)
  df_qntls_p <- create_per_quants(df_hefs, fcaststartdate, fcastenddate, "mefp_yr", "stage", q_type, fcast_i_pddate) 
  dtrmnstc   <- dtrmnstc %>% filter(fcast_i_pdt == fcast_i_pddate, fcast_t_pdt >= fcaststartdate, fcast_t_pdt <= fcastenddate) 
  
  df_mefpyr_max <- df_hefs  %>% group_by(mefp_yr) %>% filter(stage == max(stage)) 
  
  NonNAindex <- which(!is.na(dtrmnstc$cfs))
  firstNonNAdate <- min(NonNAindex)
  firstNonNAdate <- dtrmnstc$fcast_t_pdt[firstNonNAdate]
  df_hefs <- df_hefs %>% filter(fcast_t_pdt >= firstNonNAdate)
 
  df_mefpyr_max <- df_mefpyr_max %>% filter(fcast_t_pdt >= firstNonNAdate)
  #df_hefs <- df_hefs %>% filter(fcast_t_pdt >= firstNonNAdate)
  o <- "1899-01-15 12:00:00"
  ggplot() + 
    
    
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(firstNonNAdate), xmax= as.POSIXct(fcastenddate), ymin= `95%`, ymax = max,  fill="<5% (top)"),  alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(firstNonNAdate), xmax= as.POSIXct(fcastenddate), ymin= `90%`, ymax = `95%`,fill="10 -  5%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(firstNonNAdate), xmax= as.POSIXct(fcastenddate), ymin= `75%`, ymax = `90%`,fill="25 - 10%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(firstNonNAdate), xmax= as.POSIXct(fcastenddate), ymin= `50%`, ymax = `75%`,fill="50 - 25%"),   alpha=0.2) +
    #  geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(firstNonNAdate), xmax= as.POSIXct(fcastenddate), ymin= median, ymax = median,fill="median"),   alpha=0.9) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(firstNonNAdate), xmax= as.POSIXct(fcastenddate), ymin= `25%`, ymax = `50%`,fill="75 - 50%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(firstNonNAdate), xmax= as.POSIXct(fcastenddate), ymin= `10%`, ymax = `25%`,fill="90 - 75%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(firstNonNAdate), xmax= as.POSIXct(fcastenddate), ymin= `5%`, ymax = `10%`,fill="95 - 90%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(firstNonNAdate), xmax= as.POSIXct(fcastenddate), ymin= `min`, ymax = `5%`,fill=">95% (btm)"), alpha=0.2) +
    
    geom_line(data = df_hefs, aes(x = fcast_t_pdt, y = stage, group = mefp_yr, color = "mefp_yr") , size = 0.2) +
    geom_point(data= df_mefpyr_max, aes(x = fcast_t_pdt, y = stage, color = "per. max(mefp_yr)"), size = 2) +
    geom_segment(data = df_qntls_p, 
                 aes(x=as.POSIXct(firstNonNAdate),xend=as.POSIXct(fcastenddate),
                     y=median,yend=median, linetype = "median"), color = "blue") +
    geom_line(data = dtrmnstc, aes( x = fcast_t_pdt, y = stage,color = "dtrmnstc fcast" ), size = 1) +

    
    
    
    
    #facet_wrap(~fcast, ncol = 1) + labs(y = "stage", x = NULL) +
    scale_x_datetime(expand = c(0.01, 0.01),
                     date_breaks = "1 day", date_minor_breaks = "12 hours", date_labels = "%b %d\n%H:%M pst") +
    scale_fill_manual(name = "period\nmax(mefp_yr)\nquantile",
                      
                      values = c(
                        '<5% (top)' = 'snow3',
                        '10 -  5%' = 'yellow',
                        '25 - 10%' = 'green', 
                        '50 - 25%' = 'blue',
                        #  'median' = 'blue4',
                        '75 - 50%' = 'blue',
                        '90 - 75%' = 'green',
                        '95 - 90%' = 'yellow',
                        '>95% (btm)' = 'snow3'),
                      breaks = c(
                        '<5% (top)' ,
                        '10 -  5%' ,
                        '25 - 10%' , 
                        '50 - 25%' ,
                        #'median',
                        '75 - 50%' ,
                        '90 - 75%' ,
                        '95 - 90%',
                        '>95% (btm)' )) +
    
    scale_color_manual(name = "series",
                       
                       values= c(
                         'dtrmnstc fcast' = 'black',
                         'mefp_yr' = 'pink',
                         'per. max(mefp_yr)' = 'pink3'),
                       guide = guide_legend(override.aes = list(
                         linetype = c("solid",    "solid", "blank"),
                         shape = c(NA, NA, 16)))) + labs(x = NULL) +
    scale_linetype_manual(name = "period\nmax(mefp_yr)\nstat",values = c(1, 1), 
                          guide = guide_legend(override.aes = list(color = c("blue"))))
  
}

