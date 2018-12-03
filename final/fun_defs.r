
#Sys.setenv(TZ="America/Los_Angeles")

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

############################################
######## Ridges and Dists Combined
############################################





##################################
######### cum vol ridges  ########
##################################


#plotridgesandpdfs_cumvol(hefs_dtrmnstc, am_fcast, fcast_start_date, fcast_end_date, ridge_scale)

plotridgesandpdfs_cumvol <- function(hefs_dtrmnstc, am_fcast, fcast_start_date, fcast_end_date, vol_ridge_scale) {
  dtrmnstc_min <- dtrmnstc %>% filter(fcast_i_pdt == am_fcast) 
  dtrmnstc_min <- min(dtrmnstc_min$fcast_t_pdt)
  hefs_dtrmnstc <- hefs_dtrmnstc %>% filter(fcast_t_pdt >= fcast_start_date, fcast_t_pdt >= dtrmnstc_min, fcast_t_pdt <= fcast_end_date, fcast_i_pdt == am_fcast)
 # hefs_dtrmnstc <- hefs_dtrmnstc %>% group_by(fcast_t_pdt) %>%  filter(all(!is.na(cfs))) %>% ungroup() 
  
  
  start_time <- min(hefs_dtrmnstc$fcast_t_pdt)
  end_time   <- max(hefs_dtrmnstc$fcast_t_pdt)
  dt_midpoint <- format(as.POSIXct((as.numeric(end_time) + as.numeric(start_time)) / 2, origin = '1970-01-01'), "%Y-%m-%d %H:%M:%S") #R somehow internally uses 1/1/1970
  #end_time_format   <- as.POSIXct(max(hefs_dtrmnstc$fcast_t_pdt), "%Y-%m-%d %H:%M:%S")
  
  hefs_dtrmnstc_wstats <- hefs_dtrmnstc %>% mutate(taf = cfs*1.98347/24/1000) %>% group_by(mefp_yr) %>% mutate(taf_fcper = sum(taf)) %>% ungroup(mefp_yr)
  
  hefs_dtrmnstc_wstats <- hefs_dtrmnstc_wstats %>% mutate(taf_fcper_label = ifelse(fcast_t_pdt == dt_midpoint, taf_fcper, NA), 
                                                          facet_ridge_taf = paste0("Deterministic & MEFP-based Traces, Flow & Cumulative Volume (60 total); from: ", 
                                                                                   start_time, " to: ", end_time, " midnight; ranked"), facet_text_taf = "cum. taf")
  
  
  minhtaf   <- min(hefs_dtrmnstc_wstats$taf_fcper)
  maxhtaf   <- max(hefs_dtrmnstc_wstats$taf_fcper)
  
  
  
  #hefs_dtrmnstc_wstats <- hefs_dtrmnstc_wstats %>% mutate(mefp_yr = as.factor(mefp_yr))
  #hefs_dtrmnstc_wstats$mefp_yr <- factor(hefs_dtrmnstc_wstats$mefp_yr, levels=hefs_dtrmnstc_wstats$mefp_yr[order(df$taf_fcper)], ordered=TRUE)
  
  hefs_dtrmnstc_wstats <- hefs_dtrmnstc_wstats %>%  arrange(taf_fcper) %>%               # sort dataframe
    mutate(mefp_yr = factor(mefp_yr, unique(mefp_yr)))  # reset factor-column based on that order
  
  p_ridges <- ggplot(hefs_dtrmnstc_wstats, aes(fcast_t_pdt, mefp_yr, height = cfs, group=as.factor(mefp_yr), fill = taf_fcper))+
    # p_ridges <- ggplot(hefs_dtrmnstc_wstats, aes(fcast_t_pdt, mefp_yr, height = cfs, group=arrange(desc(taf_fcper)), fill = taf_fcper))+
    geom_ridgeline( stat = "identity", show.legend = T, scale = vol_ridge_scale, alpha = 0.8,
                    min_height = minhtaf) + 
    facet_grid(~facet_ridge_taf) +
    scale_fill_viridis(name = "cum\ntaf") + theme_gray() +
    theme(legend.key.width = unit(2.5, "in"), legend.key.height = unit(0.2, "in")) +
    scale_x_datetime(expand = c(0.01, 0.01)) +
    theme(legend.position="bottom") + labs(y = NULL, x = NULL) +
    theme(axis.text.y = element_text(size = 8)) +
    theme(strip.text.x = element_text(size = 8))  +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank()) 
  p_ridges_leg <- get_legend(p_ridges)
  p_ridges <- p_ridges + theme(legend.position="none")
  
  
  #p_ridges
  
  ######## 4-day text at right  ########
  
  p_ridges_t <- ggplot(hefs_dtrmnstc_wstats, aes(fcast_t_pdt, mefp_yr, height = cfs, group=as.factor(mefp_yr), fill = taf_fcper))+
    #p_ridges_t <- ggplot(hefs_dtrmnstc_wstats, aes(fcast_t_pdt, mefp_yr, height = cfs, group=arrange(desc(taf_fcper)), fill = taf_fcper))+
    geom_ridgeline( stat = "identity", show.legend = F, scale = vol_ridge_scale, fill = NA, col = NA,
                    min_height = minhtaf) + 
    facet_grid(~facet_text_taf) +
    
    
    scale_x_datetime(expand = c(0.01, 0.01)) +
    geom_text(aes(label = round(taf_fcper_label,1), color = taf_fcper, hjust = 1), size = 3.5)  +
    scale_colour_gradientn(colours = viridis(10)) + theme_gray() +
    
    geom_text(aes(label = round(taf_fcper_label,1), hjust = 1), 
              color = ifelse(hefs_dtrmnstc_wstats$taf_fcper==minhtaf |hefs_dtrmnstc_wstats$taf_fcper==maxhtaf | hefs_dtrmnstc_wstats$mefp_yr == "dtrmnstc" , 'black', NA), size = 3.5) +
    theme(legend.position="none") + labs(y = NULL, x = NULL) +
    
    #theme_void() + theme(legend.position="none") 
    # theme(strip.text.x = element_text(size = 12), strip.background = element_rect(colour = "black", fill = "gray")) 
    
    #theme(plot.title = element_text(size=10))+  
    # theme(plot.title = element_text(vjust=2.5))+ theme(axis.text.y = element_text(color = 'white'), axis.ticks = element_line(color = "white"))+
    #theme(plot.title = element_text(vjust=2.5))+
    theme(axis.text.y = element_blank(), axis.ticks = element_blank()) +
    
    theme(legend.position="none") +  labs(x = NULL, y=NULL) +
    
    #theme(plot.title = element_text(vjust=2)) +
    #theme(plot.title = element_text(hjust = 0.5))  +
    #theme(plot.title = element_text(size=11)) +
    
    theme(axis.text.x = element_text(color = 'white'), axis.ticks = element_line(color = "white")) +
    #theme(plot.margin = unit(c(0.05, 0.01, 0.05, 0.01), "cm")) 
    theme(axis.ticks = element_line(color = "white")) +  
    theme(axis.text.x = element_text(color = 'white'), axis.ticks = element_line(color = "white")) +
    
    labs(y = NULL, x = NULL) +
    #theme(axis.text.y = element_text(color = 'white', size = 8), axis.ticks = element_line(color = "white")) 
    theme(strip.text.x = element_text(size = 8))  +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank())
  #theme(axis.text.y=element_blank(),axis.ticks.y=element_blank())
  
  #p_ridges_t
  
  p_cumvol <- plot_grid(p_ridges, p_ridges_t, ncol = 2, rel_widths = c(9,1))
  
  p_cumvol <- plot_grid(p_cumvol, p_ridges_leg, ncol = 1, rel_heights = c(7,1))
  #p_cumvol
  
  #p_mid_left
  
  
  #####################################
  ######## cum vol pdf ################
  #####################################
  
  
  hefs_dtrmnstc_wstats_summary <- hefs_dtrmnstc %>% group_by(mefp_yr) %>% mutate(taf = cfs*1.98347/24/1000) %>% summarize(taf_fcper = sum(taf)) %>% 
    mutate(y="y", facet_vol = "Cumulative Volume (Distribution of 60 totals) (taf)")
  #hefs_dtrmnstc_wstats_mean <- mean(hefs_dtrmnstc_wstats_summary$taf_fcper)
  
  ####### yellow green blue quantiles #######
  
  
  p_cumvol_ygbdist <- ggplot(hefs_dtrmnstc_wstats_summary, aes(x=taf_fcper,y=y,  fill=factor(..quantile..))) +
    
    
    stat_density_ridges( geom = "density_ridges_gradient",calc_ecdf = TRUE, jittered_points=TRUE, scale = .95, rel_min_height = 0.01,
                         point_shape = "|", point_size = 3, size = 0.25, quantile_lines = TRUE, quantiles = c(0.05, 0.10,0.25,0.5,0.75,0.9,0.95), 
                         #point_shape = "|", point_size = 3, size = 0.25, quantile_lines = TRUE, quantiles = c(0.05, 0.10,0.25,0.375, 0.5,0.625, 0.75,0.9,0.95),
                         position = position_points_jitter(height = 0), alpha = 00.99, color = "red") +
    scale_y_discrete(expand = c(0.001, 0.001))  +
    scale_x_continuous(expand = c(0.01, 0), name = "taf" ,
                       breaks = pretty(hefs_dtrmnstc_wstats_summary$taf_fcper, n = 10)) +
    theme_ridges(center = TRUE)  + theme_gray() +labs(y = NULL) + scale_fill_manual(
      
      name = "prob. of excd.", values = alpha(c("snow4", "yellow", "green", "blue", "blue", "green", "yellow", "snow4"),0.2),
      #name = "Excd. Prob.", values = c("snow4", "yellow", "green","light blue", "blue", "blue","light blue", "green", "yellow", "snow4"),
      #       labels = c("(0, 0.05]", "(0.05, 0.10]", "(0.10, 0.25]", "(0.25, 0.50]", "(0.50, 0.75]", "(0.75, 0.90]", "(0.90, 0.95]", "(0.95, 1]")
      # labels = c("(0, 0.05]", "(0.05, 0.10]", "(0.10, 0.25]", "(0.25, 0.50]", "(0.50, 0.75]", "(0.75, 0.90]", "(0.90, 0.95]", "(0.95, 1]")
      # labels = c("(0.95, 1]",   "(0.90, 0.95]", "(0.75, 0.90]", "(0.50, 0.75]", "(0.25, 0.50]",  "(0.10, 0.25]", "(0.05, 0.10]", "(0, 0.05]")
      labels = c(">95% (left)", "95 - 90%", "90 - 75%", "75 - 50%", "50 - 25%", "25 - 10%", "10 - 5%", "<5% (right)") 
    ) +
    theme(axis.text.y = element_blank(), axis.ticks = element_blank())  +
    theme(legend.key.width = unit(0.2, "in"), legend.key.height = unit(0.2, "in")) +
    facet_grid(~facet_vol) +labs(x = NULL) +theme(legend.position = "bottom") + theme(axis.title.x=element_blank())
  
  #p_cumvol_ygbdist
  
  
  ############  gradient pdf ################################    
  
  p_cumvol_grad <-  ggplot(hefs_dtrmnstc_wstats_summary, aes(x = `taf_fcper`, y = y, fill = ..x..)) +
    geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01, gradient_lwd = 1.) +
    
    scale_y_discrete(expand = c(0.0, 0)) +
    scale_x_continuous(expand = c(0.01, 0), name = "taf" ,
                       breaks = pretty(hefs_dtrmnstc_wstats_summary$taf_fcper, n = 10)) +
    scale_fill_viridis( name = "taf" , limits = range(hefs_dtrmnstc_wstats$taf_fcper)) +
    # scale_fill_colorbrewer(name = "Temp. [F]", option = "C") +
    # labs(
    #  title = 'Temperatures in Lincoln NE',
    # subtitle = 'Mean temperatures (Fahrenheit) by month for 2016\nData: Original CSV from the Weather Underground'
    #) +
    #theme_ridges(font_size = 13, grid = TRUE) + 
    theme(axis.title.y = element_blank()) +
    theme_gray() +  theme(axis.text.y = element_blank(), axis.ticks = element_blank())  +
    labs(x = NULL, y = NULL) +
    theme(legend.key.width = unit(0.8, "in"), legend.key.height = unit(0.05, "in")) +
    facet_grid(~facet_vol) + theme(legend.position = "bottom") + theme(axis.title.x=element_blank())
  # p_cumvol_grad
  #p3stages <- plot_grid(p_ft, p_cumvol_grad, ncol = 1, rel_heights = c(2,1))
  #p3stages
  #ggsave("eventstagemaxft_2.pdf", dpi = 600, width = 17, height = 11, units = "in") 
  
  p_2cumvoldists <- plot_grid(p_cumvol_grad, p_cumvol_ygbdist, ncol = 1, rel_heights = c(1,1))
  #p_2cumvoldists
  
  
  ######### all together ########
  pcumvolall <- plot_grid(p_cumvol, p_2cumvoldists, ncol = 1, rel_heights = c(1.75,1))
  pcumvolall
  #ggsave("cumvol_all.pdf", dpi = 600, width = 17, height = 11, units = "in") 
}

#####################################   
############# Peak flow ############
####################################

#plotridgesandpdfs_flow(hefs_dtrmnstc, am_fcast, fcast_start_date, fcast_end_date, ridge_scale)

plotridgesandpdfs_flow <- function(hefs_dtrmnstc, am_fcast, fcast_start_date, fcast_end_date, flo_ridge_scale) {
  dtrmnstc_min <- dtrmnstc %>% filter(fcast_i_pdt == am_fcast) 
  dtrmnstc_min <- min(dtrmnstc_min$fcast_t_pdt)
  hefs_dtrmnstc <- hefs_dtrmnstc %>% filter(fcast_t_pdt >= fcast_start_date, fcast_t_pdt >= dtrmnstc_min, fcast_t_pdt <= fcast_end_date, fcast_i_pdt == am_fcast)
  #hefs_dtrmnstc <- hefs_dtrmnstc %>% group_by(fcast_t_pdt) %>%  filter(all(!is.na(cfs))) %>% ungroup() 
  
  start_time <- min(hefs_dtrmnstc$fcast_t_pdt)
  end_time <- max(hefs_dtrmnstc$fcast_t_pdt)
  dt_midpoint <- as.POSIXct((as.numeric(end_time) + as.numeric(start_time)) / 2, origin = '1970-01-01') #R somehow internally uses 1/1/1970
  dt_midpoint <- format(round(dt_midpoint, "hours"), "%Y-%m-%d %H:%M:%S")
  
  hefs_dtrmnstc_wstats <- hefs_dtrmnstc  %>% group_by(mefp_yr) %>% mutate(maxcfs_fcper = max(cfs)) %>% ungroup(mefp_yr)
  
  hefs_dtrmnstc_wstats <- hefs_dtrmnstc_wstats %>% mutate(maxcfs_fcper_label = ifelse(fcast_t_pdt == dt_midpoint, maxcfs_fcper, NA), 
                                                          facet_ridge_cfs =  paste0("Deterministic & MEFP-based Traces, Flow and Peak Flow (60 total); from: ", 
                                                                                    start_time, " to: ", end_time, " midnight; ranked"), facet_text_cfs = "max. kcfs")
  
  
  
  minhcfs   <- min(hefs_dtrmnstc_wstats$maxcfs_fcper)
  maxhcfs   <- max(hefs_dtrmnstc_wstats$maxcfs_fcper)
  
  
  
  #### 4-day peak flow ridges #######
  
  hefs_dtrmnstc_wstats <- hefs_dtrmnstc_wstats %>%  arrange(maxcfs_fcper) %>%               # sort your dataframe
    mutate(mefp_yr = factor(mefp_yr, unique(mefp_yr)))  # reset your factor-column based on that order
  
  
  p2 <- ggplot(hefs_dtrmnstc_wstats, aes(fcast_t_pdt, mefp_yr, height = cfs, group= mefp_yr, fill = cfs))+
    geom_ridgeline_gradient(scale = flo_ridge_scale) + 
    scale_fill_distiller(palette = "Spectral", name = "cfs" ) + theme_gray() +
    #scale_fill_viridis( name = "cfs" ) + theme_gray() +
    theme(legend.key.width = unit(2.5, "in"), legend.key.height = unit(0.2, "in")) +
    scale_x_datetime(expand = c(0.01, 0.01)) +
    facet_grid(~facet_ridge_cfs) +
    theme(legend.position="bottom") + labs(y = NULL, x = NULL) +
    theme(axis.text.y = element_text(size = 8)) + theme(strip.text.x = element_text(size = 8))  +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank()) + labs(x = NULL)
  p2_leg <- get_legend(p2)
  p2 <- p2 + theme(legend.position="none") 
  #p2
  
  ######## text at right  ########
  
  
  p2t <- ggplot(hefs_dtrmnstc_wstats, aes(fcast_t_pdt, mefp_yr, height = cfs, group=as.factor(mefp_yr), fill = maxcfs_fcper))+
    geom_ridgeline( stat = "identity", show.legend = F, scale = flo_ridge_scale, fill = NA, col = NA,
                    min_height = minhcfs) + 
    facet_grid(~facet_text_cfs) +
    
    
    #scale_x_datetime(expand = c(0.1, 0.1)) +
    geom_text(aes(label = round(maxcfs_fcper_label/1000,1), color = maxcfs_fcper, hjust = 1), size = 3.5)  +
    scale_color_distiller(palette = "Spectral", limits = range(hefs_dtrmnstc_wstats$cfs)) + theme_gray() +  #YlGnBu
    #scale_colour_gradientn(colours = viridis(10)) + theme_gray() +  
    geom_text(aes(label = round(maxcfs_fcper_label/1000,1), hjust = 1), 
              color = ifelse(hefs_dtrmnstc_wstats$maxcfs_fcper==minhcfs |hefs_dtrmnstc_wstats$maxcfs_fcper==maxhcfs | hefs_dtrmnstc_wstats$mefp_yr == "dtrmnstc" , 'black', NA), size = 3.5) +
    #theme(legend.position="none") + 
    #theme(legend.key.width = unit(.4, "in"), legend.key.height = unit(0.4, "in")) +
    
    #theme(legend.position="bottom") +
    labs(y = NULL, x = NULL) +
    
    #theme_void() + theme(legend.position="none") 
    # theme(strip.text.x = element_text(size = 12), strip.background = element_rect(colour = "black", fill = "gray")) 
    
    #theme(plot.title = element_text(size=10))+  
    # theme(plot.title = element_text(vjust=2.5))+ theme(axis.text.y = element_text(color = 'white'), axis.ticks = element_line(color = "white"))+
    #theme(plot.title = element_text(vjust=2.5))+
    theme(axis.text.y = element_blank(), axis.ticks = element_blank()) +
    
    theme(legend.position="none") + 
    labs(x = NULL, y=NULL) +
    
    #theme(plot.title = element_text(vjust=2)) +
    #theme(plot.title = element_text(hjust = 0.5))  +
    #theme(plot.title = element_text(size=11)) +
    
    theme(axis.text.x = element_text(color = 'white'), axis.ticks = element_line(color = "white")) +
    #theme(plot.margin = unit(c(0.05, 0.01, 0.05, 0.01), "cm")) 
    theme(axis.ticks = element_line(color = "white")) +  
    theme(axis.text.x = element_text(color = 'white'), axis.ticks = element_line(color = "white")) +
    
    labs(y = NULL, x = NULL) + theme(strip.text.x = element_text(size = 8))  +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank())
  #theme(axis.text.y = element_text(color = 'white', size = 8), axis.ticks = element_line(color = "white")) 
  #theme(strip.text.x = element_text(size = 8)) +  theme(axis.text.y=element_blank(),axis.ticks.y=element_blank())
  
  #p2t
  
  
  p_stageridges_noleg <- plot_grid(p2, p2t, ncol = 2, rel_widths = c(9,1))
  p_stageandtext <- plot_grid(p_stageridges_noleg, p2_leg, ncol = 1, rel_heights = c(7,1))
  #p_stageandtext 
  
  
  hefs_dtrmnstc_wstats_summary <- hefs_dtrmnstc %>% group_by(mefp_yr) %>% summarize(maxcfs_fcper = max(cfs)) %>% 
    mutate(y="y", facet_peak = "Peak Flow (Distribution of 60 maximums) (cfs)")
  
  
  p_flowpk_ygb <- ggplot(hefs_dtrmnstc_wstats_summary, aes(x=maxcfs_fcper,y=y,  fill=factor(..quantile..))) +
    
    stat_density_ridges( geom = "density_ridges_gradient",calc_ecdf = TRUE, jittered_points=TRUE, scale = 1000, rel_min_height = 0.01,
                         point_shape = "|", point_size = 3, size = 0.25, quantile_lines = TRUE, quantiles = c(0.05, 0.10,0.25,0.5,0.75,0.9,0.95),
                         position = position_points_jitter(height = 0), alpha = 0.9, color = "red") +
    scale_y_discrete(expand = c(0.0, 0.0))  +
    scale_x_continuous(expand = c(0, 0), name = "cfs" ,
                       breaks = pretty(hefs_dtrmnstc_wstats_summary$maxcfs_fcper, n = 10)) +
    #  breaks = c(0, 25000, 50000, 75000, 100000, 125000, 150000, 175000, 200000)) +
    #  labels = c("0", "25k", "50k", "75k", "100k", "125k", "150k", "175k", "200k"))  +
    theme_ridges(center = TRUE)  + theme_gray() +labs(y = NULL) + scale_fill_manual(
      
      name = "prob. of exc.", values = alpha(c("snow4", "yellow", "green", "blue", "blue", "green", "yellow", "snow4"),0.2),
      #       labels = c("(0, 0.05]", "(0.05, 0.10]", "(0.10, 0.25]", "(0.25, 0.50]", "(0.50, 0.75]", "(0.75, 0.90]", "(0.90, 0.95]", "(0.95, 1]")
      # labels = c("(0, 0.05]", "(0.05, 0.10]", "(0.10, 0.25]", "(0.25, 0.50]", "(0.50, 0.75]", "(0.75, 0.90]", "(0.90, 0.95]", "(0.95, 1]")
      #labels = c("(0.95, 1]", "(0.90, 0.95]", "(0.75, 0.90]", "(0.50, 0.75]", "(0.25, 0.50]",  "(0.10, 0.25]", "(0.05, 0.10]", "(0, 0.05]")
      labels = c(">95% (left)", "95 - 90%", "90 - 75%", "75 - 50%", "50 - 25%", "25 - 10%", "10 - 5%", "<5% (right)")
    ) +
    theme(axis.text.y = element_blank(), axis.ticks = element_blank())  +
    theme(legend.key.width = unit(0.8, "in"), legend.key.height = unit(0.1, "in")) +
    facet_grid(~facet_peak) + labs(x = NULL) +theme(legend.position = "bottom") + theme(axis.title.x=element_blank())
  # p_flowpk_ygb
  
  ############  gradient pdf ################################    
  
  p_maxcfs_grad <-  ggplot(hefs_dtrmnstc_wstats_summary, aes(x = `maxcfs_fcper`, y = y, fill = ..x..)) +
    geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01, gradient_lwd = 1.) +
    
    scale_y_discrete(expand = c(0.0, 0)) +
    scale_x_continuous(expand = c(0.01, 0), name = "cfs" ,
                       breaks = pretty(hefs_dtrmnstc_wstats_summary$maxcfs_fcper, n = 10)) +
    scale_fill_distiller( palette = "Spectral", name = "cfs" ,  limits = range(hefs_dtrmnstc_wstats$cfs)) +
    # scale_fill_colorbrewer(name = "Temp. [F]", option = "C") +
    # labs(
    #  title = 'Temperatures in Lincoln NE',
    # subtitle = 'Mean temperatures (Fahrenheit) by month for 2016\nData: Original CSV from the Weather Underground'
    #) +
    #theme_ridges(font_size = 13, grid = TRUE) + 
    theme(axis.title.y = element_blank()) +
    theme_gray() +  theme(axis.text.y = element_blank(), axis.ticks = element_blank())  +
    labs(x = NULL, y = NULL) +
    theme(legend.key.width = unit(0.8, "in"), legend.key.height = unit(0.05, "in")) +
    facet_grid(~facet_peak) + theme(legend.position = "bottom") + theme(axis.title.x=element_blank())
  # p_cumvol_grad
  #p3stages <- plot_grid(p_ft, p_cumvol_grad, ncol = 1, rel_heights = c(2,1))
  #p3stages
  #ggsave("eventstagemaxft_2.pdf", dpi = 600, width = 17, height = 11, units = "in") 
  # p_maxcfs_grad
  p_2peakflowdists <- plot_grid(p_maxcfs_grad,  p_flowpk_ygb, ncol = 1, rel_heights = c(1,1))
  # p_2peakflowdists
  
  
  ######### all together ########
  pflowall <- plot_grid(p_stageandtext , p_2peakflowdists, ncol = 1, rel_heights = c(1.75,1))
  pflowall
  #ggsave("flowall.pdf", dpi = 600, width = 17, height = 11, units = "in") 
}

#####################################   
############# stage #################
####################################
#plotridgesandpdfs_stage(hefs_dtrmnstc, am_fcast, fcast_start_date, fcast_end_date, ridge_scale)

plotridgesandpdfs_stage <- function(hefs_dtrmnstc, am_fcast, fcast_start_date, fcast_end_date, sta_ridge_scale) {
  dtrmnstc_min <- dtrmnstc %>% filter(fcast_i_pdt == am_fcast) 
  dtrmnstc_min <- min(dtrmnstc_min$fcast_t_pdt)
  #dtrmnstc_min <- min(dtrmnstc$fcast_t_pdt)
  hefs_dtrmnstc <- hefs_dtrmnstc %>% filter(fcast_t_pdt >= fcast_start_date, fcast_t_pdt >= dtrmnstc_min, fcast_t_pdt <= fcast_end_date, fcast_i_pdt == am_fcast)
  #hefs_dtrmnstc <- hefs_dtrmnstc %>% group_by(fcast_t_pdt) %>%  filter(all(!is.na(cfs))) %>% ungroup() 
  
  start_time <- min(hefs_dtrmnstc$fcast_t_pdt)
  end_time <- max(hefs_dtrmnstc$fcast_t_pdt)
  dt_midpoint <- as.POSIXct((as.numeric(end_time) + as.numeric(start_time)) / 2, origin = '1970-01-01') #R somehow internally uses 1/1/1970
  dt_midpoint <- as.POSIXct(round_date(dt_midpoint, unit = "hour"))
  
  hefs_dtrmnstc_wstats <- hefs_dtrmnstc  %>% group_by(mefp_yr) %>% mutate(maxft_fcper = max(feet)) %>% ungroup(mefp_yr)
  
  hefs_dtrmnstc_wstats <- hefs_dtrmnstc_wstats %>% mutate(maxft_fcper_label = ifelse(fcast_t_pdt == dt_midpoint, maxft_fcper, NA), 
                                                          facet_ridge_ft =  paste0("Deterministic & MEFP-based Traces, Stage and Peak Stage (60 total); from: ", 
                                                                                   start_time, " to: ", end_time, " midnight; ranked"), facet_text_ft = "max. stage (ft)")
  
  
  minhft   <- min(hefs_dtrmnstc_wstats$maxft_fcper)
  maxhft   <- max(hefs_dtrmnstc_wstats$maxft_fcper)
  #### 4-day stage ridges #######
  
  hefs_dtrmnstc_wstats <- hefs_dtrmnstc_wstats %>%  arrange(maxft_fcper) %>%               # sort your dataframe
    mutate(mefp_yr = factor(mefp_yr, unique(mefp_yr)))  # reset your factor-column based on that order
  
  
  
  
  p3 <- ggplot(hefs_dtrmnstc_wstats, aes(fcast_t_pdt, mefp_yr, height = feet, group=as.factor(mefp_yr), fill = feet))+
    geom_ridgeline_gradient(scale = sta_ridge_scale) + 
    scale_fill_distiller(palette = "Spectral", name = "feet" ) + theme_gray() +
    #scale_fill_viridis( name = "feet" ) + theme_gray() +
    theme(legend.key.width = unit(2.5, "in"), legend.key.height = unit(0.2, "in")) +
    scale_x_datetime(expand = c(0.01, 0.01)) +
    facet_grid(~facet_ridge_ft) +
    theme(legend.position="bottom") + labs(y = NULL, x = NULL) +
    theme(axis.text.y = element_text(size = 8)) + theme(strip.text.x = element_text(size = 8))  +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank()) + labs(x = NULL)
  p3_leg <- get_legend(p3)
  p3 <- p3 + theme(legend.position="none") 
  
  #p3
  ######## text at right  ########
  
  
  p3t <- ggplot(hefs_dtrmnstc_wstats, aes(fcast_t_pdt, mefp_yr, height = feet, group=as.factor(mefp_yr), fill = maxft_fcper))+
    geom_ridgeline( stat = "identity", show.legend = F, scale = sta_ridge_scale, fill = NA, col = NA,
                    min_height = minhft) + 
    facet_grid(~facet_text_ft) +
    
    
    #scale_x_datetime(expand = c(0.1, 0.1)) +
    geom_text(aes(label = round(maxft_fcper_label,1), color = maxft_fcper  , hjust = 1), size = 3.5)  +
    scale_color_distiller(palette = "Spectral", limits = range(hefs_dtrmnstc_wstats$feet)) + theme_gray() +  #YlGnBu
    #scale_colour_gradientn(colours = viridis(10)) + theme_gray() +  
    geom_text(aes(label = round(maxft_fcper_label,1), hjust = 1), 
              color = ifelse(hefs_dtrmnstc_wstats$maxft_fcper==minhft |hefs_dtrmnstc_wstats$maxft_fcper==maxhft | hefs_dtrmnstc_wstats$mefp_yr == "dtrmnstc" , 'black', NA), size = 3.5) +
    #theme(legend.position="none") + 
    #theme(legend.key.width = unit(.4, "in"), legend.key.height = unit(0.4, "in")) +
    
    #theme(legend.position="bottom") +
    labs(y = NULL, x = NULL) +
    
    #theme_void() + theme(legend.position="none") 
    # theme(strip.text.x = element_text(size = 12), strip.background = element_rect(colour = "black", fill = "gray")) 
    
    #theme(plot.title = element_text(size=10))+  
    # theme(plot.title = element_text(vjust=2.5))+ theme(axis.text.y = element_text(color = 'white'), axis.ticks = element_line(color = "white"))+
    #theme(plot.title = element_text(vjust=2.5))+
    theme(axis.text.y = element_blank(), axis.ticks = element_blank()) +
    
    theme(legend.position="none") + 
    labs(x = NULL, y=NULL) +
    
    #theme(plot.title = element_text(vjust=2)) +
    #theme(plot.title = element_text(hjust = 0.5))  +
    #theme(plot.title = element_text(size=11)) +
    
    theme(axis.text.x = element_text(color = 'white'), axis.ticks = element_line(color = "white")) +
    #theme(plot.margin = unit(c(0.05, 0.01, 0.05, 0.01), "cm")) 
    theme(axis.ticks = element_line(color = "white")) +  
    theme(axis.text.x = element_text(color = 'white'), axis.ticks = element_line(color = "white")) +
    
    labs(y = NULL, x = NULL) + theme(strip.text.x = element_text(size = 8))  +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank())
  #theme(axis.text.y = element_text(color = 'white', size = 8), axis.ticks = element_line(color = "white")) 
  #theme(strip.text.x = element_text(size = 8)) +  theme(axis.text.y=element_blank(),axis.ticks.y=element_blank())
  
  #p3t
  
  
  p_ft_scaleless <- plot_grid(p3, p3t, ncol = 2, rel_widths = c(9,1))
  p_ft <- plot_grid(p_ft_scaleless, p3_leg, ncol = 1, rel_heights = c(7,1))
  #p_mid_right
  
  
  
  ########## peak pdf  ###################
  
  hefs_dtrmnstc_wstats_summary <- hefs_dtrmnstc %>% group_by(mefp_yr) %>% summarize(maxft_fcper = max(feet)) %>% 
    mutate(y="y", facet_stage = "Peak Stage (Distribution of 60 maximums) (feet)")
  
  
  p_eventstagepk_ygb <- ggplot(hefs_dtrmnstc_wstats_summary, aes(x=maxft_fcper,y=y,  fill=factor(..quantile..))) +
    
    stat_density_ridges( geom = "density_ridges_gradient",calc_ecdf = TRUE, jittered_points=TRUE, scale = 1000, rel_min_height = 0.01,
                         point_shape = "|", point_size = 3, size = 0.25, quantile_lines = TRUE, quantiles = c(0.05, 0.10,0.25,0.5,0.75,0.9,0.95),
                         position = position_points_jitter(height = 0), alpha = 0.9, color = "black") +
    scale_y_discrete(expand = c(0.01, 0.01))  +
    scale_x_continuous(expand = c(0.01, 0), name = "ft" ,
                       breaks = pretty(hefs_dtrmnstc_wstats_summary$maxft_fcper, n = 10)) +
    #  breaks = c(0, 25000, 50000, 75000, 100000, 125000, 150000, 175000, 200000)) +
    #  labels = c("0", "25k", "50k", "75k", "100k", "125k", "150k", "175k", "200k"))  +
    theme_ridges(center = TRUE)  + theme_gray() +labs(y = NULL) + scale_fill_manual(
      
      name = "prob. of exc.(2d)", values = alpha(c("snow4", "yellow", "green", "blue", "blue", "green", "yellow", "snow4"),0.2),
      #       labels = c("(0, 0.05]", "(0.05, 0.10]", "(0.10, 0.25]", "(0.25, 0.50]", "(0.50, 0.75]", "(0.75, 0.90]", "(0.90, 0.95]", "(0.95, 1]")
      # labels = c("(0, 0.05]", "(0.05, 0.10]", "(0.10, 0.25]", "(0.25, 0.50]", "(0.50, 0.75]", "(0.75, 0.90]", "(0.90, 0.95]", "(0.95, 1]")
      #labels = c("(0.95, 1]", "(0.90, 0.95]", "(0.75, 0.90]", "(0.50, 0.75]", "(0.25, 0.50]",  "(0.10, 0.25]", "(0.05, 0.10]", "(0, 0.05]")
      labels = c(">95% (left)", "95 - 90%", "90 - 75%", "75 - 50%", "50 - 25%", "25 - 10%", "10 - 5%", "<5% (right)")
    ) +
    theme(axis.text.y = element_blank(), axis.ticks = element_blank())  +
    theme(legend.key.width = unit(0.8, "in"), legend.key.height = unit(0.05, "in")) +
    facet_grid(~facet_stage) + labs(x = NULL) +theme(legend.position = "bottom") +  theme(axis.title.x=element_blank())
  #p_eventstagepk_ygb
  #pstage <- plot_grid(p_ft, p_eventstagepk_ygb, ncol = 1, rel_heights = c(3,1))
  #pstage
  #ggsave("eventstagemaxft.pdf", dpi = 600, width = 17, height = 11, units = "in") 
  
  
  ###### geom gradient ridges  #########
  p_stagepk_grad <-  ggplot(hefs_dtrmnstc_wstats_summary, aes(x = maxft_fcper, y = y, fill = ..x..)) +
    geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01, gradient_lwd = 1.) +
    
    scale_y_discrete(expand = c(0.01, 0)) +
    scale_x_continuous(expand = c(0.01, 0), name = "ft" ,
                       breaks = pretty(hefs_dtrmnstc_wstats_summary$maxft_fcper, n = 10)) +
    scale_fill_distiller(palette = "Spectral", name = "feet" , limits = range(hefs_dtrmnstc_wstats$feet)) +
    # scale_fill_colorbrewer(name = "Temp. [F]", option = "C") +
    # labs(
    #  title = 'Temperatures in Lincoln NE',
    # subtitle = 'Mean temperatures (Fahrenheit) by month for 2016\nData: Original CSV from the Weather Underground'
    #) +
    #theme_ridges(font_size = 13, grid = TRUE) + 
    theme(axis.title.y = element_blank()) +
    theme_gray() +  theme(axis.text.y = element_blank(), axis.ticks = element_blank())  +
    labs(x = NULL, y = NULL) +
    theme(legend.key.width = unit(0.8, "in"), legend.key.height = unit(0.05, "in")) +
    facet_grid(~facet_stage) + theme(legend.position = "bottom") + theme(axis.title.x=element_blank())
  #p_stagepk_grad
  p3stages <- plot_grid(p_ft, p_stagepk_grad, ncol = 1, rel_heights = c(2,1))
  #p3stages
  #ggsave("eventstagemaxft_2.pdf", dpi = 600, width = 17, height = 11, units = "in") 
  
  p_2stagedists <- plot_grid(p_stagepk_grad, p_eventstagepk_ygb, ncol = 1, rel_heights = c(1,1))
  #p_2stagedists
  pstageall <- plot_grid(p_ft, p_2stagedists, ncol = 1, rel_heights = c(1.75,1))
  pstageall
  #ggsave("stage_all.pdf", dpi = 600, width = 17, height = 11, units = "in") 
}
