


##################################
######### cum vol ridges  ########
##################################


fcast_start_date <- c("2016-01-13")
fcast_end_date <- c("2016-01-17")
hefs_dtrmnstc <- hefs_dtrmnstc %>% filter(fcast_t_pdt >= fcast_start_date, fcast_t_pdt <= fcast_end_date, fcast_i_pdt == fcasts[1])

hefs_dtrmnstc <- hefs_dtrmnstc %>% group_by(fcast_t_pdt) %>%  filter(all(!is.na(cfs))) %>% filter(fcast_i_pdt == fcasts[1])
                                          
                                          
start_time <- min(hefs_dtrmnstc$fcast_t_pdt)
end_time <- max(hefs_dtrmnstc$fcast_t_pdt)
dt_midpoint <- as.POSIXct((as.numeric(end_time) + as.numeric(start_time)) / 2, origin = '1970-01-01') #R somehow internally uses 1/1/1970


fc_per <- hefs_dtrmnstc %>% mutate(taf = cfs*1.98347/24/1000) %>% group_by(mefp_yr) %>% mutate(taf_5day = sum(taf), cfs_5day = max(cfs),
                                                                                                ft_5day = max(feet)) %>% ungroup(mefp_yr)

fc_per <- fc_per %>% mutate(taf_5day_label = ifelse(fcast_t_pdt == dt_midpoint, taf_5day, NA), 
                              facet_ridge_taf = "deterministic & mefp-based scenarios, cumulative volume (60 total)", facet_text_taf = "cum. taf",
                              cfs_5day_label = ifelse(fcast_t_pdt == dt_midpoint, cfs_5day, NA), 
                              facet_ridge_cfs = "deterministic & mefp-based scenarios, hourly flow (60 total)", facet_text_cfs = "max. kcfs",
                              ft_5day_label = ifelse(fcast_t_pdt == dt_midpoint, ft_5day, NA), 
                              facet_ridge_ft = "deterministic & mefp-based scenarios,  hourly stage (60 total)", facet_text_ft = "max. height (ft)")




minhtaf   <- min(fc_per$taf_5day)
maxhtaf   <- max(fc_per$taf_5day)
minhcfs   <- min(fc_per$cfs_5day)
maxhcfs   <- max(fc_per$cfs_5day)
minhft   <- min(fc_per$ft_5day)
maxhft   <- max(fc_per$ft_5day)


#fc_per <- fc_per %>% mutate(mefp_yr = as.factor(mefp_yr))
#fc_per$mefp_yr <- factor(fc_per$mefp_yr, levels=fc_per$mefp_yr[order(df$taf_5day)], ordered=TRUE)

fc_per <- fc_per %>%  arrange(taf_5day) %>%               # sort your dataframe
          mutate(mefp_yr = factor(mefp_yr, unique(mefp_yr)))  # reset your factor-column based on that order

  p1 <- ggplot(fc_per, aes(fcast_t_pdt, mefp_yr, height = cfs, group=mefp_yr, fill = taf_5day))+
   # p1 <- ggplot(fc_per, aes(fcast_t_pdt, mefp_yr, height = cfs, group=arrange(desc(taf_5day)), fill = taf_5day))+
    geom_ridgeline( stat = "identity", show.legend = T, scale = 0.0001, alpha = 0.8,
                    min_height = minhtaf) + 
    facet_grid(~facet_ridge_taf) +
    scale_fill_viridis(name = "cum\ntaf") + theme_gray() +
    theme(legend.key.width = unit(0.4, "in"), legend.key.height = unit(2, "in")) +
    scale_x_datetime(expand = c(0.01, 0.01)) +
    theme(legend.position="left") + labs(y = NULL, x = NULL) +
    theme(axis.text.y = element_text(size = 8)) +
    theme(strip.text.x = element_text(size = 8))  +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank())
  
  p1
  ######## 4-day text at right  ########
  
  p1t <- ggplot(fc_per, aes(fcast_t_pdt, mefp_yr, height = cfs, group=as.factor(mefp_yr), fill = taf_5day))+
    #p1t <- ggplot(fc_per, aes(fcast_t_pdt, mefp_yr, height = cfs, group=arrange(desc(taf_5day)), fill = taf_5day))+
    geom_ridgeline( stat = "identity", show.legend = F, scale = 0.0001, fill = NA, col = NA,
                    min_height = minhtaf) + 
    facet_grid(~facet_text_taf) +
    
    
    scale_x_datetime(expand = c(0.01, 0.01)) +
    geom_text(aes(label = round(taf_5day_label,1), color = taf_5day, hjust = 1), size = 3.5)  +
    scale_colour_gradientn(colours = viridis(10)) + theme_gray() +
    
    geom_text(aes(label = round(taf_5day_label,1), hjust = 1), 
              color = ifelse(fc_per$taf_5day==minhtaf |fc_per$taf_5day==maxhtaf | fc_per$mefp_yr == "dtrmnstc" , 'black', NA), size = 3.5) +
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
  
  p1t
  
  p_cumvol <- plot_grid(p1, p1t, ncol = 2, rel_widths = c(7,1))
  #p_mid_left
  
  
  #####################################
  ######## cum vol pdf ################
  #####################################
  
  
  fc_per_summary <- hefs_dtrmnstc %>% group_by(mefp_yr) %>% mutate(taf = cfs*1.98347/24/1000) %>% summarize(taf_5day = sum(taf), cfs_5day = max(cfs), ft_5day = max(feet)) %>% 
    mutate(y="y", facet_vol = "Cumulative Volume (Distribution of 60 totals)", facet_peak = "Peak Flow (Distribution of 60 peaks)",
           facet_stage = "Peak Stage (Distribution of 60 peaks)" )
  fc_per_mean <- mean(fc_per_summary$taf_5day)
  p_cumvol_ygbdist <- ggplot(fc_per_summary, aes(x=taf_5day,y=y,  fill=factor(..quantile..))) +

    
  stat_density_ridges( geom = "density_ridges_gradient",calc_ecdf = TRUE, jittered_points=TRUE, scale = .95, rel_min_height = 0.01,
                       point_shape = "|", point_size = 3, size = 0.25, quantile_lines = TRUE, quantiles = c(0.05, 0.10,0.25,0.5,0.75,0.9,0.95), 
                       #point_shape = "|", point_size = 3, size = 0.25, quantile_lines = TRUE, quantiles = c(0.05, 0.10,0.25,0.375, 0.5,0.625, 0.75,0.9,0.95),
                       position = position_points_jitter(height = 0), alpha = 00.99, color = "red") +
    scale_y_discrete(expand = c(0.001, 0.001))  +
    scale_x_continuous(expand = c(0, 0), name = "taf") +
                     #  breaks = c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)) +
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
    facet_grid(~facet_vol) +labs(x = NULL) +theme(legend.position = "bottom")
  
    p_cumvol_ygbdist
    p_cumvol_all <- plot_grid(p_cumvol, p_cumvol_ygbdist, ncol = 1, rel_heights = c(3,1))
    p_cumvol_all
    ggsave("5daycumvol.pdf", dpi = 600, width = 17, height = 11, units = "in") 

    
#####################################   
############# Peak flow ############
####################################
    
  #### 4-day peak flow ridges #######
  
fc_per <- fc_per %>%  arrange(cfs_5day) %>%               # sort your dataframe
  mutate(mefp_yr = factor(mefp_yr, unique(mefp_yr)))  # reset your factor-column based on that order


  fc_per <- fc_per %>% mutate(facet_ridge_cfs =
                                  "dtrmnstc & mefp-based traces, hourly flow (60 tot.)")
  
  p2 <- ggplot(fc_per, aes(fcast_t_pdt, mefp_yr, height = cfs, group=as.factor(mefp_yr), fill = cfs))+
    geom_ridgeline_gradient(scale = 0.0001) + 
    scale_fill_distiller(palette = "Spectral", name = "cfs" ) + theme_gray() +
    #scale_fill_viridis( name = "cfs" ) + theme_gray() +
    theme(legend.key.width = unit(2, "in"), legend.key.height = unit(0.4, "in")) +
    scale_x_datetime(expand = c(0.01, 0.01)) +
    facet_grid(~facet_ridge_cfs) +
    theme(legend.position="bottom") + labs(y = NULL, x = NULL) +
    theme(axis.text.y = element_text(size = 8)) + theme(strip.text.x = element_text(size = 8))  +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank())
  p2_leg <- get_legend(p2)
  p2 <- p2 + theme(legend.position="none") 
  p2
  
  ######## text at right  ########
  
  
  p2t <- ggplot(fc_per, aes(fcast_t_pdt, mefp_yr, height = cfs, group=as.factor(mefp_yr), fill = cfs_5day))+
    geom_ridgeline( stat = "identity", show.legend = F, scale = 0.0001, fill = NA, col = NA,
                    min_height = minhcfs) + 
    facet_grid(~facet_text_cfs) +
    
    
    #scale_x_datetime(expand = c(0.1, 0.1)) +
    geom_text(aes(label = round(cfs_5day_label/1000,1), color = cfs_5day, hjust = 1), size = 3.5)  +
    scale_color_distiller(palette = "Spectral", limits = range(fc_per$cfs)) + theme_gray() +  #YlGnBu
    #scale_colour_gradientn(colours = viridis(10)) + theme_gray() +  
    geom_text(aes(label = round(cfs_5day_label/1000,1), hjust = 1), 
              color = ifelse(fc_per$cfs_5day==minhcfs |fc_per$cfs_5day==maxhcfs | fc_per$mefp_yr == "dtrmnstc" , 'black', NA), size = 3.5) +
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
  
  p2t
  
  
  p_flow_mid_scaleless <- plot_grid(p2t, p2, ncol = 2, rel_widths = c(1,9))
  p_flow_mid <- plot_grid(p_flow_mid_scaleless, p2_leg, ncol = 1, rel_heights = c(7,1))
 p_flow_mid


  
  
  
  p_eventflowpk_ygb <- ggplot(fc_per_summary, aes(x=cfs_5day,y=y,  fill=factor(..quantile..))) +

  stat_density_ridges( geom = "density_ridges_gradient",calc_ecdf = TRUE, jittered_points=TRUE, scale = 1000, rel_min_height = 0.01,
                       point_shape = "|", point_size = 3, size = 0.25, quantile_lines = TRUE, quantiles = c(0.05, 0.10,0.25,0.5,0.75,0.9,0.95),
                       position = position_points_jitter(height = 0), alpha = 0.9, color = "red") +
    scale_y_discrete(expand = c(0.01, 0.01))  +
    scale_x_continuous(expand = c(0, 0), name = "cfs" ,
                       breaks = pretty(fc_per_summary$cfs_5day, n = 10)) +
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
    facet_grid(~facet_peak) + labs(x = NULL) +theme(legend.position = "bottom")
  p_eventflowpk_ygb
  pflow <- plot_grid(p_flow_mid, p_eventflowpk_ygb, ncol = 1, rel_heights = c(3,1))
pflow
  ggsave("eventpeakmaxcfs_pdf.pdf", dpi = 600, width = 17, height = 11, units = "in") 
  
  
  #####################################   
  ############# stage #################
  ####################################
  
  #### 4-day peak flow ridges #######
  
  fc_per <- fc_per %>%  arrange(ft_5day) %>%               # sort your dataframe
    mutate(mefp_yr = factor(mefp_yr, unique(mefp_yr)))  # reset your factor-column based on that order
  
  
  fc_per <- fc_per %>% mutate(facet_ridge_ft =
                    "dtrmnstc & mefp-based traces, hourly stage (60 tot.)")
  
  p3 <- ggplot(fc_per, aes(fcast_t_pdt, mefp_yr, height = feet, group=as.factor(mefp_yr), fill = feet))+
    geom_ridgeline_gradient(scale = 0.15) + 
    scale_fill_distiller(palette = "Spectral", name = "feet" ) + theme_gray() +
    #scale_fill_viridis( name = "feet" ) + theme_gray() +
    theme(legend.key.width = unit(2.5, "in"), legend.key.height = unit(0.2, "in")) +
    scale_x_datetime(expand = c(0.01, 0.01)) +
    facet_grid(~facet_ridge_ft) +
    theme(legend.position="bottom") + labs(y = NULL, x = NULL) +
    theme(axis.text.y = element_text(size = 8)) + theme(strip.text.x = element_text(size = 8))  +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank())
  p3_leg <- get_legend(p3)
  p3 <- p3 + theme(legend.position="none") 
  
  p3
  ######## text at right  ########
  
  
  p3t <- ggplot(fc_per, aes(fcast_t_pdt, mefp_yr, height = feet, group=as.factor(mefp_yr), fill = ft_5day))+
    geom_ridgeline( stat = "identity", show.legend = F, scale = 0.15, fill = NA, col = NA,
                    min_height = minhft) + 
    facet_grid(~facet_text_ft) +
    
    
    #scale_x_datetime(expand = c(0.1, 0.1)) +
    geom_text(aes(label = round(ft_5day_label,1), color = ft_5day  , hjust = 1), size = 3.5)  +
    scale_color_distiller(palette = "Spectral", limits = range(fc_per$feet)) + theme_gray() +  #YlGnBu
    #scale_colour_gradientn(colours = viridis(10)) + theme_gray() +  
    geom_text(aes(label = round(ft_5day_label,1), hjust = 1), 
              color = ifelse(fc_per$ft_5day==minhft |fc_per$ft_5day==maxhft | fc_per$mefp_yr == "dtrmnstc" , 'black', NA), size = 3.5) +
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
  
  p3t
  
  
  p_ft_scaleless <- plot_grid(p3t, p3, ncol = 2, rel_widths = c(1,9))
  p_ft <- plot_grid(p_ft_scaleless, p3_leg, ncol = 1, rel_heights = c(7,1))
  #p_mid_right
  

  
  ########## peak pdf  ###################
  

  
  
  p_eventstagepk_ygb <- ggplot(fc_per_summary, aes(x=ft_5day,y=y,  fill=factor(..quantile..))) +
    
    stat_density_ridges( geom = "density_ridges_gradient",calc_ecdf = TRUE, jittered_points=TRUE, scale = 1000, rel_min_height = 0.01,
                         point_shape = "|", point_size = 3, size = 0.25, quantile_lines = TRUE, quantiles = c(0.05, 0.10,0.25,0.5,0.75,0.9,0.95),
                         position = position_points_jitter(height = 0), alpha = 0.9, color = "black") +
    scale_y_discrete(expand = c(0.01, 0.01))  +
    scale_x_continuous(expand = c(0.01, 0), name = "ft" ,
                       breaks = pretty(fc_per_summary$ft_5day, n = 10)) +
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
    facet_grid(~facet_stage) + labs(x = NULL) +theme(legend.position = "bottom")
  p_eventstagepk_ygb
  pstage <- plot_grid(p_ft, p_eventstagepk_ygb, ncol = 1, rel_heights = c(3,1))
  pstage
  ggsave("eventstagemaxft.pdf", dpi = 600, width = 17, height = 11, units = "in") 
  
  
  ###### geom gradient ridges  #########
  p_stagepk_grad <-  ggplot(fc_per_summary, aes(x = `ft_5day`, y = y, fill = ..x..)) +
    geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01, gradient_lwd = 1.) +

    scale_y_discrete(expand = c(0.01, 0)) +
    scale_x_continuous(expand = c(0.01, 0), name = "ft" ,
                       breaks = pretty(fc_per_summary$ft_5day, n = 10)) +
    scale_fill_distiller(palette = "Spectral", name = "feet" , limits = range(fc_per$feet)) +
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
    facet_grid(~facet_stage) + theme(legend.position = "bottom")
  p_stagepk_grad
  p3stages <- plot_grid(p_ft, p_stagepk_grad, ncol = 1, rel_heights = c(2,1))
  p3stages
  ggsave("eventstagemaxft_2.pdf", dpi = 600, width = 17, height = 11, units = "in") 
  
  p_2stagedists <- plot_grid(p_stagepk_grad, p_eventstagepk_ygb, ncol = 1, rel_heights = c(1,1))
  p_2stagedists
  p4stages <- plot_grid(p_ft, p_2stagedists, ncol = 1, rel_heights = c(1.75,1))
  p4stages
  ggsave("4stages.pdf", dpi = 600, width = 17, height = 11, units = "in") 
