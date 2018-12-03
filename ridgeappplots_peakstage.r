Sys.setenv(TZ="America/Los_Angeles")
am_fcast <- fcasts[1]
fcast_start_date <- ymd(c("2016-01-13"))
fcast_end_date <- ymd(c("2016-01-18"))
ridge_scale <- c(0.09)

  #####################################   
  ############# stage #################
  ####################################
plotridgesandpdfs_stage(hefs_dtrmnstc, am_fcast, fcast_start_date, fcast_end_date, ridge_scale)

plotridgesandpdfs_stage <- function(hefs_dtrmnstc, am_fcast, fcast_start_date, fcast_end_date, ridge_scale) {

hefs_dtrmnstc <- hefs_dtrmnstc %>% filter(fcast_t_pdt >= fcast_start_date, fcast_t_pdt <= fcast_end_date, fcast_i_pdt == am_fcast)
hefs_dtrmnstc <- hefs_dtrmnstc %>% group_by(fcast_t_pdt) %>%  filter(all(!is.na(cfs))) %>% ungroup() 

start_time <- min(hefs_dtrmnstc$fcast_t_pdt)
end_time <- max(hefs_dtrmnstc$fcast_t_pdt)
dt_midpoint <- as.POSIXct((as.numeric(end_time) + as.numeric(start_time)) / 2, origin = '1970-01-01') #R somehow internally uses 1/1/1970
dt_midpoint <- as.POSIXct(round_date(dt_midpoint, unit = "hour"))

hefs_dtrmnstc_wstats <- hefs_dtrmnstc  %>% group_by(mefp_yr) %>% mutate(maxft_fcper = max(feet)) %>% ungroup(mefp_yr)

hefs_dtrmnstc_wstats <- hefs_dtrmnstc_wstats %>% mutate(maxft_fcper_label = ifelse(fcast_t_pdt == dt_midpoint, maxft_fcper, NA), 
                                                        facet_ridge_ft =  paste0("deterministic & mefp-based traces, stage and peak stage (60 total); from: ", 
                                                                                 start_time, " to: ", end_time, " midnight"), facet_text_ft = "max. stage (ft)")


minhft   <- min(hefs_dtrmnstc_wstats$maxft_fcper)
maxhft   <- max(hefs_dtrmnstc_wstats$maxft_fcper)
  #### 4-day stage ridges #######
  
  hefs_dtrmnstc_wstats <- hefs_dtrmnstc_wstats %>%  arrange(maxft_fcper) %>%               # sort your dataframe
    mutate(mefp_yr = factor(mefp_yr, unique(mefp_yr)))  # reset your factor-column based on that order
  
  

  
  p3 <- ggplot(hefs_dtrmnstc_wstats, aes(fcast_t_pdt, mefp_yr, height = feet, group=as.factor(mefp_yr), fill = feet))+
    geom_ridgeline_gradient(scale = ridge_scale) + 
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
  
  #p3
  ######## text at right  ########
  
  
  p3t <- ggplot(hefs_dtrmnstc_wstats, aes(fcast_t_pdt, mefp_yr, height = feet, group=as.factor(mefp_yr), fill = maxft_fcper))+
    geom_ridgeline( stat = "identity", show.legend = F, scale = ridge_scale, fill = NA, col = NA,
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
  
  
  p_ft_scaleless <- plot_grid(p3t, p3, ncol = 2, rel_widths = c(1,9))
  p_ft <- plot_grid(p_ft_scaleless, p3_leg, ncol = 1, rel_heights = c(7,1))
  #p_mid_right
  

  
  ########## peak pdf  ###################
  
  hefs_dtrmnstc_wstats_summary <- hefs_dtrmnstc %>% group_by(mefp_yr) %>% summarize(maxft_fcper = max(feet)) %>% 
    mutate(y="y", facet_stage = "Peak Stage (Distribution of 60 maximums)")
  
  
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
    facet_grid(~facet_stage) + labs(x = NULL) +theme(legend.position = "bottom")
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
    facet_grid(~facet_stage) + theme(legend.position = "bottom")
  #p_stagepk_grad
  p3stages <- plot_grid(p_ft, p_stagepk_grad, ncol = 1, rel_heights = c(2,1))
  #p3stages
  ggsave("eventstagemaxft_2.pdf", dpi = 600, width = 17, height = 11, units = "in") 
  
  p_2stagedists <- plot_grid(p_stagepk_grad, p_eventstagepk_ygb, ncol = 1, rel_heights = c(1,1))
  #p_2stagedists
  pstageall <- plot_grid(p_ft, p_2stagedists, ncol = 1, rel_heights = c(1.75,1))
  pstageall
  ggsave("stage_all.pdf", dpi = 600, width = 17, height = 11, units = "in") 
}
