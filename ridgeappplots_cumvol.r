Sys.setenv(TZ="America/Los_Angeles")
am_fcast <- fcasts[1]
fcast_start_date <- ymd(c("2016-01-13"))
fcast_end_date <- ymd(c("2016-01-14"))
ridge_scale <- c(0.0001)



##################################
######### cum vol ridges  ########
##################################
plotridgesandpdfs_cumvol(hefs_dtrmnstc, am_fcast, fcast_start_date, fcast_end_date, ridge_scale)

plotridgesandpdfs_cumvol <- function(hefs_dtrmnstc, am_fcast, fcast_start_date, fcast_end_date, ridge_scale) {

hefs_dtrmnstc <- hefs_dtrmnstc %>% filter(fcast_t_pdt >= fcast_start_date, fcast_t_pdt <= fcast_end_date, fcast_i_pdt == am_fcast)
hefs_dtrmnstc <- hefs_dtrmnstc %>% group_by(fcast_t_pdt) %>%  filter(all(!is.na(cfs))) %>% ungroup() 


start_time <- min(hefs_dtrmnstc$fcast_t_pdt)
end_time   <- max(hefs_dtrmnstc$fcast_t_pdt)
dt_midpoint <- format(as.POSIXct((as.numeric(end_time) + as.numeric(start_time)) / 2, origin = '1970-01-01'), "%Y-%m-%d %H:%M:%S") #R somehow internally uses 1/1/1970
#end_time_format   <- as.POSIXct(max(hefs_dtrmnstc$fcast_t_pdt), "%Y-%m-%d %H:%M:%S")

hefs_dtrmnstc_wstats <- hefs_dtrmnstc %>% mutate(taf = cfs*1.98347/24/1000) %>% group_by(mefp_yr) %>% mutate(taf_fcper = sum(taf)) %>% ungroup(mefp_yr)

hefs_dtrmnstc_wstats <- hefs_dtrmnstc_wstats %>% mutate(taf_fcper_label = ifelse(fcast_t_pdt == dt_midpoint, taf_fcper, NA), 
                              facet_ridge_taf = paste0("deterministic & mefp-based traces, flow & cumulative volume (60 total); from: ", 
                                                       start_time, " to: ", end_time, " midnight"), facet_text_taf = "cum. taf")


minhtaf   <- min(hefs_dtrmnstc_wstats$taf_fcper)
maxhtaf   <- max(hefs_dtrmnstc_wstats$taf_fcper)



#hefs_dtrmnstc_wstats <- hefs_dtrmnstc_wstats %>% mutate(mefp_yr = as.factor(mefp_yr))
#hefs_dtrmnstc_wstats$mefp_yr <- factor(hefs_dtrmnstc_wstats$mefp_yr, levels=hefs_dtrmnstc_wstats$mefp_yr[order(df$taf_fcper)], ordered=TRUE)

hefs_dtrmnstc_wstats <- hefs_dtrmnstc_wstats %>%  arrange(taf_fcper) %>%               # sort dataframe
          mutate(mefp_yr = factor(mefp_yr, unique(mefp_yr)))  # reset factor-column based on that order

  p_ridges <- ggplot(hefs_dtrmnstc_wstats, aes(fcast_t_pdt, mefp_yr, height = cfs, group=mefp_yr, fill = taf_fcper))+
   # p_ridges <- ggplot(hefs_dtrmnstc_wstats, aes(fcast_t_pdt, mefp_yr, height = cfs, group=arrange(desc(taf_fcper)), fill = taf_fcper))+
    geom_ridgeline( stat = "identity", show.legend = T, scale = ridge_scale, alpha = 0.8,
                    min_height = minhtaf) + 
    facet_grid(~facet_ridge_taf) +
    scale_fill_viridis(name = "cum\ntaf") + theme_gray() +
    theme(legend.key.width = unit(0.4, "in"), legend.key.height = unit(1, "in")) +
    scale_x_datetime(expand = c(0.01, 0.01)) +
    theme(legend.position="left") + labs(y = NULL, x = NULL) +
    theme(axis.text.y = element_text(size = 8)) +
    theme(strip.text.x = element_text(size = 8))  +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank()) 
   
  
  #p_ridges

  ######## 4-day text at right  ########
  
  p_ridges_t <- ggplot(hefs_dtrmnstc_wstats, aes(fcast_t_pdt, mefp_yr, height = cfs, group=as.factor(mefp_yr), fill = taf_fcper))+
    #p_ridges_t <- ggplot(hefs_dtrmnstc_wstats, aes(fcast_t_pdt, mefp_yr, height = cfs, group=arrange(desc(taf_fcper)), fill = taf_fcper))+
    geom_ridgeline( stat = "identity", show.legend = F, scale = ridge_scale, fill = NA, col = NA,
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

  p_cumvol <- plot_grid(p_ridges, p_ridges_t, ncol = 2, rel_widths = c(7,1))
  #p_cumvol

  #p_mid_left
  
  
  #####################################
  ######## cum vol pdf ################
  #####################################
  
  
  hefs_dtrmnstc_wstats_summary <- hefs_dtrmnstc %>% group_by(mefp_yr) %>% mutate(taf = cfs*1.98347/24/1000) %>% summarize(taf_fcper = sum(taf)) %>% 
    mutate(y="y", facet_vol = "Cumulative Volume (Distribution of 60 totals)")
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
    facet_grid(~facet_vol) +labs(x = NULL) +theme(legend.position = "bottom")
  
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
      facet_grid(~facet_vol) + theme(legend.position = "bottom")
   # p_cumvol_grad
    #p3stages <- plot_grid(p_ft, p_cumvol_grad, ncol = 1, rel_heights = c(2,1))
    #p3stages
    #ggsave("eventstagemaxft_2.pdf", dpi = 600, width = 17, height = 11, units = "in") 
    
    p_2cumvoldists <- plot_grid(p_cumvol_grad, p_cumvol_ygbdist, ncol = 1, rel_heights = c(1,1))
    #p_2cumvoldists

    
    ######### all together ########
    pcumvolall <- plot_grid(p_cumvol, p_2cumvoldists, ncol = 1, rel_heights = c(1.75,1))
    pcumvolall
    ggsave("cumvol_all.pdf", dpi = 600, width = 17, height = 11, units = "in") 
}

