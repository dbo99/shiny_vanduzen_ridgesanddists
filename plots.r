library(cowplot)
library(ggridges)
library(viridis)
library(cowplot)
p1 <- plot_s_ahps(df_f_qntls_h, df_hefs, dtrmnstc, "2016-01-13", "2016-01-19", "2016-01-13", 4)
pleg <- get_legend(p1)
p1 <- p1 + theme(legend.position="none")

p2 <- plot_s_ahps(df_s_qntls_h, df_hefs, dtrmnstc, "2016-01-13", "2016-01-19", "2016-01-13", 4)
p2 <- p2 + theme(legend.position="none")
p_left <- plot_grid(p1, p2, ncol = 1)

plot_grid(p_left, pleg, ncol = 2, rel_widths = c(3,1))

plot_f_ahps_qtypefacet(df_f_qntls_h, df_hefs, dtrmnstc, "2016-01-13", "2016-01-19", "2016-01-13")
plot_s_ahps_qtypefacet(df_s_qntls_h, df_hefs, dtrmnstc, "2016-01-13", "2016-01-19", "2016-01-13")
library(plotly)
ggplotly(p)


plot_s_ahps_qtypefacet(df_f_qntls_h, df_hefs, dtrmnstc, "2016-01-13", "2016-01-19", "2016-01-13")

plot_s_per(df_hefs, dtrmnstc, "2016-01-13", "2016-01-15", fcasts[1] , 6)
#create_per_quants <- function(df, fcaststartdate, fcastenddate, yeartrace, value, qtype, fcast_i_pdt) 

plot_f_ahps(df_s_qntls_h, df_hefs, dtrmnstc, "2016-01-13", "2016-01-15", "2016-01-13", 4)
plot_s_ahps(df_s_qntls_h, df_hefs, dtrmnstc, "2016-01-13", "2016-01-15", "2016-01-13", 4)
plot_f_per(df_hefs, dtrmnstc, "2016-01-15", "2016-01-19", fcasts[7] , 6)


plot_grid(p6, p7, ncol = 1)


##########################
######### ridges  ########
##########################
{
hefs_dtrmnstc <- rbind(df_hefs, dtrmnstc) %>% drop_na()
hefs_dtrmnstc <- hefs_dtrmnstc %>% filter(fcast_i_pdt == fcasts[1])
fcast_start_date <- c("2016-01-13")
fcast_end_date <- c("2016-01-15")
hefs_dtrmnstc <- hefs_dtrmnstc %>% filter(fcast_t_pdt >= fcast_start_date, fcast_t_pdt <= fcast_end_date)
                                          
                                          
start_time <- min(hefs_dtrmnstc$fcast_t_pdt)
end_time <- max(hefs_dtrmnstc$fcast_t_pdt)
dt_midpoint <- as.POSIXct((as.numeric(end_time) + as.numeric(start_time)) / 2, origin = '1970-01-01') 


fiveday <- hefs_dtrmnstc %>% mutate(maf = cfs*1.98347/1000000) %>% group_by(mefp_yr) %>% mutate(maf_5day = sum(maf), cfs_5day = max(cfs)) 
fiveday <- fiveday %>% mutate(maf_5day_label = ifelse(fcast_t_pdt == dt_midpoint, maf_5day, NA), 
                              facet_ridge_maf = "deterministic & mefp-based scenarios, ~5-day cumulative volume [mean ~ 7.65 maf, median ~ 7.57 maf] (60 total)", facet_text_maf = "cum. maf",
                              cfs_5day_label = ifelse(fcast_t_pdt == dt_midpoint, cfs_5day, NA), 
                              facet_ridge_cfs = "deterministic & mefp-based scenarios, ~5-day hourly period average flow [mean max cfs ~ 75.9k cfs, median max cfs ~ 76.8k cfs] (60 total)", facet_text_cfs = "max. kcfs")
minhmaf   <- min(fiveday$maf_5day)
maxhmaf   <- max(fiveday$maf_5day)
minhcfs   <- min(fiveday$cfs_5day)
maxhcfs   <- max(fiveday$cfs_5day)


  p1 <- ggplot(fiveday, aes(fcast_t_pdt, mefp_yr, height = cfs, group=as.factor(mefp_yr), fill = maf_5day))+
    geom_ridgeline( stat = "identity", show.legend = T, scale = 0.0001, alpha = 0.8,
                    min_height = minhmaf) + 
    facet_grid(~facet_ridge_maf) +
    scale_fill_viridis(name = "cum\nmaf") + theme_gray() +
    theme(legend.key.width = unit(0.4, "in"), legend.key.height = unit(2, "in")) +
    scale_x_datetime(expand = c(0.01, 0.01)) +
    theme(legend.position="left") + labs(y = NULL, x = NULL) +
    theme(axis.text.y = element_text(size = 8)) +
    theme(strip.text.x = element_text(size = 8))  +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank())
  
  p1
  
  ######## 4-day text at right  ########
  
  p1t <- ggplot(fiveday, aes(fcast_t_pdt, mefp_yr, height = cfs, group=as.factor(mefp_yr), fill = maf_5day))+
    geom_ridgeline( stat = "identity", show.legend = F, scale = 0.000026, fill = NA, col = NA,
                    min_height = minhmaf) + 
    facet_grid(~facet_text_maf) +
    
    
    scale_x_datetime(expand = c(0.01, 0.01)) +
    geom_text(aes(label = round(maf_5day_label,2), color = maf_5day, hjust = 1), size = 3.5)  +
    scale_colour_gradientn(colours = viridis(10)) + theme_gray() +
    
    geom_text(aes(label = round(maf_5day_label,2), hjust = 1), 
              color = ifelse(fiveday$maf_5day==minhmaf |fiveday$maf_5day==maxhmaf , 'red', NA), size = 3.5) +
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
  
  p_mid_left <- plot_grid(p1, p1t, ncol = 2, rel_widths = c(7,1))
  p_mid_left
  
  
  
  #### 4-day peak flow ridges #######
  
  #fiveday <- hefs %>% mutate(maf = cfs*1.98347/1000000) %>% group_by(mefp_yr) %>% mutate(maf_5day = sum(maf)) 
  #minh <- min(fiveday$maf)
  fiveday <- fiveday %>% mutate(facet_ridge_cfs =
                                  "dtrmnstc & mefp-based scens, ~5-day hrly per. avg. flow [mean max cfs ~75.9k cfs, median max cfs ~76.8k cfs] (60 tot.)")
  
  p2 <- ggplot(fiveday, aes(fcast_t_pdt, mefp_yr, height = cfs, group=as.factor(mefp_yr), fill = cfs))+
    geom_ridgeline_gradient(scale = 0.0001) + 
    scale_fill_viridis(name = "cfs") + theme_gray() +
    theme(legend.key.width = unit(0.4, "in"), legend.key.height = unit(2, "in")) +
    scale_x_datetime(expand = c(0.01, 0.01)) +
    facet_grid(~facet_ridge_cfs) +
    theme(legend.position="left") + labs(y = NULL, x = NULL) +
    theme(axis.text.y = element_text(size = 8)) + theme(strip.text.x = element_text(size = 8))  +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank())
  p2
  
  
  ######## text at right  ########
  
  
  p2t <- ggplot(fiveday, aes(fcast_t_pdt, mefp_yr, height = cfs, group=as.factor(mefp_yr), fill = maf_5day))+
    geom_ridgeline( stat = "identity", show.legend = F, scale = 0.000026, fill = NA, col = NA,
                    min_height = minhcfs) + 
    facet_grid(~facet_text_cfs) +
    
    
    scale_x_datetime(expand = c(0.01, 0.01)) +
    geom_text(aes(label = round(cfs_5day_label/1000,1), color = cfs_5day, hjust = 1), size = 3.5)  +
    scale_colour_gradientn(colours = viridis(10)) + theme_gray() +
    
    geom_text(aes(label = round(cfs_5day_label/1000,1), hjust = 1), 
              color = ifelse(fiveday$cfs_5day==minhcfs |fiveday$cfs_5day==maxhcfs , 'red', NA), size = 3.5) +
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
    
    labs(y = NULL, x = NULL) + theme(strip.text.x = element_text(size = 8))  +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank())
  #theme(axis.text.y = element_text(color = 'white', size = 8), axis.ticks = element_line(color = "white")) 
  #theme(strip.text.x = element_text(size = 8)) +  theme(axis.text.y=element_blank(),axis.ticks.y=element_blank())
  
  p2t
  
  
  p_mid_right <- plot_grid(p2, p2t, ncol = 2, rel_widths = c(7,1))
  p_mid_right
  
  p_mid <- plot_grid(p_mid_left, p_mid_right, ncol = 2)
  p_mid
  ggsave("5daycumvol&peakflow_ridges.pdf", dpi = 600, width = 17, height = 11, units = "in") 
}
