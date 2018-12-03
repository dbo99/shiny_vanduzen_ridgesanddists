Sys.setenv(TZ="America/Los_Angeles")
source("libs.r")
source("fun_defs.r")
source("vanduzenridges_df_create.r")



fcasts_firstam <- rev(as.character(fcasts_firstam))
fcast_end_date_rev <-  rev(seq(date(min(hefs_dtrmnstc$fcast_t_pdt)), date(max(hefs_dtrmnstc$fcast_t_pdt)), by = '1 day'))
#fcasts_firstam_d <- ymd(fcasts_firstam)
## Only run examples in interactive R sessions
#if (interactive()) {
  
  ui <- fluidPage(   
       column( width = 1, offset = 0, style='padding:0px;',
        selectInput("analysis_type", "Perspective",                 
               choices = c("flow" ,   "stage",  "volume"), selected = "volume")           
                          ), 
       column( width = 2,offset = 0, style='padding:0px;',
    
       selectInput("am_fcast", "CNRFC am forecast:",
               fcasts_firstam, fcasts_firstam)),
      column( width = 1,offset = 0,style='padding:0px;',
        selectInput("fcast_start_date", "start date",
                    date(fcasts_firstam), date(fcasts_firstam))),
      column( width = 1,offset = 0, style='padding:0px;',
             selectInput("fcast_end_date", "end date",
                         fcast_end_date_rev , selected = fcast_end_date_rev[length(fcast_end_date_rev)-5] )),
    
    
    column( width = 2,offset = 0, style='padding:0px;',
    sliderInput("flo_ridge_scale", "ridge scaling, flow:", 0.00001, 0.001, .0001, .0001)),
    column( width = 2,offset = 0, style='padding:0px;',
           sliderInput("sta_ridge_scale", "ridge scaling, stage:", 0.0001, 1, .1, .001)),
    column( width = 2,offset = 0, style='padding:0px;',
           sliderInput("vol_ridge_scale", "ridge scaling, volume:", 0.00001, 0.001, .0001, .0001)),
            
    plotOutput("distPlot", height="700px")
  )
  
  
  
  
  server <- function(input, output) {
    
   
    
    
    output$distPlot <- renderPlot({
    
      if (input$analysis_type == "flow")   {
        
        
        am_fcast <- input$am_fcast
        fcast_start_date <- ymd(input$fcast_start_date)
        fcast_end_date <- ymd(input$fcast_end_date)
        flo_ridge_scale <- input$flo_ridge_scale 
        p2 <- plotridgesandpdfs_flow(hefs_dtrmnstc, am_fcast, fcast_start_date, fcast_end_date, flo_ridge_scale)
        print(p2)} 
      if (input$analysis_type == "stage")  {
        am_fcast <- input$am_fcast
        fcast_start_date <- ymd(input$fcast_start_date)
        fcast_end_date <- ymd(input$fcast_end_date)
        sta_ridge_scale <- input$sta_ridge_scale 
        p3 <- plotridgesandpdfs_stage(hefs_dtrmnstc, am_fcast, fcast_start_date, fcast_end_date, sta_ridge_scale)
        print(p3)} 
      
      if (input$analysis_type == "volume") {
        
        am_fcast <- input$am_fcast
        fcast_start_date <- ymd(input$fcast_start_date)
        fcast_end_date <- ymd(input$fcast_end_date)
        vol_ridge_scale <- input$vol_ridge_scale
        p1 <- plotridgesandpdfs_cumvol(hefs_dtrmnstc, am_fcast, fcast_start_date, fcast_end_date, vol_ridge_scale)
        
        print(p1)}  
      
    })
  }    
  
  shinyApp(ui, server)
#}