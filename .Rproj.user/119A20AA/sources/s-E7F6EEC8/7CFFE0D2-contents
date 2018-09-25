#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(viridis)
library(leaflet)
library(ggplot2)
library(tidyverse)
library(htmltools)

load("expdat.rda")
load("mark.rda")

attr(expdat$time, "tzone") <- "GMT"

expdat$polar_bsp_perc[is.infinite(expdat$polar_bsp_perc)] <- NA

normbear <- function(x){
  #converts back to a normal bearing
  x %% 360
}

fromNorth <- function(bearing){
  bearing <- normbear(bearing)  #first convert to a normal bearing
  
  fromNorth <- ifelse(bearing > 180, bearing-360, bearing)
  fromNorth <- ifelse(fromNorth < -180, 360+fromNorth, fromNorth)
  return(fromNorth)
}

expdat$twd <- fromNorth(expdat$twd)

#expdat <- expdat[expdat$time > as.POSIXct("2018-09-22 15:00:00"), ]

#========




#plot variables
plotchoices <- c("bsp", "awa", "aws", "twa", "tws", "twd", "cog", "sog", "polar_bsp_target", "polar_bsp_perc", 
                 "btm", "opt_twa", "opt_bear", "degoffmark", "opt_bsp", "opt_vmc", "act_vmc", "d_hdg_mark")

minTime <- min(expdat$time)
maxTime <- max(expdat$time)


ui <- fluidPage(
  fluidRow(
   column(6, 
          titlePanel("2018 GPYC Blue Nose"), 
          sliderInput("endtime", "Visible Time",
                      min=minTime,
                      max=maxTime, 
                      value=c(minTime, maxTime), 
                      timeFormat = "%a %H:%M", animate = TRUE, width = "100%"), 
          leafletOutput("mymap", height=500)
   ),
   column(6, 
          fluidRow(
            column(4, 
                   selectInput(inputId = "plotvar1", "Plot 1 Variable", choices = plotchoices, selected = "bsp")
            ),
            column(4, 
                   selectInput(inputId = "plotvar2", "Plot 2 Variable", choices = plotchoices, selected = "tws")
            ),
            column(4, 
                   selectInput(inputId = "colorvar", "Color Variable", choices = plotchoices, selected = "bsp")
            )            
          ), 
          fluidRow(
            column(12, 
                   plotOutput("plot1", width="auto",height=200, brush = "plot_brush"))
          ), 
          fluidRow(
            column(12, 
                   plotOutput("plot2", width="auto",height=200))
          ),
          fluidRow(
            column(12, 
                   plotOutput("xvy", width="auto",height=200))
          ),          
          fluidRow(
             column(6,
                    plotOutput("hist1", width="auto",height=150)),
             column(6,
                    plotOutput("hist2", width="auto",height=150))
          )
   )
  )
  

)

server <- function(input, output, session) {
  perfData <- reactive({
    
    expdat <- expdat[expdat$time > input$endtime[1] & expdat$time < input$endtime[2], ]
    
  })
  
  
  output$mymap <- renderLeaflet({
    
    markIcon <- makeIcon(iconUrl = "triangle.svg", 
                         iconWidth = 10, 
                         iconHeight = 10, 
                         iconAnchorX = 5, 
                         iconAnchorY = 5)
    
    m <- leaflet() %>%  addTiles()
    #m <- m %>% addPolylines(data=df, lng=~lon, lat=~lat)
    #m <- m %>% addPolylines(data=tuna, lng=~lon, lat=~lat, color="red")
    
    expdat <- perfData()
    
    # pal <- colorNumeric(
    #   palette = "Blues",
    #   domain = quantile(expdat$bsp), c(.01,99))
    # 
    m <- m %>% addPolylines(data=expdat, lng=~lon, lat=~lat, 
                             weight=2, opacity=100)
    m <- m %>% addMarkers(data=marks, lng=~mk.lon, lat=~mk.lat, icon = markIcon)
    
    
  })
  
  output$plot1 <- renderPlot({
    
    expdat <- perfData()
    
    colorrange <- quantile(expdat[, input$colorvar], c(.01, .99), na.rm=TRUE)
    plotrange <- quantile(expdat[, input$plotvar1], c(.01, .99), na.rm=TRUE)
    
    ggplot(expdat, aes_string(x="time", y=input$plotvar1, color=input$colorvar)) + geom_path()+
      scale_color_viridis(limits=colorrange) + 
      scale_y_continuous(limits=plotrange) + 
      geom_smooth(method="loess", span=0.1)
    
  })
  
  output$plot2 <- renderPlot({
    
    expdat <- perfData()
    
    colorrange <- quantile(expdat[, input$colorvar], c(.01, .99), na.rm=TRUE)
    plotrange <- quantile(expdat[, input$plotvar2], c(.01, .99), na.rm=TRUE)
    
    ggplot(expdat, aes_string(x="time", y=input$plotvar2, color=input$colorvar)) + geom_path()+
      scale_color_viridis(limits=colorrange) + 
      scale_y_continuous(limits=plotrange)+
      geom_smooth(method="loess", span=0.1)
    
  })
  
  output$hist1 <- renderPlot({
    
    expdat <- perfData()
    
    plotrange <- quantile(expdat[, input$plotvar1], c(.01, .99), na.rm=TRUE)
    
    ggplot(expdat) + 
      geom_histogram(aes_string(x=input$plotvar1), bins = 50) + scale_x_continuous(limits=plotrange)
  })
  
  output$hist2 <- renderPlot({
    
    expdat <- perfData()
    
    plotrange <- quantile(expdat[, input$plotvar2], c(.01, .99), na.rm=TRUE)
    
    ggplot(expdat) + 
      geom_histogram(aes_string(x=input$plotvar2), bins = 50) + scale_x_continuous(limits=plotrange)
  })
  
  output$xvy <- renderPlot({
    
    expdat <- perfData()
    
    plotrangex <- quantile(expdat[, input$plotvar1], c(.01, .99), na.rm=TRUE)
    plotrangey <- quantile(expdat[, input$plotvar2], c(.01, .99), na.rm=TRUE)
    colorrange <- quantile(expdat[, input$colorvar], c(.01, .99), na.rm=TRUE)
    
    ggplot(expdat)+
      geom_point(aes_string(x=input$plotvar1, y=input$plotvar2, color=input$colorvar))+
      scale_color_viridis(limits=colorrange) 
    
  })
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
