# Author: Amrik Singh
# Project Title: Analysis of bird strikes reported in USA from 1990 to 2019
# (FAA Wildlife Strike Database, from 1990 to 2019)

source("global.R")

server <- function(input, output, session) {
  
  
  ######@@@@@@@@@@@@@@@@@@@@@ Tab 1: Bird Strike 
  ######@@@@@@@@@@@@@@@@@@@@@
  
    # Dynamically resize images using session data with render image
  output$pic1 <- renderImage({
    # height <- ifelse(session$clientData$output_pic1_height > 400, 400,
    #                  session$clientData$output_pic1_height)
    width <- ifelse(session$clientData$output_pic1_width > 900, 900,
                    session$clientData$output_pic1_width)
    list(src = "www/bsgif.gif", width = width, height = "auto")
  }, deleteFile = F)
  
  

  ######@@@@@@@@@@@@@@@@@@@@@ Tab 2: Trend and Frequency
  ######@@@@@@@@@@@@@@@@@@@@@
  output$bsPerMonth <- renderPlotly({
    bsMonth(incidentMonths, input$year[1], input$year[2])
  })
  
  output$trend <- renderPlotly({
    trend1(incidentTrend, input$year[1], input$year[2])
  })
  
  output$trendNorm <- renderPlotly({
    trendNorm1(incidentTrendNorm, input$year[1], input$year[2])
  })
  
  output$inj <- renderPlotly({
    trend2(incidentTrend, input$year[1], input$year[2])
  })
  
  output$injNorm <- renderPlotly({
    trendNorm2(incidentTrendNorm, input$year[1], input$year[2])
  })
  
  output$tod <- renderPlotly({
    todPie(time_of_day, input$year[1], input$year[2])
  })
  
  
  ######@@@@@@@@@@@@@@@@@@@@@ Tab 4: Bird Strike 
  ######@@@@@@@@@@@@@@@@@@@@@
  
  # Dynamically resize images using session data with render image
  output$pic1Tab4 <- renderImage({
    width <- ifelse(session$clientData$output_pic1Tab4_width > 300, 300,
                    session$clientData$output_pic1Tab4_width)
    list(src = "www/flying.gif", width = width, height = "auto")
  }, deleteFile = F)
  output$pic2Tab4 <- renderImage({
    width <- ifelse(session$clientData$output_pic1Tab4_width > 100, 100,
                    session$clientData$output_pic1Tab4_width)
    list(src = "www/flying.gif", width = width, height = "auto")
  }, deleteFile = F)


  output$bsCloud <- renderWordcloud2({
    cloud2(bs, input$num[1])
  })
  
  output$tabSpecies <- renderDataTable({
    bsTable(bs)}, options = list(lengthMenu = c(10, 20, 30, 40, 50), pageLength = 20, scrollX = TRUE))
  
  output$bSize <- renderPlotly({
    bsize(birdMass, input$size, input$year[1], input$year[2])
  })
  
  output$civilDam <- renderPlotly({
    birdCivil(birdCivilDmg, input$year[1], input$year[2], input$bSize, input$cDam, input$aSize)
  })
  
  output$mililDam <- renderPlotly({
    birdCivil(birdMilDmg, input$year[1], input$year[2], input$bSize, input$cDam, input$aSize)
  })
  
  
  
  ######@@@@@@@@@@@@@@@@@@@@@ Tab 6: States 
  ######@@@@@@@@@@@@@@@@@@@@@
  
  
  output$leaf <- renderLeaflet({
    mapStates(dfStates, sCode, input$year[1], input$year[2])
  })
  
  output$stateTab <- renderDataTable({
    tabStates(dfStates, sCode, input$year[1], input$year[2])
  }, options = list(lengthMenu = c(10, 20, 30, 40, 50), pageLength = 20, scrollX = TRUE)
)
  
  
  }

# Run the app ----
# shinyApp(ui = ui, server = server)


