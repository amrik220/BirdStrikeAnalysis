

source("global.R")
# Define server logic ----
server <- function(input, output, session) {
  
  output$distPlot1 <- renderPlot({
    x    <- faithful$waiting
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    testPlot(x, bins)
    
  })
  
  
  output$distPlot2 <- renderPlot({
    x    <- faithful$waiting
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    testPlot(x, bins)
    
  })
  
  
  # A dynamically-sized plot
  output$plot3 <- renderImage({
    
    # width <- session$clientData$output_plot3_width
    # height <- session$clientData$output_plot3_height
    
    width <- "auto"
    height <- session$clientData$output_plot3_height
    outfile <- tempfile()
    
    list(src = "bsgif.gif", width = width, height = height)
  }, deleteFile = F)
    
  
  output$pic1 <- renderImage({
    
    # height <- ifelse(session$clientData$output_pic1_height > 400, 400, 
           # session$clientData$output_pic1_height)
    width <- ifelse(session$clientData$output_pic1_width > 900, 900,
                    session$clientData$output_pic1_width)
    
    list(src = "bsgif.gif", width = width, height = "auto")
  }, deleteFile = F)
  

  
  }

# Run the app ----
# shinyApp(ui = ui, server = server)


