



source("global.R")
# Define server logic ----
server <- function(input, output, session) {
  
    # Dynamically resize images using session data with render image
  output$pic1 <- renderImage({
    # height <- ifelse(session$clientData$output_pic1_height > 400, 400,
    #                  session$clientData$output_pic1_height)
    width <- ifelse(session$clientData$output_pic1_width > 900, 900,
                    session$clientData$output_pic1_width)
    list(src = "www/bsgif.gif", width = width, height = "auto")
  }, deleteFile = F)
  
  
  
  output$distPlot1 <- renderPlot({
    x    <- faithful$waiting
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    hist(x, breaks = bins, col = "#75AADB", border = "white",
         xlab = "Waiting time to next eruption (in mins)",
         main = "Histogram of waiting times")    
  })
  
  
  output$distPlot2 <- renderPlot({
    x    <- faithful$waiting
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    testPlot(x, bins)
    
  })
  


  }

# Run the app ----
# shinyApp(ui = ui, server = server)


