library(shiny)
library(shinythemes)

# Application layout and important heading
# navbar page >> tabs >> tab set panel >> tabs
mainTitle <- "Analysis of bird strikes reported in USA from Jan-1990 to Apr-2020"
tab1 <- "Introduction"
tab1Tspt1 <- "Problem Description"
tab1Tspt2 <- "Impact on economy and human life"
tab1Tspt3 <- "About the data"

# Define UI ----
ui <- navbarPage("", id = "Home", collapsible = TRUE, fluid = TRUE, theme = shinytheme("cerulean"),
                 # shinythemes::themeSelector(),
                 windowTitle = mainTitle,
                 selected = '#tab1',
                 header = h1(mainTitle, align = "center",
                             style = "color:Black; font-size:24px; padding-left:10px; padding-right:10px"),
                 
                 tabPanel(tab1, value = '#tab1', icon = icon("book-reader", lib = "font-awesome"),
                          fluidPage(
                            tabsetPanel(id = 'tab1Tsp', selected = '#tab1Tspt1',
                              tabPanel(tab1Tspt1, value = '#tab1Tspt1',
                                       h2(tab1Tspt1, align = "center",
                                          style = "font-size:18px; padding-left:10px; padding-right:10px"),
                                       
                                       div(plotOutput("plot3"), align = "center"),
                                       introPara1(),
                                       introPara2(),
                                       fluidPage(picture()),
                                       fluidPage(picture()),
                                       picture()
                                       
                                       ),
                              tabPanel(tab1Tspt2, value = '#tab1Tspt2'
                                       ),
                              tabPanel(tab1Tspt3, value = '#tab1Tspt3',)
                            )
                          )
                 ), # end of Background tab
                 
                 tabPanel("Bird Strike", icon = icon("dove", lib = "font-awesome"),
                          fluidPage(
                            tabsetPanel(
                              tabPanel("Frequency of bird strike per month",
                                       
                                       # Sidebar layout with input and output definitions ----
                                       sidebarLayout(position = "right", fluid = "TRUE",
                                         
                                         # Sidebar panel for inputs ----
                                         sidebarPanel(
                                           
                                           # Input: Slider for the number of bins ----
                                           sliderInput(inputId = "bins",
                                                       label = "Number of bins:",
                                                       min = 1,
                                                       max = 50,
                                                       value = 30)
                                           
                                         ),
                                         
                                         # Main panel for displaying outputs ----
                                         mainPanel(
                                           
                                           a(href="#tab1", icon("paper-plane"), tab1),
                                           tags$a(href="#tab1Tspt1", icon("paper-plane"), "tab1Tspt1"),
                                           actionLink('a1', tab1Tspt1, onclick ="location.href='#tab1Tspt1';"
                                                      ),
                                           
                                           a(href="#tab1Tsp", icon("paper-plane"), "tab1Tsp"),
                                           tags$a(href="#tab1Tspt1", icon("paper-plane"), tab1Tspt1),
                                           
                                           
                                           
                                           # Output: Histogram ----
                                           plotOutput(outputId = "distPlot1")
                                           
                                         )
                                       ),
                                       
                                       # Sidebar layout with input and output definitions ----
                                       sidebarLayout(position = "right", fluid = "TRUE",
                                         
                                         # Sidebar panel for inputs ----
                                         sidebarPanel(
                                           
                                           # Input: Slider for the number of bins ----
                                           sliderInput(inputId = "bins",
                                                       label = "Number of bins:",
                                                       min = 1,
                                                       max = 50,
                                                       value = 30)
                                           
                                         ),
                                         
                                         # Main panel for displaying outputs ----
                                         mainPanel(
                                          
                                           # Output: Histogram ----
                                           plotOutput(outputId = "distPlot2")
                                           
                                         )
                                       ),
                                       a(href="#Home", icon("paper-plane"), "Back to top"),
                                       backToTop(),
                                       
                                       ), # Tabpanel end
                              tabPanel("Trend line", h2("Trend line"), value = '#tab2'),
                              tabPanel("Injuries/accidents"),
                              tabPanel("Frequency of bird strike during the day")
                            )
                         )
                 ), # end of Bird Strike tab
                 
                 tabPanel("Aircraft", icon = icon("plane", lib = "font-awesome"),
                          fluidPage(
                            tabsetPanel(
                              tabPanel("Points of impact",
                                       uiOutput("video")
                              ),
                              tabPanel("Damage to Aircraft"),
                              tabPanel("Type/size of Aircraft"),
                              tabPanel("Reporting a bird strike")
                            )
                          )
                 ), # end of Aircraft tab
                 
                 tabPanel("Bird Species", icon = icon("crow", lib = "font-awesome"),
                          fluidPage(
                            tabsetPanel(
                              tabPanel("Most common bird species involved in",
                                       # plotOutput("plot3")
                                       ),
                              tabPanel("Size of birds"),
                              tabPanel("Relationship between bird size, aircraft size, and amount of damage")
                            )
                          )
                 ), # end of Bird Species tab
                 
                 tabPanel("Effects on Flights/Airports", icon = icon("plane-departure", lib = "font-awesome"),
                          fluidPage(
                            tabsetPanel(
                              tabPanel("Effects on Flight Operations"),
                              tabPanel("Phase of Flight vs Frequency of Bird Strike"),
                              tabPanel("Number of Incidents vs Airports")
                            )
                          )
                 ), # end of Airports tab
                 
                 tabPanel("States/Locations", icon = icon("globe-americas", lib = "font-awesome"),
                          fluidPage(
                            tabsetPanel(
                              tabPanel("Map of Number of bird strikes vs States/Location",
                                       
                                       div(HTML('<iframe width="560" height="315" 
                                       src="https://www.youtube.com/embed/tkMJRVuOQXM" 
                                                   frameborder="0" allowfullscreen></iframe>')
                                           , align = "center")
                                       
                                       
                              ),
                              tabPanel("Table of Number of bird strikes vs States/Location",
                              )
                            )
                          )
                 ) # end of States/Locations tab
                 
) # end of main navbarPage