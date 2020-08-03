# Author: Amrik Singh
# Project Title: Analysis of bird strikes reported in USA from 1990 to 2019
# (FAA Wildlife Strike Database, from 1990 to 2019)

library(shiny)
library(shinythemes)

# Application layout and important heading
# navbar page >> tabs >> tab set panel >> tabs
mainTitle <- "Analysis of bird strikes reported in USA from 1990 to 2019"
bsl = c('Small', 'Medium', 'Large') # list of bird sizes
cDam <- c("Minor", "Substantial", "Destroyed", "Uncertain") # damage to civil aircrafts
mDam <- c('A', 'B', 'C', 'D', 'E', 'M?') # Military damage classification
efl <- c("Aborted Take-off", "Engine Shut Down", "Other",
         "Precautionary Landing") # effect on flight list


# Define UI ----
ui <- navbarPage("", id = "Home", collapsible = TRUE, fluid = TRUE, theme = shinytheme("cerulean"),
                 # shinythemes::themeSelector(),
                 windowTitle = mainTitle,
                 selected = '#tab6',
                 header = h1(mainTitle, align = "center",
                             style = "color:Black; font-size:24px; padding-left:10px; padding-right:10px"),
                 
                 tabPanel("Bird Strike", value = '#tab1', icon = icon("dove", lib = "font-awesome"),
                          fluidPage(
                            tabsetPanel(id = 'tab1Tsp', selected = '#tab1Tspt1',
                              tabPanel("Problem Description", value = '#tab1Tspt1',
                                       h2("Problem Description", align = "center",
                                          style = "font-size:18px; padding-left:10px; padding-right:10px"),
                                       
                                       div(imageOutput("pic1"), align = 'center'),
                                       introPara1(),
                                       introPara2(),
                                       ),
                              tabPanel("Impact on economy and human life", value = '#tab1Tspt2',
                                       introPara1(),
                                       
                                       div(HTML('<iframe width="560" height="315" 
                                       src="https://www.youtube.com/embed/tkMJRVuOQXM" 
                                                   frameborder="0" allowfullscreen></iframe>')
                                           , align = "center")
                                       ),
                              tabPanel("About the data", value = '#tab1Tspt3',
                                       introPara1())
                              
                            )
                          )
                 ), # end of Background tab
                 
                 tabPanel("Trend and Frequency", value = '#tab2', icon = icon("poll", lib = "font-awesome"),
                          fluidPage(
                            tabsetPanel(id = 'tab2Tsp', selected = '#tab2Tspt1',
                              tabPanel("Seasonal trend", value = '#tab2Tspt1',
                                       h2("Yearly trend (monthly/seasonal frequencies)", align = "center",
                                          style = "font-size:18px; padding-left:10px; padding-right:10px;
                                          padding-bottom:2px"),
                                       
                                       # Sidebar layout with input and output definitions ----
                                       sidebarLayout(position = "right", fluid = "TRUE",
                                         # Sidebar panel for inputs ----
                                         sidebarPanel(
                                           # Input: Slider for the number of bins ----
                                           sliderInput(inputId = "year",
                                                       label = "Select range in years:",
                                                       min = 1990, max = 2019,
                                                       value = c(1990, 2019)),
                                           introPara2()
                                           
                                         ),
                                         
                                         # Main panel for displaying outputs ----
                                         mainPanel(
                                           plotlyOutput(outputId = "bsPerMonth")
                                         )
                                       ),
                                       
                                      fluidPage(
                                        introPara1()
                                        ),
                                      ), # Tabpanel end
                              
                              tabPanel("Historical trend", value = '#tab2Tspt2',
                                       h2("Historical trend", align = "center",
                                          style = "font-size:18px; padding-left:10px; padding-right:10px;
                                          padding-bottom:2px"),
                                       
                                       sidebarLayout(position = "right", fluid = "TRUE",
                                                     sidebarPanel(
                                                       sliderInput(inputId = "year",
                                                                   label = "Select range in years:",
                                                                   min = 1990, max = 2019,
                                                                   value = c(1990, 2019)),
                                                       introPara2()
                                                     ),
                                                     mainPanel(
                                                       plotlyOutput(outputId = "trend")
                                                     )
                                       ),
                                       sidebarLayout(position = "right", fluid = "TRUE",
                                                     sidebarPanel(
                                                       sliderInput(inputId = "year",
                                                                   label = "Select range in years:",
                                                                   min = 1990, max = 2019,
                                                                   value = c(1990, 2019)),
                                                       introPara2()
                                                     ),
                                                     mainPanel(
                                                       plotlyOutput(outputId = "trendNorm")
                                                     )
                                                     ),
                                       div(a(href="#Home", icon("paper-plane"), "Back to top"), align = "center")
                                       ),
                              
                              tabPanel("Injuries/accidents", value = '#tab2Tspt3',
                                       h2("Injuries/accidents", align = "center",
                                          style = "font-size:18px; padding-left:10px; padding-right:10px;
                                          padding-bottom:2px"),
                                       
                                       sidebarLayout(position = "right", fluid = "TRUE",
                                                     sidebarPanel(
                                                       sliderInput(inputId = "year",
                                                                   label = "Select range in years:",
                                                                   min = 1990, max = 2019,
                                                                   value = c(1990, 2019)),
                                                       introPara2()
                                                     ),
                                                     mainPanel(
                                                       plotlyOutput(outputId = "inj")
                                                     )
                                       ),
                                       sidebarLayout(position = "right", fluid = "TRUE",
                                                     sidebarPanel(
                                                       sliderInput(inputId = "year",
                                                                   label = "Select range in years:",
                                                                   min = 1990, max = 2019,
                                                                   value = c(1990, 2019)),
                                                       introPara2()
                                                     ),
                                                     mainPanel(
                                                       plotlyOutput(outputId = "injNorm")
                                                     )
                                                     ),
                                       div(a(href="#Home", icon("paper-plane"), "Back to top"), align = "center")
                                       ),
                              
                              tabPanel("Frequency of bird strike during the day",
                                       
                                       sidebarLayout(position = "right", fluid = "TRUE",
                                                     sidebarPanel(
                                                       sliderInput(inputId = "year",
                                                                   label = "Select range in years:",
                                                                   min = 1990, max = 2019,
                                                                   value = c(1990, 2019)),
                                                       introPara2()
                                                     ),
                                                     mainPanel(
                                                       plotlyOutput(outputId = "tod")
                                                     )
                                       )
                                       )
                            )
                         )
                 ), # end of Bird Strike tab
                 
                 
                 
                 tabPanel("Aircraft", value = '#tab3', icon = icon("plane", lib = "font-awesome"),
                          fluidPage(
                            tabsetPanel(id = 'tab3Tsp', selected = '#tab3Tspt1',
                              tabPanel("Points of impact",  value = '#tab3Tspt1',
                                       h2("Points of impact", align = "center",
                                          style = "font-size:18px; padding-left:10px; padding-right:10px;
                                          padding-bottom:2px"),
                              ),
                              tabPanel("Damage to Aircraft"),
                              tabPanel("Type/size of Aircraft"),
                              tabPanel("Reporting a bird strike")
                            )
                          )
                 ), # end of Aircraft tab
                 
                 
                 
                 tabPanel("Bird Species", value = '#tab4', icon = icon("crow", lib = "font-awesome"),
                          fluidPage(
                            tabsetPanel(id = 'tab4Tsp', selected = '#tab4Tspt1',
                              tabPanel("Most common bird species involved in bird strikes",  value = '#tab4Tspt1',
                                       h2("Most common bird species involved", align = "center",
                                          style = "font-size:18px; padding-left:10px; padding-right:10px;
                                          padding-bottom:2px"),
                                       
                                       sidebarLayout(position = "right", fluid = "TRUE",
                                                     sidebarPanel(
                                                       div(imageOutput("pic1Tab4", inline = TRUE), align = 'center'),
                                                       sliderInput(inputId = "num",
                                                                   label = "Select number:",
                                                                   min = 20, max = 500,
                                                                   value = 80),
                                                       introPara2()
                                                     ),
                                                     mainPanel(
                                                       div(wordcloud2Output("bsCloud"),
                                                       style = "padding-top:10px; padding-bottom:50px"),
                                                        
                                                       dataTableOutput('tabSpecies')
                                                     )
                                       ),
                                       div(a(href="#Home", icon("paper-plane"), "Back to top"), align = "center")
                                       ),
                              
                              
                              tabPanel("Size of birds",  value = '#tab4Tspt2',
                                       h2("Size of birds", align = "center",
                                          style = "font-size:18px; padding-left:10px; padding-right:10px;
                                          padding-bottom:2px"),
                                       
                                       sidebarLayout(position = "right", fluid = "TRUE",
                                                     sidebarPanel(
                                                       div(imageOutput("pic2Tab4", inline = TRUE), align = 'center'),
                                                       
                                                       checkboxGroupInput('size', 'Select size of the bird:',
                                                                          bsl, selected = bsl, inline = TRUE),
                                                       sliderInput(inputId = "year",
                                                                   label = "Select range in years:",
                                                                   min = 1990, max = 2019,
                                                                   value = c(1990, 2019)),
                                                       introPara2()
                                                     ),
                                                     mainPanel(plotlyOutput(outputId = "bSize"))
                                       
                                       
                                       )),
                              tabPanel("Relationship between bird size, aircraft size, and amount of damage",
                                       value = '#tab4Tspt3',
                                       h2("Relationship between bird size, aircraft size, and amount of damage", align = "center",
                                          style = "font-size:18px; padding-left:10px; padding-right:10px;
                                          padding-bottom:2px"),
                                       
                                       div(sidebarLayout(position = "right", fluid = "TRUE",
                                                     sidebarPanel(
                                                       
                                                       checkboxGroupInput('bSize', 'Select size of the bird:',
                                                                          bsl, selected = bsl, inline = TRUE),
                                                       checkboxGroupInput('cDam', 'Select size of the aircraft:',
                                                                          cDam, selected = cDam, inline = TRUE),
                                                       checkboxGroupInput('aSize', 'Select category of damage:',
                                                                          c(1:5), selected = c(1:5), inline = TRUE),
                                                       sliderInput(inputId = "year",
                                                                   label = "Select range in years:",
                                                                   min = 1990, max = 2019,
                                                                   value = c(1990, 2019)),
                                                       introPara2()
                                                       ),
                                                     mainPanel(
                                                       plotlyOutput(outputId = "civilDam")
                                                       )), style = "padding-top:10px; padding-bottom:40px"),
                                       sidebarLayout(position = "right", fluid = "TRUE",
                                                     sidebarPanel(
                                                       
                                                       checkboxGroupInput('bSize', 'Select size of the bird:',
                                                                          bsl, selected = bsl, inline = TRUE),
                                                       checkboxGroupInput('mDam', 'Select category of damage:',
                                                                          mDam, selected = mDam, inline = TRUE),
                                                       checkboxGroupInput('aSize', 'Select category of damage:',
                                                                          c(1:5), selected = c(1:5), inline = TRUE),
                                                       sliderInput(inputId = "year",
                                                                   label = "Select range in years:",
                                                                   min = 1990, max = 2019,
                                                                   value = c(1990, 2019)),
                                                       introPara2()),
                                                     mainPanel(
                                                       plotlyOutput(outputId = "mililDam")
                                                       )
                                                     ),
                                       div(a(href="#Home", icon("paper-plane"), "Back to top"), align = "center")
                                       ) ######
                                      
                                       
                              
                            )
                          )
                 ), # end of Bird Species tab
                 
                 
                 
                 tabPanel("Effects on Flights/Airports", value = '#tab5', icon = icon("plane-departure", lib = "font-awesome"),
                          fluidPage(
                            tabsetPanel(id = 'tab5Tsp', selected = '#tab5Tspt1',
                              tabPanel("Effects on Flight Operations", value = '#tab5Tspt1',
                                       h2("Effects on Flight Operations", align = "center",
                                          style = "font-size:18px; padding-left:10px; padding-right:10px;
                                          padding-bottom:2px")),
                              
                              tabPanel("Phase of Flight vs Frequency of Bird Strike"),
                              tabPanel("Number of Incidents vs Airports")
                            )
                          )
                 ), # end of Airports tab
                 
                 tabPanel("States/Locations", value = '#tab6', icon = icon("globe-americas", lib = "font-awesome"),
                          fluidPage(
                            tabsetPanel(id = 'tab6Tsp', selected = '#tab6Tspt1',
                              tabPanel("Map view of number of bird strikes per States", value = '#tab6Tspt1',
                                       h2("Map view of number of bird strikes per States", align = "center",
                                          style = "font-size:18px; padding-left:10px; padding-right:10px"),
                                       
                                       
                                       sidebarLayout(position = "right", fluid = "TRUE",
                                                     sidebarPanel(
                                                       sliderInput(inputId = "year",
                                                                   label = "Select range in years:",
                                                                   min = 1990, max = 2019,
                                                                   value = c(1990, 2019)),
                                                       introPara2()
                                                     ),
                                                     mainPanel(
                                                       leafletOutput('leaf')
                                                       )
                                       )
                                       
                              ),
                              tabPanel("Table view of number of bird strikes per Statesn",value = '#tab6Tspt2',
                                       h2("Table view of number of bird strikes per States", align = "center",
                                          style = "font-size:18px; padding-left:10px; padding-right:10px"),
                                       
                                       
                                       sidebarLayout(position = "right", fluid = "TRUE",
                                                     sidebarPanel(
                                                       sliderInput(inputId = "year",
                                                                   label = "Select range in years:",
                                                                   min = 1990, max = 2019,
                                                                   value = c(1990, 2019)),
                                                       introPara2()
                                                     ),
                                                     mainPanel(
                                                       dataTableOutput('stateTab')
                                                     )
                                       )
                              )
                            )
                          )
                 ) # end of States/Locations tab
                 
) # end of main navbarPage