library(shiny)
library(shinythemes)

# Define UI ----
ui <- navbarPage('', collapsible = TRUE,
                 fluid = TRUE, theme = shinytheme("united"), selected = "Bird Strike",
                 header = h4("Analysis and visualisation of bird strikes in USA from Jan-1990 to Apr-2020",
                             style = "color: Black;", align = "center"),
                 
                 tabPanel("Background",icon = icon("history", lib = "font-awesome"),
                          fluidPage(
                            tabsetPanel(
                              tabPanel("Problem Description"),
                              tabPanel("Impact on economy and human life"),
                              tabPanel("About the data")
                            )
                          )
                 ), # end of Background tab
                 tabPanel("Bird Strike", icon = icon("dove", lib = "font-awesome"),
                          fluidPage(
                            tabsetPanel(
                              tabPanel("Frequency of bird strike per month"),
                              tabPanel("Trend line"),
                              tabPanel("Injuries/accidents"),
                              tabPanel("Frequency of bird strike during the day")
                            )
                          )
                 ), # end of Bird Strike tab
                 
                 tabPanel("Aircraft", icon = icon("plane", lib = "font-awesome"),
                          fluidPage(
                            tabsetPanel(
                              tabPanel("Points of impact"),
                              tabPanel("Damage to Aircraft"),
                              tabPanel("Type/size of Aircraft"),
                              tabPanel("Reporting a bird strike")
                            )
                          )
                 ), # end of Aircraft tab
                 
                 tabPanel("Bird Species", icon = icon("crow", lib = "font-awesome"),
                          fluidPage(
                            tabsetPanel(
                              tabPanel("Most common bird species involved in"),
                              tabPanel("Size of birds"),
                              tabPanel("Relationship amoung bird size, aircraft size, and amount of damage")
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
                              tabPanel("Map of Number of bird strikes vs States/Location"),
                              tabPanel("Table of Number of bird strikes vs States/Location")
                            )
                          )
                 ) # end of States/Locations tab
                 
) # end of main navbarPage