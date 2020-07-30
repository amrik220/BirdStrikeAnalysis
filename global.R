# Author: Amrik Singh
# Project Title: Analysis of bird strikes reported in USA from 1990 to 2019
# (FAA Wildlife Strike Database, from 1990 to 2019)

#' Code Description: global.R file
#' Loads packages, reads data, does processing, and generate plots
#' For more information, please read  README.md included in the repository

# Import required packages
library(dplyr)
library(ggplot2)
library(ggthemes)
library(plotly)

#####@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#####@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#Plot p1: Point of Impact/Location of Damage
poi <- readRDS("rds_data/poi.rds")
lod <- readRDS("rds_data/lod.rds")

poiPlot <- function(poi, lod, yearStart, yearEnd){
  
  poi <- poi %>% filter(Year >= yearStart, Year <= yearEnd)
  lod <- lod%>% filter(Year >= yearStart, Year <= yearEnd)
  
  pointOfImpact <- data.frame(colSums(poi[-1]))
  pointOfImpact$poi <- row.names(pointOfImpact)
  names(pointOfImpact) <- c('num', 'poi')
  pointOfImpact$lodNum <- colSums(lod[-1])
  pointOfImpact$damPer <- round(pointOfImpact$lodNum/pointOfImpact$num*100,2)
  
  ay <- list(overlaying = "y", side = 'right', title = 'Percentage of cases resulted in damage')
  p1 <- plot_ly(pointOfImpact, x = ~poi, y = ~num, type = 'bar',
                text = ~num, textposition = 'outside', hoverinfo = 'y',
                name = 'Point of impact', offsetgroup = 1) %>%
    add_trace(y = ~lodNum, name = 'Number of cases resulted in damage',
              text = ~lodNum, textposition = 'outside', offsetgroup = 2) %>%
    add_bars(y = ~damPer, name = 'Percentage of cases resulted in damage',
             yaxis = 'y2', text = ~paste0(damPer,'%'), textposition = 'outside',offsetgroup = 3) %>%
    layout(title = "Points of impact of bird strike and number of cases of damage", 
           yaxis2 = ay,
           xaxis = list(title = "Point of Impact/Location of Damage"),
           yaxis = list(title = "Number of bird strikes"),
           barmode = 'group', margin = list(r = 35),
           legend = list(x=.6, y =.95))
  p1
}
# poiPlot(poi, lod, 1990, 2019)






#####@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#####@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#####@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#####@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
picture <- function() {
  img(src = "bsgif.gif")
  
  # img(src = "Picture1.jpg")
}


testPlot <- function(x, bins){
  
  
  
  hist(x, breaks = bins, col = "#75AADB", border = "white",
       xlab = "Waiting time to next eruption (in mins)",
       main = "Histogram of waiting times")
  
}

#' Text for the pages
#'

introPara1 <- function(){
  p("Lorem ipsum dolor sit amet, consectetur adipiscing elit,
sed do eiusmod tempor incididunt ut labore et dolore magna
aliqua. Ut enim ad minim veniam, quis nostrud exercitation
ullamco laboris nisi ut aliquip ex ea commodo consequat.
Duis aute irure dolor in reprehenderit in voluptate velit
esse cillum dolore eu fugiat nulla pariatur. Excepteur sint
occaecat cupidatat non proident, sunt in culpa qui officia
deserunt mollit anim id est laborum.
  ")
}

introPara2 <- function(){
  h5("Lorem ipsum dolor sit amet, consectetur adipiscing elit,
sed do eiusmod tempor incididunt ut labore et dolore magna
aliqua. Ut enim ad minim veniam, quis nostrud exercitation
ullamco laboris nisi ut aliquip ex ea commodo consequat.
  ")
}


#########################################3
#########################################3

backToTop <- function(){
  actionButton("", "Back to top",
               icon = icon("paper-plane"),
               # style = "color:maroon", 
               onclick ="location.href='#Home';")
}