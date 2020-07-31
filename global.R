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
library(tm)
library(hunspell)
library(wordcloud2)
library(reshape2)
library(leaflet)

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
  title = paste0("Points of impact of bird strike and number of cases of damage<br>from ",
                 yearStart, " to ", yearEnd)
  ay <- list(overlaying = "y", side = 'right', title = 'Percentage of cases resulted in damage')
  p1 <- plot_ly(pointOfImpact, x = ~poi, y = ~num, type = 'bar',
                text = ~num, textposition = 'outside', hoverinfo = 'y',
                name = 'Point of impact', offsetgroup = 1) %>%
    add_trace(y = ~lodNum, name = 'Number of cases resulted in damage',
              text = ~lodNum, textposition = 'outside', offsetgroup = 2) %>%
    add_bars(y = ~damPer, name = 'Percentage of cases resulted in damage',
             yaxis = 'y2', text = ~paste0(damPer,'%'), textposition = 'outside',offsetgroup = 3) %>%
    layout(title = title, 
           yaxis2 = ay,
           xaxis = list(title = "Point of Impact/Location of Damage"),
           yaxis = list(title = "Number of bird strikes"),
           barmode = 'group', margin = list(r = 35),
           legend = list(x=.6, y =.95))
  p1
}
# poiPlot(poi, lod, 1990, 2004)

#####@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#####@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Plot p2: trend line
incidentTrend <- readRDS("rds_data/incidentTrend.rds")

trend1 <- function(incidentTrend, yearStart, yearEnd){
  incidentTrend <- incidentTrend %>% filter(Year >= yearStart, Year <= yearEnd)
  # Plot trend line
  title = paste0("Number of bird strike incidents per year<br>from ",
                 yearStart, " to ", yearEnd)
  p2 <- plot_ly(incidentTrend, x = ~Year, y = ~Number_of_Incidents, type = 'scatter',
                mode = 'lines') %>% layout(title = title,margin=list(t = 40),
                                           xaxis = list(title = "Year"),
                                           yaxis = list(title = "Number of bird strikes"))
  p2
}
# trend1(incidentTrend, 1990, 2019)

#####@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#####@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Plot p3: trend line
trend2 <- function(incidentTrend, yearStart, yearEnd){
  incidentTrend <- incidentTrend %>% filter(Year >= yearStart, Year <= yearEnd)
  title = paste0("Number of persons injured due to bird strikes per year<br>from ",
                 yearStart, " to ", yearEnd)
  p3 <- plot_ly(incidentTrend, x = ~Year, y = ~Number_of_Injuries, 
                type = 'scatter', mode = 'lines') %>%
    layout(title = title, margin=list(t = 40),
           xaxis = list(title = "Year"),
           yaxis = list(title = "Number of persons injured"))
  p3
}
# trend2(incidentTrend, 1990, 2000)


#####@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#####@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Plot p4: trend line norm
incidentTrendNorm <- readRDS("rds_data/incidentTrendNorm.rds")

trendNorm1 <- function(incidentTrendNorm, yearStart, yearEnd){
  incidentTrendNorm <- incidentTrendNorm %>% filter(Year >= yearStart, Year <= yearEnd)
  title = paste0("z-score normalized trend for bird strike incidents per year<br>from ",
                 yearStart, " to ", yearEnd)
  p4 <- plot_ly(incidentTrendNorm, x = ~Year, y = ~Number_of_IncidentsNor, 
                type = 'scatter', mode = 'lines') %>% 
    layout(title = title, margin=list(t = 40),
           xaxis = list(title = "Year"),
           yaxis = list(title = "z-score for Incidents of bird strikes"))
  p4
}
# trendNorm1(incidentTrendNorm, 2002, 2015)

#####@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#####@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Plot 5: trend line norm
trendNorm2 <- function(incidentTrendNorm, yearStart, yearEnd){
  incidentTrendNorm <- incidentTrendNorm %>% filter(Year >= yearStart, Year <= yearEnd)
  # Plot the results
  title = paste0("z-score normalized trend for number of persons injured vs Year<br>from ",
                 yearStart, " to ", yearEnd)
  p5 <- plot_ly(incidentTrendNorm, x = ~Year, y = ~Number_of_InjuriesNor, 
                type = 'scatter', mode = 'lines') %>%
    layout(title = title, margin=list(t = 40),
           xaxis = list(title = "Year"),
           yaxis = list(title = "z-score for number of persons injured"))
  p5
}
# trendNorm2(incidentTrendNorm, 2002, 2015)


#####@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#####@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Plot 6: bird strike vs month
incidentMonths <- readRDS("rds_data/incidentMonths.rds")

bsMonth <- function(incidentMonths, yearStart, yearEnd){
  incidentMonths <- incidentMonths %>% filter(Year >= yearStart, Year <= yearEnd)
  incidentMonths$Year <- as.factor(incidentMonths$Year)
  labels <- c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')
  title = paste0("Number of bird strike incident Vs Month<br>from ",
                 yearStart, " to ", yearEnd)
  p6 <- ggplot(data = incidentMonths, aes(x= Month, y = Incidents)) + 
    geom_line(aes(colour= Year)) + theme_minimal() +
    scale_x_discrete(limits=factor(1:12), labels = labels) +
    labs(title = title, x = "Month", y = "Number of bird strike incidents",
         color = "Year") + theme(plot.title = element_text(hjust = 0.5))
  ggplotly(p6)
}
# bsMonth(incidentMonths, 2002, 2015)


#####@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#####@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#Plot p7: time of day vs bs
time_of_day <- readRDS("rds_data/time_of_day.rds")

todPie <- function(time_of_day, yearStart, yearEnd){
  time_of_day <- time_of_day %>% filter(Year >= yearStart, Year <= yearEnd)
  
  tod <- time_of_day %>% group_by(Time) %>% 
    summarise(Number = sum(Number)) # get data by year and time of day
  tod <- as.data.frame(tod)
  title = paste0("Time of the day and occurance of bird strike<br>from ",
                 yearStart, " to ", yearEnd, "\n")
  p7 <- plot_ly(tod, labels = ~Time, values = ~Number, type = 'pie') %>%
    layout(title = title,  margin=list(t = 70),
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           legend = list(orientation = 'h', xanchor = "center", x=.5))
  p7
}
# todPie(time_of_day, 2002, 2015)

#####@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#####@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Plot p8: word cloud remarks
wordsALL <- readRDS("rds_data/wordsALL.rds")

cloud1 <- function(wordsALL, yearStart, yearEnd, n){
  wordsALL <- wordsALL %>% filter(Year >= yearStart, Year <= yearEnd)
  p8 <- wordcloud2(wordsALL[1:n, ], color = "random-dark", size = .8,
                   fontWeight = 'bold', maxRotation = pi/3, rotateRatio = 0.45,
                   hoverFunction = NULL, shape = 'diamond')
  p8
}
# cloud1(wordsALL, 40)


#####@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#####@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Plot p9: Aircraft type
dfAtype <- readRDS("rds_data/dfAtype.rds")

airType <- function(dfAtype, yearStart, yearEnd){
  dfAtype <- dfAtype %>% filter(Year >= yearStart, Year <= yearEnd)
  
  aType <- dfAtype %>% group_by(Aircraft_Type) %>% 
    summarise(Number = sum(Number))
  aType <- as.data.frame(aType)
  aType$Percentage <- round(aType$Number/sum(aType$Number)*100,2)
  title = paste0("Most common category of aircraft invloved in bird strikes<br>from ",
                 yearStart, " to ", yearEnd)
  p9 <- plot_ly(aType, x = ~Aircraft_Type, y = ~Number, type = 'bar',
                text = ~paste0(Number, ' (~', Percentage,'%)'), textposition = 'outside',
                hoverinfo = 'x', marker =list(color = rainbow(nrow(aType)))) %>%
    layout(title = title, margin=list(t = 60),
           yaxis = list(title = "Number of bird strikes"),
           xaxis = list(title = "Category of Aircraft"))
  p9
}
# airType(dfAtype, 1995, 2001)


#####@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#####@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Plot p10: air mass/weight
AC_MASS <- readRDS("rds_data/AC_MASS.rds")

airMass <- function(AC_MASS, yearStart, yearEnd){
  AC_MASS <- AC_MASS %>% filter(Year >= yearStart, Year <= yearEnd)
  acw <- AC_MASS %>% group_by(Aircraft_Weight) %>% 
    summarise(Number = sum(Number)) 
  acw <- as.data.frame(acw) # 
  acw$Percentage <- round(acw$Number/sum(acw$Number)*100,2)
  title = paste0("Size(weight) of aircraft vs Number of bird strikes<br>from ",
                 yearStart, " to ", yearEnd)
  p10 <- plot_ly(acw, x = ~Aircraft_Weight, y = ~Number, type = 'bar',
                 text = ~paste0(Number, ' (~', Percentage,'%)'), textposition = 'outside',
                 hoverinfo = 'x', marker =list(color = rainbow(nrow(acw)))) %>%
    layout(title = title, margin = list(t=60),
           yaxis = list(title = "Number of bird strikes"),
           xaxis = list(title = "Weight of the aircraft"))
  p10
}
# airMass(AC_MASS, 1999, 2019)


#####@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#####@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Plot p11: damage civil aircraft
civil <- readRDS(file="rds_data/civil.rds")

airCivil <- function(civil, yearStart, yearEnd){
  
  civil <- civil %>% filter(Year >= yearStart, Year <= yearEnd)
  civi <- civil %>% group_by(Damage) %>% 
    summarise(Number = sum(Number)) 
  civi <- as.data.frame(civi) 
  # exlude no damage cases
  civi <- civi[which(civi$Damage != "No Damage"),]
  civi$Percentage <- round(civi$Number/sum(civil$Number)*100, 2)
  civi$Damage <- factor(civi$Damage, 
                        levels = c("Uncertain", "Minor", "Substantial", 'Destroyed'))
  ap11 <- list(x = 'Minor', xref = 'x',
               y = civi[civi$Damage == 'Minor', 'Number'], yerf = 'y',
               text = paste0('Percentage shown is <br>in respect of a total of <br>',
                             sum(civil$Number), ' bird strikes'),  align = 'left',
               showarrow = TRUE, arrowhead = 0, arrowwidth = 1.2,
               ax = 120, ay = 40, font = list(color ='white'), bgcolor = 'green'
  )
  title = paste0("Cases of damage to civil aircraft<br>from ",
                 yearStart, " to ", yearEnd)
  p11 <- plot_ly(civi, x = ~Damage, y = ~Number, type = 'bar',
                 text = ~paste0(Number, ' (~', Percentage,'%)'), textposition = 'outside',
                 hoverinfo = 'x', marker =list(color = rainbow(nrow(civi)))) %>%
    layout(title = title, annotations = ap11, margin = list(t=60),
           xaxis = list(title = 'Damage level'),
           yaxis = list(title = "Number of cases"))
  p11
}
# airCivil(civil, 2000, 2015)


#####@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#####@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Plot p12: damage military aircraft
military <- readRDS("rds_data/military.rds")

airMil <- function(military, yearStart, yearEnd){
  
  military <- military %>% filter(Year >= yearStart, Year <= yearEnd)
  mili <- military %>% group_by(Damage) %>% 
    summarise(Number = sum(Number)) 
  mili <- as.data.frame(mili) # 
  # exlude no damage cases
  mili <- mili[which(mili$Damage != "No Damage"),]
  mili$Percentage <- round(mili$Number/sum(military$Number)*100, 2)
  ap12 <- list(x = 'Class E', xref = 'x',
               y = mili[mili$Damage == 'Class E', 'Number'], yerf = 'y',
               text = paste0('Percentage shown is <br>in respect of a total of <br>',
                             sum(military$Number), ' bird strikes'),  align = 'left',
               showarrow = TRUE, arrowhead = 0, arrowwidth = 1.2,
               ax = 120, ay = 40, font = list(color ='white'), bgcolor = 'green'
  )
  title = paste0("Cases of damage to military aircraft<br>from ",
                 yearStart, " to ", yearEnd)
  
  p12 <- plot_ly(mili, x = ~Damage, y = ~Number, type = 'bar',
                 text = ~paste0(Number, ' (~', Percentage,'%)'), textposition = 'outside',
                 hoverinfo = 'x', marker =list(color = rainbow(nrow(mili)))) %>%
    layout(title = title, annotations = ap12, margin = list(t=60),
           xaxis = list(title = 'Damage classifications of military aircraft'),
           yaxis = list(title = "Number of cases"))
  p12
}
# airMil(military, 2000, 2015)


#####@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#####@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Plot p13: bird species names

bs <- readRDS("rds_data/bs.rds")

cloud2 <- function(bs, yearStart, yearEnd, n){
  bs <- bs %>% filter(Year >= yearStart, Year <= yearEnd)
  # # Wordcloud plot using wordcloud2()
  p13 <- wordcloud2(bs[1:n, ], color = "random-dark", size = .8,
                    fontWeight = 'bold', maxRotation = pi/3, rotateRatio = 0.45,
                    hoverFunction = NULL, shape = 'star')
  p13
}
# cloud2(bs, 300)


#####@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#####@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Plot p14: bird size/weight

birdMass <- readRDS("rds_data/birdMass.rds")

bsize <- function(birdMass, yearStart, yearEnd){
  birdMass <- birdMass %>% filter(Year >= yearStart, Year <= yearEnd)
  
  bw <- birdMass %>% group_by(b_weight) %>% 
    summarise(Number = sum(Number)) # get data by year and weight
  bw <- as.data.frame(bw) # convert to dataframe
  
  bw$Percentage <- round(bw$Number/sum(bw$Number)*100, 2)
  bw$b_weight <- factor(bw$b_weight, levels = 
                          c('Small', 'Medium', 'Large', 'Uncertain'))
  
  title = paste0("Size of Bird vs Number of Incidents of bird strikes<br>from ",
                 yearStart, " to ", yearEnd)
  p14 <- plot_ly(bw, x = ~b_weight, y = ~Number, type = 'bar',
                 text = ~paste0(Number, ' (~', Percentage,'%)'), textposition = 'outside',
                 hoverinfo = 'x', marker =list(color = rainbow(nrow(bw)))) %>%
    layout(title = title, margin=list(t = 70),
           yaxis = list(title = "Number of bird strikes"),
           xaxis = list(title = "Size of Bird"))
  p14
}
# bsize(birdMass, 2002, 2018)


#####@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#####@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Plot p15: bird size vs civil aircraft damage
birdCivilDmg <- readRDS("rds_data/birdCivilDmg.rds")

birdCivil <- function(birdCivilDmg, yearStart, yearEnd){
  birdCivilDmg <- birdCivilDmg %>% filter(Year >= yearStart, Year <= yearEnd)
  
  b_c_dmg <- birdCivilDmg %>% group_by(Bird_Size, Aircraft_Size, Damage) %>%
    summarise(Number = n())
  b_c_dmg <- as.data.frame(b_c_dmg) # convert to dataframe
  b_c_dmg$Bird_Size <- factor(b_c_dmg$Bird_Size, levels = 
                                c('Small', 'Medium', 'Large'))
  title = "Civil aircraft damage vs Bird size"
  x = paste0("x-axis represents aircraft size and each subplot corresponds to a bird size")
  p15 <- ggplot(data = b_c_dmg, aes(x= Aircraft_Size, y = Number, fill = Damage)) + 
    geom_bar(position="dodge", stat="identity") + facet_wrap(~Bird_Size) +
    theme_bw() + labs(title = title, x = x, y = "Number of Incidents") +
    theme(plot.title = element_text(hjust = 0.5))
  ggplotly(p15)
}
# birdCivil(birdCivilDmg, 2000, 2019)


#####@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#####@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Plot p16: bird size and military aircraft damage
birdMilDmg <- readRDS("rds_data/birdMilDmg.rds")

birdMili <- function(birdMilDmg, yearStart, yearEnd){
  birdMilDmg <- birdMilDmg %>% filter(Year >= yearStart, Year <= yearEnd)
  
  b_m_dmg <- birdMilDmg %>% group_by(Bird_Size, Aircraft_Size, Damage) %>%
    summarise(Number = n())
  b_m_dmg <- as.data.frame(b_m_dmg) # convert to dataframe
  b_m_dmg$Bird_Size <- factor(b_m_dmg$Bird_Size, levels = 
                                c('Small', 'Medium', 'Large'))
  title = "Military aircraft damage vs Bird size"
  # title = paste0("Civil aircraft damage vs Bird size<br>from ", yearStart, ' to ', yearEnd, '<br>')
  x = paste0("x-axis represents aircraft size and each subplot corresponds to a bird size")
  p16 <- ggplot(data = b_m_dmg, aes(x= Aircraft_Size, y = Number, fill = Damage)) + 
    geom_bar(position="dodge", stat="identity") + facet_wrap(~Bird_Size) +
    theme_bw() + theme(plot.title = element_text(hjust = 0.5)) +
    labs(title = pt1, x = x, y = "Number of Incidents")
  ggplotly(p16)
}
# birdMili(birdMilDmg, 2000, 2005)

#####@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#####@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Table 1: Airports
airports <- readRDS("rds_data/airports.rds")

airportTab <- function(airports, yearStart, yearEnd){
  airports <- airports %>% filter(Year >= yearStart, Year <= yearEnd)
  
  airportsSum <- airports %>% group_by(Airport) %>%
    summarise("Total bird strikes" = sum(`Total bird strikes`), "No Damage" = sum(`No Damage`),
              "Uncertain Damage" = sum(`Uncertain Damage`), "Minor Damage" = sum(`Minor Damage`),
              "Destroyed" = sum(`Destroyed`), "Damage to Military Aircraft" = 
                sum(`Damage to Military Aircraft`))
  airportsSum <- airportsSum %>% arrange(desc(`Total bird strikes`))
  airportsSum
}
# airportTab(airports, 1995, 2015)

#####@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#####@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Plot p17: flight operations
flight <- readRDS("rds_data/flight.rds")

flightPlot <- function(flight, yearStart, yearEnd){
  
  flight <- flight %>% filter(Year >= yearStart, Year <= yearEnd)
  fli <- flight %>% group_by(Effect) %>% summarise(Number = sum(Number))
  fli <- as.data.frame(fli)
  # exclude None and Not mentioned cases 
  fli <- fli %>% filter(!fli$Effect %in% c('None', 'Not mentioned'))
  fli$Percentage <- round(fli$Number/sum(flight$Number)*100, 2)
  fli$Effect <- factor(fli$Effect, levels = c("Aborted Take-off", "Precautionary Landing",
                                              "Engine Shutdown", 'Other'))
  ap17 <- list(x = 'Precautionary Landing', xref = 'x',
               y = fli[fli$Effect == 'Precautionary Landing', 'Number'], yerf = 'y',
               text = paste0('Percentage shown is <br>in respect of a total of <br>',
                             sum(flight$Number), ' bird strikes'),  align = 'left',
               showarrow = TRUE, arrowhead = 0, arrowwidth = 1.2,
               ax = 150, ay = 40, font = list(color ='white'), bgcolor = 'green'
  )
  title = paste0("Effects on flight operations due to bird strike<br>from ",
                 yearStart, " to ", yearEnd)
  p17 <- plot_ly(fli, x = ~Effect, y = ~Number, type = 'bar',
                 text = ~paste0(Number, ' (~', Percentage,'%)'), textposition = 'outside',
                 hoverinfo = 'x', marker =list(color = rainbow(nrow(fli)))) %>%
    layout(title = title, annotations = ap17, margin=list(t = 70),
           xaxis = list(title = 'Effect on flight'),
           yaxis = list(title = "Number of cases"))
  p17
}
# f lightPlot(flight, 1995, 2015)


#####@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#####@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Plot p18: Phase of flight
phase <- readRDS("rds_data/phase.rds")

phasePlot <- function(phase, yearStart, yearEnd){
  phase <- phase %>% filter(Year >= yearStart, Year <= yearEnd)
  
  pha <- phase %>% group_by(Phase) %>% summarise(Number = sum(Number))
  pha <- as.data.frame(pha)
  # exclude None and Not mentioned cases 
  pha <- pha %>% filter(!pha$Phase == 'Not mentioned')
  pha$Percentage <- round(pha$Number/sum(phase$Number)*100, 2)
  pha$Phase <- factor(pha$Phase,
                      levels = c('Take-off Run', 'Climb', 'En Route',
                                 'Approach/Descent', 'Landing Roll', 'Taxi/Park'))
  ap18 <- list(x = 'Climb', xref = 'x',
               y = pha[pha$Phase == 'Climb', 'Number'], yerf = 'y',
               text = paste0('Percentage shown is <br>in respect of a total of <br>',
                             sum(phase$Number), ' bird strikes'),  align = 'left',
               showarrow = TRUE, arrowhead = 0, arrowwidth = 1.2,
               ax = 100, ay = 30, font = list(color ='white'), bgcolor = 'green'
  )
  title = paste0("Phase of the flight vs Bird Strike<br>from ",
                 yearStart, " to ", yearEnd)
  p18 <- plot_ly(pha, x = ~Phase, y = ~Number, type = 'bar',
                 text = ~paste0(Number, ' (~', Percentage,'%)'), textposition = 'outside',
                 hoverinfo = 'x', marker =list(color = rainbow(nrow(pha)))) %>%
    layout(title = title, annotations = ap18, margin=list(t = 70),
           xaxis = list(title = 'Effect on flight'),
           yaxis = list(title = "Number of cases"))
  p18
}
# phasePlot(phase, 2000, 2010)


#####@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#####@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Plot leaflet: map states
dfStates <- readRDS("rds_data/dfStates.rds")
sCode <- readRDS("rds_data/sCode.rds")

mapStates <- function(dfStates, sCode, yearStart, yearEnd){
  
  sCode <- sCode %>% filter(Year >= yearStart, Year <= yearEnd)
 
  # count number of incidents in each states
  sc <- sCode %>% group_by(code) %>% 
    summarise(Number = n()) # group data
  sc <- as.data.frame(sc) # convert to dataframe
  
  stab <- sc %>% inner_join(dfStates)
  # calculate radius values corresponding the number of incidents in each state
  stab['radius'] <- stab$Number*30/max(stab$Number)
  stab <- stab %>% select(c("state", "Number", "gdp.m.", "population", "area.sqmi.", 
                            "code", "long", "lat", "radius"))
  names(stab) <- c("State", "Number of bird strikes", "GDP (in USD Mil.)",
                   "Population", "Ares in sqmi.", 
                   "code", "long", "lat", "radius")
  colors = c('red', 'green', 'violet', 'maroon', 'blue', 'magenta', 'pick', 'purple', 'brown', 'cyan')
  leaflet(data = stab) %>% addTiles()  %>%
    addCircles(lng= stab$long, lat = stab$lat, color = "red")  %>% 
    addCircleMarkers(~long, ~lat, radius = stab$radius, color= sample(colors, 1), 
                     popup = ~as.character(paste("State:", stab$State,"<br>",
                                                 'Number of Incidents:', stab$`Number of bird strikes`)))
}
# mapStates(dfStates, sCode, 1990, 2003)


#####@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#####@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Plot table2: table states

tabStates <- function(dfStates, sCode, yearStart, yearEnd){
  
  sCode <- sCode %>% filter(Year >= yearStart, Year <= yearEnd)
  
  # count number of incidents in each states
  sc <- sCode %>% group_by(code) %>% 
    summarise(Number = n()) # group data
  sc <- as.data.frame(sc) # convert to dataframe
  
  stab <- sc %>% inner_join(dfStates)
  # calculate radius values corresponding the number of incidents in each state
  stab['radius'] <- stab$Number*30/max(stab$Number)
  stab <- stab %>% select(c("state", "Number", "gdp.m.", "population", "area.sqmi.", 
                            "code", "long", "lat", "radius"))
  names(stab) <- c("State", "Number of bird strikes", "GDP (in USD Mil.)",
                   "Population", "Ares in sqmi.", 
                   "code", "long", "lat", "radius")
  
  stab <- stab %>% select(c("State", "Number of bird strikes", "GDP (in USD Mil.)",
                            "Population", "Ares in sqmi.")) %>% arrange(desc(`Number of bird strikes`))
  stab
}
# tabStates(dfStates, sCode, 1990, 2003)

#####@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#####@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


#####@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#####@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


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