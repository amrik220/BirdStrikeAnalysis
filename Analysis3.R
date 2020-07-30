# Author: Amrik Singh
# Project Title: Analysis of bird strikes reported in USA from Jan-1990 to Apr-2020
# (FAA Wildlife Strike Database, from Jan-1990 to Apr-2020)

#' Code Description: Data processing to explore, visualize, and analyze data
#' File Name: Analysis2.R # Airports and States
#' For more information, please read  README.md included in the repository

# Import required packages
library(dplyr)
library(ggplot2) 
library(ggthemes)
library(plotly)
# library("maps")
# library(reshape2)
library(leaflet)

setwd(choose.dir())
# read data from birdStrikesAll.rds file.
df <- readRDS("database/birdStrikesAll.rds")
# df <- read.csv("birdStrikesAll.csv", strip.white = TRUE, na.strings = c("NA",""))

############### Information About airports, phase of the filght and effect on flight######

airports <- data.frame(matrix(ncol = 3, nrow = nrow(df))) # create an empty dataframe
colnames(airports) <- c('Year', 'Airport', 'Damage')
head(airports)

airports$Year <- df$INCIDENT_YEAR
airports$Airport <- df$AIRPORT
airports$Damage <- df$DAMAGE_LEVEL

airportTab <- airports %>% group_by(Year, Airport, Damage) %>% 
  summarise(Number = n()) # group data
airportTab <- as.data.frame(airportTab) # convert to dataframe
head(airportTab)
sum(airportTab$Number)
dim(airportTab)

getwd()
if(!dir.exists(file.path(getwd(), 'rds_data'))){
  dir.create(file.path(getwd(), 'rds_data'))}
# save result as .rds file
saveRDS(airportTab, file="rds_data/airportTab.rds")

# airport[airport$Damage %in% c("M", "M?", "S", "D"),]
airports <- airport %>% group_by(Airport, Damage) %>% 
  summarise(Number_of_Incident = sum(Number))

airports <- as.data.frame(airports) # convert to dataframe


data <- data.frame(matrix(ncol = 6, nrow = length(unique(airports$Airport)))) # create an 
# empty dataframe
colnames(data) <- c('Airport', 'Number_of_Incidents', 'Uncertain', 'Minor', 
                  'Substantial', 'Destroyed')

data$Airport <- unique(airports$Airport)
for(i in 1:nrow(data)){
  data$Number_of_Incidents[i] <- 
    sum(airports$Number_of_Incident[airports$Airport == data$Airport[i]])
  data$Uncertain[i] <- 
    sum(airports$Number_of_Incident[airports$Airport == data$Airport[i] & airports$Damage == 'M?'])
  data$Minor[i] <- 
    sum(airports$Number_of_Incident[airports$Airport == data$Airport[i] & airports$Damage == 'M'])
  data$Substantial[i] <- 
    sum(airports$Number_of_Incident[airports$Airport == data$Airport[i] & airports$Damage == 'S'])
  data$Destroyed[i] <- 
    sum(airports$Number_of_Incident[airports$Airport == data$Airport[i] & airports$Damage == 'D'])
}
# data <- data[data$Airport != 'UNKNOWN', ]
airportTab[airportTab$Airport == 'UNKNOWN', ]


data <- data[order(-data$Number_of_Incidents),]
data <- data[order(data$Airport, decreasing=TRUE),] 
data <- data[order(data$Airport),] 

head(data)
class(data)
data[order(data$Number_of_Incidents, decreasing=TRUE),] 

sum(data$Number_of_Incidents)

######## Effect on flight operations

flight <- data.frame(matrix(ncol = 2, nrow = nrow(df))) # create an empty dataframe
colnames(flight) <- c('Effect_on_Flight', 'Year')

flight$Effect_on_Flight <- as.character(df$EFFECT)
flight$Year <- df$INCIDENT_YEAR

# flight <- flight[flight$Year != 2020,]
flight <- flight[complete.cases(flight),] # Remove NAs
flight <- flight[flight$Effect_on_Flight != 'None',]

flight$Effect_on_Flight[flight$Effect_on_Flight == 'ENGINE SHUT DOWN'] <- "Engine Shut Down"


dim(flight)
table(flight$Effect_on_Flight)

saveRDS(flight, file="rds_data/flight.rds")

fl <- flight %>% group_by(Effect_on_Flight) %>% 
  summarise(Number = n()) # group data
fl <- as.data.frame(fl) # convert to dataframe
head(fl)
dim(fl)

p <- plot_ly(fl, x = ~Effect_on_Flight, y = ~Number, type = 'bar',
             text = ~Number, textposition = 'outside',
             marker =list(color = rainbow(nrow(fl),1))) %>%
  layout(title = "Effects on flight operations by Bird Strike",
         yaxis = list(title = "Number of incidents"),
         xaxis = list(title = "Effect on Flight"))
print(p)
sum(fl$Number)

####### Phase of the flight when bird strikes

phase <- data.frame(matrix(ncol = 2, nrow = nrow(df))) # create an empty dataframe
colnames(phase) <- c('Phase_of_Flight', 'Year')

phase$Phase_of_Flight <- df$PHASE_OF_FLIGHT
phase$Year <- df$INCIDENT_YEAR

# phase <- phase[phase$Year != 2020,]
phase <- phase[complete.cases(phase),] # Remove NAs
phase <- phase[phase$Phase_of_Flight != 'None',]
dim(phase)
table(phase$Phase_of_Flight)

saveRDS(phase, file="rds_data/phase.rds")

ph <- phase %>% group_by(Phase_of_Flight) %>% 
  summarise(Number = n()) # group data
ph <- as.data.frame(ph) # convert to dataframe
head(ph)
dim(ph)

p <- plot_ly(ph, x = ~Phase_of_Flight, y = ~Number, type = 'bar',
             text = ~Number, textposition = 'outside',
             marker =list(color = rainbow(nrow(ph),1))) %>%
  layout(title = "Phase of Flight and Frequency of Bird Strike",
         yaxis = list(title = "Number of incidents"),
         xaxis = list(title = "Phase of Flight"))
print(p)
sum(ph$Number)


#####################################################################################

#################### Information on States
head(map_data('state'))

# read data about states, (state population, gdp, area)
dfStates <- read.csv("usaData.csv", strip.white = TRUE)
head(dfStates)
dfStates['long'] <- 0 # emppty column to store longitude
dfStates['lat'] <- 0

# extract longitude and lattitude for us states
for(i in 1:nrow(dfStates)){
  dfStates$long[i] <- 
    mean(map_data('state')$long[map_data('state')$region == tolower(dfStates$state[i])])
  dfStates$lat[i] <- 
    mean(map_data('state')$lat[map_data('state')$region == tolower(dfStates$state[i])])
}
# dfStates$long[dfStates$state == 'Florida'] <- -81.5158

dfStates <- dfStates[complete.cases(dfStates),]
nrow(dfStates) # Hawaii and Alaska excluded from this.

sCode <- data.frame(matrix(ncol = 2, nrow = nrow(df))) # create an empty dataframe
# from states and information about number of incidents in each state
colnames(sCode) <- c('code', 'Year')

sCode$code <- df$STATE # it contains the state code from the database
sCode$Year <- df$INCIDENT_YEAR

# filter states and number of incidents
# sCode <- sCode[sCode$Year != 2020,]
sCode <- sCode[complete.cases(sCode),] # Remove NAs
sCode <- sCode[sCode$code != 'None',]

# count number of incidents in each states
sc <- sCode %>% group_by(code) %>% 
  summarise(Number = n()) # group data
sc <- as.data.frame(sc) # convert to dataframe

# integrated data for states with number of incidents
dfStates['Number'] <- 0 
dfStates$code <- as.character(dfStates$code)
for(i in 1:nrow(dfStates)){
  dfStates$Number[i] <- sc$Number[sc$code == dfStates$code[i]]
}

# calculate radius values corresponding the number of incidents in each state
dfStates['radius'] <- dfStates$Number*30/max(dfStates$Number)
head(dfStates$radius)
# save result for future use
saveRDS(dfStates, file="rds_data/dfStates.rds")

pal <- colorFactor(palette = c("red","green", "blue" ) ,domain = dfStates$Number)

leaflet(data = dfStates) %>% addTiles()  %>%
  addCircles(lng= dfStates$long, lat = dfStates$lat, color = ~pal(dfStates$Number))  %>% 
  addCircleMarkers(~long, ~lat, radius = dfStates$radius, color= ~pal(Number), 
                   popup = ~as.character(paste("State:", dfStates$state,"<br>",
                    'Number of Incidents:', dfStates$Number))) 


names(dfStates)
