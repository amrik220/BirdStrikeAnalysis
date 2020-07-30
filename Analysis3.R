# Author: Amrik Singh
# Project Title: Analysis of bird strikes reported in USA from 1990 to 2019
# (FAA Wildlife Strike Database, from 1990 to 2019)

#' Code Description: Data processing to explore, visualize, and analyze data
#' File Name: Analysis3.R # Airports and States
#' For more information, please read  README.md included in the repository

# Import required packages
library(dplyr)
library(ggplot2) 
library(ggthemes)
library(plotly)
# library("maps")
library(reshape2)
library(leaflet)

setwd(choose.dir())
# read data from birdStrikesAll.rds file.
df <- readRDS("database/birdStrikesAll.rds")
# df <- read.csv("birdStrikesAll.csv", strip.white = TRUE, na.strings = c("NA",""))


##Information About airports, phase of the flight and effect on flight
##@@@@@@@@@@@@@@@@@@ Airports
airports <- df %>% select(AIRPORT, INCIDENT_YEAR, DAMAGE_LEVEL)
head(airports)

airports <- airports %>% group_by(AIRPORT, INCIDENT_YEAR, DAMAGE_LEVEL) %>% 
  summarise(Number = n()) # group data
airports <- as.data.frame(airports) # convert to dataframe

class(airports)
head(airports)
sum(airports$Number)

airports$DAMAGE_LEVEL[which(airports$DAMAGE_LEVEL == 'N')] <- "No Damage"
airports$DAMAGE_LEVEL[which(airports$DAMAGE_LEVEL == 'M?')] <- "Uncertain Damage"
airports$DAMAGE_LEVEL[which(airports$DAMAGE_LEVEL == 'M')] <- "Minor Damage"
airports$DAMAGE_LEVEL[which(airports$DAMAGE_LEVEL == 'S')] <- "Substantial Damage"
airports$DAMAGE_LEVEL[which(airports$DAMAGE_LEVEL == 'D')] <- "Destroyed"
# missing values/no reported damage level treated as no damage 
airports$DAMAGE_LEVEL[is.na(airports$DAMAGE_LEVEL)] <- "No Damage" 

airports <- dcast(airports, AIRPORT + INCIDENT_YEAR ~ DAMAGE_LEVEL,
           value.var = 'Number', fill = 0, fun.aggregate = sum)
sum(colSums(airports[,3:12]))

airports$AIRPORT[which(airports$AIRPORT == 'UNKNOWN')] <- "Bird Strike occured during flight."

airports$'Damage to Military Aircraft' <- rowSums(
  subset(airports, select = c("Class A", "Class B", "Class C", "Class D", "Class E")))

airports <- airports %>% select(-c("Class A", "Class B", "Class C", "Class D", "Class E"))
airports <- airports %>% rename(Airport = AIRPORT, Year = INCIDENT_YEAR)
airports$'Total bird strikes' <- rowSums(airports %>% select(-c(Airport, Year)))

airports <- airports %>% select(c("Airport", "Year", "Total bird strikes", "No Damage",
                                  "Uncertain Damage", "Minor Damage", "Substantial Damage",
                                  "Destroyed", "Damage to Military Aircraft"))
names(airports)
head(airports)

if(!dir.exists(file.path(getwd(), 'rds_data'))){
  dir.create(file.path(getwd(), 'rds_data'))}
# save result as .rds file
saveRDS(airports, file="rds_data/airports.rds")

####################
yearStart = 1990
yearEnd = 2000
airports %>% filter(Year >= yearStart, Year <= yearEnd)

airportsSum <- airports %>% group_by(Airport) %>%
  summarise("Total bird strikes" = sum(`Total bird strikes`), "No Damage" = sum(`No Damage`),
            "Uncertain Damage" = sum(`Uncertain Damage`), "Minor Damage" = sum(`Minor Damage`),
            "Destroyed" = sum(`Destroyed`), "Damage to Military Aircraft" = 
              sum(`Damage to Military Aircraft`))
head(airportsSum)


##@@@@@@@@@@@@@@@@@@ Effect on flight operations

flight <- df %>% select(EFFECT, INCIDENT_YEAR)
str(flight)
table(is.na(flight$EFFECT))
flight <- flight %>% group_by(INCIDENT_YEAR, EFFECT) %>% 
  summarise(Number = n())
flight <- as.data.frame(flight)
table(flight$EFFECT)

flight$EFFECT[is.na(flight$EFFECT)] <- "Not mentioned"
flight$EFFECT[which(flight$EFFECT == 'Aborted Take-off, Other')] <- "Aborted Take-off"
flight$EFFECT[which(flight$EFFECT == 'Engine Shutdown, Precautionary Landing')] <- "Precautionary Landing"
flight$EFFECT[which(flight$EFFECT == 'None, Precautionary Landing')] <- "Precautionary Landing"
flight$EFFECT[which(flight$EFFECT == 'Other, Precautionary Landing')] <- "Precautionary Landing"
table(flight$EFFECT)

flight <- flight %>% rename(Year = INCIDENT_YEAR, Effect = EFFECT)

if(!dir.exists(file.path(getwd(), 'rds_data'))){
  dir.create(file.path(getwd(), 'rds_data'))}
# save result as .rds file
saveRDS(flight, file="rds_data/flight.rds")

#####################
yearStart = 1990
yearEnd = 2000
flight %>% filter(Year >= yearStart, Year <= yearEnd)

fli <- flight %>% group_by(Effect) %>% summarise(Number = sum(Number))
fli <- as.data.frame(fli)
sum(fli$Number)

# exclude None and Not mentioned cases 

fli <- fli %>% filter(!fli$Effect %in% c('None', 'Not mentioned'))

fli$Percentage <- round(fli$Number/sum(flight$Number)*100, 2)

fli$Effect <- factor(fli$Effect, levels = c("Aborted Take-off", "Precautionary Landing",
                                            "Engine Shutdown", 'Other'))
fli

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
  layout(title = title, annotations = ap17,
         xaxis = list(title = 'Effect on flight'),
         yaxis = list(title = "Number of cases"))
print(p17)


##@@@@@@@@@@@@@@@@@@ Phase of the flight when bird strikes

phase <- df %>% select(PHASE_OF_FLIGHT, INCIDENT_YEAR)
str(phase)
table(is.na(phase$PHASE_OF_FLIGHT))
phase <- phase %>% group_by(INCIDENT_YEAR, PHASE_OF_FLIGHT) %>% 
  summarise(Number = n()) 
phase <- as.data.frame(phase)
phase <- phase %>% rename(Year = INCIDENT_YEAR, Phase = PHASE_OF_FLIGHT)
table(phase$Phase)

phase$Phase[is.na(phase$Phase)] <- "Not mentioned"
phase$Phase[which(phase$Phase == 'Departure')] <- "Climb"
phase$Phase[which(phase$Phase == 'Arrival')] <- "Approach/Descent"
phase$Phase[which(phase$Phase == 'Descent')] <- "Approach/Descent"
phase$Phase[which(phase$Phase == 'Approach')] <- "Approach/Descent"
phase$Phase[which(phase$Phase == 'Local')] <- "Taxi/Park"
phase$Phase[which(phase$Phase == 'Parked')] <- "Taxi/Park"
phase$Phase[which(phase$Phase == 'Taxi')] <- "Taxi/Park"

table(phase$Phase)
str(phase)

if(!dir.exists(file.path(getwd(), 'rds_data'))){
  dir.create(file.path(getwd(), 'rds_data'))}
# save result as .rds file
saveRDS(phase, file="rds_data/phase.rds")

################################

pha <- phase %>% group_by(Phase) %>% summarise(Number = sum(Number))
pha <- as.data.frame(pha)
sum(pha$Number)

# exclude None and Not mentioned cases 
table(pha$Phase)
pha <- pha %>% filter(!pha$Phase == 'Not mentioned')

pha$Percentage <- round(pha$Number/sum(phase$Number)*100, 2)

pha$Phase <- factor(pha$Phase,
                    levels = c('Take-off Run', 'Climb', 'En Route',
                               'Approach/Descent', 'Landing Roll', 'Taxi/Park'))
pha

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
  layout(title = title, annotations = ap18,
         xaxis = list(title = 'Effect on flight'),
         yaxis = list(title = "Number of cases"))
print(p18)



##@@@@@@@@@@@@@@@@@@ Information on States
head(map_data('state'))

# read data about states, (state population, gdp, area)
dfStates <- read.csv("database/usaData.csv", strip.white = TRUE)
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
  addCircles(lng= dfStates$long, lat = dfStates$lat, color = "red")  %>% 
  addCircleMarkers(~long, ~lat, radius = dfStates$radius, color= "red", 
                   popup = ~as.character(paste("State:", dfStates$state,"<br>",
                    'Number of Incidents:', dfStates$Number))) 


names(dfStates)
