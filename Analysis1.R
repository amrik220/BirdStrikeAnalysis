# Author: Amrik Singh
# Project Title: Analyzing Collisions between Birds and Aircraft in United States
# (FAA Wildlife Strike Database, from Jan-1990 to Apr-2020)

#' Code Description: Data processing to explore, visualize, and analyze data
#' File Name: Analysis1.R # 
#' For more information, please read  README.md included in the repository

# Import required packages
library(dplyr)
library(ggplot2)
library(ggthemes)
library(plotly)
# library(shiny)
# library(shinythemes)
# library(reshape2)
# library(tm)
# library(SnowballC)
# library(wordcloud)
# library(RColorBrewer)

setwd(choose.dir())
# read data from birdStrikesAll.rds file.
df <- readRDS("birdStrikesAll.rds")
# df <- read.csv("birdStrikesAll.csv", strip.white = TRUE, na.strings = c("NA",""))


#################         Point of Impact and Locations of Damage        #################

# Data has 23 different columns that have information about the point of impact and location of 
# damage

poi <- data.frame(matrix(ncol = 6, nrow = nrow(df))) # create an empty dataframe to store 
# information about "Point of Impact
colnames(poi) <- c("Nose/Radome", "Windshield", "Engine", "Wing", "Fuselage/Body",
                   "Tail")

poi['Nose/Radome'] <- df$STR_NOSE + df$STR_RAD
poi['Windshield'] <- df$STR_WINDSHLD
poi['Engine'] <- df$STR_ENG1 + df$STR_ENG2 + df$STR_ENG3 + df$STR_ENG4 + df$STR_PROP +
  df$INGESTED
poi['Wing'] <- df$STR_WING_ROT + df$STR_LGHTS
poi['Fuselage/Body'] <- df$STR_FUSE + df$STR_LG + df$STR_OTHER
poi['Tail'] <- df$STR_TAIL

colSums(poi) # see the values
sum(colSums(poi)) # check total number of cases where the information about the point
# of impact is available

# view the percentage of point of impact
round(colSums(poi)/sum(colSums(poi))*100,2)


p <- plot_ly(poi, x = ~colnames(poi), y = ~colSums(poi), type = 'bar',
             text = ~colSums(poi), textposition = 'outside',
             marker =list(color = rainbow(ncol(poi),1))) %>%
  layout(title = "Point of Impact vs Number of bird strikes",
         yaxis = list(title = "Number of bird strikes"),
         xaxis = list(title = "Point of Impact"))
print(p)


###### Location of damage. whether the bird strike resulted in damage of the parts of
# aircraft

lod <- data.frame(matrix(ncol = 6, nrow = nrow(df))) # create an empty dataframe to store 
# information about "Location of Damage"
colnames(lod) <- c("Nose/Radome", "Windshield", "Engine", "Wing", "Fuselage/Body",
                   "Tail")

lod['Nose/Radome'] <- df$DAM_NOSE + df$DAM_RAD
lod['Windshield'] <- df$DAM_WINDSHLD
lod['Engine'] <- df$DAM_ENG1 + df$DAM_ENG2 + df$DAM_ENG3 + df$DAM_ENG4 + df$DAM_PROP +
  df$INGESTED
lod['Wing'] <- df$DAM_WING_ROT + df$DAM_LGHTS
lod['Fuselage/Body'] <- df$DAM_FUSE + df$DAM_LG + df$DAM_OTHER
lod['Tail'] <- df$DAM_TAIL

colSums(lod) # see the values
sum(colSums(lod)) # check total number of cases where the information about the point
# of impact is available

# view the percentage of point of impact
round(colSums(lod)/sum(colSums(lod))*100,2)

# main plot for final visualization
p <- plot_ly(poi, x = ~colnames(poi), y = ~colSums(poi), type = 'bar',
             text = ~colSums(poi), textposition = 'outside',
             name = 'Point of Impact') %>%
  add_trace(y = ~colSums(lod), name = 'Location of Damage') %>%
  layout(title = "Point of Impact and Location of Damage Vs Number of bird strikes",
         yaxis = list(title = "Number of bird strikes"),
         xaxis = list(title = "Point of Impact and Location of Damage"))
print(p)

damageCases <- round((colSums(lod)/colSums(poi))*100,2)

# plot for final visualization
p <- plot_ly(poi, x = ~colnames(poi), y = ~damageCases,
             type = 'bar', text = ~damageCases, textposition = 'outside',
             marker =list(color = rainbow(ncol(poi),1))) %>%
  layout(title = "Percentage of cases when bird strike resulted in damage",
         yaxis = list(title = "Percentage cases resulted in damage"),
         xaxis = list(title = "Location of Damage/Point of Impact"))
print(p)

# filter and save data for above result
poi_lod <- cbind(as.data.frame(colSums(poi)), as.data.frame(colSums(lod)))
poi_lod$names <- rownames(poi_lod)
rownames(poi_lod) <- NULL
colnames(poi_lod) <- c("poi", 'lod', 'name')
poi_lod['percent'] <- round((poi_lod$lod/poi_lod$poi)*100,2)

if(!dir.exists(file.path(getwd(), 'rds_data'))){
  dir.create(file.path(getwd(), 'rds_data'))}
saveRDS(poi_lod, file="rds_data/poi_lod.rds")

getwd()

##########################################################################################
####################                  Trend Lines                       ##################

# Number of incidents per year

incidentTab <- df %>% group_by(INCIDENT_YEAR) %>% 
  summarise(Number_of_Incidents = n()) # get data by months

incidentTab <- as.data.frame(incidentTab)
colnames(incidentTab) <- c('Year', 'Number_of_Incidents')

# incidentTab <- incidentTab[incidentTab$Year != 2020,]
# class(incidentTab)

# Plot trend line
plot_ly(incidentTab, x = ~Year, y = ~Number_of_Incidents, type = 'scatter',
        mode = 'lines') %>% layout(title = "Number of incidents vs Year",
         xaxis = list(title = "Year"),
         yaxis = list(title = "Number of Incidents of bird strikes"))


# Filter data related to number of persons injured in the incidents
dfInjury <- data.frame(matrix(ncol = 3, nrow = nrow(df))) # create an empty dataframe to 
# store information about injuries to persons
colnames(dfInjury) <- c('Year', 'Number_of_Injuries', 'fatalities')
dfInjury$Year <- df$INCIDENT_YEAR
dfInjury$Number_of_Injuries <- df$NR_INJURIES
dfInjury$fatalities <- df$NR_FATALITIES
dfInjury[is.na(dfInjury)] <- 0

dfInjury$Number_of_Injuries <- dfInjury$Number_of_Injuries + dfInjury$fatalities

# head(dfInjury)

injuryData <- dfInjury %>% group_by(Year) %>% 
  summarise(Number_of_Injuries = sum(Number_of_Injuries))

# injuryData <- injuryData[injuryData$Year != 2020,]
class(dfInjury)

plot_ly(injuryData, x = ~injuryData$Year, y = ~injuryData$Number_of_Injuries, 
        type = 'scatter', mode = 'lines') %>%
  layout(title = "Number of persons injured vs Year",
         xaxis = list(title = "Year"),
         yaxis = list(title = "Number of persons injured"))

# In 2009 there was 116 injuries, this is due to one major 
# incident in which 100 people where injured. This is famous Hudson River incident.

# combine the result of the two steps above
incidentTab <- cbind(incidentTab, injuryData)
incidentTab
incidentTab <- subset(incidentTab, select = -3)
incidentTab
# plot_ly(data = incidentTab, x = ~Number_of_Injuries, 
# y = ~Number_of_Incidents, type = 'scatter') # Not much insights

if(!dir.exists(file.path(getwd(), 'rds_data'))){
  dir.create(file.path(getwd(), 'rds_data'))}
# save result as .rds file
saveRDS(incidentTab, file="rds_data/incidents.rds")


############### Data Integration and Normalization to account for increase in air traffic

#' We have obtained the data about number of passenger Enplanements per year from Jan 2000 t0
#' Apr 2020: Source - U.S Air Carrier Traffic Statistics
#' Retrieved from https://www.transtats.bts.gov/: https://www.transtats.bts.gov/TRAFFIC/

#' z-score normalization is used to plot normalized trend lines 
#' Step 1: Divide number of injuries and number of incidents by number of passenger
#' empanelments of corresponding years
#' Step 2: Apply z-score normalization using scale() function.
#' Step 3: Plot trend lines as done earlier.

# read data on passenger empanelments
dfPassengers <- read.csv("USCarrier_TrafficData.csv", strip.white = TRUE)

dfIncidents <- incidentTab[incidentTab$Year >= 2000,] # filter original data
rownames(dfIncidents) <- seq(length=nrow(dfIncidents)) # reset row index

# integrate two data sets
dfIncidents <- cbind(dfIncidents, dfPassengers)
dfIncidents <- subset(dfIncidents, select = -4)

# get relative scores for number of injuries and incidents
dfIncidents['Number_of_IncidentsNor'] <- 
  dfIncidents$Number_of_Incidents/dfIncidents$Passengers*100000
dfIncidents['Number_of_InjuriesNor'] <- 
  dfIncidents$Number_of_Injuries/dfIncidents$Passengers*100000

# apply z-score normalization
dfIncidents$Number_of_IncidentsNor <- 
scale(dfIncidents$Number_of_IncidentsNor, center = TRUE, scale = TRUE)
dfIncidents$Number_of_InjuriesNor <-
scale(dfIncidents$Number_of_InjuriesNor, center = TRUE, scale = TRUE)

# Plot the results
plot_ly(dfIncidents, x = ~dfIncidents$Year, y = ~dfIncidents$Number_of_IncidentsNor, 
        type = 'scatter', mode = 'lines') %>% 
  layout(title = "z-score normalization: Number of incidents vs Year",
                  xaxis = list(title = "Year"),
                  yaxis = list(title = "z-score for Incidents of bird strikes"))

plot_ly(dfIncidents, x = ~dfIncidents$Year, y = ~dfIncidents$Number_of_InjuriesNor, 
        type = 'scatter', mode = 'lines') %>%
  layout(title = "z-score normalization: Number of persons injured vs Year",
         xaxis = list(title = "Year"),
         yaxis = list(title = "z-score for Number of persons injured"))

# save result as .rds file
saveRDS(dfIncidents, file="rds_data/incidentsNorm.rds")


########### Visualize pattern in number of incidents through different years #############
# Which part of the year a bird strike is most like to occur #

incidentMonths <- df %>% group_by(INCIDENT_YEAR, INCIDENT_MONTH) %>% 
  summarise(Number_of_Incidents = n()) # get data by months

# remove incomplete year 2020
# incidentMonths <- incidentMonths[incidentMonths$INCIDENT_YEAR != 2020,]
incidentMonths <- as.data.frame(incidentMonths)
incidentMonths$INCIDENT_YEAR <- as.factor(incidentMonths$INCIDENT_YEAR)


# Plot the data
g <- ggplot(data = incidentMonths, aes(x= INCIDENT_MONTH, y = Number_of_Incidents)) + 
  geom_line(aes(colour= INCIDENT_YEAR)) + theme_minimal() +
  scale_x_discrete(limits=1:12, labels = 1:12) +
  labs(title = "Number of Incident Vs Months\n", x = "Month", y = "Number of Incidents",
       color = "Year")
ggplotly(g)

# We could see from the graph that number of bird strike incidents relative higher in
# months from June to October. This is interesting, the bird strikes occurs more often
# in summer and autumn

# save result as .rds file
saveRDS(incidentMonths, file="rds_data/incidentMonths.rds")



#################### Incidents with respect to time of the day ###########################

time_of_day <- data.frame(matrix(ncol = 2, nrow = nrow(df))) # create an empty dataframe to 
# store information about time_of_day
colnames(time_of_day) <- c('Time', 'Year')

time_of_day$Time <- df$TIME_OF_DAY
time_of_day$Year <- df$INCIDENT_YEAR

time_of_day <- time_of_day[complete.cases(time_of_day),] # Remove NAs

time_of_day$Time[which(time_of_day$Time == 'DAWN')] <- "Dawn"
time_of_day$Time[which(time_of_day$Time == 'DAY')] <- "Day"
time_of_day$Time[which(time_of_day$Time == 'DUSK')] <- "Dusk"
time_of_day$Time[which(time_of_day$Time == 'NIGHT')] <- "Night"

time_of_day <- time_of_day[time_of_day$Time != 'UNKNOWN',]

time_of_day <- time_of_day %>% group_by(Year, Time) %>% 
  summarise(Number = n()) # get data by year and time of day

time_of_day <- as.data.frame(time_of_day) # convert to dataframe
# time_of_day <- time_of_day[time_of_day$Year != 2020,]
# save result as .rds file
saveRDS(time_of_day, file="rds_data/timePie.rds")

# time_of_day <- time_of_day[time_of_day$Year >= 1990,]
# time_of_day <- time_of_day[time_of_day$Year <= 2015,]

dim(time_of_day)

tod <- time_of_day %>% group_by(Time) %>% 
  summarise(Number = sum(Number)) # get data by year and time of day
tod <- as.data.frame(tod) # convert to dataframe

plot_ly(tod, labels = ~Time, values = ~Number, type = 'pie') %>%
  layout(title = 'Time of the Day and Occurance of Bird Strike',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

# From the pie chart above, we can say that majority of the bird strikes occurs
# during the day time, which is expected as birds are mostly actively during the day.
