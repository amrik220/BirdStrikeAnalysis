# Author: Amrik Singh
# Project Title: Analysis of bird strikes reported in USA from 1990 to 2019
# (FAA Wildlife Strike Database, from 1990 to 2019)

#' Code Description: Data processing to explore, visualize, and analyze data
#' File Name: Analysis1.R # Introduction and Bird Strike
#' For more information, please read  README.md included in the repository

# Import required packages
library(dplyr)
library(ggplot2)
library(ggthemes)
library(plotly)

getwd()
setwd(choose.dir())
# read data from birdStrikesAll.rds file.
df <- readRDS("database/birdStrikesAll.rds")
# df <- read.csv("birdStrikesAll.csv", strip.white = TRUE, na.strings = c("NA",""))


#################         Point of Impact and Locations of Damage        #################

# Data has 23 different columns that have information about the point of impact and location of 
# damage

poi <- data.frame(matrix(ncol = 7, nrow = nrow(df))) # create an empty dataframe to store 
# information about "Point of Impact
colnames(poi) <- c("Year", "Nose/Radome", "Windshield", "Engine", "Wing", "Fuselage/Body",
                   "Tail")
poi$Year <- df$INCIDENT_YEAR
poi['Nose/Radome'] <- df$STR_NOSE + df$STR_RAD

poi['Windshield'] <- df$STR_WINDSHLD
poi['Engine'] <- df$STR_ENG1 + df$STR_ENG2 + df$STR_ENG3 + df$STR_ENG4 + df$STR_PROP +
  df$INGESTED
poi['Wing'] <- df$STR_WING_ROT + df$STR_LGHTS
poi['Fuselage/Body'] <- df$STR_FUSE + df$STR_LG + df$STR_OTHER
poi['Tail'] <- df$STR_TAIL

colSums(poi[-1]) # see the values
sum(colSums(poi[-1])) # check total number of cases where the information about the point
# of impact is available

# view the percentage of point of impact
round(colSums(poi[-1])/sum(colSums(poi[-1]))*100,2)

pointOfImpact <- data.frame(colSums(poi[-1]))
pointOfImpact <- cbind(pointOfImpact,
                       round(colSums(poi[-1])/sum(colSums(poi[-1]))*100,2))

pointOfImpact$poi <- row.names(pointOfImpact)
names(pointOfImpact) <- c('num', 'percentage', 'poi')
pointOfImpact <- subset(pointOfImpact, select = c('poi', 'num', 'percentage'))
pointOfImpact

plot_ly(pointOfImpact, x = ~poi, y = ~num, type = 'bar',
             text = ~num, textposition = 'outside',
             marker =list(color = rainbow(ncol(poi),1))) %>%
  layout(title = "Point of Impact vs Number of bird strikes",
         yaxis = list(title = "Number of bird strikes"),
         xaxis = list(title = "Point of Impact"))


###### Location of damage. whether the bird strike resulted in damage of the parts of
# aircraft

lod <- data.frame(matrix(ncol = 7, nrow = nrow(df))) # create an empty dataframe to store 
# information about "Location of Damage"
colnames(lod) <- c('Year', "Nose/Radome", "Windshield", "Engine", "Wing", "Fuselage/Body",
                   "Tail")
lod$Year <- df$INCIDENT_YEAR
lod['Nose/Radome'] <- df$DAM_NOSE + df$DAM_RAD
lod['Windshield'] <- df$DAM_WINDSHLD
lod['Engine'] <- df$DAM_ENG1 + df$DAM_ENG2 + df$DAM_ENG3 + df$DAM_ENG4 + df$DAM_PROP +
  df$INGESTED
lod['Wing'] <- df$DAM_WING_ROT + df$DAM_LGHTS
lod['Fuselage/Body'] <- df$DAM_FUSE + df$DAM_LG + df$DAM_OTHER
lod['Tail'] <- df$DAM_TAIL

colSums(lod[-1]) # see the values
sum(colSums(lod[-1])) # check total number of cases where the information about the point
# of impact is available

# view the percentage of point of impact
round(colSums(lod[-1])/sum(colSums(lod[-1]))*100,2)


pointOfImpact$lodNum <- colSums(lod[-1])
pointOfImpact$lodPer <- round(colSums(lod[-1])/sum(colSums(lod[-1]))*100,2)
pointOfImpact$damPer <- round(pointOfImpact$lodNum/pointOfImpact$num*100,2)

pointOfImpact
sum(pointOfImpact$num)

# main plot for final visualization
yearStart = 1990; yearEnd = 2019
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
print(p1)

if(!dir.exists(file.path(getwd(), 'rds_data'))){
  dir.create(file.path(getwd(), 'rds_data'))}
saveRDS(pointOfImpact, file="rds_data/pointOfImpact.rds")
saveRDS(poi, file="rds_data/poi.rds")
saveRDS(lod, file="rds_data/lod.rds")

##########################################################################################
####################                  Trend Lines                       ##################

# Number of incidents per year

incidentTrend <- df %>% group_by(INCIDENT_YEAR) %>% 
  summarise(Number_of_Incidents = n()) # get data by months

incidentTrend <- as.data.frame(incidentTrend)
colnames(incidentTrend) <- c('Year', 'Number_of_Incidents')


# Plot trend line
yearStart = 1990; yearEnd = 2019
title = paste0("Number of bird strike incidents per year<br>from ",
               yearStart, " to ", yearEnd)
p2 <- plot_ly(incidentTrend, x = ~Year, y = ~Number_of_Incidents, type = 'scatter',
        mode = 'lines') %>% layout(title = title,
         xaxis = list(title = "Year"), margin=list(t = 40),
         yaxis = list(title = "Number of bird strikes"))
print(p2)

# Filter data related to number of persons injured in the incidents
injuryTrend <- data.frame(matrix(ncol = 3, nrow = nrow(df))) # create an empty dataframe to 
# store information about injuries to persons
colnames(injuryTrend) <- c('Year', 'Number_of_Injuries', 'fatalities')
injuryTrend$Year <- df$INCIDENT_YEAR
injuryTrend$Number_of_Injuries <- df$NR_INJURIES
injuryTrend$fatalities <- df$NR_FATALITIES
injuryTrend[is.na(injuryTrend)] <- 0

injuryTrend$Number_of_Injuries <- injuryTrend$Number_of_Injuries + injuryTrend$fatalities

injuryTrend <- injuryTrend %>% group_by(Year) %>% 
  summarise(Number_of_Injuries = sum(Number_of_Injuries))

# injuryTrend <- injuryTrend[injuryTrend$Year != 2020,]
class(injuryTrend)
yearStart = 1990; yearEnd = 2019
title = paste0("Number of persons injured due to bird strikes per year<br>from ",
               yearStart, " to ", yearEnd)
p3 <- plot_ly(injuryTrend, x = ~Year, y = ~Number_of_Injuries, 
        type = 'scatter', mode = 'lines') %>%
  layout(title = title, margin=list(t = 40),
         xaxis = list(title = "Year"),
         yaxis = list(title = "Number of persons injured"))
print(p3)
# In 2009 there was 116 injuries, this is due to one major 
# incident in which 100 people where injured. This is famous Hudson River incident.

# combine the result of the two steps above
incidentTrend$Number_of_Injuries <- injuryTrend$Number_of_Injuries
head(incidentTrend)
# plot_ly(data = incidentTrend, x = ~Number_of_Injuries, 
# y = ~Number_of_Incidents, type = 'scatter') # Not much insights

if(!dir.exists(file.path(getwd(), 'rds_data'))){
  dir.create(file.path(getwd(), 'rds_data'))}
# save result as .rds file
saveRDS(incidentTrend, file="rds_data/incidentTrend.rds")


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
dfPassengers <- read.csv("database/USCarrier_TrafficData.csv", strip.white = TRUE)

incidentTrendNorm <- incidentTrend[incidentTrend$Year >= 2000,] # filter original data
rownames(incidentTrendNorm) <- seq(length=nrow(incidentTrendNorm)) # reset row index

# integrate two data sets
incidentTrendNorm$Passengers <- dfPassengers$Passengers
incidentTrendNorm

# get relative scores for number of injuries and incidents
incidentTrendNorm['Number_of_IncidentsNor'] <- 
  incidentTrendNorm$Number_of_Incidents/incidentTrendNorm$Passengers
incidentTrendNorm['Number_of_InjuriesNor'] <- 
  incidentTrendNorm$Number_of_Injuries/incidentTrendNorm$Passengers
incidentTrendNorm

# apply z-score normalization
incidentTrendNorm$Number_of_IncidentsNor <- 
scale(incidentTrendNorm$Number_of_IncidentsNor, center = TRUE, scale = TRUE)
incidentTrendNorm$Number_of_InjuriesNor <-
scale(incidentTrendNorm$Number_of_InjuriesNor, center = TRUE, scale = TRUE)

incidentTrendNorm
str(incidentTrendNorm)
incidentTrendNorm$Number_of_IncidentsNor <- as.numeric(incidentTrendNorm$Number_of_IncidentsNor)
incidentTrendNorm$Number_of_InjuriesNor <- as.numeric(incidentTrendNorm$Number_of_InjuriesNor)

# Plot the results
yearStart = 1990; yearEnd = 2019
title = paste0("z-score normalized trend for bird strike incidents per year<br>from ",
               yearStart, " to ", yearEnd)
p4 <- plot_ly(incidentTrendNorm, x = ~Year, y = ~Number_of_IncidentsNor, 
        type = 'scatter', mode = 'lines') %>% 
  layout(title = title, margin=list(t = 40),
                  xaxis = list(title = "Year"),
                  yaxis = list(title = "z-score for Incidents of bird strikes"))
print(p4)
title = paste0("z-score normalized trend for number of persons injured vs Year<br>from ",
               yearStart, " to ", yearEnd)
p5 <- plot_ly(incidentTrendNorm, x = ~Year, y = ~Number_of_InjuriesNor, 
        type = 'scatter', mode = 'lines') %>%
  layout(title = title,margin=list(t = 40),
         xaxis = list(title = "Year"),
         yaxis = list(title = "z-score for number of persons injured"))
print(p5)
# save result as .rds file
saveRDS(incidentTrendNorm, file="rds_data/incidentTrendNorm.rds")


########### Visualize pattern in number of incidents through different years #############
# Which part of the year a bird strike is most like to occur #

incidentMonths <- df %>% group_by(INCIDENT_YEAR, INCIDENT_MONTH) %>% 
  summarise(Incidents = n()) # get data by months

names(incidentMonths) <- c('Year', 'Month', 'Incidents')
incidentMonths <- as.data.frame(incidentMonths)

# save result as .rds file
saveRDS(incidentMonths, file="rds_data/incidentMonths.rds")

incidentMonths$Year <- as.factor(incidentMonths$Year)
str(incidentMonths)

# Plot the data
labels <- c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')
title = paste0("Number of bird strike incident Vs Month<br>from ",
               yearStart, " to ", yearEnd)
p6 <- ggplot(data = incidentMonths, aes(x= Month, y = Incidents)) + 
  geom_line(aes(colour= Year)) + theme_minimal() +
  scale_x_discrete(limits=factor(1:12), labels = labels) +
  labs(title = title, x = "Month", y = "Number of bird strike incidents",
       color = "Year") + theme(plot.title = element_text(hjust = 0.5))
ggplotly(p6)

# We could see from the graph that number of bird strike incidents relative higher in
# months from June to October. This is interesting, the bird strikes occurs more often
# in summer and autumn


#################### Incidents with respect to time of the day ###########################

# store information about time_of_day
time_of_day <- df %>% select(TIME_OF_DAY, INCIDENT_YEAR)
colnames(time_of_day) <- c('Time', 'Year')

#time_of_day <- time_of_day[complete.cases(time_of_day),] # Remove NAs

table(is.na(time_of_day$Time))
time_of_day$Time[is.na(time_of_day$Time)] <- "Not mentioned"
table(time_of_day$Time)

time_of_day$Time[which(time_of_day$Time == 'DAWN')] <- "Dawn"
time_of_day$Time[which(time_of_day$Time == 'DAY')] <- "Day"
time_of_day$Time[which(time_of_day$Time == 'DUSK')] <- "Dusk"
time_of_day$Time[which(time_of_day$Time == 'NIGHT')] <- "Night"

time_of_day <- time_of_day[time_of_day$Time != 'Not mentioned',]

time_of_day <- time_of_day %>% group_by(Year, Time) %>% 
  summarise(Number = n()) # get data by year and time of day

time_of_day <- as.data.frame(time_of_day) # convert to dataframe
# save result as .rds file
saveRDS(time_of_day, file="rds_data/time_of_day.rds")

dim(time_of_day)

tod <- time_of_day %>% group_by(Time) %>% 
  summarise(Number = sum(Number)) # get data by year and time of day
tod <- as.data.frame(tod)
title = paste0("Time of the day and occurance of bird strike<br>from ",
               yearStart, " to ", yearEnd)
p7 <- plot_ly(tod, labels = ~Time, values = ~Number, type = 'pie') %>%
  layout(title = title, margin=list(t = 70),
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         legend = list(orientation = 'h', xanchor = "center", x=.5))
print(p7)
# From the pie chart above, we can say that majority of the bird strikes occurs
# during the day time, which is expected as birds are mostly actively during the day.
