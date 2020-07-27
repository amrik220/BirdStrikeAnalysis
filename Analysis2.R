# Author: Amrik Singh
# Project Title: Analysis and visualisation of bird strikes in USA from Jan-1990 to Apr-2020
# (FAA Wildlife Strike Database, from Jan-1990 to Apr-2020)

#' Code Description: Data processing to explore, visualize, and analyze data
#' File Name: Analysis2.R # Aircraft and Bird species
#' For more information, please read  README.md included in the repository

# Import required packages
library(dplyr)
library(ggplot2) 
library(ggthemes)
library(plotly)
# library(shiny)
# library(shinythemes)
# library(reshape2)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)


setwd(choose.dir())
# read data from birdStrikesAll.rds file.
df <- readRDS("birdStrikesAll.rds")
# df <- read.csv("birdStrikesAll.csv", strip.white = TRUE, na.strings = c("NA",""))



#################         Word Cloud on Remarks entered in database   #################
# The code used to make word cloud is adopted from the following link:
# Credit: https://georeferenced.wordpress.com/2013/01/15/rwordcloud/
remarks <- toString(df$REMARKS)

class(remarks)
textCorpus = Corpus(VectorSource(as.character(remarks)))
textCorpus = tm_map(textCorpus, content_transformer(tolower))
textCorpus = tm_map(textCorpus, removePunctuation)
textCorpus = tm_map(textCorpus, removeNumbers)
textCorpus = tm_map(textCorpus, removeWords, stopwords("english"))
textCorpus = tm_map(textCorpus, removeWords, c('reptd', 'rwy', 'twy', 'arpt', 'dmg',
                                               'found', 'inspn', 'ldg', 'ops', 'just',
                                               'note', 'eng', 'smithsonian', 'apch', 'sml'))

# textCorpus <- tm_map(textCorpus, stemDocument)

wordcloud(textCorpus, max.words=200, random.order=FALSE, rot.per=0.3, 
          colors=brewer.pal(8, "Dark2"))


# save result as .rds file
saveRDS(textCorpus, file="rds_data/textCorpus.rds")



################ Aircraft Type vs Bird Strike ############################################

dfAtype <- data.frame(matrix(ncol = 2, nrow = nrow(df))) # create an empty dataframe 
# to store information about type of aircraft
colnames(dfAtype) <- c('Year', 'Aircraft_Type')

dfAtype$Aircraft_Type <- as.character(df$AC_CLASS)
dfAtype$Year <- df$INCIDENT_YEAR
dfAtype[is.na(dfAtype)] <- 'Unknown'

class(dfAtype)
head(dfAtype)
unique(dfAtype$Aircraft_Type)

dfAtype <- dfAtype %>% group_by(Year, Aircraft_Type) %>% 
  summarise(Number = n()) # get data by year and aircraft type

dfAtype <- as.data.frame(dfAtype) # convert to dataframe

# dfAtype <- dfAtype[dfAtype$Year != 2020,]
# save result as .rds file
saveRDS(dfAtype, file="rds_data/dfAtype.rds")

aType <- dfAtype %>% group_by(Aircraft_Type) %>% 
  summarise(Number = sum(Number)) # get data by year and time of day
aType <- as.data.frame(aType) # convert to dataframe

aType$Aircraft_Type[which(aType$Aircraft_Type == 'A')] <- "Airplane"
aType$Aircraft_Type[which(aType$Aircraft_Type == 'B')] <- "Helicopter"
aType$Aircraft_Type[which(aType$Aircraft_Type == 'J')] <- "Ultrlight"

p <- plot_ly(aType, x = ~Aircraft_Type, y = ~Number, type = 'bar',
             text = ~Number, textposition = 'outside',
             marker =list(color = rainbow(8,1))) %>%
  layout(title = "Type of Aircraft vs Number of bird strikes",
         yaxis = list(title = "Number of bird strikes"),
         xaxis = list(title = "Type of Aircraft"))
print(p)


# From the bar chart above, we can say that majority of the bird strikes occurs
# with airplanes.



################ Aircraft Size based of weight vs Bird Strike ##########################

#' AC_MASS	1 = 2,250 kg or less: 2 = ,2251-5700 kg: 3 = 5,701-27,000 kg: 
#' 4 = 27,001-272,000 kg: 5 = above 272,000 kg
#' 


AC_MASS <- data.frame(matrix(ncol = 2, nrow = nrow(df))) # create an empty dataframe 
# to store information about type of aircraft
colnames(AC_MASS) <- c('Year', 'Aircraft_Weight')

AC_MASS$Aircraft_Weight <- df$AC_MASS
AC_MASS$Year <- df$INCIDENT_YEAR
AC_MASS <- AC_MASS[complete.cases(AC_MASS),] # Remove NAs

class(AC_MASS)
head(AC_MASS)
unique(AC_MASS$Aircraft_Weight)

AC_MASS <- AC_MASS %>% group_by(Year, Aircraft_Weight) %>% 
  summarise(Number = n()) # get data by year and aircraft weight

AC_MASS <- as.data.frame(AC_MASS) # convert to dataframe

# AC_MASS <- AC_MASS[AC_MASS$Year != 2020,]
# save result as .rds file
saveRDS(AC_MASS, file="rds_data/AC_MASS.rds")


acw <- AC_MASS %>% group_by(Aircraft_Weight) %>% 
  summarise(Number = sum(Number)) # get data by year and time of day
acw <- as.data.frame(acw) # convert to dataframe

# str(acw$Aircraft_Weight)
acw$Aircraft_Weight[which(acw$Aircraft_Weight == 1)] <- "2250 kg or less"
acw$Aircraft_Weight[which(acw$Aircraft_Weight == 2)] <- "2251-5700 kg"
acw$Aircraft_Weight[which(acw$Aircraft_Weight == 3)] <- "5701-27000 kg"
acw$Aircraft_Weight[which(acw$Aircraft_Weight == 4)] <- "27001-272000 kg"
acw$Aircraft_Weight[which(acw$Aircraft_Weight == 5)] <- "above 272000 kg"

p <- plot_ly(acw, x = ~Aircraft_Weight, y = ~Number, type = 'bar',
             text = ~Number, textposition = 'outside',
             marker =list(color = rainbow(7,1))) %>%
  layout(title = "Size of Aircraft based on Weight vs Number of bird strikes",
         yaxis = list(title = "Number of bird strikes"),
         xaxis = list(title = "Size of Aircraft based on weight"))
print(p)

# From the bar chart above, we can say that majority of the bird strikes occurs
# with aircraft of medium size or most the aircraft are of medium size.


################### Dollar value of damage to aircraft    ###########################3

#' Let's first separate civil and military aircraft because they have different
#' system of recording damage. In case of civil aircraft, apart from classifying the
#' damage in the following categories, the actual dollar values of assessed damage is 
#' also recorded where possible:
#' Damage (Civil)- M	Minor, M?	Uncertain S	Substantial, D	Destroyed

#' Where as military has following system to classify damage:
#' Damage (Military)
#' Class A	Over $2,000,000, Class B	$500,000 - $2,000,000
#' Class C	$50,000 - Less than $500,000, Class D $10000 - $50000, Class E Less than 10000

civil <- subset(df, OPID != 'MIL', select = c('COST_REPAIRS_INFL_ADJ', 'COST_OTHER_INFL_ADJ',
                                'DAMAGE_LEVEL', 'INCIDENT_YEAR', 'AC_MASS')) # subset data
mil <- df[df$OPID == 'MIL', c('COST_REPAIRS_INFL_ADJ', 'COST_OTHER_INFL_ADJ',
                              'DAMAGE_LEVEL', 'INCIDENT_YEAR', 'AC_MASS')]
civil[,1:2][is.na(civil[,1:2])] <- 0 # replace NAs

damageDollars <- sum(civil$COST_REPAIRS_INFL_ADJ) + sum(civil$COST_OTHER_INFL_ADJ)

cat("Total value of estimated damage to aircraft USD:", damageDollars)

### civil aircraft
civil <- subset(civil, select = -5)
mil <- subset(mil, select = -5)

class(civil)
head(civil)
names(civil)
table(civil$DAMAGE_LEVEL)

civil$DAMAGE_LEVEL <- as.character(civil$DAMAGE_LEVEL)
#civil <- civil[civil$DAMAGE_LEVEL %in% c("M", "M?", "S", "D", "N"),]

civil <- civil %>% group_by(INCIDENT_YEAR, DAMAGE_LEVEL) %>% 
  summarise(Number = n()) # get data by year and DAMAGE classification

civil <- as.data.frame(civil) # convert to dataframe
=
# civil <- civil[civil$INCIDENT_YEAR != 2020,]
civil <- civil[complete.cases(civil),] # Remove NAs

# save result as .rds file
saveRDS(civil, file="rds_data/civil.rds")


civi <- civil %>% group_by(DAMAGE_LEVEL) %>% 
  summarise(Number = sum(Number)) # get data by year and time of day
civi <- as.data.frame(civi) # convert to dataframe

civi$DAMAGE_LEVEL[which(civi$DAMAGE_LEVEL == 'N')] <- "No Damage"
civi$DAMAGE_LEVEL[which(civi$DAMAGE_LEVEL == 'M?')] <- "Uncertain"
civi$DAMAGE_LEVEL[which(civi$DAMAGE_LEVEL == 'M')] <- "Minor"
civi$DAMAGE_LEVEL[which(civi$DAMAGE_LEVEL == 'S')] <- "Substantial"
civi$DAMAGE_LEVEL[which(civi$DAMAGE_LEVEL == 'D')] <- "Destroyed"
# exlude no damage cases
civi <- civi[which(civi$DAMAGE_LEVEL != "No Damage"),]


g <- ggplot(data = civi, aes(x= DAMAGE_LEVEL, y = Number, fill = DAMAGE_LEVEL)) + 
  geom_bar(stat="identity") +
  theme_minimal() + scale_x_discrete(limits = c('Uncertain', 'Minor', 'Substantial', 
                                                'Destroyed')) +
  labs(title = "Damage to civil aircraft Vs Number of incidents\n", 
       x = "Damage to civil aircraft", y = "Number of Incidents")
ggplotly(g)

# for setting order of bars in plot
xform <- list(categoryorder = "array",
              categoryarray = c("Uncertain", "Minor", "Substantial", 'Destroyed'))
plot_ly(civi, x = ~DAMAGE_LEVEL, y = ~Number, type = 'bar',
        marker =list(color = rainbow(4,1))) %>%
  layout(title = "Damage to civil aircraft Vs Number of incidents",
         xaxis = xform,
         yaxis = list(title = "Number of incidents"))


# We know that in most of the cases there is no damage to the aircraft and from the
# the plot we can say that in case of damage, most of the time damage is minor



### military aircraft

class(mil)
head(mil)
table(mil$DAMAGE)
mil$DAMAGE <- as.character(mil$DAMAGE)

mil <- mil %>% group_by(INCIDENT_YEAR, DAMAGE) %>% 
  summarise(Number = n()) # get data by year and DAMAGE classification

mil <- as.data.frame(mil) # convert to dataframe

mil <- mil[mil$INCIDENT_YEAR != 2016,]
mil <- mil[complete.cases(mil),] # Remove NAs

# save result as .rds file
saveRDS(mil, file="rds_data/mil.rds")

mil <- mil[mil$INCIDENT_YEAR >= 1990,]
mil <- mil[mil$INCIDENT_YEAR <= 2015,]


mili <- mil %>% group_by(DAMAGE) %>% 
  summarise(Number = sum(Number)) # get data by year and time of day
mili <- as.data.frame(mili) # convert to dataframe

# exlude no damage cases
mili <- mili[which(mili$DAMAGE != "N"),]

p <- plot_ly(mili, x = ~DAMAGE, y = ~Number, type = 'bar',
             marker =list(color = heat.colors(6,1))) %>%
  layout(title = "Damage to Military aircraft Vs Number of incidents",
         xaxis = list(title = "Damage to Military aircraft"),
         yaxis = list(title = "Number of incidents"))
print(p)

# We know that in most of the cases there is no damage to the aircraft and from the
# the plot we can say that in case of damage, most of the time damage is minor.



##################### Bird Species #####################################################

birds <- data.frame(matrix(ncol = 4, nrow = nrow(df))) # create an empty dataframe to
# store information about bird species involved
colnames(birds) <- c('Year', 'Bird_Name', 'Bird_Size', 'Damage')

birds$Year <- df$INCIDENT_YEAR
birds$Bird_Name <- df$SPECIES
birds$Bird_Size <- df$SIZE
birds$Damage <- df$DAMAGE

birds <- birds[birds$Year != 2016,]
birds <- birds[complete.cases(birds),] # Remove NAs
dim(birds)

birds <- birds[birds$Bird_Name != 'Unknown bird - large',]
birds <- birds[birds$Bird_Name != 'Unknown bird - medium',]
birds <- birds[birds$Bird_Name != 'Unknown bird - small',]
birds <- birds[birds$Bird_Name != 'Unknown bird',]
length(unique(birds$Bird_Name))
unique(birds$Year)
saveRDS(birds, file="rds_data/birds.rds")

birds <- birds[birds$Year >= 1990,]
birds <- birds[birds$Year <= 2015,]

bird <- birds %>% group_by(Bird_Name) %>% 
  summarise(Number = n()) # get data by year and time of day
bird <- as.data.frame(bird) # convert to dataframe

bird <- bird[order(-bird$Number),] 
head(bird)

###         Word Cloud on Bird Species Names
# The code used to make word cloud is adopted from the following link:
# Credit: https://georeferenced.wordpress.com/2013/01/15/rwordcloud/

birdsText = Corpus(VectorSource(as.character(toString(birds$Bird_Name))))

wordcloud(birdsText, max.words=200, random.order=FALSE, rot.per=0.2, 
          colors=brewer.pal(8, "Dark2"))

################ Bird Size vs Number of Incidents ################################
#' Classification of bird:
#' Small:	Less than 1000 gm, Medium:	1 kg to 2kg, Large:	more than 2 kg

birdMass <- data.frame(matrix(ncol = 3, nrow = nrow(df))) # create an empty dataframe 
# to store information about the size of birds and damage
colnames(birdMass) <- c('Year', 'b_weight', 'damage')

birdMass$b_weight <- df$SIZE
birdMass$Year <- df$INCIDENT_YEAR
birdMass$damage <- df$DAMAGE
birdMass <- birdMass[complete.cases(birdMass$b_weight),] # Remove NAs

class(birdMass)
head(birdMass)
unique(birdMass$b_weight)

birdMass <- birdMass %>% group_by(Year, b_weight) %>% 
  summarise(Number = n()) # get data by year and aircraft weight

birdMass <- as.data.frame(birdMass) # convert to dataframe

birdMass <- birdMass[birdMass$Year != 2016,]
# save result as .rds file
saveRDS(birdMass, file="rds_data/birdMass.rds")

birdMass <- birdMass[birdMass$Year >= 1990,]
birdMass <- birdMass[birdMass$Year <= 2015,]


bw <- birdMass %>% group_by(b_weight) %>% 
  summarise(Number = sum(Number)) # get data by year and weight
bw <- as.data.frame(bw) # convert to dataframe

p <- plot_ly(bw, x = ~b_weight, y = ~Number, type = 'bar',
             marker =list(color = heat.colors(3,1))) %>%
  layout(title = "Size of Bird vs Number of Incidents of bird strikes",
         yaxis = list(title = "Number of bird strikes"),
         xaxis = list(title = "Size of Bird"))
print(p)


# relation among bird size, aircraft size and amount of damage 

civil <- data.frame(matrix(ncol = 4, nrow = nrow(df))) # create an empty dataframe 
mil <- data.frame(matrix(ncol = 4, nrow = nrow(df))) # create an empty dataframe 
civil <- df[df$OPID != 'MIL', c('SIZE', 'AC_MASS', 'DAMAGE', 'INCIDENT_YEAR')] #subset data
mil <- df[df$OPID == 'MIL', c('SIZE', 'AC_MASS', 'DAMAGE', 'INCIDENT_YEAR')] #subset data

### civil aircraft
civil$DAMAGE <- as.character(civil$DAMAGE)
civil <- civil[civil$INCIDENT_YEAR != 2016,]
civil <- civil[complete.cases(civil),] # Remove NAs

# Expand damage classification code
civil$DAMAGE_LEVEL[which(civil$DAMAGE == 'N')] <- "No Damage"
civil$DAMAGE_LEVEL[which(civil$DAMAGE == 'M?')] <- "Uncertain"
civil$DAMAGE_LEVEL[which(civil$DAMAGE == 'M')] <- "Minor"
civil$DAMAGE_LEVEL[which(civil$DAMAGE == 'S')] <- "Substantial"
civil$DAMAGE_LEVEL[which(civil$DAMAGE == 'D')] <- "Destroyed"
# exlude no damage cases
civil <- civil[which(civil$DAMAGE != "No Damage"),]

colnames(civil) <- c('Bird_Size', 'Aircraft_Size', 'Damage', 'Year')

# save result as .rds file
saveRDS(civil, file="rds_data/b_civil.rds")
head(civil)

civil <- civil[civil$Year >= 1990,]
civil <- civil[civil$Year <= 2015,]

b_civil <- civil %>% group_by(Bird_Size, Aircraft_Size, Damage) %>%
  summarise(Number = n())

b_civil <- as.data.frame(b_civil) # convert to dataframe
head(b_civil)
str(b_civil$Bird_Size)
pt1 = "Cases of damage against different sizes of birds and different sizes of aircraft"
#pt2 = "Sizes of birds: Large > 2Kg, Medium 1Kg to 2kg, Small < 1Kg"
pt2 = "Each facet corresponds to different bird size and"
pt3 = "y-axis in each facet corresponds to size of aircraft"

g <- ggplot(data = b_civil, aes(x= Aircraft_Size, y = Number, fill = Damage)) + 
  geom_bar(position="dodge", stat="identity") + facet_wrap(~Bird_Size) +
  theme_bw() +
  labs(title = pt1, x = paste(pt2, pt3, sep = " "), y = "Number of Incidents")
ggplotly(g)

### military aircraft
mil$DAMAGE <- as.character(mil$DAMAGE)
mil <- mil[mil$INCIDENT_YEAR != 2016,]
mil <- mil[complete.cases(mil),] # Remove NAs

# exlude no damage cases
mil <- mil[which(mil$DAMAGE != "N"),]

colnames(mil) <- c('Bird_Size', 'Aircraft_Size', 'Damage', 'Year')

# save result as .rds file
saveRDS(mil, file="rds_data/b_mil.rds")

mil <- mil[mil$Year >= 1990,]
mil <- mil[mil$Year <= 2015,]


b_mil <- mil %>% group_by(Bird_Size, Aircraft_Size, Damage) %>%
  summarise(Number = n())
b_mil <- as.data.frame(b_mil) # convert to dataframe

pt1 = "Cases of damage against different sizes of birds and different sizes of aircraft"
#pt2 = "Sizes of birds: Large > 2Kg, Medium 1Kg to 2kg, Small < 1Kg"
pt2 = "Each facet corresponds to different bird size and"
pt3 = "y-axis in each facet corresponds to size of aircraft"

g <- ggplot(data = b_mil, aes(x= Aircraft_Size, y = Number, fill = Damage)) + 
  geom_bar(position="dodge", stat="identity") + facet_wrap(~Bird_Size) +
  theme_bw() +
  labs(title = pt1, x = paste(pt2, pt3, sep = " "), y = "Number of Incidents")
ggplotly(g)
unique(b_mil$Damage)
