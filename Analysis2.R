# Author: Amrik Singh
# Project Title: Analysis of bird strikes reported in USA from 1990 to 2019
# (FAA Wildlife Strike Database, from 1990 to 2019)

#' Code Description: Data processing to explore, visualize, and analyze data
#' File Name: Analysis2.R # Aircraft and Bird species
#' For more information, please read  README.md included in the repository

# Import required packages
library(dplyr)
library(ggplot2) 
library(ggthemes)
library(plotly)
library(tm)
library(hunspell)
library(wordcloud2)

getwd()
setwd(choose.dir())
# read data from birdStrikesAll.rds file.
df <- readRDS("database/birdStrikesAll.rds")
# df <- read.csv("birdStrikesAll.csv", strip.white = TRUE, na.strings = c("NA",""))
# str(df)

#################         Word Cloud on Remarks entered in database   #################
# remarksCorpus = VCorpus(VectorSource(df$REMARKS))
remarksCorpus = Corpus(VectorSource(df$REMARKS))
remarksCorpus = tm_map(remarksCorpus, content_transformer(tolower))
remarksCorpus = tm_map(remarksCorpus, stripWhitespace)
remarksCorpus = tm_map(remarksCorpus, removePunctuation)
remarksCorpus = tm_map(remarksCorpus, removeNumbers)
remarksCorpus = tm_map(remarksCorpus, removeWords, stopwords())
remarksCorpus = tm_map(remarksCorpus, stemDocument)
str(remarksCorpus)

# processing for wordCloud
dtm = DocumentTermMatrix(remarksCorpus)
dtm = removeSparseTerms(dtm, 0.9999)
dataset = as.matrix(dtm)
remarksVector = sort(colSums(dataset),decreasing=TRUE)
rm(dataset)
wordsALL = as.data.frame(remarksVector)
wordsALL$word <- row.names(wordsALL)
names(wordsALL) <- c('freq', 'word')
class(wordsALL)
head(wordsALL)

wordsALL <- cbind(as.data.frame(wordsALL$word), wordsALL$freq)
names(wordsALL) <- c('word', 'freq')
head(wordsALL)

"
# typos <- hunspell_check(wordsALL$word)
# typos <- as.data.frame(typos)
# class(typos)
# head(typos)
# 
# typos$word <- wordsALL$word
# table(typos$typos)
# typos$suggest <- sapply(wordsALL$word, function(x){ 
#   ifelse(hunspell_check(x), x, hunspell_suggest(x)[[1]][1])})
# wordsALL$word <- sapply(wordsALL$word, function(x){ 
#   ifelse(hunspell_check(x), x, hunspell_suggest(x)[[1]][1])})
"

typos <- read.csv('database/typos.csv')

wordsALL$word <- typos$suggest 
wordsALL[duplicated(wordsALL$word),]
wordsALL <- wordsALL %>% group_by(word) %>% summarise(freq = sum(freq))
wordsALL <- wordsALL %>% filter(nchar(word) > 2)
wordsALL <- wordsALL %>% filter(freq/max(freq) > 0.01)
wordsALL <- wordsALL[order(wordsALL$freq, decreasing = TRUE),]

head(wordsALL)
dim(wordsALL)

n=200
# # Wordcloud plot using wordcloud2()
# getwd()
# wordcloud2(w, color = "random-dark", figPath = "www/t.png")
p8 <- wordcloud2(wordsALL[1:n, ], color = "random-dark", size = .8,
           fontWeight = 'bold', maxRotation = pi/3, rotateRatio = 0.45,
           hoverFunction = NULL, shape = 'diamond')
print(p8)

getwd()
if(!dir.exists(file.path(getwd(), 'rds_data'))){
  dir.create(file.path(getwd(), 'rds_data'))}
# save result as .rds file
saveRDS(wordsALL, file="rds_data/wordsALL.rds")


################ Aircraft Type vs Bird Strike ############################################

dfAtype <- data.frame(matrix(ncol = 2, nrow = nrow(df))) # create an empty dataframe 
# to store information about type of aircraft
colnames(dfAtype) <- c('Year', 'Aircraft_Type')

dfAtype$Aircraft_Type <- as.character(df$AC_CLASS)
dfAtype$Year <- df$INCIDENT_YEAR
head(dfAtype)
dfAtype[is.na(dfAtype$Aircraft_Type), "Aircraft_Type"] <- 'Unknown'

class(dfAtype)
unique(dfAtype$Aircraft_Type)

dfAtype <- dfAtype %>% group_by(Year, Aircraft_Type) %>% 
  summarise(Number = n()) # get data by year and aircraft type

dfAtype <- as.data.frame(dfAtype) # convert to dataframe

dfAtype$Aircraft_Type[which(dfAtype$Aircraft_Type == 'A')] <- "Airplane"
dfAtype$Aircraft_Type[which(dfAtype$Aircraft_Type == 'A/B')] <- "Airplane"
dfAtype$Aircraft_Type[which(dfAtype$Aircraft_Type == 'B')] <- "Helicopter"
dfAtype$Aircraft_Type[which(dfAtype$Aircraft_Type == 'C')] <- "Glider"
dfAtype$Aircraft_Type[which(dfAtype$Aircraft_Type == 'J')] <- "Ultrlight"
dfAtype$Aircraft_Type[which(dfAtype$Aircraft_Type == 'NULL')] <- "Military"
head(dfAtype)

# dfAtype <- dfAtype[dfAtype$Year != 2020,]
# save result as .rds file
saveRDS(dfAtype, file="rds_data/dfAtype.rds")

aType <- dfAtype %>% group_by(Aircraft_Type) %>% 
  summarise(Number = sum(Number)) # get data by year and time of day
aType <- as.data.frame(aType) # convert to dataframe
aType$Percentage <- round(aType$Number/sum(aType$Number)*100,2)

p9 <- plot_ly(aType, x = ~Aircraft_Type, y = ~Number, type = 'bar',
             text = ~paste0(Number, ' (~', Percentage,'%)'), textposition = 'outside',
             hoverinfo = 'x', marker =list(color = rainbow(nrow(aType)))) %>%
  layout(title = "Most common category of aircraft invloved in bird strikes",
         yaxis = list(title = "Number of bird strikes"),
         xaxis = list(title = "Category of Aircraft"))
print(p9)
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

AC_MASS[is.na(AC_MASS$Aircraft_Weight), 'Aircraft_Weight'] <- 'Unknown'

class(AC_MASS)
unique(AC_MASS$Aircraft_Weight)
table(AC_MASS$Aircraft_Weight)

AC_MASS <- AC_MASS %>% group_by(Year, Aircraft_Weight) %>% 
  summarise(Number = n()) # get data by year and aircraft weight
AC_MASS <- as.data.frame(AC_MASS) # convert to dataframe
head(AC_MASS)

# str(acw$Aircraft_Weight)
AC_MASS$Aircraft_Weight[which(AC_MASS$Aircraft_Weight == 1)] <- "2250 kg or less"
AC_MASS$Aircraft_Weight[which(AC_MASS$Aircraft_Weight == 2)] <- "2251-5700 kg"
AC_MASS$Aircraft_Weight[which(AC_MASS$Aircraft_Weight == 3)] <- "5701-27000 kg"
AC_MASS$Aircraft_Weight[which(AC_MASS$Aircraft_Weight == 4)] <- "27001-272000 kg"
AC_MASS$Aircraft_Weight[which(AC_MASS$Aircraft_Weight == 5)] <- "above 272000 kg"
AC_MASS$Aircraft_Weight[which(AC_MASS$Aircraft_Weight == 'NULL')] <- "Unknown"

# AC_MASS <- AC_MASS[AC_MASS$Year != 2020,]
# save result as .rds file
saveRDS(AC_MASS, file="rds_data/AC_MASS.rds")


acw <- AC_MASS %>% group_by(Aircraft_Weight) %>% 
  summarise(Number = sum(Number)) # get data by year and time of day
acw <- as.data.frame(acw) # convert to dataframe
acw$Percentage <- round(acw$Number/sum(acw$Number)*100,2)
head(acw)

p10 <- plot_ly(acw, x = ~Aircraft_Weight, y = ~Number, type = 'bar',
             text = ~paste0(Number, ' (~', Percentage,'%)'), textposition = 'outside',
             hoverinfo = 'x', marker =list(color = rainbow(nrow(acw)))) %>%
  layout(title = "Size of aircraft based on Weight vs Number of bird strikes",
         yaxis = list(title = "Number of bird strikes"),
         xaxis = list(title = "Weight of the aircraft"))
print(p10)


################### Dollar value of damage to aircraft    ###########################3

#' Damage (Civil)- M	Minor, M?	Uncertain S	Substantial, D	Destroyed
#' Damage (Military)
#' Class A	Over $2,000,000, Class B	$500,000 - $2,000,000
#' Class C	$50,000 - Less than $500,000, Class D $10000 - $50000, Class E Less than 10000

civil <- subset(df, OPID != 'MIL', select = c('COST_REPAIRS_INFL_ADJ', 'COST_OTHER_INFL_ADJ',
                                'DAMAGE_LEVEL', 'INCIDENT_YEAR', 'AC_MASS')) # subset data
civil[,1:2][is.na(civil[,1:2])] <- 0 # replace NAs

damageDollars <- sum(civil$COST_REPAIRS_INFL_ADJ) + sum(civil$COST_OTHER_INFL_ADJ)

cat("Total cost of damage/repairs in case of civil aircraft (in USD):", damageDollars)

### civil aircraft
class(civil)
head(civil)
names(civil)
table(civil$DAMAGE_LEVEL)

civi$DAMAGE_LEVEL[is.na(civi$DAMAGE_LEVEL)]

civil$DAMAGE_LEVEL <- as.character(civil$DAMAGE_LEVEL)
#civil <- civil[civil$DAMAGE_LEVEL %in% c("M", "M?", "S", "D", "N"),]

civil <- civil %>% group_by(INCIDENT_YEAR, DAMAGE_LEVEL) %>% 
  summarise(Number = n()) # get data by year and DAMAGE classification

civil <- as.data.frame(civil) # convert to dataframe

civil$DAMAGE_LEVEL[which(civil$DAMAGE_LEVEL == 'N')] <- "No Damage"
civil$DAMAGE_LEVEL[which(civil$DAMAGE_LEVEL == 'M?')] <- "Uncertain"
civil$DAMAGE_LEVEL[which(civil$DAMAGE_LEVEL == 'M')] <- "Minor"
civil$DAMAGE_LEVEL[which(civil$DAMAGE_LEVEL == 'S')] <- "Substantial"
civil$DAMAGE_LEVEL[which(civil$DAMAGE_LEVEL == 'D')] <- "Destroyed"
# missing values/no reported damage level treated as no damage 
civil$DAMAGE_LEVEL[is.na(civil$DAMAGE_LEVEL)] <- "No Damage" 

# save result as .rds file
saveRDS(civil, file="rds_data/civil.rds")

civi <- civil %>% group_by(DAMAGE_LEVEL) %>% 
  summarise(Number = sum(Number)) 
civi <- as.data.frame(civi) 

# exlude no damage cases
civi <- civi[which(civi$DAMAGE_LEVEL != "No Damage"),]
head(civi)
sum(civi$Number)
sum(civil$Number)
round(sum(civi$Number)/sum(civil$Number)*100,2)
civi$Percentage <- round(civi$Number/sum(civil$Number)*100, 2)

civi$DAMAGE_LEVEL <- factor(civi$DAMAGE_LEVEL, 
                            levels = c("Uncertain", "Minor", "Substantial", 'Destroyed'))

ap11 <- list(x = 'Minor', xref = 'x',
             y = civi[civi$DAMAGE_LEVEL == 'Minor', 'Number'], yerf = 'y',
             text = paste0('Percentage shown is <br>in respect of a total of <br>',
                           sum(civil$Number), ' bird strikes'),  align = 'left',
             showarrow = TRUE, arrowhead = 0, arrowwidth = 1.2,
             ax = 150, ay = 20, bordercolor = 'green'
)

p11 <- plot_ly(civi, x = ~DAMAGE_LEVEL, y = ~Number, type = 'bar',
               text = ~paste0(Number, ' (~', Percentage,'%)'), textposition = 'outside',
               hoverinfo = 'x', marker =list(color = rainbow(nrow(civi)))) %>%
  layout(title = "Cases of damage to civil aircraft", annotations = ap11,
         xaxis = list(title = 'Damage level'),
         yaxis = list(title = "Number of cases"))
print(p11)


###### military aircraft
mili <- df[df$OPID == 'MIL', c('COST_REPAIRS_INFL_ADJ', 'COST_OTHER_INFL_ADJ',
                               'DAMAGE_LEVEL', 'INCIDENT_YEAR', 'AC_MASS')]
class(mili)
head(mili)
table(mili$DAMAGE_LEVEL)
mili$DAMAGE_LEVEL <- as.character(mili$DAMAGE_LEVEL)

mili <- mili %>% group_by(INCIDENT_YEAR, DAMAGE_LEVEL) %>% 
  summarise(Number = n()) # get data by year and DAMAGE classification

mili <- as.data.frame(mili) # convert to dataframe

mili$DAMAGE_LEVEL[which(mili$DAMAGE_LEVEL == 'D')] <- "Destroyed"
mili$DAMAGE_LEVEL[which(mili$DAMAGE_LEVEL == 'M?')] <- "Uncertain"

# save result as .rds file
saveRDS(mili, file="rds_data/mili.rds")

mil <- mili %>% group_by(DAMAGE_LEVEL) %>% 
  summarise(Number = sum(Number)) 
mil <- as.data.frame(mil) # convert to dataframe
# exlude no damage cases
mil <- mil[which(mil$DAMAGE_LEVEL != "N"),]
mil
sum(mil$Number)
sum(mili$Number)
round(sum(mil$Number)/sum(mili$Number)*100,2)
mil$Percentage <- round(mil$Number/sum(mil$Number)*100, 2)
mil

p12 <- plot_ly(mil, x = ~DAMAGE_LEVEL, y = ~Number, type = 'bar',
               text = ~paste0(Number, ' (~', Percentage,'%)'), textposition = 'outside',
               hoverinfo = 'x', marker =list(color = rainbow(nrow(mil)))) %>%
  layout(title = "Cases of damage to military aircraft",
         xaxis = list(title = 'Damage classifications of military aircraft'),
         yaxis = list(title = "Number of cases"))
print(p12)


##################### Bird Species #####################################################

bs <- data.frame(matrix(ncol = 4, nrow = nrow(df))) # create an empty dataframe to
# store information about bird species involved
colnames(bs) <- c('Year', 'Bird_Name', 'Bird_Size', 'Damage')

bs$Year <- df$INCIDENT_YEAR
bs$Bird_Name <- df$SPECIES
bs$Bird_Size <- df$SIZE
bs$Damage <- df$DAMAGE

bs <- bs[complete.cases(bs),] # Remove NAs
dim(bs)


bs <- bs[bs$Bird_Name != 'Unknown bird - large',]
bs <- bs[bs$Bird_Name != 'Unknown bird - medium',]
bs <- bs[bs$Bird_Name != 'Unknown bird - small',]
bs <- bs[bs$Bird_Name != 'Unknown bird',]
dim(bs)
length(unique(bs$Bird_Name))

bs <- bs %>% group_by(Bird_Name) %>% 
  summarise(Number = n()) # get data by year and time of day
bs <- as.data.frame(bs) # convert to dataframe

bs <- bs[order(-bs$Number),] 
head(bs, 10)
dim(bs)
saveRDS(bs, file="rds_data/bs.rds")

###         Word Cloud on Bird Species Names
n=200
# # Wordcloud plot using wordcloud2()
# getwd()
# wordcloud2(w, color = "random-dark", figPath = "www/t.png")
p13 <- wordcloud2(bs[1:n, ], color = "random-dark", size = .8,
                 fontWeight = 'bold', maxRotation = pi/3, rotateRatio = 0.45,
                 hoverFunction = NULL, shape = 'star')
print(p13)

################ Bird Size vs Number of Incidents ################################
#' Classification of bird:
#' Small:	Less than 1000 gm, Medium:	1 kg to 2kg, Large:	more than 2 kg

birdMass <- data.frame(matrix(ncol = 3, nrow = nrow(df))) # create an empty dataframe 
# to store information about the size of birds and damage
colnames(birdMass) <- c('Year', 'b_weight', 'damage')

birdMass$b_weight <- df$SIZE
birdMass$Year <- df$INCIDENT_YEAR
birdMass$damage <- df$DAMAGE
# birdMass <- birdMass[complete.cases(birdMass$b_weight),] # Remove NAs

class(birdMass)
head(birdMass)
unique(birdMass$b_weight)

birdMass <- birdMass %>% group_by(Year, b_weight) %>% 
  summarise(Number = n()) 

birdMass <- as.data.frame(birdMass) # convert to dataframe

birdMass[is.na(birdMass$b_weight), "b_weight"] <- 'Uncertain'

# save result as .rds file
saveRDS(birdMass, file="rds_data/birdMass.rds")

bw <- birdMass %>% group_by(b_weight) %>% 
  summarise(Number = sum(Number)) # get data by year and weight
bw <- as.data.frame(bw) # convert to dataframe

bw$Percentage <- round(bw$Number/sum(bw$Number)*100, 2)
bw$b_weight <- factor(bw$b_weight, levels = 
                        c('Small', 'Medium', 'Large', 'Uncertain'))

p14 <- plot_ly(bw, x = ~b_weight, y = ~Number, type = 'bar',
               text = ~paste0(Number, ' (~', Percentage,'%)'), textposition = 'outside',
               hoverinfo = 'x', marker =list(color = rainbow(nrow(bw)))) %>%
  layout(title = "Size of Bird vs Number of Incidents of bird strikes",
         yaxis = list(title = "Number of bird strikes"),
         xaxis = list(title = "Size of Bird"))
print(p14)



# relation between bird size, aircraft size and amount of damage 


civil1 <- data.frame(matrix(ncol = 4, nrow = nrow(df))) # create an empty dataframe 
civil1 <- df[df$OPID != 'MIL', c('SIZE', 'AC_MASS', 'DAMAGE_LEVEL', 'INCIDENT_YEAR')] #subset data
head(civil1)
colnames(civil1) <- c('Bird_Size', 'Aircraft_Size', 'Damage', 'Year')

### civil aircraft
civil1$Damage <- as.character(civil1$Damage)
table(civil1$Damage)

# Expand damage classification code
civil1$Damage[which(civil1$Damage == 'N')] <- "No Damage"
civil1$Damage[which(civil1$Damage == 'D')] <- "Destroyed"
civil1$Damage[which(civil1$Damage == 'S')] <- "Substantial"
civil1$Damage[which(civil1$Damage == 'M')] <- "Minor"
civil1$Damage[which(civil1$Damage == 'M?')] <- "Uncertain"
# exlude no damage cases
civil1 <- civil1[which(civil1$Damage != "No Damage"),]
civil1 <- civil1[!is.na(civil1$Bird_Size),] # Remove NAs
dim(civil1)
# save result as .rds file
saveRDS(civil1, file="rds_data/civil1.rds")
head(civil1)

b_civil <- civil1 %>% group_by(Bird_Size, Aircraft_Size, Damage) %>%
  summarise(Number = n())

b_civil <- as.data.frame(b_civil) # convert to dataframe
head(b_civil)
str(b_civil$Bird_Size)
pt1 = "Cases of damage vs different sizes of Birds and Civil Aircraft"
pt2 = "Each facet corresponds to a different bird size and"
pt3 = "bars in facets correspond different sizes of aircraft.."

p15 <- ggplot(data = b_civil, aes(x= Aircraft_Size, y = Number, fill = Damage)) + 
  geom_bar(position="dodge", stat="identity") + facet_wrap(~Bird_Size) +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = pt1, x = paste(pt2, pt3, sep = " "), y = "Number of Incidents")
ggplotly(p15)


####### military aircraft

mili1 <- data.frame(matrix(ncol = 4, nrow = nrow(df))) # create an empty dataframe 
mili1 <- df[df$OPID == 'MIL', c('SIZE', 'AC_MASS', 'DAMAGE_LEVEL', 'INCIDENT_YEAR')] #subset data
head(mili1)
colnames(mili1) <- c('Bird_Size', 'Aircraft_Size', 'Damage', 'Year')
mili1 <- mili1[!is.na(mili1$Bird_Size),] # Remove NAs

mili1$Damage <- as.character(mili1$Damage)
table(mili1$Damage)

# exlude no damage cases
mili1 <- mili1[which(mili1$Damage != "N"),]
mili1$Damage[which(mili1$Damage == 'D')] <- "Destroyed"
mili1$Damage[which(mili1$Damage == 'M?')] <- "Uncertain"

# save result as .rds file
saveRDS(mil, file="rds_data/mili1.rds")

b_mil <- mili1 %>% group_by(Bird_Size, Aircraft_Size, Damage) %>%
  summarise(Number = n())
b_mil <- as.data.frame(b_mil) # convert to dataframe

pt1 = "Cases of damage vs different sizes of Birds and Military Aircraft"
pt2 = "Each facet corresponds to a different bird size and"
pt3 = "bars in facets correspond different sizes of aircraft."

p16 <- ggplot(data = b_mil, aes(x= Aircraft_Size, y = Number, fill = Damage)) + 
  geom_bar(position="dodge", stat="identity") + facet_wrap(~Bird_Size) +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = pt1, x = paste(pt2, pt3, sep = " "), y = "Number of Incidents")
ggplotly(p16)