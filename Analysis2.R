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
# str(remarksCorpus)

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

dfAtype <- df %>% select(INCIDENT_YEAR, AC_CLASS)
colnames(dfAtype) <- c('Year', 'Aircraft_Type')

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
dfAtype$Aircraft_Type[is.na(dfAtype$Aircraft_Type)] <- "Unknown"
head(dfAtype)

# dfAtype <- dfAtype[dfAtype$Year != 2020,]
# save result as .rds file
saveRDS(dfAtype, file="rds_data/dfAtype.rds")

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
print(p9)
# From the bar chart above, we can say that majority of the bird strikes occurs
# with airplanes.



################ Aircraft Size based of weight vs Bird Strike ##########################

#' AC_MASS	1 = 2,250 kg or less: 2 = ,2251-5700 kg: 3 = 5,701-27,000 kg: 
#' 4 = 27,001-272,000 kg: 5 = above 272,000 kg
AC_MASS <- df %>% select(INCIDENT_YEAR, AC_MASS)
colnames(AC_MASS) <- c('Year', 'Aircraft_Weight')

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

# save result as .rds file
saveRDS(AC_MASS, file="rds_data/AC_MASS.rds")


acw <- AC_MASS %>% group_by(Aircraft_Weight) %>% 
  summarise(Number = sum(Number)) 
acw <- as.data.frame(acw) # 
acw$Percentage <- round(acw$Number/sum(acw$Number)*100,2)
head(acw)

title = paste0("Size(weight) of aircraft vs Number of bird strikes<br>from ",
               yearStart, " to ", yearEnd)
p10 <- plot_ly(acw, x = ~Aircraft_Weight, y = ~Number, type = 'bar',
             text = ~paste0(Number, ' (~', Percentage,'%)'), textposition = 'outside',
             hoverinfo = 'x', marker =list(color = rainbow(nrow(acw)))) %>%
  layout(title = title, margin = list(t=60),
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
table(civil$DAMAGE_LEVEL)

civil <- civil %>% group_by(INCIDENT_YEAR, DAMAGE_LEVEL) %>% 
  summarise(Number = n()) # get data by year and DAMAGE classification

civil <- as.data.frame(civil) # convert to dataframe
colnames(civil) <- c('Year', 'Damage', 'Number')
class(civil)
head(civil)
names(civil)

civil$Damage[which(civil$Damage == 'N')] <- "No Damage"
civil$Damage[which(civil$Damage == 'M?')] <- "Uncertain"
civil$Damage[which(civil$Damage == 'M')] <- "Minor"
civil$Damage[which(civil$Damage == 'S')] <- "Substantial"
civil$Damage[which(civil$Damage == 'D')] <- "Destroyed"
# missing values/no reported damage level treated as no damage 
civil$Damage[is.na(civil$Damage)] <- "No Damage" 

# save result as .rds file
saveRDS(civil, file="rds_data/civil.rds")

civi <- civil %>% group_by(Damage) %>% 
  summarise(Number = sum(Number)) 
civi <- as.data.frame(civi) 
table(civi$Damage[is.na(civi$Damage)])

# exlude no damage cases
civi <- civi[which(civi$Damage != "No Damage"),]
sum(civi$Number)
sum(civil$Number)
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
print(p11)


###### military aircraft
military <- df[df$OPID == 'MIL', c('COST_REPAIRS_INFL_ADJ', 'COST_OTHER_INFL_ADJ',
                               'DAMAGE_LEVEL', 'INCIDENT_YEAR', 'AC_MASS')]
class(military)
head(military)
table(military$DAMAGE_LEVEL)
military$DAMAGE_LEVEL <- as.character(military$DAMAGE_LEVEL)

military[,1:2][is.na(military[,1:2])] <- 0 # replace NAs
damageDollarsM <- sum(military$COST_REPAIRS_INFL_ADJ) + sum(military$COST_OTHER_INFL_ADJ)

cat("Total cost of damage/repairs in case of civil aircraft (in USD):", damageDollarsM)


military <- military %>% group_by(INCIDENT_YEAR, DAMAGE_LEVEL) %>% 
  summarise(Number = n()) # get data by year and DAMAGE classification
military <- as.data.frame(military) # convert to dataframe
colnames(military) <- c('Year', 'Damage', 'Number')
head(military)

military$Damage[which(military$Damage == 'N')] <- "No Damage"
military$Damage[which(military$Damage == 'D')] <- "Destroyed"
military$Damage[which(military$Damage == 'M?')] <- "Uncertain"
military$Damage[is.na(military$Damage)] <- "No Damage" 

sum(military$Number) + sum(civil$Number)

# save result as .rds file
saveRDS(military, file="rds_data/military.rds")


mili <- military %>% group_by(Damage) %>% 
  summarise(Number = sum(Number)) 
mili <- as.data.frame(mili) # 
# exlude no damage cases
mili <- mili[which(mili$Damage != "No Damage"),]
mili
sum(mili$Number)
sum(military$Number)
round(sum(mili$Number)/sum(military$Number)*100,2)
mili$Percentage <- round(mili$Number/sum(military$Number)*100, 2)
mili

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
print(p12)

##################### Bird Species #####################################################

bs <- df %>% select(INCIDENT_YEAR, SPECIES, SIZE, DAMAGE_LEVEL)
colnames(bs) <- c('Year', 'Bird_Name', 'Bird_Size', 'Damage')

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
p13 <- wordcloud2(bs[1:n, ], color = "random-dark", size = .8,
                 fontWeight = 'bold', maxRotation = pi/3, rotateRatio = 0.45,
                 hoverFunction = NULL, shape = 'star')
print(p13)

################ Bird Size vs Number of Incidents ################################
#' Classification of bird:
#' Small:	Less than 1000 gm, Medium:	1 kg to 2kg, Large:	more than 2 kg

birdMass <- df %>% select( INCIDENT_YEAR, SIZE, DAMAGE_LEVEL)
colnames(birdMass) <- c('Year', 'b_weight', 'damage')
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

title = paste0("Size of Bird vs Number of Incidents of bird strikes<br>from ",
               yearStart, " to ", yearEnd)
p14 <- plot_ly(bw, x = ~b_weight, y = ~Number, type = 'bar',
               text = ~paste0(Number, ' (~', Percentage,'%)'), textposition = 'outside',
               hoverinfo = 'x', marker =list(color = rainbow(nrow(bw)))) %>%
  layout(title = title, margin=list(t = 70),
         yaxis = list(title = "Number of bird strikes"),
         xaxis = list(title = "Size of Bird"))
print(p14)


# relation between bird size, aircraft size and amount of damage 

birdCivilDmg <- df[df$OPID != 'MIL', c('SIZE', 'AC_MASS', 'DAMAGE_LEVEL', 'INCIDENT_YEAR')] #subset data
head(birdCivilDmg)
colnames(birdCivilDmg) <- c('Bird_Size', 'Aircraft_Size', 'Damage', 'Year')

### civil aircraft
birdCivilDmg$Damage <- as.character(birdCivilDmg$Damage)
table(birdCivilDmg$Damage)
table(birdCivilDmg$Bird_Size)

# Expand damage classification code
birdCivilDmg$Damage[which(birdCivilDmg$Damage == 'N')] <- "No Damage"
birdCivilDmg$Damage[which(birdCivilDmg$Damage == 'D')] <- "Destroyed"
birdCivilDmg$Damage[which(birdCivilDmg$Damage == 'S')] <- "Substantial"
birdCivilDmg$Damage[which(birdCivilDmg$Damage == 'M')] <- "Minor"
birdCivilDmg$Damage[which(birdCivilDmg$Damage == 'M?')] <- "Uncertain"
birdCivilDmg$Damage[is.na(birdCivilDmg$Damage)] <- "No Damage" 

# exclude case with no damage and bird size unavailable 
birdCivilDmg <- birdCivilDmg[which(birdCivilDmg$Damage != "No Damage"),]
birdCivilDmg <- birdCivilDmg[!is.na(birdCivilDmg$Bird_Size),] 

dim(birdCivilDmg)
head(birdCivilDmg)
# save result as .rds file
saveRDS(birdCivilDmg, file="rds_data/birdCivilDmg.rds")

b_c_dmg <- birdCivilDmg %>% group_by(Bird_Size, Aircraft_Size, Damage) %>%
  summarise(Number = n())

b_c_dmg <- as.data.frame(b_c_dmg) # convert to dataframe
head(b_c_dmg)
b_c_dmg$Bird_Size <- factor(b_c_dmg$Bird_Size, levels = 
                              c('Small', 'Medium', 'Large'))
title = "Civil aircraft damage vs Bird size"
# title = paste0("Civil aircraft damage vs Bird size<br>from ", yearStart, ' to ', yearEnd, '<br>')
x = paste0("x-axis represents aircraft size and each subplot corresponds to a bird size")
p15 <- ggplot(data = b_c_dmg, aes(x= Aircraft_Size, y = Number, fill = Damage)) + 
  geom_bar(position="dodge", stat="identity") + facet_wrap(~Bird_Size) +
  theme_bw() + labs(title = title, x = x, y = "Number of Incidents") +
  theme(plot.title = element_text(hjust = 0.5))
ggplotly(p15)


####### military aircraft
birdMilDmg <- df[df$OPID == 'MIL', c('SIZE', 'AC_MASS', 'DAMAGE_LEVEL', 'INCIDENT_YEAR')] #subset data
head(birdMilDmg)
colnames(birdMilDmg) <- c('Bird_Size', 'Aircraft_Size', 'Damage', 'Year')
birdMilDmg <- birdMilDmg[!is.na(birdMilDmg$Bird_Size),] # Remove NAs

table(birdMilDmg$Damage)
table(birdMilDmg$Bird_Size)
table(birdMilDmg$Aircraft_Size)

# exclude no damage cases
birdMilDmg <- birdMilDmg[which(birdMilDmg$Damage != "N"),]
birdMilDmg$Damage[which(birdMilDmg$Damage == 'D')] <- "Destroyed"
birdMilDmg$Damage[which(birdMilDmg$Damage == 'M?')] <- "Uncertain"

# save result as .rds file
saveRDS(birdMilDmg, file="rds_data/birdMilDmg.rds")

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

