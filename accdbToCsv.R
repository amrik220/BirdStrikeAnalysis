# Author: Amrik Singh
# Project Title: Analysis of bird strikes reported in USA from 1990 to 2019
# (FAA Wildlife Strike Database, from 1990 to 2019)

#' Code Description:
#' 1. Read "wildlife.accdb" file (MS Access database file) in R and save 
#'    output in "birdstrikesAll.rds" file.
#' 2. Describe attributes/column names in the database (provided in file read_me.xls)
#' 3. Check of errors and missing values in the database
#' 
#' For more information, please read  README.md included in the repository
#' 
#' Download wildlife.accdb database from the following links
#' https://wildlife.faa.gov/search

# You can connect to .accdb database using one of the following methods:
# 1. Using odbc connection via Data Source Name (DSN) and R: This method is explained
# below.
# 2. Using MS-Access: Use MS Access to open the database file wildlife.accdb and then 
# export the data in .csv or . xls format.

#' First, we need to set-up a new Data Source Name (DSN), which is a connection to a 
#' specific database. Follow these steps:
#' 1. Go to control panel -> administrative services -> odbc32/odbc32 or directly search for 
#' odbc32/odbc32 in windows search
#' 
#' 2. odbc32/odbc32 -> in userDSN add -> microsoft access db (mdb,accdb) -> name it as "wildlife"
#' and provide the path of the file wildlife.accdb
#' 
#' Now, go to R strudio -> tools -> global options -> change R software to 32/64 bit software
#' install.packages('RODBC') # if not already installed 

library(RODBC) # load the package
library(dplyr)
library(readxl)

setwd(choose.dir())
getwd()
wildlife <- odbcConnect("wildlife") # Connect to Access database using DSN we created above

sqlTables(wildlife) # to view data table names. Look for tables that are not system type.

#' Finally, read the data from the database using the code lines below:
#' loads the table called 'STRIKE_REPORTS' in the original Access file
df <- sqlFetch(wildlife, "STRIKE_REPORTS")

View(df[0:10,]) # preview data
# head(df)
str(df) # view structure of data
dim(df) # check the dimensions of the data

data_descr <- list() # initialize an empty list to store data from 
file <- choose.files() # Select read_me.xls file

# Description of column names
data_descr[[1]] <- read_excel(file, sheet = 1)
# Description of Engine Models
data_descr[[2]] <- read_excel(file, sheet = 2)
# Description of Aircraft Types
data_descr[[3]] <- read_excel(file, sheet = 3) 
# Engine Position codes
data_descr[[4]] <- read_excel(file, sheet = 4)

View(data_descr[[1]]) # View explanation of Column Name and Codes
View(data_descr[[2]]) # view description of Engine Models
View(data_descr[[3]]) # view description of Aircraft types
View(data_descr[[4]]) # view description of Engine Position


df <- data.frame(sapply(df, trimws)) # strip leading and trailing white spaces
cat("Total number of incidents of bird strike: ", dim(df)[1])
cat("Total number of attributes for a bird strike: ", dim(df)[2])

# Filter data from Jan 1990 to Apr 2020

df <- df %>% filter(INCIDENT_YEAR >= 1990, INCIDENT_YEAR <= 2019)
dim(df)
df <- type.convert(df, na.strings = c("NA", ''), as.is = TRUE, dec = ".", numerals = "no.loss")
str(df)

# check for missing values in columns
sapply(df, function(x) sum(is.na(x)))
# There are lots of NAs in the data. Some of them are missing values, while others are 
# where the data is not applicable. For instance, 'time_of_day' column has many missing 
# values, whereas in 'damage', a NA value means there was no damage to the plane.

# Check for error and missing values in important columns

# AC_CLASS: Type of aircraft (see description_of_Attributes for more details)
table(df$AC_CLASS) # There are missing values. But no error observed. We'll exclude 
sum(table(df$AC_CLASS)) # missing values for visualization
dim(df)

#' TYPE_ENG	Type of power A = reciprocating engine (piston): B = Turbojet: C = Turboprop: 
#' D = Turbofan: E = None (glider): F = Turboshaft (helicopter): Y = Other
table(df$TYPE_ENG) # There are missing values. But no error observed. We'll exclude 
sum(table(df$TYPE_ENG)) # missing values for visualization
dim(df)

#' AC_MASS	1 = 2,250 kg or less: 2 = ,2251-5700 kg: 3 = 5,701-27,000 kg: 
#' 4 = 27,001-272,000 kg: 5 = above 272,000 kg
table(df$AC_MASS)
sum(table(df$AC_MASS)) # There are missing values. But no error observed. We'll exclude 
# missing values for visualization

# TIME_OF_DAY	or Light conditions
table(df$TIME_OF_DAY) # There are missing values. But no error observed. We'll exclude 
sum(table(df$TIME_OF_DAY))# missing values for visualization

# STATE	State
table(df$STATE) # There are missing values. But no error observed. We'll exclude 
# missing values for visualization
length(unique(df$STATE))

# PHASE_OF_FLT	Phase of flight during which strike occurred
table(df$PHASE_OF_FLIGHT) # There are missing values. But no error observed. We'll exclude 
sum(table(df$PHASE_OF_FLIGHT)) # missing values for visualization

#' DAMAGE: Blank	Unknown
#' M = minor	When the aircraft can be rendered airworthy by simple repairs or 
#' replacements and an extensive inspection is not necessary.
#' M? = uncertain level	The aircraft was damaged, but details as to the extent of the 
#' damage are lacking.
#' S = substantial	When the aircraft incurs damage or structural failure which adversely 
#' affects the structure strength, performance or flight characteristics of the aircraft 
#' and which would normally require major repair or replacement of the affected component.
#' D = Destroyed	When the damage sustained makes it inadvisable to restore the aircraft 
#' to an airworthy condition.
table(df$DAMAGE) # There are missing values. But no error observed. We'll exclude 
# missing values for visualization
sum(table(df$DAMAGE))
table(df$INDICATED_DAMAGE)


# EFFECT	Effect on flight
table(df$EFFECT) # Check case when flight was affected
sum(complete.cases(df$EFFECT_OTHER)) # other effects on flight, other then above


# SPECIES	Common name for bird or other wildlife
table(df$SPECIES)
cat("Diffrent number of bird species invloved:",length(unique(df$SPECIES)))

# NUM_STRUCK	Number of birds/wildlife struck
table(df$NUM_STRUCK)

#' SIZE	Size of bird as reported by pilot is a relative scale. Entry should reflect the 
#' perceived size as opposed to a scientifically determined value. If more than one 
#' species was struck, larger bird is entered.
#' Small:	Less than 1000 gm
#' Medium:	1 kg to 2kg
#' Large:	more than 2 kg
table(df$SIZE)

#' AOS	Time aircraft was out of service in hours.
table(na.omit(df$AOS))
length(na.omit(df$AOS))

# COST_REPAIRS_INFL_ADJ	Costs adjusted for inflation
sum(na.omit(df$COST_REPAIRS_INFL_ADJ))

# COST_OTHER_INFL_ADJ	Other cost adjusted for inflation
sum(na.omit(df$COST_OTHER_INFL_ADJ))

# NR_INJURIES	Number of people injured
table(df$NR_INJURIES)

# NR_FATALITIES	Number of human fatalities
table(df$NR_FATALITIES) # we need to replace zeros in this column with NAs. We'll do this
# when we do data visualization.


dim(df)
cat("Total number of incidents of bird strikes: ", dim(df)[1])
cat("Total number of attributes for a bird strikes: ", dim(df)[2])


if(!dir.exists(file.path(getwd(), 'database'))){
  dir.create(file.path(getwd(), 'database'))}
saveRDS(df, file="database/birdStrikesAll.rds")
# save the data in .CSV format
# write.csv(df, file = "birdStrikesAll.csv", row.names=FALSE)
rm(df) # remove dataframe from memory
