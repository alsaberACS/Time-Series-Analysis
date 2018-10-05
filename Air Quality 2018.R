#---------------Libraries-------------- (Step 1)
library(readxl)
library(plyr)
library(tidyr)
library(gtools)
library(data.table)
library(questionr)
library(lubridate)
library(Hmisc)
library(doBy)
library(xlsx)
library(xlsxjars)
library(rJava)
library(dplyr)
#---------------Loading Data---------- (Step 2)
Air2012 <- read_excel("Air_Quality_2012.xlsx")
Air2013 <- read_excel("Air_Quality_2013.xlsx")
Air2014 <- read_excel("Air_Quality_2014.xlsx")
Air2015 <- read_excel("Air_Quality_2015.xlsx")
Air2016 <- read_excel("Air_Quality_2016.xlsx")
Air2017 <- read_excel("Air_Quality_2017.xlsx")
AirWD <- read_excel("Weather_2011-2017_Daily2.xlsx")
AirQD <- read_excel("Air_2011-2017_Daily.xlsx")
#-------------Data Merging togother--------- (Step 3)
AIRH1 = rbindlist(list(Air2012, Air2013, Air2014, Air2015, Air2016, Air2017), fill = TRUE) #Compine all data
#----------Converting parameter units------- (Step 4)
AIRH1$V6 <- (AIRH1$V4)
AirQD$V6 <- (AirQD$V4)
AIRH1$V7 <- (AIRH1$V4)/(1000)
AirQD$V7 <- as.numeric(as.character(AirQD$V4))/(1000)
AIRH1$V4 <- ifelse(AIRH1$V5 == "ppb" , AIRH1$V7, AIRH1$V4) #Convert from ppb to ppm
AIRH1$V4 <- ifelse(AIRH1$V5 == "µg/m³" , AIRH1$V7, AIRH1$V4) #Convert from ppb to ppm
AirQD$V4 <- ifelse(AirQD$V5 == "ppb" , AirQD$V7, AirQD$V4) #Convert from ppb to ppm
AirQD$V4 <- ifelse(AirQD$V5 == "µg/m³" , AirQD$V7, AirQD$V4) #Convert from ppb to ppm
#----------Data Spreading Columns based on Pollutants------- (Step 5)
AIRH2 = AIRH1[,list(AIRH1$V1, AIRH1$V2, AIRH1$V3, AIRH1$V4)] #deselcting "Unit" and "Symbol"
AIRS1 = AirQD %>%
  select(V1, V2, V3, V4) #deselcting "Parameter Unit" 
AIRS2 = AirWD %>%
  select(V1, V3, S1, S2) #deselcting "Parameter Unit" 
AIRH3 <- AIRH2 %>% 
  tidyr::spread(key="V2", value="V4") #Spread data in term of variable name
AIRS <- AIRS1 %>% 
  tidyr::spread(key="V2", value="V4") #Spread data in term of variable name
AIRW <- AIRS2 %>% 
  tidyr::spread(key="S1", value="S2") #Spread data in term of variable name
#AIRH <- rename(AIR11, c("V1"="Location", "V3"="Date")) #Rename variables
#AIRS <- rename(AIR22, c("V1"="Location", "V2"="Paramter Name", "V3"="Date", "V4"="Parameter Value")) #Rename variables
#------Data Fixing time factor------- (Step 6)
#AIRX <- as.data.frame(AIRH3)
as.POSIXct(AIRH3$V3, format = "%y/%m/%d %H:%M:%S") #Define time for aggregate
AIRH3$Date <- AIRH3$V3
AIRH3$Date <- strftime(AIRH3$V3, format="%y/%m/%d") #Split time in term of Hours
#AIRH3$Date2 <- strftime(AIRH3$V3, format="%y/%m/%d") #Split time in term of Hours
AIRH3$Hours <- strftime(AIRH3$V3, format="%H") #Split time in term of Hours
AIRH3$Days <- strftime(AIRH3$V3, format="%d") #Split time in term of Days
AIRH3$Month <- strftime(AIRH3$V3, format="%m") #Split time in term of Month
AIRH3$Year <- strftime(AIRH3$V3, format="%y") #Split time in term of Month
#-----------Data Aggregating---------- (Step 7)

#AIRH <- aggregate(cbind("Benzene","Benzene","Carbon Monoxide", "meta and para-Xylene", "Nitric Oxide",
#                        "Nitrogen Dioxide", "Nitrogen Oxides", "Non-Methane Hydrocarbon",
#                       "ortho-Xylene", "Ozone", "Particulate Matter - less than 10 micrometers",
#                       "Styrene", "Sulfur Dioxide", "Toluene", "Location")~Date,
#                 data=AIR,FUN=mean, na.rm=TRUE, na.action=NULL)

AIRD <-aggregate(AIRH3, by=list(AIRH3$V1, AIRH3$Year, AIRH3$Month, AIRH3$Days, AIRH3$Date),FUN=mean, na.rm=TRUE)
AIRD2 <- select(AIRD,c(-V1,-V3))
AIRD2$V1 <- AIRD2$Group.1
AIRD2$V3 <- AIRD2$Group.5
as.POSIXct(AIRD2$V3, format = "%y/%m/%d")
#AIRD2$V3 <- strftime(AIRH2$V3, format="%y/%m/%d")
AIRW$V3 <- strftime(AIRW$V3, format="%y/%m/%d")
AIRD3 = left_join(AIRD2,AIRW,by = c("V1","V3"))
AIRM <-aggregate(AIRH3, by=list(AIRH3$V1, AIRH3$Year, AIRH3$Month),FUN=mean, na.rm=TRUE)
AIRY <-aggregate(AIRH3, by=list(AIRH3$V1, AIRH3$Year),FUN=mean, na.rm=TRUE)
#-----------Data Combination-------------- (Step 8)
AIRS$V3 <- strftime(AIRS$V3, format="%y/%m/%d")
AIRT = left_join(AIRS,AIRW,by = c("V1","V3"))
#-----------Data Export to Excel---------- (Step 9)
write.csv(AIRD, "Desktop/Heba Project ACS/Air Quality/AIRD.csv")