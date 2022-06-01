setwd("~/Desktop/Coursework_files")
getwd()

# Data preparation

# Library needed
library(vioplot)
library(tidyr)
library(ggplot2) 
library(dplyr)
library(ggstatsplot)
library(lubridate)
library(ggpubr)
library(caret)
library(Metrics)
library(superml)
library(caTools)
library(ranger)
library(rpart)
library(ROSE)

# Data import and preparation 
# Read all the data we will be using.

airports <- read.csv("./dataverse_files/airports.csv", sep=",", header = TRUE)
carriers <- read.csv("./dataverse_files/carriers.csv", sep=",", header = TRUE)
planes <- read.csv("./dataverse_files/plane-data.csv", sep=",", header = TRUE)
variable_descriptions <- read.csv("/Users/sangbin/Desktop/Coursework_files/dataverse_files/variable-descriptions.csv",sep=",", header = TRUE)

df_03 <- read.csv("./dataverse_files/2003.csv.bz2", sep=",", header = TRUE)
df_04 <- read.csv("./dataverse_files/2004.csv.bz2", sep=",", header = TRUE)
df_05 <- read.csv("./dataverse_files/2005.csv.bz2", sep=",", header = TRUE)

# Concatenate all the flight detail dataset into one dataset.
df <- rbind(df_03, df_04, df_05)

# Create full date column
df$Date <- make_date(df$Year,df$Month,df$DayofMonth)

# Make a copy of df just in case we need the original data.
# We are able to identify that if the flight is cancelled or diverted, there is no delay occurred.
# A flight diversion is when an aircraft is unable to arrive at its final destination.
# Hence we will filter out the cancelled and diverted flights for questions 1 to 4 just for now.

df_copy <- df
df_copy <- subset(df_copy, Cancelled == 0 & Diverted == 0)

# Drop cancellation code
df_copy <- subset(df_copy, select=-c(CancellationCode))


# Make new columns to indicate delay of arrival and departure.
# 0 = on time , 1 = delayed

df_copy$DelayedArr <- 0
df_copy$DelayedDep <- 0

# According to Bureau of Transportation statistics, a flight is counted as "on time" if the flight
# operated less than 15 minutes later than the scheduled time shown in the carriers' Computerized Reservations Systems (CRS).
# Thus, On-time means a flight that arrives less than 15 minutes after its announced arrival time.

df_copy$DelayedArr[df_copy$ArrDelay > 15] <- 1
df_copy$DelayedDep[df_copy$DepDelay > 15] <- 1

# Missing data (or Not a Number data) tells us that these features are not the cause of it. 
# Hence convert NaN values into 0

df_copy$CarrierDelay <- df_copy$CarrierDelay %>% replace_na(0)
df_copy$WeatherDelay <- df_copy$WeatherDelay %>% replace_na(0)
df_copy$NASDelay <- df_copy$NASDelay %>% replace_na(0)
df_copy$SecurityDelay <- df_copy$SecurityDelay %>% replace_na(0)
df_copy$LateAircraftDelay <- df_copy$LateAircraftDelay %>% replace_na(0)

# We can identify that there are no missing values except "ArrDelay" and "ActualElapsedTime".
summary(df_copy) 

# Hence, drop na values
df_copy <- df_copy %>% drop_na(ArrDelay)

#convert concatenated hours and minutes into separate columns
#Hour
df_copy$DepHour = df_copy$DepTime %/% 100L
df_copy$ArrHour = df_copy$ArrTime %/% 100L
df_copy$CRSDepHour = df_copy$CRSDepTime %/% 100L
df_copy$CRSArrHour = df_copy$CRSArrTime %/% 100L

# Convert Hour's data type numeric to character.
df_copy$DepHour <- as.character(df_copy$DepHour)
df_copy$ArrHour <- as.character(df_copy$ArrHour)
df_copy$CRSDepHour <- as.character(df_copy$CRSDepHour)
df_copy$CRSArrHour <- as.character(df_copy$CRSArrHour)

# Convert the time into 24-hour notation.
df_copy$DepHour = with(df_copy, ifelse(DepHour %in% c("24","0") , "00",
                                       ifelse(DepHour %in% c("25","1") , "01",
                                              ifelse(DepHour %in% c("26","2") , "02",
                                                     ifelse(DepHour %in% c("27","3") , "03",
                                                            ifelse(DepHour %in% c("28","4") , "04",
                                                                   ifelse(DepHour %in% c("29","5") , "05",
                                                                          ifelse(DepHour %in% c("6") , "06",
                                                                                 ifelse(DepHour %in% c("7") , "07",
                                                                                        ifelse(DepHour %in% c("8") , "08",
                                                                                               ifelse(DepHour %in% c("9") , "09",
                                                                                                      DepHour)))))))))))

df_copy$ArrHour = with(df_copy, ifelse(ArrHour %in% c("24","0") , "00",
                                       ifelse(ArrHour %in% c("25","1") , "01",
                                              ifelse(ArrHour %in% c("26","2") , "02",
                                                     ifelse(ArrHour %in% c("27","3") , "03",
                                                            ifelse(ArrHour %in% c("28","4") , "04",
                                                                   ifelse(ArrHour %in% c("29","5") , "05",
                                                                          ifelse(ArrHour %in% c("6") , "06",
                                                                                 ifelse(ArrHour %in% c("7") , "07",
                                                                                        ifelse(ArrHour %in% c("8") , "08",
                                                                                               ifelse(ArrHour %in% c("9") , "09",
                                                                                                      ArrHour)))))))))))

df_copy$CRSDepHour = with(df_copy, ifelse(CRSDepHour %in% c("24","0") , "00",
                                       ifelse(CRSDepHour %in% c("25","1") , "01",
                                              ifelse(CRSDepHour %in% c("26","2") , "02",
                                                     ifelse(CRSDepHour %in% c("27","3") , "03",
                                                            ifelse(CRSDepHour %in% c("28","4") , "04",
                                                                   ifelse(CRSDepHour %in% c("29","5") , "05",
                                                                          ifelse(CRSDepHour %in% c("6") , "06",
                                                                                 ifelse(CRSDepHour %in% c("7") , "07",
                                                                                        ifelse(CRSDepHour %in% c("8") , "08",
                                                                                               ifelse(CRSDepHour %in% c("9") , "09",
                                                                                                      CRSDepHour)))))))))))

df_copy$CRSArrHour = with(df_copy, ifelse(CRSArrHour %in% c("24","0") , "00",
                                       ifelse(CRSArrHour %in% c("25","1") , "01",
                                              ifelse(CRSArrHour %in% c("26","2") , "02",
                                                     ifelse(CRSArrHour %in% c("27","3") , "03",
                                                            ifelse(CRSArrHour %in% c("28","4") , "04",
                                                                   ifelse(CRSArrHour %in% c("29","5") , "05",
                                                                          ifelse(CRSArrHour %in% c("6") , "06",
                                                                                 ifelse(CRSArrHour %in% c("7") , "07",
                                                                                        ifelse(CRSArrHour %in% c("8") , "08",
                                                                                               ifelse(CRSArrHour %in% c("9") , "09",
                                                                                                      CRSArrHour)))))))))))

# Groupby distance to see the relationship between delays
Distance_delay <- df_copy %>%
  group_by(Distance) %>%
  summarise(Average_ArrDelay = mean(ArrDelay))

# Average arrival delay based on distance.
a<- ggplot(data = Distance_delay , mapping = aes(x = Distance, y = Average_ArrDelay)) +
  geom_line(stat='identity') +
  geom_text(mapping = aes(label = round(Average_ArrDelay,0), fontface = 'bold', vjust = -0.2), size = 2) +
  labs(title = "Distribution of arrival delay based on distance",x = "Distance", y ="Average ArrDelay (min)") + 
  theme(plot.title = element_text(hjust = 0.5),axis.text.x=element_text(angle=90, hjust=1))
# We can observe that shorter distanced flights 
# seem to have more fluctuation on delays compared to long distance flights

#===========================================================================================
# Frequency of the flight over distance
# We can observe that there are more of shorter distanced flights compared to long distance flights.
b<-ggplot(data = Distance_delay ,aes(x = Distance)) +
  geom_histogram(aes(y=..density..),
                 binwidth=100) +
  labs(title = "Frequency of the flight over distance",x = "Distance", y ="Frequency of the flight") + 
  theme(plot.title = element_text(hjust = 0.5),axis.text.x=element_text(angle=90, hjust=1))+
  geom_density(alpha=.2, fill="white")

# Zoom to see the clear view of the graphs
ggarrange(a,b, 
          labels = c("A", "B"),
          ncol = 2, nrow = 1)
# ==============================================================================================
# 1) When is the best time of day, day of the week, and time of year to fly to minimize delays?
# ==============================================================================================

# Deep dive into Delayed arrival flights

Delayed_flights <- filter(df_copy,df_copy$DelayedArr == 1)


#ArrDelay Boxplot(before)
c <- ggplot(data=Delayed_flights, aes(y=ArrDelay))+
  geom_boxplot(fill='slategrey',color='darkslategrey',width=0.3)+
  labs(title = "ArrDelay Boxplot(before)") + 
  theme(plot.title = element_text(hjust = 0.5),axis.text.x=element_text(angle=90, hjust=1))

#DepDelay Boxplot(before)
d <- ggplot(data=Delayed_flights, aes(y=DepDelay))+
  geom_boxplot(fill='slategrey',color='darkslategrey',width=0.3)+
  labs(title = "DepDelay Boxplot(before)") + 
  theme(plot.title = element_text(hjust = 0.5),axis.text.x=element_text(angle=90, hjust=1))

# Zoom to see the clear view of the graphs
ggarrange(c,d, 
          labels = c("C", "D"),
          ncol = 2, nrow = 1)

# identify first, third quartile and interquartile range to identify the lower and upper whisker
Q_ArrDelay <- quantile(Delayed_flights$ArrDelay, probs=c(.25, .75), na.rm = FALSE)
IQR_ArrDelay <- IQR(Delayed_flights$ArrDelay)
Upper_Whisker_ArrDelay <-  Q_ArrDelay[2]+1.5*IQR_ArrDelay
Lower_Whisker_ArrDelay <- Q_ArrDelay[1]-1.5*IQR_ArrDelay
Q_DepDelay <- quantile(Delayed_flights$DepDelay, probs=c(.25, .75), na.rm = FALSE)
IQR_DepDelay <- IQR(Delayed_flights$DepDelay)
Upper_Whisker_DepDelay <-  Q_DepDelay[2]+1.5*IQR_DepDelay
Lower_Whisker_DepDelay <- Q_DepDelay[1]-1.5*IQR_DepDelay

# Outliers will be any points below Lower_Whisker or 
# above Upper_Whisker hence sort them out from arrival delayed flights.
Delayed_flights<- subset(Delayed_flights, Delayed_flights$ArrDelay > Lower_Whisker_ArrDelay & 
                           Delayed_flights$ArrDelay < Upper_Whisker_ArrDelay &
                           Delayed_flights$DepDelay > Lower_Whisker_DepDelay & 
                           Delayed_flights$DepDelay < Upper_Whisker_DepDelay)

#ArrDelay Boxplot (after)
e <- ggplot(data=Delayed_flights, aes(y=ArrDelay))+
  geom_boxplot(fill='slategrey',color='darkslategrey',width=0.3)+
  labs(title = "ArrDelay Boxplot(after)") + 
  theme(plot.title = element_text(hjust = 0.5),axis.text.x=element_text(angle=90, hjust=1))

#DepDelay Boxplot (after)
f <- ggplot(data=Delayed_flights, aes(y=DepDelay))+
  geom_boxplot(fill='slategrey',color='darkslategrey',width=0.3)+
  labs(title = "DepDelay Boxplot(after)") + 
  theme(plot.title = element_text(hjust = 0.5),axis.text.x=element_text(angle=90, hjust=1))

# Zoom to see the clear view of the graphs
ggarrange(e,f, 
          labels = c("E", "F"),
          ncol = 2, nrow = 1)

#===========================================================

# Total number of flights per hour
total_flights_per_DepHour <- df_copy %>%
  group_by(DepHour) %>%
  summarise(Count = sum(DelayedArr))

g <- ggplot(data = total_flights_per_DepHour, mapping = aes(x = reorder(DepHour,-Count), y = Count)) +
  geom_bar(stat='identity',fill="steelblue") +
  geom_text(mapping = aes(label = round(Count,0), fontface = 'bold', vjust = -0.2), size = 2) +
  labs(title = "Total number of flights per hour",x = "Actual Departure Hour", y ="Number of flights") + 
  theme(plot.title = element_text(hjust = 0.5))

# Number of delayed flights per hour
delayed_flights_per_DepHour <- Delayed_flights %>%
  group_by(DepHour) %>%
  summarise(Count = sum(DelayedArr))

h <- ggplot(data = delayed_flights_per_DepHour, mapping = aes(x = reorder(DepHour,-Count), y = Count)) +
  geom_bar(stat='identity',fill="steelblue") +
  geom_text(mapping = aes(label = round(Count,0), fontface = 'bold', vjust = -0.2), size = 2) +
  labs(title = "Number of delayed flights per hour",x = "Actual Departure Hour", y ="Number of delayed flights") + 
  theme(plot.title = element_text(hjust = 0.5))

# Zoom to see the clear view of the graphs
ggarrange(g,h, 
          labels = c("G", "H"),
          ncol = 2, nrow = 1)

# We can identify that number of flights differ by hour. 

# Actual Departure Hour

Delayed_flights_DepHour <- Delayed_flights %>%
  group_by(DepHour) %>%
  summarise(Average_DepDelay = mean(DepDelay),Average_ArrDelay = mean(ArrDelay))

i <- ggplot(data = Delayed_flights_DepHour, mapping = aes(x = DepHour, y = Average_DepDelay)) +
  geom_bar(stat='identity',fill="steelblue") +
  geom_text(mapping = aes(label = round(Average_DepDelay,0), fontface = 'bold', vjust = -0.2), size = 2) +
  labs(title = "Distribution of delay over the hours day",x = "Actual Departure Hour", y ="Average DepDelay (min)") + 
  theme(plot.title = element_text(hjust = 0.5))

j <- ggplot(data = Delayed_flights_DepHour, mapping = aes(x = DepHour, y = Average_ArrDelay)) +
  geom_bar(stat='identity',fill="steelblue") +
  geom_text(mapping = aes(label = round(Average_ArrDelay,0), fontface = 'bold', vjust = -0.2), size = 2) +
  labs(title = "Distribution of delay over the hours day",x = "Actual Departure Hour", y ="Average ArrDelay (min)") + 
  theme(plot.title = element_text(hjust = 0.5))

# CRSDeparture Hour

Delayed_flights_CRSDepHour <- Delayed_flights %>%
  group_by(CRSDepHour) %>%
  summarise(Average_DepDelay = mean(DepDelay),Average_ArrDelay = mean(ArrDelay))

k <- ggplot(data = Delayed_flights_CRSDepHour, mapping = aes(x = CRSDepHour, y = Average_DepDelay)) +
  geom_bar(stat='identity',fill="steelblue") +
  geom_text(mapping = aes(label = round(Average_DepDelay,0), fontface = 'bold', vjust = -0.2), size = 2) +
  labs(title = "Distribution of delay over the hours day",x = "CRS Departure Hour", y ="Average DepDelay (min)") + 
  theme(plot.title = element_text(hjust = 0.5))


l <- ggplot(data = Delayed_flights_CRSDepHour, mapping = aes(x = CRSDepHour, y = Average_ArrDelay)) +
  geom_bar(stat='identity',fill="steelblue") +
  geom_text(mapping = aes(label = round(Average_ArrDelay,0), fontface = 'bold', vjust = -0.2), size = 2) +
  labs(title = "Distribution of delay over the hours day",x = "CRS Departure Hour", y ="Average ArrDelay (min)") + 
  theme(plot.title = element_text(hjust = 0.5))

# Zoom to see the clear view of the graphs
ggarrange(i,j,k,l, 
          labels = c("I", "J","K","L"),
          ncol = 2, nrow = 2)

# Identify the best day of week to book a flight

Delayed_flights_Day <- Delayed_flights %>%
  group_by(DayOfWeek) %>%
  summarise(Count = sum(DelayedArr),Average_DepDelay = mean(DepDelay),Average_ArrDelay = mean(ArrDelay))

m <-ggplot(data = Delayed_flights_Day, mapping = aes(x = reorder(wday(DayOfWeek,label = TRUE),-Average_ArrDelay), y = Average_ArrDelay)) +
  geom_bar(stat='identity',fill="steelblue") +
  geom_text(mapping = aes(label = round(Average_ArrDelay,0), fontface = 'bold', vjust = -0.2), size = 2) +
   labs(title = "Average Arrival Delay by Day",x = "Day", y ="Average ArrDelay (min)") + 
  theme(plot.title = element_text(hjust = 0.5))

# Number of Arrival Delay by Day

n <- ggplot(data = Delayed_flights_Day, mapping = aes(x = reorder(wday(DayOfWeek,label = TRUE),-Count), y =Count)) +
  geom_bar(stat='identity',fill="steelblue") +
  geom_text(mapping = aes(label = round(Count,0), fontface = 'bold', vjust = -0.2), size = 2) +
  labs(title = "Number of Arrival Delay by Day",x = "Day", y ="Arrival Delay Count") + 
  theme(plot.title = element_text(hjust = 0.5))

# Zoom to see the clear view of the graphs
ggarrange(m,n, 
          labels = c("M", "N"),
          ncol = 2, nrow = 1)

# Identify the best day of month to book a flight

Delayed_flights_Month <- Delayed_flights %>%
  group_by(Month) %>%
  summarise(Count= sum(DelayedArr),Average_DepDelay = mean(DepDelay),Average_ArrDelay = mean(ArrDelay))

o <- ggplot(data = Delayed_flights_Month, mapping = aes(x = reorder(month(Month,label = TRUE),-Average_ArrDelay), y = Average_ArrDelay)) +
  geom_bar(stat='identity',fill="steelblue") +
  geom_text(mapping = aes(label = round(Average_ArrDelay,0), fontface = 'bold', vjust = -0.2), size = 2) +
  labs(title = "Average Arrival Delay by Month",x = "Month", y ="Average ArrDelay (min)")+
  theme(plot.title = element_text(hjust = 0.5)) 

# Number of Arrival Delay by Month

p <- ggplot(data = Delayed_flights_Month, mapping = aes(x = reorder(month(Month,label = TRUE),-Count), y =Count)) +
  geom_bar(stat='identity',fill="steelblue") +
  geom_text(mapping = aes(label = round(Count,0), fontface = 'bold', vjust = -0.2), size = 2) +
  labs(title = "Number of Arrival Delay by Day",x = "Month", y ="Arrival Delay Count") + 
  theme(plot.title = element_text(hjust = 0.5))

# Zoom to see the clear view of the graphs
ggarrange(o,p, 
          labels = c("O", "P"),
          ncol = 2, nrow = 1)

#From the plots, we can identify the best time to minimize delays.

# ==============================================================================================
# 2) Do older planes suffer more delays?
# ==============================================================================================

# To check whether older planes suffer more delays, 
# use delayed flight dataset from question 1 which excludes the outliers.
# Then, merge planes dataset which includes aircraft details into delayed flight dataset.

plane_characteristic <- Delayed_flights 
  
plane_characteristic <- left_join(plane_characteristic, planes, by = c("TailNum"='tailnum'))%>%
  group_by(year) %>%
  summarise(Average_DepDelay = mean(DepDelay),Average_ArrDelay = mean(ArrDelay), Average_CarrierDelay = mean(CarrierDelay))

# Drop non-indicated year values
plane_characteristic <- plane_characteristic %>%
  filter(!is.na(year), plane_characteristic$year !="",plane_characteristic$year !="0000",plane_characteristic$year !="None",plane_characteristic$year !="2007")  

# Plot Average delay of each engine year

# Average Arrival Delay
q<- ggplot(data = plane_characteristic, mapping = aes(x = year, y = Average_ArrDelay)) +
  geom_bar(stat='identity',fill="steelblue") +
  geom_text(mapping = aes(label = round(Average_ArrDelay,0), fontface = 'bold', vjust = -0.2), size = 2) +
  labs(title = "Distribution of delay over the engine year of the plane",x = "Engine Year", y ="Average ArrDelay (min)") + 
  theme(plot.title = element_text(hjust = 0.5),axis.text.x=element_text(angle=90, hjust=1))
# Average Departure Delay
r<- ggplot(data = plane_characteristic, mapping = aes(x = year, y = Average_DepDelay)) +
  geom_bar(stat='identity',fill="steelblue") +
  geom_text(mapping = aes(label = round(Average_ArrDelay,0), fontface = 'bold', vjust = -0.2), size = 2) +
  labs(title = "Distribution of delay over the engine year of the plane",x = "Engine Year", y ="Average DepDelay (min)") + 
  theme(plot.title = element_text(hjust = 0.5),axis.text.x=element_text(angle=90, hjust=1))
# Average Carrier Delay
s<- ggplot(data = plane_characteristic, mapping = aes(x = year, y = Average_CarrierDelay)) +
  geom_bar(stat='identity',fill="steelblue") +
  geom_text(mapping = aes(label = round(Average_ArrDelay,0), fontface = 'bold', vjust = -0.2), size = 2) +
  labs(title = "Distribution of delay over the engine year of the plane",x = "Engine Year", y ="Average CarrierDelay (min)") + 
  theme(plot.title = element_text(hjust = 0.5),axis.text.x=element_text(angle=90, hjust=1))

# Zoom to see the clear view of the graphs
ggarrange(q,r,s, 
          labels = c("Q", "R", "S"),
          ncol = 2, nrow = 2)
# From the graph, we don't see much of fluctuation or difference between each engine year. 
# Hence, we can assume that age of the plane does not have great impact on its delays.

# ==============================================================================================
# 3) How does the number of people flying between different locations change over time?
# ==============================================================================================
Destination <- df_copy

Destination$season <- month(Destination$Date,label = TRUE) 

# Concatenate flights origin and destination to observe its route.
Destination$trip <-paste(Destination$Origin,Destination$Dest,sep="-")

# Indicate season for each month
# December to February = Winter
# March to May = Spring
# June to August = Summer
# September to November = Autumn
Destination$season = with(Destination, ifelse(season%in% c("Dec","Jan","Feb") , "Winter",
                                       ifelse(season %in% c("Mar","Apr","May") , "Spring",
                                              ifelse(season %in% c("Jun","Jul","Aug") , "Summer",
                                                     ifelse(season %in% c("Sep","Oct","Nov") , "Autumn",
                                                            season)))))
# Merge airport dataset which includes airport details.       
Destination <- left_join(Destination, airports, by = c("Dest"='iata'))

# To identify the number of people flying between different locations over time,
# plot most visited trips, airport, city, and state.

# Top 10 Most Frequent Trips
Destination_trip <- Destination %>%
  group_by(season,trip)%>%
  summarise(count = n())

  Destination_trip %>% 
  arrange(desc(count)) %>%
  slice(1:10) %>%
  ggplot(., mapping = aes(x = reorder(trip, -count), y = count)) +
  facet_wrap(~season)+
  geom_bar(stat='identity',fill="steelblue") +
  geom_text(mapping = aes(label = round(count,0), fontface = 'bold', vjust = -0.2), size = 1.5) +
  labs(title = "Top 10 Most Frequent Trips",x = "Trip", y ="Total Trip Count") + 
  theme(plot.title = element_text(hjust = 0.5),axis.text.x=element_text(angle=90, hjust=1))

# Top 10 Most Visited Airport
Destination_airport <- Destination %>%
  group_by(season,airport)%>%
  summarise(count = n())

Destination_airport %>% 
  arrange(desc(count)) %>%
  slice(1:10) %>%
  ggplot(., mapping = aes(x = reorder(airport, -count), y = count)) +
  facet_wrap(~season)+
  geom_bar(stat='identity',fill="steelblue") +
  geom_text(mapping = aes(label = round(count,0), fontface = 'bold', vjust = -0.2), size = 1.5) +
  labs(title = "Top 10 Most Frequent airports",x = "airport", y ="Total airport Count") + 
  theme(plot.title = element_text(hjust = 0.5),axis.text.x=element_text(angle=90, hjust=1))

# Top 10 Most Visited City
Destination_city <- Destination %>%
  group_by(season,city)%>%
  summarise(count = n())

  Destination_city %>% 
  arrange(desc(count)) %>%
  slice(1:10) %>%
  ggplot(., mapping = aes(x = reorder(city, -count), y = count)) +
  facet_wrap(~season)+
  geom_bar(stat='identity',fill="steelblue") +
  geom_text(mapping = aes(label = round(count,0), fontface = 'bold', hjust = .5,vjust = 2), size = 2) +
  labs(title = "Top 10 Most Frequent citys",x = "city", y ="Total city Count") + 
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x=element_text(angle=90, hjust=1, size = 8),
        axis.text.y=element_text(hjust=1, size = 8),
        strip.text = element_text(size = 8))

# or

Destination_city %>% 
  arrange(desc(count)) %>%
  slice(1:10) %>%
  ggplot(., mapping = aes(x = reorder(city, -count), y = count,fill=season)) +
  geom_bar(stat='identity',position = 'dodge') +
  geom_text(mapping = aes(label = round(count,0), fontface = 'bold', hjust = 1.3,angle=90), size = 1.6,position=position_dodge(1)) +
  labs(title = "Top 10 Most Frequent citys",x = "city", y ="Total city Count") + 
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x=element_text(angle=90, hjust=1, size = 8),
        axis.text.y=element_text(hjust=1, size = 8))

# Top 10 Most Visited State
Destination_state <- Destination %>%
  group_by(season,state)%>%
  summarise(count = n())

  Destination_state %>% 
  arrange(desc(count)) %>%
  slice(1:10) %>%
  ggplot(., mapping = aes(x = reorder(state, -count), y = count)) +
  facet_wrap(~season)+
  geom_bar(stat='identity',fill="steelblue") +
  geom_text(mapping = aes(label = round(count,0), fontface = 'bold', hjust = .5,vjust = 2), size = 2) +
  labs(title = "Top 10 Most Frequent citys",x = "city", y ="Total city Count") + 
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x=element_text(angle=90, hjust=1, size = 8),
        axis.text.y=element_text(hjust=1, size = 8),
        strip.text = element_text(size = 8))
  
# We divided 4 different sections to demonstrate how the number of people fly between 
# different locations change over time. As we can see from the plot, most frequent trip
# happened in between Los Angeles International Airport (LAX) and San Diego International Airport (SAN)
# during winter compared to other seasons. This makes sense because temperatures in San Diego 
# are sufficiently warm year-round, hence we can assume that people tend to visit 
# San Diego during winter to get rid of the cold weather and fill the warm air.
# Similar finding could be applicable for other flight's route too.
# Moreover, we can identify that having the most frequent trip doesn’t necessarily mean 
# it would be parallel to the most visited airport, city and state since numerous airports 
# are correlated with lots of different city and state. Thus, "Top 10 Most Visited Airport" plot with 
# William B Hartsfield-Atlanta Intl airport as the most visited airport located in the city 
# of Atlanta is contrasting to "Top 10 Most Visited City" plot showing Chicago as the most visited city. 
# Furthermore, people tend to travel more on spring and summer when the weather is ideal.
# For summer, it may be due to summer vacation trips. While for spring, considered as off-season
# compared to summer, could have fewer crowds and the prices of touring may be less compared to
# summer when prices soar which would then result to encourage people to travel during spring. 
# Hence month and origin of the airport could be an important indicator for prediction model.
  
# ==============================================================================================
# 4) Can you detect cascading failures as delays in one airport create delays in others?
# ==============================================================================================

# First, cascading delay occurs when a delay at previous airport flight 
# cause a ripple effect in the following airport flight.

# We used "Destination" dataset from question 3 which includes the airport details.
# Filter out flights with no "LateAircraftDelay" from "Destination" DF
# since we are trying to detect cascading failures as delays in one airport create delays in others

Airport <- filter(Destination,Destination$LateAircraftDelay > 0) 

# In addition, Filter out flights with no delayed arrival and departure
Airport <- subset(Airport, Airport$DelayedArr == 1 & Airport$DelayedDep == 1)

# We will investigate flights with Tail Number "N336" and "N337"  to find the 
# cascading failures as delays in one airport create delays in others

Airport_cascading_failure <- subset(Airport, Airport$TailNum %in% c("N336","N337"))

# we can identify that LateAircraftDelay seems decreasing
no_cascading_failure <- Airport_cascading_failure %>%
        filter(Date =="2004-04-12" & FlightNum == 2967)%>%
        arrange(CRSDepHour)

t <- ggplot(data = no_cascading_failure, mapping = aes(x = CRSDepHour, y = LateAircraftDelay)) +
  geom_line(group=1) +
  geom_text(mapping = aes(label = LateAircraftDelay, fontface = 'bold', vjust = -0.2), size = 2) +
  labs(title = "LateAircraftDelay on 2004-04-12 for tail N336",x = "CRSDepHour", y ="LateAircraftDelay (min)") + 
  theme(plot.title = element_text(hjust = 0.5))

# We can identify that LateAircraftDelay seems increasing
cascading_failure <- Airport_cascading_failure %>%
  filter(Date =="2005-06-10" & TailNum == "N337")%>%
  arrange(CRSDepHour)

u <- ggplot(data = cascading_failure, mapping = aes(x = CRSDepHour, y = LateAircraftDelay)) +
  geom_line(group=1) +
  geom_text(mapping = aes(label = LateAircraftDelay, fontface = 'bold', vjust = -0.2), size = 2) +
  labs(title = "LateAircraftDelay on 2005-06-10 for tail N337",x = "CRSDepHour", y ="LateAircraftDelay (min)") + 
  theme(plot.title = element_text(hjust = 0.5))

# Zoom to see the clear view of the graphs
ggarrange(t,u, 
          labels = c("T", "U"),
          ncol = 2, nrow = 1)

# We can identify that LateAircraftDelay seems decreasing and increasing in some cases.
# To further investigate, plot top 10 airports with longest delay.

Longest_carrier_delay_flights <- Airport %>%
  group_by(airport) %>%
  summarise(Average_DepDelay = mean(DepDelay),Average_ArrDelay = mean(ArrDelay), Average_CarrierDelay = mean(CarrierDelay))

# Top 10 airports with longest departure delay
v <- Longest_carrier_delay_flights %>% 
  arrange(desc(Average_DepDelay)) %>%
  slice(1:10) %>%
  ggplot(., mapping = aes(x = reorder(airport, -Average_DepDelay), y =Average_DepDelay)) +
  geom_bar(stat='identity',fill="steelblue")+
  geom_text(mapping = aes(label = round(Average_DepDelay,0), fontface = 'bold', vjust = -0.2), size = 2) +
  labs(title = "Top 10 airports with longest departure delay",x = "Airport", y ="Average DepDelay (min)") + 
  theme(plot.title = element_text(hjust = 0.5),axis.text.x=element_text(angle=90, hjust=1))

# Top 10 airports with longest arrival delay
w <- Longest_carrier_delay_flights %>% 
  arrange(desc(Average_ArrDelay)) %>%
  slice(1:10) %>%
  ggplot(., mapping = aes(x = reorder(airport, -Average_ArrDelay), y =Average_ArrDelay)) +
  geom_bar(stat='identity',fill="steelblue")+
  geom_text(mapping = aes(label = round(Average_ArrDelay,0), fontface = 'bold', vjust = -0.2), size = 2) +
  labs(title = "Top 10 airports with longest arrival delay",x = "Airport", y ="Average ArrDelay (min)") + 
  theme(plot.title = element_text(hjust = 0.5),axis.text.x=element_text(angle=90, hjust=1))

# Top 10 airports with longest carrier delay
x <- Longest_carrier_delay_flights %>% 
  arrange(desc(Average_CarrierDelay)) %>%
  slice(1:10) %>%
  ggplot(., mapping = aes(x = reorder(airport, -Average_CarrierDelay), y =Average_CarrierDelay)) +
  geom_bar(stat='identity',fill="steelblue")+
  geom_text(mapping = aes(label = round(Average_CarrierDelay,0), fontface = 'bold', vjust = -0.2), size = 2) +
  labs(title = "Top 10 airports with longest carrier delay",x = "Airport", y ="Average CarrierDelay (min)") + 
  theme(plot.title = element_text(hjust = 0.5),axis.text.x=element_text(angle=90, hjust=1))

# Zoom to see the clear view of the graphs
ggarrange(v,w,x,
          labels = c("V", "W", "X"),
          ncol = 2, nrow = 2)

# As it gives top 10 of all different airports for each different delays, we can
# assume that there is a fluctuation of average delays for different airports

#=====================================================================

# Furthermore, check if different airlines have unusual characteristics on its delays.
# Distribution of average delay over Airline

Airline <- Delayed_flights 

Airline <- left_join(Airline, carriers, by = c("UniqueCarrier"='Code'))%>%
  group_by(UniqueCarrier) %>%
  summarise(Average_DepDelay = mean(DepDelay),Average_ArrDelay = mean(ArrDelay), Average_CarrierDelay = mean(CarrierDelay))

# Average Arrival Delay
y <- ggplot(data = Airline, mapping = aes(x = UniqueCarrier, y = Average_ArrDelay)) +
  geom_bar(stat='identity',fill="steelblue") +
  geom_text(mapping = aes(label = round(Average_ArrDelay,0), fontface = 'bold', vjust = -0.2), size = 2) +
  labs(title = "Distribution of arrival delay over Airline",x = "Airline Code", y ="Average ArrDelay (min)") + 
  theme(plot.title = element_text(hjust = 0.5),axis.text.x=element_text(angle=90, hjust=1))

# Average Departure Delay
z <- ggplot(data = Airline, mapping = aes(x = UniqueCarrier, y = Average_DepDelay)) +
  geom_bar(stat='identity',fill="steelblue") +
  geom_text(mapping = aes(label = round(Average_ArrDelay,0), fontface = 'bold', vjust = -0.2), size = 2) +
  labs(title = "Distribution of departure delay over Airline",x = "Airline Code", y ="Average DepDelay (min)") + 
  theme(plot.title = element_text(hjust = 0.5),axis.text.x=element_text(angle=90, hjust=1))

# Average Carrier Delay
aa<- ggplot(data = Airline, mapping = aes(x = UniqueCarrier, y = Average_CarrierDelay)) +
  geom_bar(stat='identity',fill="steelblue") +
  geom_text(mapping = aes(label = round(Average_ArrDelay,0), fontface = 'bold', vjust = -0.2), size = 2) +
  labs(title = "Distribution of carrier delay over Airline",x = "Airline Code", y ="Average CarrierDelay (min)") + 
  theme(plot.title = element_text(hjust = 0.5),axis.text.x=element_text(angle=90, hjust=1))

# Zoom to see the clear view of the graphs
ggarrange(y,z,aa,
          labels = c("Y", "Z", "AA"),
          ncol = 2, nrow = 2)

# we observed some fluctuation of average delays between different airports and airlines. 
# Hence, we concluded that delays could vary based on their origin of the airports and
# their airlines

# ==============================================================================================
# 5) Use the available variables to construct a model that predicts delays.
# ==============================================================================================

# Since the dataset is huge, we will be modeling using 0.1% of the original dataset.

prediction <- df_copy %>% filter(row_number() %% 1000 == 0) %>%
  dplyr::select(c(DelayedArr,CRSDepHour,Origin,UniqueCarrier,Month,Distance))

# Turn it into factor
prediction[prediction$DelayedArr == 0,]$DelayedArr <- "on-time"
prediction[prediction$DelayedArr == 1,]$DelayedArr <- "delayed"
prediction$DelayedArr <- as.factor(prediction$DelayedArr)


# Encode categorical features we will be using into numerical value to allow the model to perform with it.

lbl <- LabelEncoder$new()

prediction$CRSDepHour <- lbl$fit_transform(prediction$CRSDepHour)
prediction$Origin <- lbl$fit_transform(prediction$Origin)
prediction$UniqueCarrier <- lbl$fit_transform(prediction$UniqueCarrier)

set.seed(1)
# Create a list of 80% of the rows in the prediction dataset.
training_sample <- createDataPartition(prediction$DelayedArr,p = 0.8, list = FALSE)

# Use the 80% of data for training and testing the models.
dataset<- prediction[training_sample,]

# Use the 20% of the data to validate the model.
validation <- prediction[-training_sample,]

# Since our aim is to predict delayed flights, pick "DelayedArr" as independent variable(label).
# Pick dependent variables(features) that are available in the
# future flights and assumed to be influencing delays.

# split input and output
output <- dataset[,1]

input <- dataset[,2:6]

# Violin plot for each attribute on one image
par(mfrow=c(1,5))

for(i in 2:6) {vioplot(prediction[,i], main=names(input)[i-1])}

# Run algorithms using 10-fold cross validation

# We will be using 10-fold cross-validation

control <- trainControl(method="cv", number=10)

metric <- "Accuracy"


# From the table, we can observe that delayed is much less compared to on-time
# This may cause class imbalance which results to bias.
table(dataset$DelayedArr)


# Since we are interested on delayed flights, we need to revise the model because
# on-time flight is dominated over delayed flight.
prop.table(table(dataset$DelayedArr))
# Proportion rate of class shows 19% for delayed flights and 81% for on-time flights.

par(mfrow=c(1, 1))
barplot(prop.table(table(dataset$DelayedArr)),
        ylim = c(0, 0.9),
        main = "Class Distribution")

# To make the class balanced, we will be using 3 techniques to balance the class

# Under sampling
set.seed(1)
undersampling <- ovun.sample(DelayedArr~., data=dataset, method = "under",N = 6082)$data
table(undersampling$DelayedArr)
# Oversampling
set.seed(1)
oversampling <- ovun.sample(DelayedArr~., data=dataset, method = "over",N = 26488)$data
table(oversampling$DelayedArr)
# Both (Over & Under)
set.seed(1)
bothsampling <- ovun.sample(DelayedArr~., data=dataset, method = "both",N = 16285, p=.5)$data
table(bothsampling$DelayedArr)


# with 3 techniques, compute the model using each data and evaluate its accuracy.
# build decision tree models

tree.over <- rpart(DelayedArr~., data=oversampling)
tree.under <- rpart(DelayedArr~., data=undersampling)
tree.both <- rpart(DelayedArr~., data=bothsampling)

pred.tree.over <- predict(tree.over, newdata = dataset)
pred.tree.under <- predict(tree.under, newdata = dataset)
pred.tree.both <- predict(tree.both, newdata = dataset)

#AUC Undersampling
roc.curve(dataset$DelayedArr, pred.tree.under[,2])
#AUC Oversampling
roc.curve(dataset$DelayedArr, pred.tree.over[,2])
#AUC Bothsampling
roc.curve(dataset$DelayedArr, pred.tree.both[,2])

# Use either Undersampling or Bothsampling as it gives highest AUC score.

# 1) Logistic Regression

set.seed(1)
fit.glm_undersampling <- train(DelayedArr~., data=undersampling, method="glm", metric=metric, trControl=control, family="binomial")
set.seed(1)
fit.glm_oversampling <- train(DelayedArr~., data=oversampling, method="glm", metric=metric, trControl=control, family="binomial")
set.seed(1)
fit.glm_bothsampling <- train(DelayedArr~., data=bothsampling, method="glm", metric=metric, trControl=control, family="binomial")

predictions_glm_undersampling <- predict(fit.glm_undersampling, validation)
predictions_glm_oversampling <- predict(fit.glm_oversampling, validation)
predictions_glm_bothsampling <- predict(fit.glm_bothsampling, validation)

confusionMatrix(predictions_glm_undersampling, validation$DelayedArr)
confusionMatrix(predictions_glm_oversampling, validation$DelayedArr)
confusionMatrix(predictions_glm_bothsampling, validation$DelayedArr)

results <- resamples(list(under=fit.glm_undersampling, over=fit.glm_oversampling, both=fit.glm_bothsampling))

summary(results)

# 2) Random Forest

set.seed(1)
fit.rf_undersampling <- train(DelayedArr~., data=undersampling, method="rf", metric=metric, trControl=control)
set.seed(1)
fit.rf_oversampling <- train(DelayedArr~., data=oversampling, method="rf", metric=metric, trControl=control)
set.seed(1)
fit.rf_bothsampling <- train(DelayedArr~., data=bothsampling, method="rf", metric=metric, trControl=control)

predictions_rf_undersampling <- predict(fit.rf_undersampling, validation)
predictions_rf_oversampling <- predict(fit.rf_oversampling, validation)
predictions_rf_bothsampling <- predict(fit.rf_bothsampling, validation)

confusionMatrix(predictions_rf_undersampling, validation$DelayedArr)
confusionMatrix(predictions_rf_oversampling, validation$DelayedArr)
confusionMatrix(predictions_rf_bothsampling, validation$DelayedArr)

results <- resamples(list(under=fit.rf_undersampling, over=fit.rf_oversampling, both=fit.rf_bothsampling))

summary(results)

# 3) CART

set.seed(1)
fit.cart_undersampling <- train(DelayedArr~., data=undersampling, method="rpart", metric=metric, trControl=control)
set.seed(1)
fit.cart_oversampling <- train(DelayedArr~., data=oversampling, method="rpart", metric=metric, trControl=control)
set.seed(1)
fit.cart_bothsampling <- train(DelayedArr~., data=bothsampling, method="rpart", metric=metric, trControl=control)

predictions_cart_undersampling <- predict(fit.cart_undersampling, validation)
predictions_cart_oversampling <- predict(fit.cart_oversampling, validation)
predictions_cart_bothsampling <- predict(fit.cart_bothsampling, validation)

confusionMatrix(predictions_cart_undersampling, validation$DelayedArr)
confusionMatrix(predictions_cart_oversampling, validation$DelayedArr)
confusionMatrix(predictions_cart_bothsampling, validation$DelayedArr)

results <- resamples(list(under=fit.cart_undersampling, over=fit.cart_oversampling, both=fit.cart_bothsampling))

summary(results)

# 4) KNN

set.seed(1)
fit.knn_undersampling <- train(DelayedArr~., data=undersampling, method="knn", metric=metric, trControl=control)
set.seed(1)
fit.knn_oversampling <- train(DelayedArr~., data=oversampling, method="knn", metric=metric, trControl=control)
set.seed(1)
fit.knn_bothsampling <- train(DelayedArr~., data=bothsampling, method="knn", metric=metric, trControl=control)

predictions_knn_undersampling <- predict(fit.knn_undersampling, validation)
predictions_knn_oversampling <- predict(fit.knn_oversampling, validation)
predictions_knn_bothsampling <- predict(fit.knn_bothsampling, validation)

confusionMatrix(predictions_knn_undersampling, validation$DelayedArr)
confusionMatrix(predictions_knn_oversampling, validation$DelayedArr)
confusionMatrix(predictions_knn_bothsampling, validation$DelayedArr)

results <- resamples(list(under=fit.knn_undersampling, over=fit.knn_oversampling, both=fit.knn_bothsampling))

summary(results)

results_total <- resamples(list(under_glm=fit.glm_undersampling, over_glm=fit.glm_oversampling, both_glm=fit.glm_bothsampling,
under_rf=fit.rf_undersampling, over_rf=fit.rf_oversampling, both_rf=fit.rf_bothsampling,
under_cart=fit.cart_undersampling, over_cart=fit.cart_oversampling, both_cart=fit.cart_bothsampling,
under_knn=fit.knn_undersampling, over_knn=fit.knn_oversampling, both_knn=fit.knn_bothsampling))

summary(results_total)

# Among different models, we could pick the best model with appropriate value of 
# accuracy, sensitivity and specificity which are a good indicators to evaluate the model.
# However, for this research, we will deep dive into Random Forest.
#===========================================================================================

# Tuning Random forest in R
# We will be using undersampling result to tune the model

fit.rf_undersampling

# mtry = number of variables randomly sampled
# It shows that mtry of 5 has the highest accuracy for fit.rf_undersampling

# Grid Search
control_grid <- trainControl(method="cv", number=10 , search="grid")

set.seed(1)

fit.rf_undersampling_grid <- train(DelayedArr~., data=undersampling, method="rf", metric=metric, trControl=control_grid)

print(fit.rf_undersampling_grid)

plot(fit.rf_undersampling_grid)

# grid-search takes quite long time to load, hence random search is often used instead. 
# Although it is done within certain amount of time, it has systematic way to find the appropriate
# model instead of just putting random values several times. Moreover, it is also called 
# coarse search because it proceeds two to three times and gradually narrows down.
# Morever, for random forest, we could tune the model by assigning mtry and ntree parameters.

# RandomSearch
control_random <- trainControl(method="cv", number=10, search="random")

set.seed(1)

fit.rf_undersampling_random <- train(DelayedArr~., data=undersampling, method="rf", metric=metric, trControl=control_random)

print(fit.rf_undersampling_random)

plot(fit.rf_undersampling_random)

#======================================================
# Furthermore, to identify the importance of each variable, we will
# be using permutation based feature importance method to identify.

# random forest model
rf_permutation <- ranger(DelayedArr~ ., dataset, importance = 'permutation')

# barplot
rf_permutation_importance <- data.frame(
  var = names(rf_permutation$variable.importance), imp = c(rf_permutation$variable.importance))

ggplot(rf_permutation_importance) + 
  geom_bar(aes(x=reorder(var, imp), y=imp), stat = 'identity',fill="steelblue") +
  labs(title = "Importance of variables",x = "Variables", y ="Importance") + 
  theme(plot.title = element_text(hjust = 0.5))

# From the graph, we can identify that "CRSDepHour","Distance" and "Origin" have 
# high influence to the model.
# Thus for further research, we could optimize the best parameter for get the best result of the model.
