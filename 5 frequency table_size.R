library(dplyr)
setwd('C:/Coffee and Weather Code/data')

# IMPORTING DRINKS DATA ========================================================

Drinks <- read.csv("0-drinks-size.csv")
colnames(Drinks) <- c("Order ID", "Day", "HH:MM", "Drink", "Modifiers", 
                      "Size", "Water", "Tea", "Espresso", "Filtered", "Chocolate", "Milk", 
                      "Frothed", "Frothing.level", "Seasonal", "Juice", "Specialty Milk", "Cold", 
                      "High Sugar", "High Caffeine", "ml")

# Converting factors' (Size, Frothing.level) levels to dummies:

sizes <- as.data.frame(model.matrix(~Size-1, Drinks))
colnames(sizes) <- c("Size.12oz", "Size.2oz", "Size.4oz", "Size.8oz") 
sizes <- sapply(sizes, function(x) {x<-as.logical(x)})

frothing <- as.data.frame(model.matrix(~Frothing.level+0, Drinks))
colnames(frothing) <- c("Frothing.High", "Frothing.Low", "Frothing.None")
frothing <- sapply(frothing, function(x) {x<-as.logical(x)})

Drinks <- cbind(Drinks, frothing, sizes)
Drinks <- Drinks[,-c(1,3:6,14)]

Drinks$Day <- as.Date(Drinks$Day)

# Summarizing the logicals to produce final table:

logicals <- Drinks %>% 
  group_by(Day) %>%
  summarise_if(is.logical, sum)

numerics <- Drinks %>% group_by(Day) %>% summarise_if(is.numeric, mean)

GrandFinale <- as.data.frame(merge(logicals, numerics, by="Day"))

colnames(GrandFinale)[length(colnames(GrandFinale))] <- "Mean Size"

# IMPORTING WEATHER DATA ==============================================================================

# Date sequence for which we have data:

dateSequence <- seq(from = as.Date("2016-09-01"),
                    to = as.Date("2017-11-22"),
                    by = "days")

# Reading airport weather data downloaded from the gov's Climate website
# (data from 2016 and 2017 needs to be binded):

airport2016 <- read.csv("weather_airport_2016.csv", encoding="UTF-8")
airport2017 <- read.csv("weather_airport_2017.csv", encoding="UTF-8")
airport <- rbind(airport2016,airport2017)
rm(airport2016, airport2017)
airport <- airport[,!grepl(".Flag", colnames(airport))]

colnames(airport)[1] <- "Day"
airport$Day <- as.Date(airport$Day)

# Keeping only days for which we have Drinks data:

airport <- airport %>% filter(is.element(Day, dateSequence))

airport$Spd.of.Max.Gust..km.h. <- as.integer(gsub("<|>","",airport$Spd.of.Max.Gust..km.h.))

airport <- airport[,-11]

colnames(airport) <- c("Day", "Max Temp", "Min Temp", "Mean Temp", 
                       "Heating Degree Days", "Cooling Degree Days", 
                       "Total Rain", "Total Snow", "Total Precipitation",
                       "Snow on Ground", "Wind Max Gust Speed")

# Merging Drinks and Weather data and saving to file: 

GrandFinale <- as.data.frame(merge(GrandFinale, airport, by = "Day"))

# The table contains data on daily sales of drinks containing a particular element or preparation feature.
# An observation should be read as: "on September 1st, 2016, we sold 13 drinks containing water, 
# 12 drinks containing tea, 62 espresso-based drinks, ..." 
# The table also contains weather data for that day.
# See http://climate.weather.gc.ca/glossary_e.html for explanations of weather variables.

write.csv(GrandFinale, "_FinalTableDrinksAndWeather-sizes.csv", 
          row.names=FALSE, fileEncoding = "UTF-8")