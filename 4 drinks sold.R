library(stringr)
library(data.table)
library(dplyr)
library(tidyr)
library(snowfall)
library(tictoc)
setwd('C:/Coffee and Weather Code/data')

# # DESCRIPTION --------------------------------------------------------------------------------------
# ~ ~ ~ ~ ~ ~ 
# This script turns a table of ORDERS THAT INCLUDED DRINKS (where one observation is an order containing
# one or more drinks sold) into a table of DRINKS SOLD (where one observation is *one* drink sold, 
# including all relevant coding variables from the coding book as prescribed by the item's coding
# as well as modifiers included with the drink).
#
# Steps taken in this script:
#
#   (1) parsing the table with orders and creating a data frame of drinks;
#   (2) replacing duplicate menu items (promos and typos) with originals in the drinks data frame;
#   (3) joining the drinks data frame with relevant variables from the coding book;
#   (4) rewriting coded variables according to modifiers included with the drink;
#   (5) generating a table of daily statistics re: drink sales, 
#       importing weather data from CSV files, and joining the two sets.
#
# The final table contains data on daily sales of drinks containing a particular element, 
# trait, or preparation feature.
# An observation should be read as: "on September 1st, 2016, we sold 13 drinks containing 
# water, 12 drinks containing tea, 62 espresso-based drinks, ..." 
# For info on weather variables, see http://climate.weather.gc.ca/glossary_e.html.
#        ~ ~ ~

# # 0. IMPORTING DATA -----------------------------------------------------------------------------

  tic("Importing data")
  
    # Importing the codebook and fixing encoding issues:
    codeBook <- read.csv("codebook_edits2.csv", encoding="UTF-8")
    colnames(codeBook)[c(1,16,22)] <- c("ID","Cold","ml")
    codeBook$Description <- as.character(codeBook$Description)
    codeBook$Duplicate <- as.character(codeBook$Duplicate)
    
    # Importing data on orders:
    Orders <- read.csv("0-drinks-only-orders.csv")
    colnames(Orders) <- c("Order ID", "Time", "Contents")
    Orders$Contents <- as.character(Orders$Contents)
    
    # Making a selector for dummy variables only, turning them into logicals, and substituting NAs:
    dummies <- as.logical(sample(1:(ncol(codeBook))))
    names(dummies) <- paste0(seq(1:length(colnames(codeBook))), " ", colnames(codeBook))
    dummies[c(1:7,13,22)] <- FALSE
    codeBook[dummies] <- as.logical(unlist(codeBook[dummies]))
  
  toc()

  
# # 1. PARSING ORDERS -----------------------------------------------------------------------------

# First, a function for getting to items within an order:

readOrderContents <- function(orderString) { 
  Items <- unlist(strsplit(orderString, "\\{")) 
  return(Items)
}

# Find the row of an item in the code book to determine if it's an item or modifier:

is.item <- function(Item) {
  Item <- unlist(strsplit(Item, ","))
  
  referenceRow <- intersect( which(Item[1] == codeBook$ID),
                             which(Item[2] == codeBook$Description) )
  if (!is.null(referenceRow) || referenceRow>0) 
    { if (codeBook[referenceRow, 4] == "item") 
           { return(TRUE)  } else { return(FALSE) }
    } else { return(FALSE) }
  # returns TRUE if item, FALSE if modifier
}

# Parsing an order (via a row from the Orders data frame) & outputting a data frame of drinks ordered:

parseOrder <- function(row) {
  orderID    <- row[1]
  orderTime  <- row[2]
  ItemsNMods <- readOrderContents(as.character(row[3]))

  #print(paste0("Order ID: ",orderID, " Order time: ",orderTime, " Order contents: ", ItemsNMods))
    
  output <- data.frame(matrix(ncol=7,nrow=0))
  colnames(output) <- c("Order ID",   "Time",         "Drink",           "Mod 1", "Mod 2", "Mod 3", "Mod 4")
  #                      Order ID    | Time of order | Item number,name | Mod number,name
  #                      1             2               3                  4       5        6        7 
  # (See end of script for an explanation of why there are 4 modifier columns.)
  
  i <- 1
  for (i in 1:length(ItemsNMods)) {
    oldRow <- nrow(output)
    newRow <- nrow(output)+1
    
    if (is.item(ItemsNMods[i])) {
      output[newRow,1] = orderID
      output[newRow,2] = orderTime
      output[newRow,3] = ItemsNMods[i]
      output[newRow,4:7] = ""
    } else {
            # Checking which modifier columns in the old row are EMPTY 
            # and writing the modifier into the first match:
            emptyMods <- as.logical(output[oldRow,]=="")
            output[oldRow,min(which(emptyMods==TRUE))]=ItemsNMods[i]
           }
  }
  return(output)
}

  # Base R call: # Drinks <- rbindlist(apply(Orders, 1, parseOrder))

  tic("Parsing orders (creating a data frame of drinks ordered)")
    clus <- makeCluster(4)
    clusterExport(clus, c("Orders","codeBook","readOrderContents","is.item","parseOrder"))

    Drinks <- rbindlist(parRapply(clus, Orders, parseOrder))

    stopCluster(clus)  
  toc()              
  
  
# # 2. REPLACING DUPLICATES -----------------------------------------------------------------------  
  
# Since some menu items are typos or promotional versions of other items, replacing duplicates:

replaceDuplicate <- function(Item) {
  # Checking an item from "Drink" column against "Duplicated" in codebook;
  # if it is in fact a duplicate, then the item is substituted for the real one.
  ItemVector <- unlist(strsplit(Item, ","))

  referenceRow <- intersect( which(ItemVector[1] == codeBook$ID),
                             which(ItemVector[2] == codeBook$Description) )

  if (!is.null(referenceRow) || referenceRow>0) 
  {
    if (codeBook[referenceRow, 6] == "") { return(Item) } else { return(codeBook[referenceRow, 6]) }
    
  } else { return(Item) }
}

  tic("Replacing duplicates in Drinks data frame")
    Drinks$Drink <- sapply(Drinks$Drink, replaceDuplicate)
  toc()                

  
# # 3. JOINING DRINKS & CODEBOOK ------------------------------------------------------------------  
  
  # Using dplyr's join after making codeBook's formatting of "Drink" 
  # (combination of ID & Description) match that of Drinks:
  codeBook <- mutate(codeBook, Drink=paste0(ID,",",Description))
  codeBook <- codeBook %>% select(Drink,Size:ml)
  Drinks <- left_join(Drinks, codeBook, by=c("Drink" = "Drink"))

  
# # 4. OVERWRITING MODIFIERS ----------------------------------------------------------------------

# readModifier is fed a modifier (a string), and returns a vector of the length of howmanyever variables 
# (i.e. code book colums describing variables) there are. The output vector has empty values where 
# no change is needed, and with the value for the overwrite where change is needed:

  # ifelse in readModifier likes codeBook to be a matrix, so converting it:
  codeBook <- as.matrix(codeBook)

readModifier <- function(Modifier) {
  referenceRow <- which(Modifier == codeBook[,1])
  if (Modifier == "") {return(character(length=ncol(codeBook)-1))}
  if (!is.null(referenceRow) || referenceRow>0) 
  {
    # Creating the output vector (right now, all cols in codeBook except for Drink correspond to variables):
    overwriteVector <- ifelse(codeBook[referenceRow,]!="", codeBook[referenceRow,], "")[-1]
    # For some reason, some TRUE/FALSE values have spaces in them when recorded as characters:
    overwriteVector <- gsub(" ", "", overwriteVector)

    overwriteVector <- ifelse(overwriteVector=="", NA, overwriteVector)
    
    return(overwriteVector)
    
  } else {return(character(length=ncol(codeBook)-1))}
}

# processAllModifiers is fed a row of Drinks. Based on the contents of Modifiers 1 thru 4, 
# it returns a small data frame of three columns designating the changes to be made in Drinks: 
# row of Drinks to change, variable-containing column to change, and the new value to write in.

  # Adding a column containing row number for simplicity of storing relevant row numbers for the function:
  Drinks$Row <- 1:nrow(Drinks) 

processAllModifiers <- function(row) {

  Modifiers <- row[4:7]

  Overwrites <- sapply(Modifiers, readModifier)
  Overwrites <- t(data.frame(Overwrites))
  
  collapseCol <- function(column) {
    output <- column[!is.na(column)]
    if (length(output[1])==0) {return(NA)} else {return(output[1])}
  }

  Overwrites <- as.logical(apply(Overwrites, 2, collapseCol))
  positions <- which(!is.na(Overwrites))

  if (length(positions)==0) {return(NULL)} else {
    contents <- Overwrites[positions]
    drinkrow <- vector(length=length(positions))
    drinkrow <- sapply(drinkrow, function(x) { drinkrow=as.numeric(row[24]) })
    output <- data.frame(drinkrow, positions, contents)
    colnames(output) <- c("Row", "Variable", "Content")
  }
  return(output)
}

# Making a data frame where 1 observation is 1 column to change in the Drinks table:
  
  # Base R call: # Changes <- rbindlist(apply(Drinks, 1, processAllModifiers))

  tic("Generating a data frame of changes to Drinks based on modifiers")
    clus <- makeCluster(4)
    clusterExport(clus, c("codeBook","readModifier","processAllModifiers"))

    Changes <- rbindlist(parRapply(clus, Drinks, processAllModifiers))
    
    stopCluster(clus)   
  toc()               

# overwriteModifiers is applied to one row of Changes. 
# After overwriting the relevant cells in Drinks, returns a silent NULL:

overwriteModifiers <- function(x) {
  row=x[1]
  changee=x[2]
  changer=x[3]

  # The numeration stored in Changes$Variable is only within the subset of columns that contain code book variables 
  # (i.e., the first variable column, "Size", is the first one in this numeration). Given this, need
  # to do some simple calculation to properly refer to the relevant column within Drinks:
  
  Drinks[row, changee+grep("Size", colnames(Drinks))-1] <<- as.logical(changer)
  
  return(invisible(NULL))
}

  tic("Overwriting modifiers in Drinks based on Changes")
    apply(Changes, 1, overwriteModifiers)
  toc()  
  
# # 5. CLEAN-UP ===================================================================================
  
  tic("Final preparations: cleaning the table of Drinks, adding weather data & output of final table CAFE")

    # Keeping a concatenated column with modifiers for reference:
    Drinks <- Drinks %>% mutate(Modifiers=paste(`Mod 1`, `Mod 2`, `Mod 3`, `Mod 4`, sep="; "))
    Drinks$Modifiers <- gsub("(; ){2,4}", "", Drinks$Modifiers)
    
    # Cosmetics: removing unnecessary columns, moving columns around, 
    # replacing NAs, as well as an empty level lable:
    Drinks <- Drinks[,-c(4:7,24)]
    colnames(Drinks) <- gsub("\\.", " ", colnames(Drinks))
    colnames(Drinks) <- c("ID", "Time", "Item", "Size.oz", "Tea",
                          "Espresso", "Drip", "RegularMilk", "Frothed", "Frothing", 
                          "Chocolate", "Water", "Cold", 
                          "HighInSugar", "HighInCaffeine", 
                          "Seasonal", "Juice", 
                          "SpecialtyMilk", "Size.Mean", "Modifiers")
    setcolorder(Drinks, 
      c("ID", "Time", "Item", "Modifiers",                            # order info (1:4)
        "Size.oz", "Size.Mean",                                           # size (5:6)
        "Espresso", "Drip", "Water", "Tea", "RegularMilk", "SpecialtyMilk",  # contents/ingredients
          "Chocolate", "Seasonal", "Juice",                 # (7:15)
        "Cold", "HighInSugar", "HighInCaffeine", "Frothed",           # characteristics (16:19)
        "Frothing"))                                                  # preparation info (20)
    # Appending column categories to var names:
    colnames(Drinks)[c(1:4)]   <- paste0("Order.", colnames(Drinks[c(1:4)]))
    colnames(Drinks)[c(7:15)]  <- paste0("Content.", colnames(Drinks[c(7:15)]))
    colnames(Drinks)[c(16:19)] <- paste0("Trait.", colnames(Drinks[c(16:19)]))
    colnames(Drinks)[20] <- "Prep.Frothing"
    
    Drinks <- separate(Drinks, Order.Time, into=c("Order.Day", "Order.Time"), sep = " ")
    Drinks$Order.Day <- as.Date(Drinks$Order.Day, "%Y-%m-%d")
    
    levels(Drinks$Prep.Frothing)[levels(Drinks$Prep.Frothing)==""] <- "none"
    
    Drinks[,c(13:20)] <- replace(Drinks[,c(13:20)], is.na(Drinks[,c(13:20)]), FALSE)
    
    # Do any of the columns have missing values? Returns a 0 length vector, so all is good.
    if (length(colnames(Drinks)[!unname(apply(Drinks, 2, 
                 function(col) { all(!is.na(col)) } ))])==0) { print("Very clean.") }

    # Some vars must be mutually exclusive (cold drinks can't be frothed, regular milk doesn't mix 
    # with specialty milk - - some obs marked like this due to human error when taking orders):
    Drinks <- Drinks %>% 
      mutate(Content.RegularMilk=replace(Content.RegularMilk, Content.SpecialtyMilk==TRUE, FALSE))
    
    Drinks <- Drinks %>%
      mutate(Trait.Frothed=replace(Trait.Frothed, Trait.Cold==TRUE, FALSE)) %>%
      mutate(Prep.Frothing=replace(Prep.Frothing, Trait.Cold==TRUE, "none"))
    
    # Remove size column for oz, won't use it - - 
    # hard to describe drink volume with factors w/o there being too many:
    Drinks <- Drinks[,-6]
    
    try(file.remove("0-drinks.csv"), silent=TRUE)
    write.csv(Drinks, file = "0-drinks.csv", row.names = FALSE)
    
  rm(Changes, clus, Orders, dummies)
    
    # Add Sales vars: Total, Morning/Day/Night (for shifts: 7-12, 12-17, 17-21)
    # two obs (Drinks[28191,], Drinks[28192,]) have "NA" (string) as value in Time 
    # (first orders of the day, likely due to a glitch in cash register), fixing:
    Drinks$Order.Time[which(Drinks$Order.Time=="NA")] <- "7:00"
    
    Drinks <- Drinks %>% separate(Order.Time, into=c("Order.Hour", "Order.Minute"), sep = ":")
    Drinks$Order.Hour   <- as.integer(Drinks$Order.Hour)
    Drinks$Order.Minute <- as.integer(Drinks$Order.Minute)
    morning <- Drinks %>% group_by(Order.Day) %>% filter(Order.Hour <= 11) %>% tally
    morning <- as.data.frame(morning)
    colnames(morning) <- c("Day", "Sales.Morning")
    day     <- Drinks %>% group_by(Order.Day) %>% filter(Order.Hour >= 12, Order.Hour < 17)  %>% tally
    day     <- as.data.frame(day)
    colnames(day) <- c("Day", "Sales.Day")
    night   <- Drinks %>% group_by(Order.Day) %>% filter(Order.Hour >= 17, Order.Hour <= 21) %>% tally
    night <- as.data.frame(night)
    colnames(night) <- c("Day", "Sales.Night")
  
    sales <- left_join(morning, day, by="Day")
    sales <- left_join(sales, night, by="Day")
    sales <- replace(sales, is.na(sales), 0)
    sales$Sales.Total <- sales$Sales.Morning+sales$Sales.Day+sales$Sales.Night
    colnames(sales)[1] <- "Order.Day"
    setcolorder(sales, c("Order.Day", "Sales.Total", "Sales.Morning", "Sales.Day", "Sales.Night"))
    
  # Converting frothing factor to dummies:
  frothing <- as.data.frame(model.matrix(~Prep.Frothing+0, Drinks))
  colnames(frothing) <- c("None", "High", "Low")
  colnames(frothing) <- paste0("Prep.Frothing", colnames(frothing))
  frothing <- sapply(frothing, function(x) {x<-as.logical(x)})
  Drinks <- Drinks[,-c(20,21)]
  Drinks <- cbind(Drinks, frothing)
  colnames(Drinks)[20:22] <- c("Trait.Froth", "Trait.LongFroth", "Trait.ShortFroth")
  # NoFroth is now Froth to simplify things:
  Drinks <- Drinks %>% mutate(Trait.Froth = !Trait.Froth)
  
  # Summarizing the logicals to produce final table:
  
  logicals <- Drinks %>% group_by(Order.Day) %>% summarise_if(is.logical, sum)
  numerics <- Drinks %>% group_by(Order.Day) %>% summarise_if(is.numeric, mean)
  GrandFinale <- as.data.frame(merge(numerics, logicals, by="Order.Day"))
  GrandFinale <- merge(sales, GrandFinale, by="Order.Day")
  colnames(GrandFinale)[1] <- "Day"
  GrandFinale <- GrandFinale[,-c(6:7)]
  
  # Making seasonal variables (based on date ranges from: https://www.climatestotravel.com/climate/canada/montreal)
    fallSeq <- c(seq(from = as.Date("2016-09-01"), to = as.Date("2016-11-14"), by="days"),
                 seq(from = as.Date("2017-09-01"), to = as.Date("2017-11-14"), by="days"))
  winterSeq <- c(seq(from = as.Date("2016-11-15"), to = as.Date("2017-03-15"), by="days"),
                 seq(from = as.Date("2017-11-15"), to = as.Date("2018-03-15"), by="days"))
  springSeq <-   seq(from = as.Date("2017-03-16"), to = as.Date("2017-05-31"), by="days")
  summerSeq <-   seq(from = as.Date("2017-06-01"), to = as.Date("2017-08-31"), by="days")
  
  GrandFinale$Season.Fall   <- ifelse(is.element(GrandFinale$Day, fallSeq), TRUE, FALSE)
  GrandFinale$Season.Winter <- ifelse(is.element(GrandFinale$Day, winterSeq), TRUE, FALSE)
  GrandFinale$Season.Spring <- ifelse(is.element(GrandFinale$Day, springSeq), TRUE, FALSE)
  GrandFinale$Season.Summer <- ifelse(is.element(GrandFinale$Day, summerSeq), TRUE, FALSE)
  
  # Making student session variable:
  examSeq <- c(seq(from=as.Date("2016-12-01"), to=as.Date("2016-12-22"), by="days"),
               seq(from=as.Date("2017-12-01"), to=as.Date("2017-12-22"), by="days"),
               seq(from=as.Date("2017-04-07"), to=as.Date("2017-04-30"), by="days"))
  
  GrandFinale$Season.Exams <- ifelse(is.element(GrandFinale$Day, examSeq), TRUE, FALSE)
  
# # 6. IMPORTING WEATHER DATA ==============================================================================
  
  # Date sequence for which we have data:
  dateSequence <- seq(from = as.Date("2016-09-01"), to = as.Date("2017-11-22"), by = "days")
  
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
  colnames(airport) <- c("Day", "Temp.Max", "Temp.Min", "Temp.Mean", 
                         "Temp.HeatDegrDays", "Temp.CoolDegrDays", 
                         "Precip.TotalRain", "Precip.TotalSnow", "Precip.TotalPrecipitation",
                         "Precip.SnowOnGround", "Wind.MaxGustSpeed")
  airport$Temp.Diff <- airport$Temp.Max-airport$Temp.Min
  airport$Precip.Snow <- ifelse(airport$Precip.TotalSnow>0, TRUE, FALSE)
  airport$Precip.Rain <- ifelse(airport$Precip.TotalRain>0, TRUE, FALSE)
  airport <- airport[,order(names(airport))]
  
  # adding hourly dimension (more weather vars, averaged daily stats) from airport data
  hourly <- read.csv("hourly_airport.csv", encoding="UTF-8")
  colnames(hourly)[1] <-"DateTime"
  hourly <- separate(hourly, DateTime, into=c("Day", "HM"), sep=" ")
  hourly$Day <- as.Date.character(hourly$Day)
  
  hourly <- hourly %>% 
    group_by(Day) %>%
    filter(is.element(Day, dateSequence)) %>%
    summarise_if(is.numeric, mean)
  hourly <- as.data.frame(hourly)
  colnames(hourly) <- c("Day", "Temp.DewPoint", "Humidity", "Wind.Speed", 
                        "Visibility", "Pressure", "Temp.WindChill")
  hourly <- hourly[,order(names(hourly))]
  
  airport <- as.data.frame(merge(airport, hourly, by="Day"))
  
  # Merging Drinks and Weather data: 
  GrandFinale <- as.data.frame(merge(GrandFinale, airport, by = "Day"))
  
  # Exploring where we have NAs in weather variables, except for the two winter-specific vars
  # that naturally have NAs outside winter times:
  cafe <- subset(GrandFinale, select=-c(29,43))
  empties <- which(!apply(cafe, 1, function(x) { all(!is.na(x)) } )) 
  # Returns a vector of just 6 obs, exploring - - No apparent pattern in missing values, 
  # but one of these obs is perfectly fine, fixing manually:
  GrandFinale[153,27] <- FALSE
  GrandFinale[153,31] <- 0
  empties <- unname(empties)[-1]
  # Dropping the empties from the table - getting rid of NAs:
  GrandFinale <- GrandFinale[-empties,]
  sum(is.na(GrandFinale$Temp.WindChill))/length(GrandFinale$Temp.WindChill)
  # Wind chill variable has NAs for 85% of obs, dropping this column:
  GrandFinale <- GrandFinale[,-which(colnames(GrandFinale)=="Temp.WindChill")]
  # Snow on ground has NAs for when there's no snow on the ground (most days),
  # and we only have data for one winter. Will get rid of this too:
  GrandFinale <- GrandFinale[,-which(colnames(GrandFinale)=="Precip.SnowOnGround")]
  
  setcolorder(GrandFinale, 
              c("Day", "Sales.Total", "Sales.Morning", "Sales.Day", "Sales.Night", 
                "Size.Mean", 
                "Content.Espresso", "Content.Drip", "Content.Water", "Content.Tea", 
                      "Content.RegularMilk", "Content.SpecialtyMilk",
                      "Content.Chocolate", "Content.Seasonal", "Content.Juice", 
                "Trait.HighInSugar", "Trait.HighInCaffeine", "Trait.Cold", 
                      "Trait.Froth", "Trait.ShortFroth", "Trait.LongFroth", 
                "Season.Fall", "Season.Winter", "Season.Spring", "Season.Summer", "Season.Exams",
                "Precip.Rain", "Precip.Snow", 
                    "Precip.TotalPrecipitation", "Precip.TotalRain", "Precip.TotalSnow", 
                "Temp.Mean", "Temp.Max", "Temp.Min", "Temp.Diff", 
                      "Temp.CoolDegrDays", "Temp.HeatDegrDays", "Temp.DewPoint",
                "Humidity", "Pressure", "Visibility", 
                "Wind.Speed", "Wind.MaxGustSpeed"))
  
  try(file.remove("cafe.csv"), silent=TRUE)
  write.csv(GrandFinale, file = "cafe.csv", row.names = FALSE)
  
toc()  
  
# # NOTES -----------------------------------------------------------------------------------------------
# ~ ~ ~ ~ ~ ~ 
#
# While developing parseOrder, used this code to discover that a drink could have only up to 4 modifiers:
#
# max(lengths(unname(sapply(Drinks$Modifiers, readOrderContents)), use.names = FALSE))
#
# Given the low maximum number of modifiers, it made sense to make separate columns for each modifier, 
# which would be easy to compare with the relevant column of the code book later on.