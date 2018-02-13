library(stringr)
library(data.table)
library(dplyr)
library(tidyr)
library(snowfall)
library(tictoc)
library(zoo)
setwd('C:/Coffee and Weather Code/data')

# # DESCRIPTION --------------------------------------------------------------------------------------
# ~ ~ ~ ~ ~ ~ ~
#         GENERATING A DATA FRAME OF HOURLY SALES COUNTS BY DRINK FEATURE + WEATHER VARIABLES
#
# This script first turns a table of ORDERS THAT INCLUDED DRINKS (where one observation is an order containing
# *one or more* drinks sold) into a table of DRINKS SOLD (where one observation is *one* drink sold, 
# including all relevant coding variables from the coding book as prescribed by the item's coding
# as well as modifiers included with the drink). Then, it produces counts of drinks sold, by feature,
# in every hour of the cafe's operation. Finally, this data is joined with hourly weather data.
#
# Steps taken in this script:
#
#   (1) parsing the table with orders made at the cafe and creating a data frame of drinks;
#   (2) replacing duplicate drinks (promos and typos) with their original names;
#   (3) joining the drinks data frame with relevant variables from the coding book;
#   (4) rewriting coded variables according to modifiers included with the drink;
#   (5) generating a table of hourly counts of drink sales by feature;
#   (6) importing and wrangling hourly weather data from CSV files;
#   (7) joining the hourly counts of drink sales with hourly weather data.
#
# The final table contains data on counts of hourly sales of drinks with particular contents and traits.
#
# For info on weather variables, see http://climate.weather.gc.ca/glossary_e.html.
#        ~ ~ ~

# # 0. IMPORTING DATA ------------------------------------------------------------------------------

  tic("Importing data")
  
    codeBook <- read.csv("codebook_edits2.csv", encoding="UTF-8")
    colnames(codeBook)[c(1,16,22)] <- c("ID","Cold","ml")
    codeBook$Description <- as.character(codeBook$Description)
    codeBook$Duplicate <- as.character(codeBook$Duplicate)
    
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

# This section parses the data frame Orders (of drinks only). Right now, each observation contains 
# a string with all the drinks that were ordered together. For instance, an order can be this:
# "Small americano{Extra shot{Large Latte{Almond Milk{Espresso". In this order, 
# "Small americano{Extra shot", "Large Latte{Almond Milk", and "Espresso" are all separate drinks.
# To proceed further, I need all of these drinks to be in separate observations.
#
# So, here, function parseOrder is fed a row from Orders. Using functions readOrderContents 
# and is.item, parseOrder checks whether each item it encounters is a menu item.
# If it's an item (e.g., Latte), it has its own observation, and everything that follows after it, 
# until another item, is a modifier, and hence has to be recorded in the same observation.
# (Because we need to decide what to do based on a previous element, we have to use loops here.) 
#
# parseOrder returns a small data frame, and when called on every row of the data frame of orders,
# its outputs are bound to make the data frame Drinks.
  
# First, a function for reading the contents of an order:

readOrderContents <- function(orderString) { 
  Items <- unlist(strsplit(orderString, "\\{")) 
  return(Items)
}

# Find the row of an item in the code book & determine if it's an item or modifier:

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
  
  # This code showed that a drink could have only up to 4 modifiers:
  # max(lengths(unname(sapply(Drinks$Modifiers, readOrderContents)), use.names = FALSE))
  # Given the low maximum number of modifiers, it made sense to make separate columns for each modifier, 
  # which would be easy to compare with the relevant column of the code book later on:
    
  output <- data.frame(matrix(ncol=7,nrow=0))
  colnames(output) <- c("Order ID",   "Time",         "Drink",           "Mod 1", "Mod 2", "Mod 3", "Mod 4")
  #                      Order ID    | Time of order | Item number,name | Mod number,name
  #                      1             2               3                  4       5        6        7 

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


  tic("Parsing orders (creating a data frame of drinks ordered)")
    clus <- makeCluster(4)
    clusterExport(clus, c("Orders","codeBook","readOrderContents","is.item","parseOrder"))

    Drinks <- rbindlist(parRapply(clus, Orders, parseOrder))
    # Base R call: # Drinks <- rbindlist(apply(Orders, 1, parseOrder))
    
    stopCluster(clus)  
  toc()              
  
  
# # 2. REPLACING DUPLICATES -----------------------------------------------------------------------  

# Now, we have a data frame of Drinks. The challenge right now is that some drinks are recorded as
# typos due to human error when creating and maintaining the menu via the cash register system.
# E.g., "CappucciNNo" should be recorded as "CappucciNo", and for a few days "CappucciNNos" were
# sold. We want them to be recorded as the rest of the "CappucciNos" sold on all other days.
# There are also some promotional items that refer to products normally sold at a higher price.
# E.g., "ESPRESSO PROMO", a discounted espresso sold for a few days in March, refers to "Espresso."
#
# To properly differentiate among the drinks recorded, and to make sure we are not losing records 
# of all relevant sales, we need to ensure the drinks are recorded under their proper names. 
#  
# The code book contains a field "Duplicated" where the correct names for typos or promos are recorded.
# If the name is correct as is, the field is empty.  
# The function replaceDuplicate checks if the code book contains information on the correct spelling 
# of the item checked by the function. If the spelling was incorrect, a correct spelling is returned.
# If the spelling was correct, the function returns the original input. 
# replaceDuplicate is then called on every element of column "Drink" (string) in Drinks data frame.

replaceDuplicate <- function(Item) {
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

# The code book contains information on how to code each menu item. E.g., "Latte Large" would be
# coded as "12oz" on Size, "TRUE" on Content.Espresso, "TRUE" on Content.Milk, etc.
# This is fairly easy to do:
    
  # Using dplyr's join after making codeBook's formatting of "Drink" 
  # (combination of ID & Description) match that of Drinks:
  codeBook <- mutate(codeBook, Drink=paste0(ID,",",Description))
  codeBook <- codeBook %>% select(Drink,Size:ml)
  Drinks <- left_join(Drinks, codeBook, by=c("Drink" = "Drink"))

  
# # 4. OVERWRITING MODIFIERS ----------------------------------------------------------------------

# However, the real challenge to properly recording the features of every drink sold is to keep track
# of all the different options to the drink, or modifiers. 
# For instance, for "Latte Large{Almond Milk{Decaf", we will need to change the default "TRUE" on 
# Content.Espresso for "FALSE", and tick off "TRUE" on Content.SpecialtyMilk. 
#
# This is the most complicated procedure so far in my data wrangling.
#  
# I go over each row in Drinks using processAllModifiers function. Each Drink has up to 4 modifiers
# to it, each recorded in a separate column. Each modifier is checked against the code book via
# readModifier function. If some option for a drink has to be rewritten based on the codebook
# (e.g., "Iced" would mean we need to record "TRUE" on "Iced" variable), processAllModifiers 
# returns a small data frame containing the coordinates for changes: row # in Drinks, variable name,
# and new content for the variable (e.g., in row 1001, "Iced" will need to be changed for "TRUE").
# These coordinates are bound into a dataframe of Changes when processAllModifiers is called on Drinks.   
# Function overwriteModifiers then uses information saved in Changes to modify relevant rows in Drinks.
  
  
# readModifier is fed a modifier (a string), and returns a vector of the length of howmanyever variables 
# (i.e. code book colums describing variables) there are. The output vector has empty values where 
# no change is needed, and with the value for the overwrite where change is needed:

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

  # ifelse in readModifier likes codeBook to be a matrix, so converting it:
  codeBook <- as.matrix(codeBook)
  # Adding a column containing row number for simplicity of 
  # storing relevant row numbers for the function:
  Drinks$Row <- 1:nrow(Drinks) 
  
  tic("Generating a data frame of changes to Drinks based on modifiers")
    clus <- makeCluster(4)
    clusterExport(clus, c("codeBook","readModifier","processAllModifiers"))

    Changes <- rbindlist(parRapply(clus, Drinks, processAllModifiers))
    # Base R call: # Changes <- rbindlist(apply(Drinks, 1, processAllModifiers))
    
    stopCluster(clus)
  toc()

  
# overwriteModifiers is applied to a row of Changes. 
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
  
  
# # 4.5. CLEANING UP ---------------------------------------------------------------------------

# Everything in this section is simple clean-up: renaming colnames, handling levels, NAs, etc.
    
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
  rm(Changes, clus, Orders, dummies, codeBook)

  #Drinks <- read.csv("0-drinks.csv")

  # Two observations (Drinks[28191,], Drinks[28192,]) have "NA" (string) as value in Time 
  # (first orders of the day, likely a glitch in the cash register), fixing:
  Drinks$Order.Time[which(Drinks$Order.Time=="NA")] <- "7:00"
  
  Drinks <- Drinks %>% separate(Order.Time, into=c("Order.Hour", "Order.Minute"), sep = ":")
  Drinks$Order.Hour   <- as.integer(Drinks$Order.Hour)
  Drinks$Order.Minute <- as.integer(Drinks$Order.Minute)

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
  rm(frothing)
  
  
# # 5. COUNTING HOURLY SALES ------------------------------------------------------------

# In this section, we're transforming the data frame of Drinks into a data frame of
# hourly counts in sales of drinks with specific traits and features.
# In other words, we're tallying hourly sales for each category of drink that was coded
# (e.g., 15 espresso-based drinks sold at 4PM on September 7, 2016).  
  
  # Summarizing logicals and numerics to produce a final table of hourly counts:
  
  Drinks <- unite(Drinks, Time, Order.Day, Order.Hour, sep=" ")
  Drinks$Time <- as.factor(Drinks$Time)
  logicals <- Drinks %>% group_by(Time) %>% summarise_if(is.logical, sum)
  numerics <- Drinks %>% group_by(Time) %>% summarise_if(is.numeric, mean)
  GrandFinale <- as.data.frame(merge(numerics, logicals, by="Time"))
  rm(logicals, numerics)
  GrandFinale <- GrandFinale[,-2]
  
  GrandFinale <- separate(GrandFinale, Time, into=c("Day","Hour"), sep=" ", remove=FALSE)
  GrandFinale$Day <- as.Date(GrandFinale$Day)
  GrandFinale$Hour <- as.integer(GrandFinale$Hour)
  
  # Making seasonal variables (based on date ranges from
  # https://www.climatestotravel.com/climate/canada/montreal) for control variables:
  
    fallSeq <- c(seq(from = as.Date("2016-09-01"), to = as.Date("2016-11-14"), by="days"),
                 seq(from = as.Date("2017-09-01"), to = as.Date("2017-11-14"), by="days"))
  winterSeq <- c(seq(from = as.Date("2016-11-15"), to = as.Date("2017-03-15"), by="days"),
                 seq(from = as.Date("2017-11-15"), to = as.Date("2018-03-15"), by="days"))
  springSeq <-   seq(from = as.Date("2017-03-16"), to = as.Date("2017-05-31"), by="days")
  summerSeq <-   seq(from = as.Date("2017-06-01"), to = as.Date("2017-08-31"), by="days")
  
  GrandFinale$Season.Fall   <- ifelse(is.element(GrandFinale$Day, fallSeq),   TRUE, FALSE)
  GrandFinale$Season.Winter <- ifelse(is.element(GrandFinale$Day, winterSeq), TRUE, FALSE)
  GrandFinale$Season.Spring <- ifelse(is.element(GrandFinale$Day, springSeq), TRUE, FALSE)
  GrandFinale$Season.Summer <- ifelse(is.element(GrandFinale$Day, summerSeq), TRUE, FALSE)
  
  # Making student session control variable:
  examSeq <- c(seq(from=as.Date("2016-12-01"), to=as.Date("2016-12-22"), by="days"),
               seq(from=as.Date("2017-12-01"), to=as.Date("2017-12-22"), by="days"),
               seq(from=as.Date("2017-04-07"), to=as.Date("2017-04-30"), by="days"))
  
  GrandFinale$Season.Exams <- ifelse(is.element(GrandFinale$Day, examSeq), TRUE, FALSE)
  GrandFinale$Season.Tank <- ifelse(GrandFinale$Season.Summer==TRUE | GrandFinale$Season.Winter==TRUE, TRUE, FALSE)
  
  # It will be helpful to record how many total drink sales we had per hour:
  Sales <- Drinks %>% group_by(Time) %>% tally
  colnames(Sales)[2] <- "DrinksSold"
  GrandFinale <- merge(Sales, GrandFinale, by="Time")
  
# # 6. WRANGLING WEATHER DATA ---------------------------------------------------------------

# In this section, we're wrangling hourly weather data downloaded from Climate Canada website.
# The only challenge of this data is that some useful information is recorded in text form,
# like weather phenomena ("Cloudy", "Clear", "Rain", etc.) that are very important to how
# humans understand weather. A simple procedure to find out what the available phenomena are,
# along with creating relevant logical variables, follows.
# Only one row containing NAs is spotted and is handled in secion 7 -- the final section.  
  
  # Date sequence for which we have data:
  dateSequence <- seq(from = as.Date("2016-09-01"), to = as.Date("2017-11-22"), by = "days")
  
  hourly <- read.csv("hourly_airport.csv", encoding="UTF-8")
  colnames(hourly)[1] <-"DateTime"
  hourly <- separate(hourly, DateTime, into=c("Day", "Hour"), sep=" ")
  hourly$Day <- as.Date.character(hourly$Day)
  hourly$Hour <- as.integer(gsub("\\:00$", "", hourly$Hour))
  
  colnames(hourly) <- c("Day", "Hour", "Temp", "DewPoint", 
                        "Humidity", "WindSpeed", "Pressure", "Weather")
  
  # Making use of textually recorded weather conditions ("Clear", "Fog", "Rain", etc.):
  
  hourly$Weather <- as.character(hourly$Weather)
  # some empty values, replacing w/ NA:
  hourly$Weather[hourly$Weather==""] <- NA
  hourly$Weather <- na.locf(hourly$Weather)
  
  # Exploring: making a vector of all possible conditions:
  conditions <- sort(unique(hourly$Weather))
  moreThan1 <- conditions[grepl("\\,", conditions)]
  conditions <- subset(conditions, !grepl("\\,", conditions))
  conditions <- unique(sort(rbind(conditions, unlist(strsplit(moreThan1, ",")))))
  conditions
  
  # Making new columns based on this:
  hourly$Clear  <- ifelse(grepl("\\<Clear\\>", hourly$Weather), TRUE, FALSE)
  hourly$Fog    <- ifelse(grepl("\\<Fog\\>|\\<Haze\\>", hourly$Weather), TRUE, FALSE)
  hourly$Rain   <- ifelse(grepl("\\<Rain\\>|\\<Drizzle\\>", hourly$Weather), TRUE, FALSE)
  hourly$Snow   <- ifelse(grepl("\\<Snow\\>|\\<Ice\\>", hourly$Weather), TRUE, FALSE)
  
  # Checking for NAs:
  notmissing <- apply(hourly, 1, function(x) { all(!is.na(x)) } )
  hourly[which(!notmissing),]
  # Just one hour missing.. will have to lose a few drinks for that hour.
  
  # Only keep days within our date range of interest:
  hourly <- hourly %>% filter(is.element(Day, dateSequence)) 
  
# # 7. JOINING SALES & WEATHER DATA ------------------------------------------------------

# This has been a long journey! Joining the two data frames, hourly weather data and 
# hourly drink sales tallies, removing NAs, doing some final clean-up and saving the table.
    
  # To join weather with drinks data, need to make Day+Hour into one field:
  hourly <- unite(hourly, Time, Day, Hour, sep=" ", remove=TRUE)
  
  GG <- merge(GrandFinale, hourly, by="Time")
  rm(GrandFinale, hourly, Drinks, Sales)
  
  # Removing the one missing hour:
  notmissing <- apply(GG, 1, function(x) { all(!is.na(x)) })
  GG <- GG[-which(!notmissing),]
  
  # Seasonal dummies as factor:
  GG$Season <- "Season"
  GG$Season <- ifelse(GG$Season.Fall==TRUE,   "Fall",   GG$Season)
  GG$Season <- ifelse(GG$Season.Winter==TRUE, "Winter", GG$Season)
  GG$Season <- ifelse(GG$Season.Spring==TRUE, "Spring", GG$Season)
  GG$Season <- ifelse(GG$Season.Summer==TRUE, "Summer", GG$Season)
  
  # Clean-up:
  GG <- GG[,-c(1, 5, 20:25)]
  colnames(GG)[c(18, 19)] <- c("ExamPeriod", "LowSalesPeriod")
  colnames(GG)[20] <- "Temperature"
  colnames(GG)[9] <- "Content.Milk"
  GG <- GG[,-which(colnames(GG)=="Weather")]
  setcolorder(GG, c("Day", "Hour", "Season", "ExamPeriod", "LowSalesPeriod", "DrinksSold",
                    "Size.Mean", "Content.Espresso", "Content.Drip", "Content.Water", "Content.Tea", 
                    "Content.Milk", "Content.SpecialtyMilk", "Content.Chocolate", "Content.Seasonal", 
                    "Content.Juice", "Trait.Cold", "Trait.HighInSugar", "Trait.HighInCaffeine", "Trait.Froth", 
                    "Temperature", "DewPoint", "Humidity", "WindSpeed", "Pressure", "Clear", "Fog", "Rain", "Snow"))
  GG$Size.Mean <- round(GG$Size.Mean, 0)
  
  write.csv(GG, "CafeHourly.csv", row.names = FALSE)