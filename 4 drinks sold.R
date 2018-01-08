library(stringr)
library(data.table)
library(dplyr)
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
#   (4) rewriting coded variables according to modifiers included with the drink.
#
#        ~ ~ ~

# # CODE ---------------------------------------------------------------------------------------------

# Importing the codebook and fixing encoding issues:
codeBook <- read.csv("codebook.csv", encoding="UTF-8")
colnames(codeBook) <- c("ID", "Description",	"Frequency",	"Hierarchy",	"Type",	"Duplicate",	
                        "Size",	"Tea",	"Espresso",	"Filtered",	"Milk",	"Frothed",	"Frothing Level",	
                        "Chocolate",	"Watered down",	"Iced/Chilled",	"Sugary",	"Extra Caffeine",	
                        "Seasonal ingredient",	"Juice",	"Specialty milk")
codeBook$Description <- as.character(codeBook$Description)
codeBook$Duplicate <- as.character(codeBook$Duplicate)
# Encoding(codeBook$Description) <- "UTF-8"
# Encoding(codeBook$Duplicate) <- "UTF-8"

# Importing data on orders:
Orders <- read.csv("0-drinks-only-orders.csv")
colnames(Orders) <- c("Order ID", "Time", "Contents")
Orders$Contents <- as.character(Orders$Contents)

# Making a selector for dummy variables only, turning them into logicals, and substituting NAs:
dummies <- as.logical(sample(1:ncol(codeBook)))
dummies[c(1:7,13)] <- FALSE
codeBook[dummies] <- as.logical(unlist(codeBook[dummies]))

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
  #                      Order ID    | Time of order | Item number,name | Mod(s) number,name
  #                      1             2               3                  4       5        6        7 
  
  # (See lines after this function for an explanation for why there are 4 columns for modifiers.)
  
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
            # Checking which modifier columns in the old row are EMPTY and write the modifier in the first match:
            emptyMods <- as.logical(output[oldRow,]=="")
            output[oldRow,min(which(emptyMods==TRUE))]=ItemsNMods[i]
           }
  }
  return(output)
}

Drinks <- rbindlist(apply(Orders, 1, parseOrder))

# While developing parseOrder, used this code to discover there were only up to 4 modifiers that a drink can have:
# Drinks$ModNumber <- lengths(unname(sapply(Drinks$Modifiers, readOrderContents)), use.names = FALSE)
# max(Drinks$ModNumber)
# (this gives NA right now because has to be called on Modifiers before it's edited using parseOrder; 
# pls ignore for now, will fix in a later commit)
# Given the low maximum number of modifiers, made sense to make separate columns for each modifier, 
# which would be easy for comparison with the code book later.

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

Drinks$Drink <- sapply(Drinks$Drink, replaceDuplicate)

# Using dplyr's mutating join after making codeBook's formatting of "Drink" 
# (combination of ID & Description) match that of Drinks:
codeBook <- mutate(codeBook, Drink=paste0(ID,",",Description))
codeBook <- codeBook %>% select(Drink,Size:`Specialty milk`)
Drinks <- left_join(Drinks, codeBook, by=c("Drink" = "Drink"))

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
# it returns a small data frame of three columns designating changes to be made in Drinks: 
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
    drinkrow <- sapply(drinkrow, function(x) { drinkrow=as.numeric(row[23]) })
    output <- data.frame(drinkrow, positions, contents)
    colnames(output) <- c("Row", "Variable", "Content")
  }
  return(output)
}

# Making a data frame where 1 observation is 1 column to change in the Drinks table:

Changes <- rbindlist(apply(Drinks, 1, processAllModifiers))

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

apply(Changes, 1, overwriteModifiers)

# Removing unnecessary columns, replacing NAs with FALSE values for logicals, and saving to file:
Drinks <- Drinks[,-c(4:7,23)]
Drinks[,c(5:9,11:18)] <- replace(Drinks[,c(5:9,11:18)], is.na(Drinks[,c(5:9,11:18)]), FALSE)

write.csv(Drinks, file = "0-drinks.csv", row.names = FALSE)