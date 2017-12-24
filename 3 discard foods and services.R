library(stringr)
setwd('C:/Coffee and Weather Code/data')

# # DESCRIPTION --------------------------------------------------------------------------------------
# ~ ~ ~ ~ ~ ~ 
# At this point, all wrangling has been done and all potentially useful data has been extracted.
# Now, it's time to start chipping away at this data and remove irrelevant data, 
# like orders of food and services.
#
# This third script in the project takes in the data frame of all orders made at the coffee shop and 
# gets rid of menu items and modifiers that are not drink-related. A reference table containing
# item types is used to compare observations via regexs. After this is done, some observations
# are empty: not all orders necessarily contain drinks. The script drops them and saves
# the output in a new file.
#            ~ ~ ~

# # FUNCTIONS ----------------------------------------------------------------------------------------
# ~ ~ ~ ~ ~ ~
# (1) is.drink is fed a short string "[[:digit:]]+,item/mod name". Using the "," separator,
#     the function breaks this string up into two elements: item ID and item description.
#     These two elements are then looked up in a reference table.
#
#     is.drink returns TRUE if the item it was fed is in fact a drink item that should be kept.
#     This function will be used via the apply family in the next function, dropNonDrinks.

is.drink <- function(Item) {
  
  Item <- unlist(strsplit(Item, ","))
  
  # The row of the  relevant menu item/mod is the row where locations of ID and Description intersect.
  # While it's possible to use regex here ("^Item$"), the data's already been wrangled, so a simple
  # logical comparison will suffice--and it works faster, too:
  
  referenceRow <- intersect( which(Item[1] == referenceTable$ID),
                             which(Item[2] == referenceTable$Description) )
  
  # Wrapping the condition in some null-checks to avoid nasty errors:
  
  if (!is.null(referenceRow) || referenceRow>0) 
    {
    
      # The function checks if the matched row in the reference table's column with item types 
      # indicates that this item is indeed a drink, and not food or service:
    
      if (referenceTable[referenceRow, 5] == "drink") { return(TRUE) } else { return(FALSE) }
    
    } else { return(FALSE) }
  
}
#   ~ ~ ~ ~

# (2) dropNonDrinks is fed a string from orderContents. For example:
#         "54,Gateau citron Vegan{3,Latte 8oz{4,Lait ordinaire{3,Latte 8oz{4,Lait ordinaire"
#
#     The function breaks this string up into a vector of strings "[[:digit:]]+,item/mod name" 
#     using the "{" separator. After is.drink is applied to each element, 
#     only those that return TRUE are kept. 
#
#     dropNonDrinks returns an edited string. The function is used via the apply family on the column 
#     containing order contents from the data frame with all orders, allOrders$orderContents.

dropNonDrinks <- function(orderString) {
  
  Items <- unlist(strsplit(orderString, "\\{"))
  
  keep <- unlist(sapply(Items, is.drink))
  
  outputString <- paste0(Items[keep], "{", collapse='')
  
  # Dropping the unnecessary last "{" separator at end of string:
  
  outputString <- gsub("\\{$", "", outputString)
  
  return(outputString)
  
}

# # INPUT, FUNCTION CALL, OUTPUT ---------------------------------------------------------------------

# Reading the orders data frame generated in script #2 & converting column from factor to character:

allOrders <- read.csv("0-all-completed-orders.csv")
allOrders$orderContents <- as.character(allOrders$orderContents)

# Reference table was edited manually in Excel, and now its encoding is misbehaving
# (we're working with French characters). Fixing encoding-related issues:

referenceTable <- read.csv("menu-items-and-modifiers.csv", 
                           as.is = c("ï..ID", "Description", "Frequency", "Hierarchy", "Type", "Duplicate"))
colnames(referenceTable) <- c("ID", "Description", "Frequency", "Hierarchy", "Type", "Duplicate")
Encoding(referenceTable$Description) <- "UTF-8"

# There are some nastily named items (e.g., "9,Decaf#!!$/%/") -- they contain special characters, 
# like $ -- this poses a problem for is.drink (the check in referenceTable$Description returns 0). 
# Fixing with regexs:

allOrders$orderContents    <- str_replace_all(allOrders$orderContents, "#!!\\$/%/", "")
referenceTable$Description <- str_replace_all(referenceTable$Description, "#!!\\$/%/", "")

Encoding(allOrders$orderContents) <- "UTF-8"

# The star of the show:

allOrders$orderContents <- sapply(allOrders$orderContents, dropNonDrinks)

# Investigating how many observations we're dropping (i.e., how many do not contain any drinks
# and hence are now empty):

1-nrow(allOrders[!(is.na(allOrders$orderContents) | allOrders$orderContents==""), ])/nrow(allOrders)

# This shows we're getting rid of about 13% of all orders. (That is, in 87% of all orders,
# the customers ordered drinks.) Initially, we had 69,397 orders, and now we have 60,332 left.

try(file.remove("0-drinks-only-orders.csv"), silent=TRUE)

write.csv(allOrders[!(is.na(allOrders$orderContents) | allOrders$orderContents==""), ], 
          file = "0-drinks-only-orders.csv",
          row.names=FALSE)

# The resultant file is ~3.23MB. (We got rid of about 1.14MB of data compared to the last file.)