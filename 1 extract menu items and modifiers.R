library(stringr)
library(dplyr)
setwd('C:/Coffee and Weather Code/data')

# # DESCRIPTION --------------------------------------------------------------------------------------
# ~ ~ ~ ~ ~ ~ 
# Going over the 448 daily sales files produced by the cash register, this project's first script:
#
#   (1) wrangles the data using regular expressions, extracting all useful information on orders,
#   (2) appends unique menu items and modifiers from each file to a separate CSV file, and
#   (3) selects unique menu items and modifiers from the resultant CSV file, saving to another file.
#
# The data in the output file will be manually categorized to overcome the main challenge of the data:
#   THE CASH REGISTER SYSTEM DOES NOT DIFFERENTIATE PROPERLY BETWEEN MENU ITEMS ("4, large latte") 
#   AND MENU ITEM MODIFIERS ("4, soy milk"), WHICH RESULTS IN DOUBLE-BOOKED I.D. NUMBERS
#   REFERRING TO BOTH MENU ITEMS AND MENU ITEM MODIFIERS.
#
# To proceed, it is first necessary to extract a list of all menu items and their modifiers, and
# separate them into two piles manually. After that is done, it will be possible to move on to the 
# next stage of the project: building data frames of items ordered during each day.
#            ~ ~ ~

# # FUNCTION: READ & WRANGLE A FILE, DUMP UNIQUE MENU ITEMS & MODIFIERS INTO SEPARATE CSV ------------
runFile <- function(x) {

# File names are standard and contain the date. Function will be fed the sequence of dates 
# for which there are files available. Reading lines from a file:
  
dFile <- readLines(paste0("Master-", x, "_000000-", x, "_235959.csv"), encoding="UTF-8")

# = WRANGLING ========================================================================================

# head(dFile) via console showed file contains escape backslashes with quotes all over; removing:

dFile <- unname(sapply(dFile, function(x) {
  gsub('\"', "", x)
}))

# Saving the file's date into variable fileDate before dropping lines that don't contain orders:

fileDate <- as.Date(substr(dFile[2], 1, 10), format="%Y-%m-%d")

# Every order number begins with a row where a [[:digit:]]+ is followed by ",,,,,,,,".
# Hence, relevant lines in the file can be grabbed with regex "[[:digit:]]+,+" for a completed order.
# But, there are also outlier orders, like cancelled and refunded. Including regexs for them, too:

orderRows <- unname(sapply(dFile, function(x) {
  grepl("([[:digit:]]+,+)|(^[[:digit:]]+ \\<Cancelled\\> ,,$)|(^[[:digit:]]+ \\<Refunded\\> ,,$)", x)
#       normal order num | cancelled order                   | refunded order
  }))

# Keeping only rows with order information:

dFile <- dFile[orderRows]

# If the shop was closed, the file will have 0 relevant lines, because there was 0 orders.
# Only proceed with the function if there's more than zero order rows:

if ( length(dFile) > 0 ) {

# Counting all orders, including completed, cancelled, refunded, and non-sequential
# (the cash register appears to push some orders in non-sequential positions to conserve memory,
# and these orders are formatted a bit differently):

orderCounter <- unname(sapply(dFile, function(x) {
  grepl( "(^[[:digit:]]{1,3},{2}$)|(^[[:digit:]]+ \\<Cancelled\\> ,,$)|(^[[:digit:]]+ \\<Cancelled\\> ,[[:digit:]]+,$)|(^[[:digit:]]+,[[:digit:]]+,$)|(^[[:digit:]]+ \\<Refunded\\> ,)", x)
# descr. | compeleted order:      |  cancelled order:                 | cancelled order (with digits):                | non-sequential order:        | refunded order
#        #                        #                                   #                                               #                              #          
# format | number two commas      |  number Cancelled two commas      | number Cancelled comma number comma           | number comma number comma    | number Refunded
}))
orderCounter <- sum(orderCounter)

# Later on, orderCounter must match the number of order timestamps extracted.

# Cleaning up unnecessary commas:

dFile <- unname(sapply(dFile, function(x) {
  gsub("(,,+)|(,+$)", "", x)
}))

# In at least one file, 2017-09-29, some elements literally equal to "00.00" (origin unclear). Removing:

dFile <- dFile[dFile != "00.00"]

# Extracting a vector of order timestamps (everything that matches HH:MM pattern):

orderTimes <- unname(sapply(dFile, function(x) {
  str_extract_all(x, "[[:digit:]]+:[[:digit:]]+")
}))
orderTimes <- unlist(orderTimes[lapply(orderTimes,length)>0])

# Checking that orderTimes gives the correct # of orders:

if (length(orderTimes)!=orderCounter) 
{stop("Number of orders recorded doesn't match the number of timestamps extracted in ",
      fileDate, " (dateSequence[", match(format(fileDate,"%Y%m%d"),dateSequence), "])")}

# Removing elements containing timestamps (all of them end in "HH:MM"):

dFile <- unname(sapply(dFile, function(x) {
  gsub(".*:[[:digit:]]{2}$", "", x)
}))
dFile <- dFile[dFile != ""]

# Removing elements containing prices:

dFile <- unname(sapply(dFile, function(x) {
  gsub("(,+[[:digit:]]+.[[:digit:]]+$)|(,+[[:digit:]]+.[[:digit:]]+ \\(-[[:digit:]]+.[[:digit:]]+%\\) \\:\\: [[:digit:]]+.[[:digit:]]+$)|(,+[[:digit:]]+.[[:digit:]]+ \\(-[[:digit:]]+%\\) \\:\\: [[:digit:]]+.[[:digit:]]+$)", "", x)
# descr.|normal price indicator       | discounted price indicator: percentage with decimals                                            | discounted price indicator: percentage without decimals
#       #                             #                                                                                                 #
# format|comma price[dd.dd] end of str| "16,Drink 12oz,0.00 (-100.00%) :: 0.00"                                                         | "4,Ordinary milk,0.00 (-100%) :: 0.00"
}))
  
# Converting the rare non-sequential orders' numbers into regular order numbers: 
# instead of "orderNumber,digit(s)", use just "orderNumber". (The non-sequential orders are normal/completed, 
# just recorded differently and outside temporal order; no need to keep them in this format).

dFile <- unname(sapply(dFile, function(x) {
  gsub(",[[:digit:]]+$", "", x)
}))

# Substituting commas in item descriptions (e.g., "98,Coffee pot (185g = 2,5L)"):

dFile <- unname(sapply(dFile, function(x) {
  gsub("^([^,]*,)|,", "\\1", x)
}))

# = TESTING ==========================================================================================

# Identifying order numbers and order contents separately via regex (non-sequentials are gone, 
# only have to include compeleted, cancelled, and refunded orders):

orderPattern <- "(^[[:digit:]]+$)|(^[[:digit:]]+ \\<Cancelled\\> $)|(^[[:digit:]]+ \\<Refunded\\> $)"
orders <- grepl(orderPattern, dFile)
items  <- !orders

# All elements containing order numbers must be unique. Checking:
# (Disabled because inconsequential here, and only present in one file - 20171012, dateSequence[407])

# if (length(dFile[orders])!=length(unique(dFile[orders])) ) 
# { stop("Not all elements containing order numbers are unique in ",fileDate,
#        " (dateSequence[", match(format(fileDate,"%Y%m%d"),dateSequence), "]):\n ",
#        length(dFile[orders])," total numbers vs. ",length(unique(dFile[orders]))," unique numbers.") }

# Selecting odd and even among only elements that contain ordered items (*not* order numbers).
# Making two vectors, one for item identification numbers (odd), another for item descriptions (even).

itemIDs   <- unlist( strsplit(unique(dFile[items]), ",") )
itemNums  <- itemIDs[seq(1, length(itemIDs), 2)] 
itemDescs <- itemIDs[seq(0, length(itemIDs), 2)]

# All items described must have identification numbers with them. Checking:

if (length(itemNums)!=length(itemDescs))
{ stop("Not all item descriptions have corresponding identification numbers in ",
       fileDate," (dateSequence[", match(format(fileDate,"%Y%m%d"),dateSequence), "])") }

# = EXPORTING ========================================================================================

# Preparing a data frame with unique item IDs and descriptions from this file:

uniqueItems <- data.frame( as.integer(itemNums), itemDescs )
colnames(uniqueItems) <- c("ID", "Description")
uniqueItems <- uniqueItems[order(uniqueItems$ID),]
# write.csv(uniqueItems, file = paste0("unique-items-", fileDate, ".csv"))

# Appending the data frame to a separate CSV file with unique item IDs and descriptions:

if (!file.exists("0-all-unique-items.csv")) { file.create("0-all-unique-items.csv") }

fileWithItems <- try(read.csv("0-all-unique-items.csv"), silent=TRUE)
bothItemsDFs  <- rbind(fileWithItems[-1], uniqueItems)
write.csv(bothItemsDFs, file = "0-all-unique-items.csv")

}

}

# FUNCTION CALL --------------------------------------------------------------------------------------

# Making a vector sequence of dates for the daily sales files that are available,
# and formatting to match how files are named by the cash register.

 dateSequence <- seq(from = as.Date("2016-09-01"),
                     to = as.Date("2017-11-22"),
                     by = "days")
 dateSequence <- format(dateSequence, "%Y%m%d")

 sapply(dateSequence, runFile)
 
# It takes a couple of minutes to make the 764KB file with 27,562 rows. 

# FINAL OUTPUT: MAKING A FILE CONTAINING UNIQUE MENU ITEMS & MODIFIERS ACROSS 448 FILES --------------

 output <- read.csv("0-all-unique-items.csv")

 output <- output %>% 
   group_by(ID, Description) %>% 
   summarize(Frequency=n())
 
 write.csv(unique(output), "0-menu-items-and-modifiers.csv", row.names=FALSE)

# This makes an 8KB file with 279 menu items & modifiers. 
#
# The file also includes frequencies for how often items feature in the data (1 under "Frequency" 
# would mean the item was sold on one day). Frequencies will be helpful for understanding duplicate 
# items (typos in the cash register system--there are a few--or promotional items). 
#
# The file will be edited manually to make a distinction between menu items and menu item modifiers.
# Resultant data frame will be used to build a proper data frame of items ordered.
 
# LOG 1: COMMON ISSUES FIXED -------------------------------------------------------------------------
# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
# The following is a reference list of random files that were used when developing regular expressions 
# for wrangling and issues (not specific) encountered & fixed during initial manual runs. 
#
# dFile <- readLines('Master-20171122_000000-20171122_235959.csv', encoding="UTF-8") #: regex issues
# dFile <- readLines('Master-20171121_000000-20171121_235959.csv', encoding="UTF-8") #: regex issues
# dFile <- readLines('Master-20170301_000000-20170301_235959.csv', encoding="UTF-8") #: discount not parsed via regex
# dFile <- readLines('Master-20170316_000000-20170316_235959.csv', encoding="UTF-8") #: first order refunded
# dFile <- readLines('Master-20161227_000000-20161227_235959.csv', encoding="UTF-8") #: one cancelled order
# dFile <- readLines('Master-20161105_000000-20161105_235959.csv', encoding="UTF-8") #: no issues
# dFile <- readLines('Master-20160903_000000-20160903_235959.csv', encoding="UTF-8") #: no issues
# dFile <- readLines('Master-20171008_000000-20171008_235959.csv', encoding="UTF-8") #: no issues
# dFile <- readLines('Master-20170128_000000-20170128_235959.csv', encoding="UTF-8") #: no issues
# dFile <- readLines('Master-20170418_000000-20170418_235959.csv', encoding="UTF-8") #: no issues
# dFile <- readLines('Master-20170617_000000-20170617_235959.csv', encoding="UTF-8") #: one cancelled order

# LOG 2: UNCOMMON ISSUES FIXED -----------------------------------------------------------------------
# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
# These issues in particular files were encountered & fixed when running script on the complete 
# sequence of files until an error popped up.
# 
# (0) dateSequence[329] (file "2017-07-26") missing; requested & received from client
# 
# (1) in 2017-06-17, 2016-09-17, 2016-09-13: strsplit by comma in unique item names bit broken by commas in item descriptions
# examples:   "98,Coffee pot (185g = 2,5L)"    "65,Pen 0,7mm"    "64,Pen 0,5mm"
#
# (2) Christmas holidays ("2016-12-24" = dateSequence[115] , 116, 121, 122, ...): shop closed => 0 order rows
# 
# (3) table number appears randomly right after order number (extremely rare)
# in two files only, "2017-03-13" : dateSequence[194] ("1340" after order #107) and 
#                    "2017-09-29" : dateSequence[394] ("930" after order #39 Cancelled)
#
# (4) in 2017-09-29, order #127, item number 0 appears w/ no description and 0 for price

# LOG 3: OTHER ISSUES  -------------------------------------------------------------------------------
# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
# Discovered as result of error messages set up to ensure correct wrangling.
# 
# (0) dateSequence[407]: Not all elements containing order numbers are unique in 2017-10-12:
# 201 total numbers vs. 107 unique numbers. 
#
# ANALYSIS: In this highly irregular file, cash register generated orders 13 thru 106,
# then continued starting with 1, and carried on with 13 thru 107. This doesn't pose a problem for 
# this part of the project, and likely not in the future either (orders have both numeric IDs *and*
# timestamps that identify them). This doesn't happen in ANY other file. Given this, checking for this
# condition was disabled in this script for now (see lines 137-142).