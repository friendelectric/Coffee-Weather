library(stringr)
library(dplyr)
library(stringi)
setwd('C:/Coffee and Weather Code/data')

# # DESCRIPTION --------------------------------------------------------------------------------------
# ~ ~ ~ ~ ~ ~ ~
#                GENERATING A DATA FRAME OF ALL ORDERS COMPELETED AT THE CAFE
#
# This second script in the project creates a data frame of all orders available within the files.
# The goals of this script are to:
#
#  (1) create a vector of all lines within a daily sales file via wrangling procedure from script 1,
#  (2) create a data frame of that day's orders, including cancelled and refunded orders, and
#  (3) create a data frame of all completed orders, having dropped cancelled and refunded orders.
#            ~ ~ ~

# # FUNCTION: READ & WRANGLE A DAILY SALES FILE, GENERATE & EXPORT DATA FRAME OF ORDERS --------------

# As in the first script, this function is fed a date which features in a file name.

runFile <- function(x) {
  
  dFile <- readLines(paste0("Master-", x, "_000000-", x, "_235959.csv"), encoding="UTF-8")
  
  dFile <- stri_trans_general(dFile, "Latin-ASCII")
  Encoding(dFile) <- "Latin-ASCII"
  dFile <- enc2utf8(dFile)
  Encoding(dFile) <- "UTF-8"
  
  # = WRANGLING ======================================================================================
  
  # This wrangling procedure from script 1 is pasted here w/o comments.
  
  dFile <- dFile[-1]
  dFile <- unname(sapply(dFile, function(x) { gsub('\"', "", x) }))
  
  fileDate <- as.Date(substr(dFile[1], 1, 10), format="%Y-%m-%d")
  print(fileDate)
  
  dFile <- dFile[-1] 
  
  emptyRows <- unname(sapply(dFile, function(x) { grepl("(^,+$)|(^\\-+,+$)|(^\\-+$)|(^[[:alpha:]]+ )", x) }))
  dFile <- dFile[!emptyRows]
  
  is.empty <- sapply(dFile, function(x) { if (x=="") {return(TRUE)} else {return(FALSE)} })
  
  if ( (length(dFile) > 0)&&(!all(is.empty)) ) {

    orderCounter <- unname(sapply(dFile, function(x) {
      grepl( "(^[[:digit:]]{1,3},+$)|(^[[:digit:]]{1,3},[[:digit:]]+,+$)|(^[[:digit:]]+ \\<Cancelled\\> ,+$)|(^[[:digit:]]+ \\<Cancelled\\> ,[[:digit:]]+,$)|(^[[:digit:]]+,[[:digit:]]+,$)|(^[[:digit:]]+ \\<Refunded\\> ,)", x)
    }))
    orderCounter <- sum(orderCounter)
    
    dFile <- unname(sapply(dFile, function(x) { gsub("(,,+)|(,+$)", "", x) }))
    
    dFile <- dFile[dFile != "00.00"]
    
    orderTimes <- unname(sapply(dFile, function(x) { str_extract_all(x, "[[:digit:]]+:[[:digit:]]+") }))
    orderTimes <- unlist(orderTimes[lapply(orderTimes,length)>0])
    
    if (length(orderTimes)!=orderCounter)
    { stop("Number of orders recorded (", orderCounter, ") doesn't match the number of timestamps ", 
           length(orderTimes), "  extracted in ", fileDate, " (dateSequence[", 
           match(format(fileDate,"%Y%m%d"),dateSequence), "])") }
    
    dFile <- unname(sapply(dFile, function(x) { gsub(".*:[[:digit:]]{2}$", "", x) }))
    dFile <- dFile[dFile != ""]
    
    dFile <- unname(sapply(dFile, function(x) { gsub("(,+[[:digit:]]+.[[:digit:]]+$)|(,+[[:digit:]]+.[[:digit:]]+ \\(-[[:digit:]]+.[[:digit:]]+%\\) \\:\\: [[:digit:]]+.[[:digit:]]+$)|(,+[[:digit:]]+.[[:digit:]]+ \\(-[[:digit:]]+%\\) \\:\\: [[:digit:]]+.[[:digit:]]+$)", "", x)}))
    
    dFile <- unname(sapply(dFile, function(x) { gsub('#!!\\$/%/', '', x) }))
    dFile <- unname(sapply(dFile, function(x) { gsub("\\$", "", x) }))
    
    dFile <- unname(sapply(dFile, function(x) { gsub(",[[:digit:]]+$", "", x) }))
    
    dFile <- unname(sapply(dFile, function(x) { gsub("^([^,]*,)|,", "\\1", x) }))
    
    dFile <- unname(sapply(dFile, function(x) { gsub("\\(|\\)|\\:\\:", "", x) }))
    
    wordsOnly <- unname(sapply(dFile, function(x) { grepl("(^[[:alpha:]]+|[[:punct:]]+$)|(\\<certificats\\>)", x) }))
    dFile <- dFile[!wordsOnly]
    
    if (fileDate=="2017-10-10") { dFile <- dFile[-434] }
    if (fileDate=="2017-10-27") { dFile <- dFile[-356] }

    # = GENERATING A DATA FRAME ========================================================================
    
    # The wrangling procedure above produces a vector called dFile containing a sequence of
    # numbers of orders and items ordered. For example, see this snippet of dFile from one of the days:
    #
    #  [1] "3,Latte 8oz"      "4,Lait ordinaire" "15" "11,Latte 12 oz"   "4,Lait ordinaire" "16"
    #  [7] "7,Americano 12oz" "30,Chocolatine"   "17" "7,Americano 12oz" "9,Decaf#!!$/%/"  
    #
    # A "for" loop is necessary in this case, because there is no way of knowing how many menu items
    # and modifiers are in each order. So, it's not possible to use the apply family here, as 
    # what the script does with each successful element of the vector depends on the other elements.
    #
    # In the dFile vector, it's easy to recognize the pattern of how an element containing an order
    # number is structured. So, the following "for" loop is built around a regex condition that spots 
    # elements within dFile that contain order numbers, and either creates a new row if it encounters
    # an order number, or, if dFile element contains menu items and modifiers, continues to append them 
    # to a previously recorded order.
    
    orderPattern <- "(^[[:digit:]]+$)|(^[[:digit:]]+ \\<Cancelled\\> $)|(^[[:digit:]]+ \\<Refunded\\> $)"
    
    # This is all recorded in daysOrders, a data frame of the day's orders. While data types for columns
    # are not defined explicitly in the script below, here is daysOrders' projected structure:
    #
    #   c o l u m n   | d e s c r i p t i o n
    #   ..............|..................................................................................
    #   orderNum      | number of the order as per that day's numeration within the cash register system
    #                 | (this includes cancelled and refunded orders--i.e., "[[:digit:]]+ Cancelled "-- 
    #                 | hence it should be a character() for now)
    #   orderTime     | time of the order as combination of fileDate & HH:MM in orderTimes
    #                 | (storing as character string for now, planned to be deparsed later via lubridate)
    #   discardLater  | dummy for cancelled and refunded orders
    #                 | (used for exploration purposes--curious about how many such orders there are)
    #   orderContents | string containing items ordered, broken up by "{" symbol
    #                 | ("{" was chosen because it's a symbol that does not appear in any of the files,
    #                 | which was quickly verified by running a grepWin search on the folder w/ the data)
    #         ~ ~ ~
    
    daysOrders <- data.frame(matrix(ncol=4,nrow=0))
    colnames(daysOrders) <- c("orderNum","orderTime","discardLater","orderContents")
    
    i <- 1
    orderCount <- 1
    
    # See block of comments above for an explanation of the why and how of the following "for" loop.
    
    for(i in 1:length(dFile)) {
      
      oldRow <- nrow(daysOrders)
      newRow <- nrow(daysOrders)+1
      
      if (grepl(orderPattern, dFile[i])) {
        
        daysOrders[newRow, 1] <- dFile[i]
        
        timeString <- as.character(paste(fileDate, orderTimes[orderCount], sep=" "))
        daysOrders[newRow, 2] <- timeString
        
        if (grepl("(^[[:digit:]]+ \\<Cancelled\\> $)|(^[[:digit:]]+ \\<Refunded\\> $)", dFile[i])) 
        
             { daysOrders[newRow, 3] <- TRUE  } 
        
        else { daysOrders[newRow, 3] <- FALSE }
        
        orderCount <- orderCount+1
        
      } else { 
        
        if (is.na(daysOrders[oldRow, 4]) || daysOrders[oldRow, 4]=='') 
        
             { daysOrders[oldRow, 4] = dFile[i] } 
        
        else { daysOrders[oldRow, 4] = paste(daysOrders[oldRow, 4], dFile[i], sep="{") }
        
      }
      i <- i + 1    
    }
    
    # Exporting to a file: reading what's in it, and binding the two data frames.
    
    fileName <- "0-all-orders.csv"
    
    if (!file.exists(fileName)) { file.create(fileName) }
    
    fileWithOrders <- try(read.csv(fileName), silent=TRUE)
    write.csv(rbind(fileWithOrders[-1], daysOrders), file = fileName)
    
  } 
}

# = FUNCTION CALL ==================================================================================

  dateSequence <- seq(from = as.Date("2016-09-01"),
                      to = as.Date("2017-11-22"),
                      by = "days")
  dateSequence <- format(dateSequence, "%Y%m%d")
  
  sapply(dateSequence, runFile)

  # This generates "0-all-orders.csv" containing all orders made at the cafe 
  # on days marked in the dateSequence vector.

# = FINAL OUTPUT ===================================================================================

  orders <- read.csv("0-all-orders.csv")
  orders <- orders[-1]

  sum(orders$discardLater)
  sum(!orders$discardLater)
  sum(orders$discardLater)/nrow(orders)*100
  
  # 786 orders (a little over 1%) were cancelled or refunded and 69,397 orders were completed.
  
  # Dropping the observations for cancelled and refunded orders, as well as the discardLater column,
  # and saving into a file:

  orders <- orders[!orders$discardLater, ]
  orders <- orders[-3]
  
  write.csv(orders, file = "0-all-completed-orders.csv", row.names=FALSE)

  # This generates a 4.35MB CSV file with all 69,397 actual sales made from 2016-09-01 to 2017-11-12.