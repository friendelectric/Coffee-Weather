library(stringr)
setwd('C:/Coffee and Weather Code/data')

# # DESCRIPTION --------------------------------------------------------------------------------------
# ~ ~ ~ ~ ~ ~ 
# This second script in the project creates a data frame of all orders available within the files.
# The goals of this script are to:
#
#  (1) create a vector of all lines within a daily sales file via wrangling procedure from script 1
#  (2) create a data frame of that day's orders, including cancelled and refunded orders
#  (3) create a data frame of all completed orders, having dropped cancelled and refunded orders
#            ~ ~ ~

# # FUNCTION: READ & WRANGLE A DAILY SALES FILE, GENERATE & EXPORT DATA FRAME OF ORDERS --------------

# As in the first script, this function is fed a date which features in a file name.

runFile <- function(x) {
  
  dFile <- readLines(paste0("Master-", x, "_000000-", x, "_235959.csv"), encoding="UTF-8")
  
  # = WRANGLING ======================================================================================
  
  # This wrangling procedure from script 1 is pasted here w/o comments.
  
  dFile <- unname(sapply(dFile, function(x) { gsub('\"', "", x) }))
  
  fileDate <- as.Date(substr(dFile[2], 1, 10), format="%Y-%m-%d")
  
  orderRows <- unname(sapply(dFile, function(x) {
    grepl("([[:digit:]]+,+)|(^[[:digit:]]+ \\<Cancelled\\> ,,$)|(^[[:digit:]]+ \\<Refunded\\> ,,$)", x)
  }))
  
  dFile <- dFile[orderRows]
  
  if ( length(dFile) > 0 ) {
    
    dFile <- unname(sapply(dFile, function(x) { gsub("(,,+)|(,+$)", "", x) }))
    
    dFile <- dFile[dFile != "00.00"]
    
    orderTimes <- unname(sapply(dFile, function(x) { str_extract_all(x, "[[:digit:]]+:[[:digit:]]+") }))
    orderTimes <- unlist(orderTimes[lapply(orderTimes,length)>0])
    
    dFile <- unname(sapply(dFile, function(x) { gsub(".*:[[:digit:]]{2}$", "", x)  }))
    
    dFile <- dFile[dFile != ""]
    
    dFile <- unname(sapply(dFile, function(x) {
      gsub("(,+[[:digit:]]+.[[:digit:]]+$)|(,+[[:digit:]]+.[[:digit:]]+ \\(-[[:digit:]]+.[[:digit:]]+%\\) \\:\\: [[:digit:]]+.[[:digit:]]+$)|(,+[[:digit:]]+.[[:digit:]]+ \\(-[[:digit:]]+%\\) \\:\\: [[:digit:]]+.[[:digit:]]+$)", "", x)
    }))
    
    dFile <- unname(sapply(dFile, function(x) { gsub(",[[:digit:]]+$", "", x) }))
    
    dFile <- unname(sapply(dFile, function(x) { gsub("^([^,]*,)|,", "\\1", x) }))
    
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
    #
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
    
    orderPattern <- "(^[[:digit:]]+$)|(^[[:digit:]]+ \\<Cancelled\\> $)|(^[[:digit:]]+ \\<Refunded\\> $)"
    
    daysOrders <- data.frame(matrix(ncol=4,nrow=0))
    colnames(daysOrders) <- c("orderNum","orderTime","discardLater","orderContents")
    
    # See block of comments above for an explanation of the why and how of the following "for" loop.
    
    i <- 1
    orderCount <- 1
    
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

# This generates a 5.35MB CSV file containing all orders made at the coffee shop on days marked
# in the dateSequence vector. There is a total of 70,183 orders recorded, including cancelled and
# refunded orders.

# = FINAL OUTPUT ===================================================================================

orders <- read.csv("0-all-orders.csv")
orders <- orders[-1]

#  sum(orders$discardLater) via console shows a total of 786 orders were cancelled or refunded.
# sum(!orders$discardLater) via console shows a total of 69,397 orders were completed, and hence 
# will be kept for analysis. Now, dropping the observations for cancelled and refunded orders (just
# over 1% of all orders recorded), as well as the discardLater column itself and saving into a file.

orders <- orders[!orders$discardLater, ]
orders <- orders[-3]

write.csv(orders, file = "0-all-completed-orders.csv", row.names=FALSE)

# This generates a 4.37MB CSV file with all 69,397 actual sales made from 2016-09-01 to 2017-11-12.
#
# (The difference compared to the previously generated file is about 1MB. Obviously, the dropped 
# observations wouldn't take up that much space. In effect, this final file is significantly smaller 
# because it drops an unnecessary column of row names.)