library(stringr)
library(stringi) #stri_isempty
setwd('C:/Coffee and Weather Code/data')

# # DESCRIPTION --------------------------------------------------------------------------------------
# ~ ~ ~ ~ ~ ~ 
# This second script in the project creates a data frame of all orders available within the files.
# To achieve this, it does the following:
#
# (1) replicates the wrangling procedure & creates a vector of all lines within file
# (2) creates a data frame of ORDERS & examines the vector's elements according to a reference table
#     generated in the first script and edited manually to separate menu items from modifiers
#            ~ ~ ~

# # FUNCTION: READ & WRANGLE A DAILY SALES FILE, GENERATE DATAFRAME OF ORDERS FROM THAT DAY ----------
#runFile <- function(x) {


dateSequence <- seq(from = as.Date("2016-09-01"),
                    to = as.Date("2017-11-22"),
                    by = "days")
dateSequence <- format(dateSequence, "%Y%m%d")

x<-dateSequence[448]
dFile <- readLines(paste0("Master-", x, "_000000-", x, "_235959.csv"), encoding="UTF-8")

# = WRANGLING ========================================================================================

dFile <- unname(sapply(dFile, function(x) {
  gsub('\"', "", x)
}))

fileDate <- as.Date(substr(dFile[2], 1, 10), format="%Y-%m-%d")

orderRows <- unname(sapply(dFile, function(x) {
  grepl("([[:digit:]]+,+)|(^[[:digit:]]+ \\<Cancelled\\> ,,$)|(^[[:digit:]]+ \\<Refunded\\> ,,$)", x)
}))

dFile <- dFile[orderRows]

if ( length(dFile) > 0 ) {
  
  orderCounter <- unname(sapply(dFile, function(x) {
    grepl( "(^[[:digit:]]{1,3},{2}$)|(^[[:digit:]]+ \\<Cancelled\\> ,,$)|(^[[:digit:]]+ \\<Cancelled\\> ,[[:digit:]]+,$)|(^[[:digit:]]+,[[:digit:]]+,$)|(^[[:digit:]]+ \\<Refunded\\> ,)", x)
  }))

  dFile <- unname(sapply(dFile, function(x) {
    gsub("(,,+)|(,+$)", "", x)
  }))

  dFile <- dFile[dFile != "00.00"]
  
  orderTimes <- unname(sapply(dFile, function(x) {
    str_extract_all(x, "[[:digit:]]+:[[:digit:]]+")
  }))
  orderTimes <- unlist(orderTimes[lapply(orderTimes,length)>0])

  dFile <- unname(sapply(dFile, function(x) {
    gsub(".*:[[:digit:]]{2}$", "", x)
  }))
  dFile <- dFile[dFile != ""]
  
  dFile <- unname(sapply(dFile, function(x) {
    gsub("(,+[[:digit:]]+.[[:digit:]]+$)|(,+[[:digit:]]+.[[:digit:]]+ \\(-[[:digit:]]+.[[:digit:]]+%\\) \\:\\: [[:digit:]]+.[[:digit:]]+$)|(,+[[:digit:]]+.[[:digit:]]+ \\(-[[:digit:]]+%\\) \\:\\: [[:digit:]]+.[[:digit:]]+$)", "", x)
  }))

  dFile <- unname(sapply(dFile, function(x) {
    gsub(",[[:digit:]]+$", "", x)
  }))

  dFile <- unname(sapply(dFile, function(x) {
    gsub("^([^,]*,)|,", "\\1", x)
  }))
  

  # for loop: 
  # condition - vector element X corresponds to an ORDER NUMBER
  #       if true, 
  #               (create new row in daysOrders - i.e. create a new observation)
  #               daysOrders[nrow(daysOrders)+i, orderNum] = X
  #                                              orderTime = fileDate combined with HH:MM from orderTimes[?]
  #               if canceled/refunded -> discardLater=1, else 0
  #       else
  #               if orderContents empty, just insert X
  #               else insert after "{" (a symbol that doesn't appear in files)
  
  # daysOrders <- data.frame(orderNum=character(),            # number of the order as per that day's numeration
  #                          orderTime=as.Date(character()),  # time of the order as combination of fileDate & HH:MM in orderTimes
  #                          discardLater=logical(),          # dummy for cancelled and refunded orders
  #                          orderContents=character())       # string containing items ordered, broken up by "{" symbol

  orderPattern <- "(^[[:digit:]]+$)|(^[[:digit:]]+ \\<Cancelled\\> $)|(^[[:digit:]]+ \\<Refunded\\> $)"
  
  daysOrders <- data.frame(matrix(ncol=4,nrow=0))
  colnames(daysOrders) <- c("orderNum","orderTime","discardLater","orderContents")
  
  i <- 1
  orderCount <- 1
  for(i in 1:length(dFile)) {
    oldRow <- nrow(daysOrders)
    newRow <- nrow(daysOrders)+1

    print(paste0("in iteration ", i, " we're working with '", dFile[i], "' old row is ",oldRow, " and new row is ",newRow))
        
    if (grepl(orderPattern, dFile[i])) {

      daysOrders[newRow, 1] <- dFile[i]
      daysOrders[newRow, 2] <- fileDate
      # if (grepl("(^[[:digit:]]+ \\<Cancelled\\> $)|(^[[:digit:]]+ \\<Refunded\\> $)", dFile[i])) {
      #          daysOrders[newRow, 3] <- TRUE
      # } else { daysOrders[newRow, 3] <- FALSE }
    } else { 
            if (is.null(daysOrders[oldRow, 4]) || daysOrders[oldRow, 4]=='') { daysOrders[oldRow, 4]=dFile[i] 
              } else { 
                        daysOrders[oldRow, 4]=paste(daysOrders[oldRow, 4],dFile[i],sep="{")
           }
    i <- i + 1
    orderCount <- orderCount+1
      }
    
  
  # itemIDs   <- unlist( strsplit(dFile[items], ",") )
  # itemNums  <- itemIDs[seq(1, length(itemIDs), 2)] 
  # itemDescs <- itemIDs[seq(0, length(itemIDs), 2)]
  # 
  # daysOrders <- data.frame( as.integer(itemNums), itemDescs )

  }  
#}