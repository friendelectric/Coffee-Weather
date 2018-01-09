library(dplyr)
library(tidyr)
library(sqldf)
setwd('C:/Coffee and Weather Code/data')

Drinks <- read.csv("0-drinks.csv")
colnames(Drinks) <- gsub("\\.", "_", colnames(Drinks))

# tidyr has function separate for this, would like to try it:
Drinks <- separate(Drinks, Time, into=c("Day", "HH:MM"), sep = " ")
Drinks$Day <- as.Date(Drinks$Day, "%Y-%m-%d")

# generating vector of days
days <- as.Date.character(unique(Drinks$Day))

# generating vector of codedVariables
codedVariables <- colnames(Drinks)[5:19]

readVarSQL <- function(variable) {
  require("sqldf")
  return(sqldf(paste0("select DISTINCT ",variable," from Drinks")))
}
values <- sapply(codedVariables, readVarSQL)
names(values) <- gsub("\\.[[:print:]]+$", "", as.character(names(values)))

# ^ pls don't mind the messy NAs and "" values in some columns - - 
# still working on codebook & how to code some of the vars in particular

# The following are examples of what I'd like to do within a function:

size <- Drinks %>% group_by(Day) %>% count(Size)
size <- as.data.frame(spread(size, Size, n, fill=0))
colnames(size) <- paste0("Size_",colnames(size))
colnames(size)[1] <- "Day"

tea <- Drinks %>% group_by(Day) %>% count(Tea)
tea <- as.data.frame(spread(tea, Tea, n, fill=0))
colnames(tea)[3] <- "Tea"
tea <- tea[,c(1,3)]

espresso <- Drinks %>% group_by(Day) %>% count(Espresso)
espresso <- as.data.frame(spread(espresso, Espresso, n, fill=0))
colnames(espresso)[3] <- "Espresso"
espresso <- espresso[,c(1,3)]

vars <- left_join(size, tea, espresso, by=c("Day" = "Day"))


# Attempting to do the same within a function:

library(lazyeval)
makeDF <- function(columnName, dataset) {
  columnName <- interp(~ x, x=as.name(columnName))

  DF <- dataset %>% group_by(Day) %>% count(columnName)
  DF <- as.data.frame(spread(DF, columnName, n, fill=0))
  colnames(DF)[2:length(colnames(DF))] <- paste0(columnName,"_",colnames(DF)) 
}
sapply(codedVariables, makeDF, Drinks)

# another try:

makeDF <- function(columnName, dataset) {
  columnName <- enquo(columnName)
  
  DF <- dataset %>% group_by(Day) %>% count(!!columnName)
  DF <- as.data.frame(spread(DF, !!columnName, n, fill=0))
  colnames(DF)[2:length(colnames(DF))] <- paste0(columnName,"_",colnames(DF)) 
}
sapply(codedVariables, makeDF, Drinks)

# another one:

makeDF <- function(columnName, dataset) {
  DF <- dataset %>% group_by(Day) %>% count(!!columnName)
  DF <- as.data.frame(spread(DF, !!columnName, n, fill=0))
  colnames(DF)[2:length(colnames(DF))] <- paste0(columnName,"_",colnames(DF)) 
}
sapply(quo(codedVariables), makeDF, Drinks)