# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

#' Convert all character columns in a data frame to factors
#'
#' @param df a dataframe
#' @param exclude a vector of column indices to exclude
#' @return The same dataframe with the character columns converted to factors
#' @export
convertCharacterColsToFactor <- function(df, exclude=NULL){

  dat <- df

  # looping through each of the columns
  for(i in 1:ncol(dat)){
    if(sapply(dat, class)[i] %in% c('character') & !(i %in% exclude)){
      dat[,i] <- as.factor(dat[,i])
    }
  }

  return(dat)

}

#' Remove duplicate columns from a data frame
#'
#' @param df a dataframe
#' @return The same dataframe without any duplicate columns (this is based on the name of the column)
removeDuplicateColumns <- function(df){

  data <- as.data.frame(df)
  newdf <- data.frame()
  for(i in 1:ncol(data)){
    if(dim(newdf)[1] == 0){
      newdf <- as.data.frame(data[,i])
      names(newdf) <- names(data)[i]
    } else if(names(data)[i] %in% names(newdf)){
      print('Variable already in dataset')
    } else {
      newdf <- cbind(newdf, as.data.frame(data[,i]))
      names(newdf) <- c(names(newdf)[1:length(names(newdf))-1], names(data)[i])
    }
  }

  return(newdf)

}

#' Plots missing data by column in a simple bar chart
#'
#' @param df a dataframe
#' @param prop A boolean: TRUE if you want proportions plotted, FALSE if you want raw numbers
### want to write a function to inspect proportion of missing values
plotMissing <- function(df, prop=TRUE){

  # required packages
  require(dplyr)
  require(tidyr)
  require(plotly)

  # getting the baseline numbers
  miss = sapply(df, function(x) sum(is.na(x)))
  tots = sapply(df, length)

  if(prop==TRUE){
    mets <- as.data.frame(miss/tots)
    mets$name <- row.names(mets)
    names(mets) <- c('mets', 'name')
    p <- plot_ly(data=mets, x=~name, y=~mets, type='bar') %>%
      layout(margin=list(b=90, r=90), title='Proportion of Missing Data by Column',
             xaxis=list(title='Column'), yaxis=list(title='Proportion Missing'))
  } else {
    mets <- as.data.frame(miss)
    mets$name <- row.names(mets)
    names(mets) <- c('mets', 'name')
    p <- plot_ly(data=mets, x=~name, y=~mets, type='bar') %>%
      layout(margin=list(b=90, r=90), title='Missing Data by Column',
             xaxis=list(title='Column'), yaxis=list(title='Missing'))
  }

  print(p)
}

#' Loads in a sql file
#'
#' @param path String: The path to a sql file
sql.read <- function(path=getwd()){
  sql <- readChar(path, file.info(path)$size)
  return(sql)
}

#' Finds the indices of all character occurrences in a string. This only works with single
#' characters.
#'
#' @param string String: The character string you want to search
#' @param item String: The item you would like to find in the string
#' @return A list of all of the indices where the character can be found
find_occurrences <- function(string, item){

  # first check to see if the item is in the string at all
  if(grepl(item, string)==FALSE){
    print(paste0(item, ' is not in ', string))
    return()
  }

  # set up a list to store values
  list_iter <- 1
  storage = list()

  # loop through each of the characters in the string to check
  for(iter in 1:nchar(string)){
    # if the item is there, add to the list of indices
    if(string[iter] == item){
      storage[[list_iter]] <- string[iter]
      list_iter <- list_iter + 1
    }
  }

  return(storage)

}

#' Counts the number of NAs in a column
#'
#' @param col String: The column you want to use
#' @return An integer with the number of NAs in that column
sumNAs <- function(col) {
  return(sum(is.na(col)))
}

#' Counts the number of NAs in all columns in a data frame
#'
#' @param df a Dataframe
#' @return An integer vector containing nas in the column
detectNAs <- function(df) {
  naCols <- apply(df, 2, sumNAs)
  naCols <- as.data.frame(naCols)
  naCols$column <- row.names(naCols)
  row.names(naCols) <- NULL
  return(naCols)
}

#' Counts the number of blanks in a column
#'
#' @param col String: The column you want to use
#' @return An integer with the number of blanks in that column
sumBlanks <- function(col) {
  return(sum(col==''))
}

#' Counts the number of blanks in all columns in a data frame
#'
#' @param df a Dataframe
#' @return An integer vector containing blanks in the column
detectBlanks <- function(df) {
  blankCols <- apply(df, 2, sumBlanks)
  blankCols <- as.data.frame(blankCols)
  blankCols$column <- row.names(blankCols)
  row.names(blankCols) <- NULL
  return(blankCols)
}

#' Counts the number of distinct values in a character or factor column
#'
#' @param col the column you want to understand
#' @return An integer with the number of distinct values
cardinality <- function(col) {
  if(class(col) == 'character') {
    return(length(unique(col)))
  } else {
    return(-1)
  }
}

#' Counts the number of unique values in all columns in a data frame
#'
#' @param df The dataframe
#' @return a Dataframe of all of the columns and their cardinality
detectCardinality <- function(df) {
  cards <- sapply(df, cardinality)
  cards <- as.data.frame(cards)
  cards$column <- row.names(cards)
  row.names(cards) <- NULL
  return(cards)
}
