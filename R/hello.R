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
