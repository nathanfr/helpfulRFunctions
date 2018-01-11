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