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
