###################################################################################################
#
# Portal 173 - Functions
#
###################################################################################################


coalesceNA <- function(character, replacement){
  if(length(character) == 0 || is.na(character)){
    replacement
  } else {
    character
  }
}

coalesceNULL <- function(character, replacement){
  if(length(character) == 0 || is.null(character)){
    replacement
  } else {
    character
  }
}

coalesceLengthZero <- function(character, replacement){
  if(length(character) == 0 || is.null(character) || is.na(character)){
    replacement
  } else {
    character
  }
}