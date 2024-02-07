###################################################################################################
#
# Portal 173 - Functions
#
###################################################################################################


coalesceNA <- function(character, replacement){
  if(is.na(character)){
    replacement
  } else {
    character
  }
}

coalesceNULL <- function(character, replacement){
  if(is.null(character)){
    replacement
  } else {
    character
  }
}