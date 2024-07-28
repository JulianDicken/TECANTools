# utility function
# Function to calculate width and height of a range from cell references
Utility.excel_bbox = function(cell_refs) {
  start_cell  = cell_refs[1]
  end_cell    = cell_refs[2]

  start_row = as.numeric(gsub("[A-Z]", "", start_cell))
  end_row   = as.numeric(gsub("[A-Z]", "", end_cell))
  start_col = Utility.excel_to_numindex(gsub("[0-9]", "", start_cell))
  end_col   = Utility.excel_to_numindex(gsub("[0-9]", "", end_cell))

  return(list(width = end_col - start_col + 1, height = end_row - start_row + 1))
}
Utility.excel_to_numindex <- function(colindex) {
  numindex <- 0
  length_colindex <- nchar(colindex)

  for (i in 1:length_colindex) {
    current_char  <- substr(colindex, i, i)
    current_value <- match(current_char, LETTERS)
    numindex      <- numindex * 26 + current_value
  }

  return(numindex)
}
Utility.excel_to_colindex <- function(numerical_index) {
  excel_colindex <- ""

  while (numerical_index > 0) {
    remainder       <- (numerical_index - 1) %% 26
    excel_colindex  <- paste0(LETTERS[remainder + 1], excel_colindex)
    numerical_index <- (numerical_index - 1) %/% 26
  }

  return(excel_colindex)
}
