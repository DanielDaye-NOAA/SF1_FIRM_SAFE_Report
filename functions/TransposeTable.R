#' TransposeTable
#'
#' Transposes data within a table to fit the format required for tables in the 
#' SAFE report.
#'
#' If colToRowName == TRUE, the leftmost column will be made into
#' column names for the flipped table; otherwise, the row names will be converted
#' to the new column names ()
#'
#' Example:
#' exdat <- data.frame(x=c("A","B","C"), y=c(1,2,3), z=c("A1","B2","C3"))
#' exdat %>% TransposeTable()
#' exdat %>% TransposeTable(colToRowName = FALSE)

TransposeTable <- function(data, colToRowName = TRUE) {
  require(tidyverse)
  
  if (colToRowName) {
    # Transpose table and convert back to df
    data <- data %>% t() %>% as.data.frame()
    # Fix column names
    names(data) <- data[1,]
    data <- data[-1,]
  } else {
    # Transpose and don't change col names
    data <- data %>% t() %>% as.data.frame()
  }
  return(data)
}