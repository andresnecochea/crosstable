# Add stat table to an existing contingency table
add.tables <- function(x, format=NULL, ...) {
  if (is.list(..1)) tables <- ..1
  else tables <- list(...)

  tablePrint <- attr(x, "tablePrint")
  tableStats <- c(attr(x, "arguments")$stats, names(tables))

  if (is.null(format)) {
    format <- rep(attr(x, "arguments")$format[1], length(tables))
  }

  # Calculate the new dim and dimnames
  dimTablePrint <- dim(tablePrint)
  dimNamesTablePrint <- dimnames(tablePrint)
  dimTablePrint[length(dimTablePrint)] <- dimTablePrint[length(dimTablePrint)]+length(tables)
  dimNamesTablePrint[[length(dimNamesTablePrint)]] <- c(dimNamesTablePrint[[length(dimNamesTablePrint)]], names(tables))

  #Join original table with Expected values
  newTable <- c(tablePrint, unlist(tables))
  dim(newTable) <- dimTablePrint
  dimnames(newTable) <- dimNamesTablePrint

  #Return table
  attr(x, "tablePrint") <- newTable
  attr(x, "arguments")$format <- c(attr(x, "arguments")$format, format)
  attr(x, "arguments")$stats <- tableStats
  x
}
