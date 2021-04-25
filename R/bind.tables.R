# Function borrwed from RKward to join tables.
bind.tables <- function (...) {
  if (is.list(..1)) tables <- ..1
  else tables <- list(...)
  output <- unlist(tables)
  dim (output) <- c(dim(tables[[1]]), length(tables))
  dimnames(output) <- c (dimnames(tables[[1]]), list("statistic"=names(tables)))
  output
}
