#' Join two tables
#'
#' Function borrwed from RKward to bind tables.
#' You can use it to join two statistic and format them with ftable
#'
#' @examples
#' tab1 <- xtabs(Freq ~ Type + Cont, data=MASS::housing)
#' ptab1 <- prop.table(tab1)
#'
#' # Join the a contingency table with the proportion table
#' bind.tables(n = tab1, prop = ptab1)
#'
#' # You can use ftable to get a more readable output
#' ftable(bind.tables(n = tab1, prop = ptab1))
#'
#' @export
bind.tables <- function (...) {
  if (is.list(..1)) tables <- ..1
  else tables <- list(...)
  output <- unlist(tables)
  dim (output) <- c(dim(tables[[1]]), length(tables))
  dimnames(output) <- c (dimnames(tables[[1]]), list("statistic"=names(tables)))
  output
}
