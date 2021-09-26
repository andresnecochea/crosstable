#' Uses flextable to print the table in html or Latex
#'
#' @param x A crosstable object
#' @examples
#' library(flextable)
#' library(magrittr)
#'
#' crosstable(Titanic, col.vars=c("Sex", "Survived"), stats=c("count", "column")) %>%
#'   as_flextable
#'
#' @exportS3Method  flextable::as_flextable
as_flextable.crosstable <- function(x) {
  tablePrint <- attr(x, "tablePrint")
  arguments <- attr(x, "arguments")

  tableDimNames <- dimnames(tablePrint)
  tablePrint.names <- names(dimnames(tablePrint))

  if(arguments$stats.on.cols) {
    col.vars <- c(arguments$col.vars, (length(dim(x))+1):length(dim(tablePrint)))
    row.vars <- arguments$row.vars
  }else{
    col.vars <- arguments$col.vars
    row.vars <- c(arguments$row.vars, (length(dim(x))+1):length(dim(tablePrint)))
  }

  row.names <- tablePrint.names[row.vars]
  row.dim <- dim(tablePrint)[row.vars]
  col.names <- tablePrint.names[col.vars]
  col.dim <- dim(tablePrint)[col.vars]

  #-------------------------#
  # MAKE THE TABLE TO PRINT #
  #-------------------------#
  dimTable <- dim(tablePrint)

  # Index of original table to make
  s <- sapply(1:prod(dimTable[(length(dimTable)-1):length(dimTable)]),rep,prod(dimTable[-((length(dimTable)-1):length(dimTable))]))
  dim(s) <- dimTable
  s <- sweep(s, max(c(row.vars, col.vars)), 0, "+")
  s <- sort(s, index.return=TRUE)$ix

  tablePrint <- sweep(tablePrint, max(c(row.vars, col.vars)), arguments$format, "%f%")

  # Now we sort the table using the s index
  tablePrint <- as.vector(tablePrint)[s]
  dim(tablePrint) <- dimTable

  # Turn table in to ftable to make the data.frame
  ftable(tablePrint, col.vars = col.vars) %>%
    as.matrix %>%
    as.data.frame -> tablePrint

  # Make the row names
  for(i in length(row.vars):1) {
    sapply(tableDimNames[[row.vars[i]]], rep, prod(c(row.dim,1)[i:length(row.dim)+1])) %>%
      as.vector %>%
      rep(prod(c(1,row.dim)[1:i])) %>%
      data.frame(tablePrint) -> tablePrint
  }

  names(tablePrint) <- paste0(rep("a.",ncol(tablePrint)), 1:ncol(tablePrint))

  table_headers <- c(row.names,
                     rep(tableDimNames[[col.vars[length(col.vars)]]], prod(col.dim[1:(length(col.dim)-1)])))
  names(table_headers) <- names(tablePrint)

  flextable(tablePrint) -> FT

  for(i in length(col.vars):1) {
    repNum <- prod(c(1, col.dim)[1:i])
    colSpan <- prod(c(col.dim,1)[i:length(col.dim)+1])
    repHeader <- rep(tableDimNames[[col.vars[i]]], repNum)
    if (i < length(col.vars)) {
      FT <- add_header_row(FT,
                           values = c(row.names, repHeader),
                           colwidths = c(rep(1,length(row.names)),
                                         rep(colSpan,length(repHeader))))
    } else {
      FT <- set_header_labels(FT, values = table_headers)
    }
    repName <- rep(names(tableDimNames)[col.vars[i]], prod(c(1, col.dim)[1:i]))
    nameSpan <- prod(col.dim[i:length(col.dim)])
    FT <- add_header_row(FT,
                         values = c(row.names, repName),
                         colwidths = c(rep(1,length(row.names)),
                                       rep(nameSpan,length(repName))))
  }

  FT %>%
    theme_box %>%
    align(j=(length(row.vars)+1):ncol(tablePrint), align="center", part="header") %>%
    align(j=(length(row.vars)+1):ncol(tablePrint), align="right") %>%
    merge_v(j=1:length(row.vars), part="header") %>%
    merge_v(j=1:max(1,(length(row.vars)-1)), part="body")
}
