#' Create a contingency table with summary statistics using ftable
#'
#' This function will create a contingency table with summary statistics
#' using bind.tales and ftable
#'
#' @examples
#' ctftable(Freq ~ Type + Cont, stats=c("count", "row"), data=MASS::housing)
#'
#' @export
ctftable <- function(..., data=parent.frame(), row.vars = NULL, col.vars = NULL, stats = "count") {
  # Se recibe como argumento filas y columnas, una tabla o una fórmula
  if (is.table(..1))
    table1 <- ..1
  else if (class(..1)=="formula")
    table1 <- xtabs(..1, data=data)
  else
    table1 <- table(...)

  # Definir variables en filas y columnas si no están definidas
  if(xor(is.null(col.vars), is.null(row.vars))) {
    if (is.null(row.vars)) {
      if (is.character(col.vars))
        col.vars <- which(names(dimnames(table1)) %in% col.vars)
      row.vars <- 1:length(dim(table1))
      row.vars <- row.vars[-col.vars]
    } else if (is.null(col.vars)) {
      if (is.character(row.vars))
        row.vars <- which(names(dimnames(table1)) %in% row.vars)
      col.vars <- 1:length(dim(table1))
      col.vars <- col.vars[-row.vars]
    }
  } else if(is.null(col.vars) & is.null(row.vars)) {
    col.vars <- length(dim(table1))
    row.vars <- 1:(length(dim(table1))-1)
  }

  # La tabla es una lista de tablas que se le dará formato con ftable.
  tablePrint <- lapply(stats, function(x) {
    if (x == "count")
      table1
    else if (x == "total")
      prop.table(table1)*100
    else if (x == "row")
      prop.table(table1, col.vars)*100
    else if (x == "column")
      prop.table(table1, row.vars)*100
  })

  # Con esto se generan los nombres para las celdas de estadísticas
  f <- stats %in% c("column","row","total")
  tableNames <- stats
  tableNames[f] <- paste("% of", tableNames[f])
  tableFormat <- rep("0.00", length(stats))
  tableFormat[f] <- "0.00%"

  names(tablePrint) <- tableNames
  tablePrint <- bind.tables(tablePrint)

  # He usado Sweep para agregar el símbolo % a las celdas de porcentaje
  tablePrint <- sweep(tablePrint, length(dimTable), tableFormat, "%f%")

  col.vars <- c(col.vars, (length(dim(table1))+1):length(dim(tablePrint)))
  ftable(tablePrint, col.vars=col.vars)
}
