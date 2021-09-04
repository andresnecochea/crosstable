# Returns a crosstable class object
crosstable <- function(..., data=parent.frame(), row.vars = NULL, col.vars = NULL, stats = "count", format = NULL, col.total = FALSE, row.total = FALSE, stats.on.cols = TRUE, digits = 2) {
  # This function will use table or xtabs to make an R table
  # can recive as arguments a couple of vectors or an already made table
  if (is.table(..1))
    table1 <- ..1
  else if (class(..1)=="formula")
    table1 <- xtabs(..1, data=data)
  else
    table1 <- table(...)

  # We should build the arguments col.vars or row.vars if not defined
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

  if (is.null(format)) {
    f <- stats %in% c("column","row","total")
    format <- rep(c("#,#0.00 ","#,#0.")[as.numeric(stats.on.cols)+1], length(stats))
    format[f] <- "#,#0.00%"
  }

  # The table to print will be a list of tables made with prop.table
  # the main table is frequency other tables are percentajes
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

  # This will define the names of the stats columns
  f <- stats %in% c("column","row","total")
  tableNames <- stats
  tableNames[f] <- paste("% of", tableNames[f])

  names(tablePrint) <- tableNames
  tablePrint <- bind.tables(tablePrint)

  # The table is the main table to perform chisq.
  # The tablePrint is an attribute only to show.
  structure(table1,
            tablePrint = tablePrint,
            arguments = list(row.vars = row.vars, col.vars = col.vars, stats = stats, format = format, stats.on.cols = stats.on.cols, digits = digits),
            class="crosstable")
}
