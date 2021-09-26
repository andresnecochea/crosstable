#' Formating and Printing a Cross Tabulation
#'
#' Create a contingency table or crosstabulation with
#' two or more dimensions with easy to read summary statistics
#' like row or column percent.
#'
#' @param ... Can be a group of factors, a formula or a regular table. All arguments will be passed to table or xtabs.
#' @param data An optional matrix or data frame if a formula is used to introduce the variables.
#' @param row.vars A number or character vector giving the names of the variabbles to be used for the rows.
#' @param col.vars A number or character vector giving the names of the variabbles to be used for the columns.
#' @param stats Character vector of count or percentages to compute c("count", "row", "column", "total")
#' @param format Character vector with the format of the stats. See help("\%f\%")
#' @param stats.on.cols Logical. ¿should be the stats on cols? TRUE by default.
#' @return  Acontingency table of class "crosstable".
#'   It can be used in the same way that a regular table, but with a
#'   different method to print it.
#'
#' @examples
#' # Generate random data
#' gender <- sample(c(1,2), 131, replace=TRUE)
#' gender <- factor(gender, levels=c(1,2), labels=c("Man", "Woman"))
#' strata <- sample(c(1,2,3), 131, replace=TRUE)
#' strata <- factor(strata, levels=c(1,2,3), labels=c("Low", "Middle", "High"))
#' party <- sample(c(1,2), 131, replace=TRUE)
#' party <- factor(party, levels=c(1,2), labels=c("Right", "Left"))
#'
#' # Standar two variable table
#' # Normal table
#' crosstable(gender, party)
#' # add column percent
#' crosstable(gender, party, stats=c("count", "column"))
#' # the same with stats on rows
#' crosstable(gender, party, stats=c("count", "column"), stats.on.cols=FALSE)
#' # add all percents
#' crosstable(gender, party, stats=c("count", "column", "row", "total"))
#'
#' # If you want to add custom stat columns like chisq expected values,
#' # you can use add.tables()
#' # First store the table in a object
#' gxp <- crosstable(gender, party, stats=c("count", "column"))
#' # Perform Chi square test
#' gxp_xsq <- chisq.test(gxp)
#' # Add the Chisq expected values to the table
#' gxp <- add.tables(gxp, "expected" = gxp_xsq$expected)
#'
#' print(gxp)
#'
#' # More than two variable table
#' crosstable(gender, strata, party, stats=c("count", "column", "row", "total"))
#'
#' # Use an existing table like Titanic
#' crosstable(Titanic, stats=c("count", "column"))
#' # You can arrange freely the col and row vars.
#' crosstable(Titanic, col.vars=c("Sex", "Survived"), stats=c("count", "column"))
#'
#' # Using a data.frame
#' cars <- MASS::Cars93
#' with(cars, crosstable(Type, Origin, Man.trans.avail, col.vars=c("Origin", "Man.trans.avail")))
#'
#' # The same with a Formula Method
#' crosstable(~Type+Origin+Man.trans.avail , data=cars, col.vars=c("Origin", "Man.trans.avail"))
#'
#' @export "crosstable"
#' @exportClass "crosstable"
setClass("crosstable", contains = "table")
crosstable <- function(..., data=parent.frame(), row.vars = NULL, col.vars = NULL, stats = "count", format = NULL, col.total = FALSE, row.total = FALSE, stats.on.cols = TRUE) {
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
    format <- rep(c("#,#0.00 ","#,#0")[as.numeric(stats.on.cols)+1], length(stats))
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
            arguments = list(row.vars = row.vars, col.vars = col.vars, stats = stats, format = format, stats.on.cols = stats.on.cols),
            class="crosstable")
}
