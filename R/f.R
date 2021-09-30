#' Binary operator to give format to a number
#'
#' Binary operator to format a number to be used with the sweep function.
#'
#' @usage x %f% format
#' @param x A numeric R object
#' @param format A string with a format like "$#,#0,0.-"
#'   it will provide a prefix, a suffix, a big mark, a decimal
#'   mark and digit number.
#'
#' @details
#' The purpose of this function is provide a binary operator to format a
#'   number to be used in the function sweep. The string with the format
#'   is very similar to the formating string in common spreadsheet software.
#'   An example string could be a prefix with a dolar sign: "$0"
#'
#' @return Returns a string with the number formated to be printed in a report
#'
#' @examples
#' # Add a dollar sign as prefix:
#' 5000 %f% "$0"
#' # Add a percent sign as suffix:
#' 5000 %f% "0%"
#' # Use "," as big.mark
#' 5000 %f% "#,#0"
#' # Use "," as decimal.mark with two digits
#' pi %f% "0.00"
#' # Use "$" as prefix, ".-" as suffix, "," as decimal.mark, "." as bigmark with two digits
#' 5000 %f% "$#.#0,00.-"
#'
#' # Make a matrix with 9 numbers
#' # Then format the first column with a dollar sign,
#' # second row with percent sign and thrid surounded with parenthesis.
#' m <- matrix(seq(10,90, length.out=9), ncol = 3)
#' sweep(m,2,c("$0","0%","(0)"), "%f%")
#'
#' @rdname f
#' @export
"%f%" <- function(x,f) {
  if(length(c(f)) > 1 | sum(dim(c(f))) > 1) {
    output <- mapply(function(x,f) x %f% f, f=f, x=x)
    dim(output) <- dim(x)
    names(output) <- names(x)
    dimnames(output) <- dimnames(x)
    output
  } else {
    numFormat <- sub("^[^0]*(0+.?0*)[^0]*$", "\\1", f)
    prefix <- sub("(#.?#*)?0{1}(.{1}0+)?.*","",f)
    suffix <- sub(".*(#.?#*)?0{1}(.{1}0+)?","",f)
    sub("0{1}(.{1}0+)?.*", "", f) %>%
      sub("^[^#]*", "", .) %>%
      gsub("#","",.) -> big.mark
    decimal.mark <- gsub("0", "", numFormat)
    decimal.mark <- c(getOption("OutDec"), decimal.mark)[as.numeric(grepl("^.*0.0+.*$", numFormat))+1]
    sub("(0+.?)(0*)", "\\2", numFormat) %>%
      nchar -> digits
    round(x, digits) %>%
      format(nsmall=digits ,big.mark=big.mark, decimal.mark=decimal.mark) %>%
      paste0(prefix, ., suffix)
  }
}
