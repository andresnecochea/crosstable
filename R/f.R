# Binary operator to give format to a number
"%f%" <- function(x,f) {
  numFormat <- sub("^[^0]*(0+.?0*)[^0]*$", "\\1", f)
  prefix <- sub("(#.?#*)?0{1}(.{1}0+)?.*","",f)
  suffix <- sub(".*(#.?#*)?0{1}(.{1}0+)?","",f)
  sub("^.*(#.#)0.*$", "\\1", f) %>%
    gsub("#", "", .) -> big.mark
  big.mark <- c("", big.mark)[as.numeric(big.mark == "")+1]
  decimal.mark <- gsub("0", "", numFormat)
  decimal.mark <- c(getOption("OutDec"), decimal.mark)[as.numeric(grepl("^.*0.0+.*$", numFormat))+1]
  sub("(0+.?)(0*)", "\\2", numFormat) %>%
    nchar -> digits
  round(x, digits) %>%
    format(x,nsmall=digits ,big.mark=big.mark, decimal.mark=as.character(decimal.mark)[1]) %>%
    paste0(prefix, ., suffix)
}
