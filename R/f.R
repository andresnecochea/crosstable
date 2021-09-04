# Binary operator to give format to a number
"%f%" <- function(x,f) {
  prefix <- sub("(.*)(#+.{1}#+)(0+.?0*)(.*)", "\\1", f)
  suffix <- sub("(.*)(#+.{1}#+)(0+.?0*)(.*)", "\\4", f)
  sub("(.*)(#+.{1}#+)(0+.?0*)(.*)", "\\2", f) %>%
    gsub("#", "", .) -> big.mark
  sub("(.*)(#+.{1}#+)(0+.?0*)(.*)", "\\3", f) %>%
    gsub("0", "", .) -> decimal.mark
  sub("(.*)(#+.{1}#+)(0+.?0*)(.*)", "\\3", f) %>%
    sub("(0+.?)(0*)", "\\2", .) %>%
    nchar -> digits
  format(x,nsmall=digits ,big.mark=big.mark, decimal.mark=as.character(decimal.mark)[1]) %>%
    paste0(prefix, ., suffix)
}
