% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/as_flextable.crosstable.R
\name{as_flextable.crosstable}
\alias{as_flextable.crosstable}
\title{Uses flextable to print the table in html or Latex}
\usage{
\method{as_flextable}{crosstable}(x)
}
\arguments{
\item{x}{A crosstable object}
}
\description{
Uses flextable to print the table in html or Latex
}
\examples{
library(flextable)
library(magrittr)

crosstable(Titanic, col.vars=c("Sex", "Survived"), stats=c("count", "column")) \%>\%
  as_flextable

}
