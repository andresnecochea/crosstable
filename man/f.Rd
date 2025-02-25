% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/f.R
\name{\%f\%}
\alias{\%f\%}
\title{Binary operator to give format to a number}
\usage{
x %f% format
}
\arguments{
\item{x}{A numeric R object}

\item{format}{A string with a format like "$#,#0,0.-"
it will provide a prefix, a suffix, a big mark, a decimal
mark and digit number.}
}
\value{
Returns a string with the number formated to be printed in a report
}
\description{
Binary operator to format a number to be used with the sweep function.
}
\details{
The purpose of this function is provide a binary operator to format a
  number to be used in the function sweep. The string with the format
  is very similar to the formating string in common spreadsheet software.
  An example string could be a prefix with a dolar sign: "$0"
}
\examples{
# Add a dollar sign as prefix:
5000 \%f\% "$0"
# Add a percent sign as suffix:
5000 \%f\% "0\%"
# Use "," as big.mark
5000 \%f\% "#,#0"
# Use "," as decimal.mark with two digits
pi \%f\% "0.00"
# Use "$" as prefix, ".-" as suffix, "," as decimal.mark, "." as bigmark with two digits
5000 \%f\% "$#.#0,00.-"

# Make a matrix with 9 numbers
# Then format the first column with a dollar sign,
# second row with percent sign and thrid surounded with parenthesis.
m <- matrix(seq(10,90, length.out=9), ncol = 3)
sweep(m,2,c("$0","0\%","(0)"), "\%f\%")

}
