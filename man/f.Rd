\name{\%f\%}
\alias{\%f\%}
\title{
Format a number with a binary operator.
}
\description{
Binary operator to format a number to be used with the sweep function
}
\usage{
x \%f\% format
}

\arguments{
  \item{number}{
    Any numeric R object
}
  \item{format}{
    A string with a format like "$#,#0,0.-" it will provide a prefix, a suffix, a big mark, a decimal mark and digit number.
}
}
\details{
The purpose of this function is provide a binary operator to format a number to be used in the function sweep.
The string with the format is very similar to the formating string in common spreadsheet software.
An example string could be a prefix with a dolar sign: ""
}
\value{
Returns a string with the number formated to be printed in a report
}
\references{
}
\author{
Andrés Necochea
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
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
library(magrittr)
m <- matrix(seq(10,90, length.out=9), ncol = 3)
sweep(m,2,c("$0","0\%","(0)"), "\%f\%") \%>\%
  matrix(ncol=3)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
