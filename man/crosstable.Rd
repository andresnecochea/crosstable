\name{crosstable}
\alias{crosstable}
\title{
Formating and Printing a Cross Tabulation
}
\description{
  Create a contingency table or crosstabulation with two or more dimensions with easy to read summary statistics like row or column percent.
}
\usage{
crosstable(..., data = parent.frame(), row.vars = NULL, col.vars = NULL, stats = "count", col.total = FALSE, row.total = FALSE, stats.on.cols = TRUE, digits = 2)
}

\arguments{
  \item{\dots}{
    Can be a group of factors, a formula or a regular table. All arguments will be passed  to table or xtabs.
}
  \item{data}{
    An optional matrix or data frame if a formula is used to introduce the variables.
}
  \item{row.vars}{
    A number or character vector giving the names of the variabbles to be used for the rows.
}
  \item{col.vars}{
    A number or character vector giving the names of the variabbles to be used for the columns.
}
  \item{stats}{
    Character vector of count or percentages to compute c("count", "row", "column", "total")
}
  \item{col.total}{
    Unused
}
  \item{row.total}{
    Unused
}
  \item{stats.on.cols}{
    Logical, TRUE by default, it
}
  \item{digits}{
    Number of decimal digits to be round.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  crosstable returns a contingency table of class "crosstable". It can be used in the same way that a regular table, but with a different method to print it.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Andrés Necochea Vega
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
# Generate random data
gender <- sample(c(1,2), 131, replace=TRUE)
gender <- factor(gender, levels=c(1,2), labels=c("Man", "Woman"))
strata <- sample(c(1,2,3), 131, replace=TRUE)
strata <- factor(strata, levels=c(1,2,3), labels=c("Low", "Middle", "High"))
party <- sample(c(1,2), 131, replace=TRUE)
party <- factor(party, levels=c(1,2), labels=c("Right", "Left"))

# Standard two variable table
# Normal table
crosstable(gender, party)
# add column percent
crosstable(gender, party, stats=c("count", "column"))
# the same with stats on rows
crosstable(gender, party, stats=c("count", "column"), stats.on.cols=FALSE)
# add all percents
crosstable(gender, party, stats=c("count", "column", "row", "total"))

# If you want to add custom stat columns like chisq expected values,
# you can use add.tables()
# First store the table in a object
gxp <- crosstable(gender, party, stats=c("count", "column"))
# Perform Chi square test
gxp_xsq <- chisq.test(gxp)
# Add the Chisq expected values to the table
gxp <- add.tables(gxp, "expected" = gxp_xsq$expected)

print(gxp)

# More than two variable table
crosstable(gender, strata, party, stats=c("count", "column", "row", "total"))

# Use an existing table like Titanic
crosstable(Titanic, stats=c("count", "column"))
# You can arrange freely the col and row vars.
crosstable(Titanic, col.vars=c("Sex", "Survived"), stats=c("count", "column"))

# Using a data.frame
cars <- MASS::Cars93
with(cars, crosstable(Type, Origin, Man.trans.avail, col.vars=c("Origin", "Man.trans.avail")))

# The same with a Formula Method
crosstable(~Type+Origin+Man.trans.avail , data=cars, col.vars=c("Origin", "Man.trans.avail"))

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
