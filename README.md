# Multitabulation
R package to format a cross tabulation for two or more variables.

Features:
* Produces a table with total, row and column percent.
* The table can be of two or more varibles.
* You can include another table with usefull stats like expected values of chisq.test

TODO:
* SPSS like frequency tables
* Percent over other margins in tables with more than two variables.
* Totals and subtotals.
* A more flexible way to format cells

# Installation
In order to install the package from github, you must be sure to have installed devtols:
```
library(devtools)
install_github("andresnecochea/multitabulation", ref="main")
```
Once installed you can run the next code to find some examples:
```
library(multitabulation)
?crosstable
```
See examples in the vignette:
https://andresnecochea.github.io/multitabulation/crosstable.html
