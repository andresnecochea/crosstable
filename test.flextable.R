library(multitabulation)
library(flextable)

gender <- sample(c(1,2), 131, replace=TRUE) %>%
  factor(levels=c(1,2), labels=c("Man", "Woman"))
strata <- sample(c(1,2,3), 131, replace=TRUE) %>%
  factor(levels=c(1,2,3), labels=c("Low", "Middle", "High"))
party <- sample(c(1,2), 131, replace=TRUE) %>%
  factor(levels=c(1,2), labels=c("Right", "Left"))

# Standard two variable table
# Normal table
crosstable(gender, party) %>%
  as_flextable
# add column percent
crosstable(gender, party, stats=c("count", "column")) %>%
  as_flextable
# the same with stats on rows
crosstable(gender, party, stats=c("count", "column"), stats.on.cols=FALSE) %>%
  as_flextable
# add all percents
crosstable(gender, party, stats=c("count", "column", "row", "total")) %>%
  as_flextable

# If you want to add custom stat columns like chisq expected values,
# you can use add.tables()
# First store the table in a object
gxp <- crosstable(gender, party, stats=c("count", "column"))
# Perform Chi square test
gxp_xsq <- chisq.test(gxp)
# Add the Chisq expected values to the table
gxp <- add.tables(gxp, "expected" = gxp_xsq$expected)

as_flextable(gxp)

# More than two variable table
crosstable(gender, strata, party, stats=c("count", "column", "row", "total")) %>%
  as_flextable

# Use an existing table like Titanic
crosstable(Titanic, stats=c("count", "column")) %>%
  as_flextable
# You can arrange freely the col and row vars.
crosstable(Titanic, col.vars=c("Sex", "Survived"), stats=c("count", "column")) %>%
  as_flextable

# Using a data.frame
cars <- MASS::Cars93
with(cars, crosstable(Type, Origin, Man.trans.avail, col.vars=c("Origin", "Man.trans.avail"))) %>%
  as_flextable

# The same with a Formula Method
crosstable(~Type+Origin+Man.trans.avail , data=cars, col.vars=c("Origin", "Man.trans.avail")) %>%
  as_flextable
