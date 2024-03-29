% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/util.R
\name{groups_colnums}
\alias{groups_colnums}
\title{Get a named vector of column indices
for the grouping variables in a
grouped data frame}
\usage{
groups_colnums(data)
}
\arguments{
\item{data}{A data frame.}
}
\value{
A named integer vector.
}
\description{
Constructed a named vector of indices
equivalent to the vectors produced by
tidyselect::eval_select, but for the grouping
variables in an object of class
"grouped_df".
}
\details{
If \code{data} is not grouped, then \code{groups_colnums}
returns a zero-length vector.
}
\examples{
library(dplyr)
df <- data.frame(x = 1:4,
                 g = c(1, 1, 2, 2))
groups_colnums(df)
df <- group_by(df, g)
groups_colnums(df)
}
