% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sexgender.R
\name{reformat_sex}
\alias{reformat_sex}
\title{Reformat a binary sex variable}
\usage{
reformat_sex(x, factor = TRUE)
}
\arguments{
\item{x}{A vector.}

\item{factor}{Whether the return value
should be a factor.}
}
\value{
If \code{factor} is \code{TRUE},
then \code{reformat_age()} returns a factor
with levels \code{"Female"}, \code{"Male"}.
otherwise it returns a character vector.
}
\description{
Reformat a binary sex variable so
that it consists entirely of
values \code{"Female"}, \code{"Male"},
and possibly \code{NA}.
}
\details{
When parsing labels, \code{reformat_sex()}
ignores case: \code{"FEMALE"} and \code{"fEmAlE"}
are equivalent.

White space is removed from the beginning
and end of labels.

\code{reformat_sex()} does not try to interpreting
numeric codes (eg \code{1}, \code{2}).
}
\examples{
reformat_sex(c("F", "female", NA, "MALES"))

## return an ordinary character vector
reformat_sex(c("F", "female", NA, "MALES"),
             factor = FALSE)
}
\seealso{
\code{\link[=age_labels]{age_labels()}}, \code{\link[=reformat_sex]{reformat_sex()}}
}