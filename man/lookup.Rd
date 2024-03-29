% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getResult.R
\docType{data}
\name{lookup}
\alias{lookup}
\title{Result Lookup}
\format{
a tibble with one info-line per fitted group:
\describe{
 \item{id}{unique identifier (key) for matching with results-object components}
 \item{with_ranefs}{does this result include random effects in its model?}
 \item{err_model}{"A"dditive or "M"ultiplicative.}
 \item{region, fertility, soil}{the grouping variables}
}
}
\usage{
lookup
}
\description{
Result Lookup
}
\seealso{
\link[vmiBiomass]{getResults}, \link[vmiBiomass]{results}
}
\keyword{datasets}
