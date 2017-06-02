\docType{data}
\name{plantGrowth}
\alias{plantGrowth}
\title{An example of a Dasst object}
\description{
  An example of a Dasst object based DSSAT file
  PlantGro.OUT. The originally data set can be found within
  package directory as \file{extdata/PlantGro.OUT}.
}
\details{
  \code{plantGrowth} contains the daily plant growth for 3
  treatments repeated on 10 years. A summary of its
  contents follows

  \describe{ \item{Table 1}{45 fields and 131 records}
  \item{Table 2}{45 fields and 112 records} \item{Table
  3}{45 fields and 124 records} \item{Table ...}{...}
  \item{Table 30}{45 fields and 123 records} }

  Tables ranging from 1 to 10 have plant output data for
  treatment 1 which simulates maize growth from 1970 to
  1979 (south hemisphere). Treatment 2 adds fertilization
  with 50kg/ha of urea, and treatment 3 adds fertilization
  with 100kg/ha of urea.
}
\examples{
\dontrun{

data(plantGrowth)
plot(plantGrowth[[1]][,"DAP"],plantGrowth[[1]][,"LAID"])

# Or

plot(plantGrowth[[1]][,c("DAP","LAID")])

}
}
\seealso{
  \code{\linkS4class{Dasst}} for class definition and the
  example at \code{\link{read.dssat}}.
}
\keyword{dataset}
\keyword{datasets}

