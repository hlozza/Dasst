\name{stackTables}
\alias{stackTables}
\title{Stack tables from object of class Dasst}
\usage{
  stackTables(object, indices)
}
\arguments{
  \item{object}{Object of class
  \code{\linkS4class{Dasst}}.}

  \item{indices}{An integer vector. A vector with the
  position of the tables which will be stacked.  Values
  range form 1 to \code{length(x)}.}
}
\value{
  A \code{\link{data.frame}} composed of the stacked
  tables.
}
\description{
  \code{stackTables} stacks the selected tables of an
  object of class \code{\linkS4class{Dasst}}.
}
\details{
  This function stacks the selected tables of an object of
  class \code{\linkS4class{Dasst}}. The result is given as
  a \code{\link{data.frame}}.
}
\examples{
data(plantGrowth)
nrow(plantGrowth[1])
nrow(plantGrowth[2])
plantgro12 <- stackTables(plantGrowth, c(1,2))
nrow(plantgro12)
}

