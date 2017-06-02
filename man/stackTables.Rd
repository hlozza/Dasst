\name{stackTables}
\alias{stackTables}
\title{Stack the tables of an object of class Dasst}
\usage{
  stackTables(object)
}
\arguments{
  \item{object}{Object of class
  \code{\linkS4class{Dasst}}.}
}
\value{
  A \code{\link{data.frame}} composed of the stacked
  tables.
}
\description{
  \code{stackTables} stacks the tables of an object of
  class \code{\linkS4class{Dasst}}.
}
\details{
  This function stacks the tables of an object of class
  \code{\linkS4class{Dasst}}. The result is given as a
  \code{\link{data.frame}}.
}
\examples{
data(plantGrowth)
nrow(plantGrowth[[1]])
nrow(plantGrowth[[2]])
plantgro12 <- stackTables(plantGrowth[1:2])
nrow(plantgro12)
}

