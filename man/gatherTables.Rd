\name{gatherTables}
\alias{gatherTables}
\title{Gather tables from object of class Dasst.}
\usage{
  gatherTables(object, indices, coCol, opCol, operation,
    ...)
}
\arguments{
  \item{object}{Object of class
  \code{\linkS4class{Dasst}}.}

  \item{indices}{An integer vector. A vector with the
  position of the tables which will be stacked.  Values
  range form 1 to \code{length(x)}.}

  \item{coCol}{A character vector. The field names of those
  columns that will be copied identically into the result.}

  \item{opCol}{A character vector. The field names of those
  columns that will be gather by means of applying the
  required operation.}

  \item{operation}{A function. The function name for the
  required operation.  i.e. \code{\link{mean}},
  \code{\link{sum}}, etc.}

  \item{...}{Other parameters for the \code{\link{apply}}
  function as additional arguments for the operation.}
}
\value{
  A \code{\link{data.frame}} with the values gathered after
  the application of the operator to the required columns.
}
\description{
  \code{gatherTables} gathers the result of performing a
  certain operation over selected tables of an object of
  class \code{\linkS4class{Dasst}}.
}
\details{
  This function gathers the result of performing a certain
  operation over selected tables of an object of class
  \code{\linkS4class{Dasst}}. The result is given as a
  \code{\link{data.frame}}.
}
\examples{
data(plantGrowth)
plantgro12 <- gatherTables(plantGrowth, 1:10, c("DAP"),
   c("SWAD","LWAD","GWAD"), mean)
}

