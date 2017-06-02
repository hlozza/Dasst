\docType{methods}
\name{getAncillary}
\alias{getAncillary}
\alias{getAncillary,Dasst,missing-method}
\alias{getAncillary,Dasst,numeric-method}
\title{Get ancillary data from an object of
class \code{\linkS4class{Dasst}}}
\arguments{
  \item{x}{An object of class \code{\linkS4class{Dasst}}.}

  \item{i}{An optional integer vector. Orders where to
  retrieve ancillary data. The default action is to
  retrieve all the available ancillary data.}
}
\value{
  An object of class Ancillary which contains the retrieved
  ancillary data for the selected table orders.
}
\description{
  \code{getAncillary} gets ancillary data from an object of
  class \code{\linkS4class{Dasst}} connected to the
  selected table orders.
}
\details{
  This method gets ancillary data from an object of class
  \code{\linkS4class{Dasst}} connected to the selected
  table orders. Values are arranged in tables, and the
  order is the number assigned succesively to each of them
  after the data have been stored within the
  \code{\linkS4class{Dasst}} object. \code{getAncillary}
  provides ancillary data such as the file name which was
  originally read, and the section and the header which
  introduced the values within the file.
}
\examples{
data(plantGrowth)
getAncillary(plantGrowth, c(1,3,5))
}

