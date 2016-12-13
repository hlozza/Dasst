\name{as}
\alias{as}
\title{As forces an object of class Dasst to belong to class list}
\description{
  Coerces an object of class \code{\linkS4class{Dasst}} to
  an object of class \code{\link{list}}.
}
\details{
  This function enables the function \code{\link{as}} to
  coerce objects of class \code{\linkS4class{Dasst}} to
  belong to class \code{\link{list}}.
}
\examples{
data(plantGrowth)
lplantgro <- as(plantGrowth, "list")
class(lplantgro)
}

