\name{print.Ancillary}
\alias{print.Ancillary}
\title{Print object of class \code{Ancillary}}
\usage{
  \method{print}{Ancillary} (x, ...)
}
\arguments{
  \item{x}{Object of class \code{Ancillary}.}

  \item{...}{Arguments that may be passed to other
  functions.}
}
\value{
  An invisible object.
}
\description{
  \code{print.Ancillary} prints the contents of an object
  of class \code{Ancillary}.
}
\details{
  This function extends the S3 \code{\link{print}} generic
  function. It prints the contents of an object of class
  \code{Ancillary}.
}
\examples{
data(plantGrowth)
getAncillary(plantGrowth, 1:5)
}

