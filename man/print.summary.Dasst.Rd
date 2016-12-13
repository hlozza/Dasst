\name{print.summary.Dasst}
\alias{print.summary.Dasst}
\title{Print object of class \code{summary.Dasst}}
\usage{
  \method{print}{summary.Dasst} (x, ...)
}
\arguments{
  \item{x}{Object of class \code{summary.Dasst}.}

  \item{...}{Arguments that may be passed to other
  functions.}
}
\value{
  An invisible object.
}
\description{
  \code{print.summary.Dasst} prints the contents of an
  object of class \code{summary.Dasst}.
}
\details{
  This function extends the S3 \code{\link{print}} generic
  function. It prints the contents of an object of class
  \code{summary.Dasst}.
}
\examples{
data(plantGrowth)
summary(plantGrowth)
}

