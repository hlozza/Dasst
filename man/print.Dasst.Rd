\name{print.Dasst}
\alias{print.Dasst}
\title{Print object of class Dasst}
\usage{
  \method{print}{Dasst} (x, ix = 1, ...)
}
\arguments{
  \item{x}{Object of class \code{\linkS4class{Dasst}}.}

  \item{ix}{An integer number. The contents of the first
  table are print by default. Others table contents can be
  display setting this parameter in the range form 1 to
  \code{length(x)}.}

  \item{...}{Other parameters for the
  \code{\link{print.data.frame}} function that specify how
  tables should look.}
}
\value{
  An invisible object.
}
\description{
  \code{print.Dasst} prints the contents of an object of
  class \code{\linkS4class{Dasst}}.
}
\details{
  This function extends the S3 \code{\link{print}} generic
  function. It prints the contents of an object of class
  \code{\linkS4class{Dasst}}.
}
\examples{
data(plantGrowth)
print(plantGrowth)
}

