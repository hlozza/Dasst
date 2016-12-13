\docType{methods}
\name{summary}
\alias{summary}
\alias{summary,Dasst}
\alias{summary,Dasst-method}
\alias{summary.Dasst}
\title{Summary method for class \code{\linkS4class{Dasst}}}
\usage{
  \method{summary}{Dasst} (object, ...)
}
\arguments{
  \item{object}{An object of class
  \code{\linkS4class{Dasst}}.}

  \item{...}{Arguments that may be passed to other
  functions.}
}
\value{
  An object of class \code{summary.Dasst}
}
\description{
  \code{summary} summarizes the contents of an object of
  class \code{\linkS4class{Dasst}}.
}
\details{
  This method summarizes the contents of the object of
  object of class \code{\linkS4class{Dasst}}. After reading
  a DSSAT file, summary can give an idea of the volume of
  information stored in that file.
}
\examples{
data(plantGrowth)
summary(plantGrowth)
}

