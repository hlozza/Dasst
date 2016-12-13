\name{length<-.Dasst}
\alias{length<-.Dasst}
\title{Set the length of an object of class \code{\linkS4class{Dasst}}.}
\usage{
  \method{length}{Dasst} (x) <- value
}
\arguments{
  \item{x}{Object of class \code{\linkS4class{Dasst}}.}

  \item{value}{Integer value. Sets the new length of the
  \code{\linkS4class{Dasst}} object.}
}
\value{
  An integer value corresponding to the actual length of
  the object.
}
\description{
  This sets the length of an object of class
  \code{\linkS4class{Dasst}}.
}
\details{
  \code{length<-.Dasst} function extends the S3
  \code{\link{length}} generic function. It sets the length
  of an object of class \code{\linkS4class{Dasst}}. The
  object can be shrinked or extended adding \code{NULL} or
  \code{NA} contents.
}
\examples{
data(plantGrowth)
length(plantGrowth)
length(plantGrowth) <- 8
length(plantGrowth)
}
\seealso{
  \code{\link{length.Dasst}}
}

