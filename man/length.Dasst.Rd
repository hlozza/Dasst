\name{length.Dasst}
\alias{length.Dasst}
\title{Length of an object of class \code{\linkS4class{Dasst}}.}
\usage{
  \method{length}{Dasst} (x)
}
\arguments{
  \item{x}{Object of class \code{\linkS4class{Dasst}}.}
}
\value{
  An integer representing the length of the object.
}
\description{
  \code{length.Dasst} computes the length of an object of
  class \code{\linkS4class{Dasst}}.
}
\details{
  This function extends the S3 \code{\link{length}} generic
  function. It computes the length of an object of class
  \code{\linkS4class{Dasst}}. The length equals the
  quantity of stored tables. The empty object has length
  \code{0}.
}
\examples{
data(plantGrowth)
length(plantGrowth)
}
\seealso{
  \code{\link{length<-.Dasst}}
}

