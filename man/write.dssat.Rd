\name{write.dssat}
\alias{write.dssat}
\title{Write a DSSAT file from an object of class Dasst}
\usage{
  write.dssat(object)
}
\arguments{
  \item{object}{An object of class
  \code{\linkS4class{Dasst}}.}
}
\description{
  \code{write.dssat} writes the contents of an object of
  class \code{\linkS4class{Dasst}} to a file.
}
\details{
  This function writes the contents of an object of class
  \code{\linkS4class{Dasst}} to a file striving to maintain
  compatibility with the DSSAT file format.
}
\examples{
\dontrun{

data(plantGrowth)
length(plantGrowth) <- 1
write.dssat(plantGrowth)

}
}

