\docType{methods}
\name{[}
\alias{[}
\alias{[,Dasst,numeric-method}
\title{"[" method for class Dasst}
\arguments{
  \item{x}{An object of class \code{\linkS4class{Dasst}}.}

  \item{i}{An integer value. Position where values will be
  retrieved.}
}
\value{
  The values retrieved from the table at position i as
  \code{\link{data.frame}}.
}
\description{
  \code{"["} gets the contents of a table from an object of
  class \code{\linkS4class{Dasst}}.
}
\details{
  This method gets the contents of the selected table
  stored in an object of class \code{\linkS4class{Dasst}}.
  Tables are internally stored and retrieved as
  \code{\link{data.frame}}. Rules for subset can be
  applied.
}
\examples{
data(plantGrowth)
class(plantGrowth[1])
plantGrowth[1]
plantGrowth[1][1:10,]
}
\seealso{
  \code{\link{[<-}}
}

