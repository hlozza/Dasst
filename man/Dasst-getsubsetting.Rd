\docType{methods}
\name{[}
\alias{[}
\alias{[,Dasst,logical-method}
\alias{[,Dasst,numeric-method}
\title{"[" method for class Dasst}
\arguments{
  \item{x}{An object of class \code{\linkS4class{Dasst}}.}

  \item{i}{An integer or logical vector. This is the subset
  that will be retrieved from the whole object.}
}
\value{
  A new object of class \code{\linkS4class{Dasst}} that
  comprises the elements from the selected subset.
}
\description{
  \code{"["} gets a subset of an object of class
  \code{\linkS4class{Dasst}}.
}
\details{
  This method gets a subset of an object of class
  \code{\linkS4class{Dasst}}. Shorter objects in the
  expression are recycled as often as need be until they
  match the length of the longest object.
}
\examples{
data(plantGrowth)
length(plantGrowth)
plantgro1 <- plantGrowth[1:10]
length(plantgro1)
class(plantgro1)

# Drop contents corresponing to selected orders
summary(plantGrowth)
plantgro2 <- plantGrowth[-1]
summary(plantgro2)
}
\seealso{
  \code{\link{[<-}}
}

