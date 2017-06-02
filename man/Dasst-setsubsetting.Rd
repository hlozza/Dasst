\docType{methods}
\name{[<-}
\alias{[<-}
\alias{[<-,Dasst,logical-method}
\alias{[<-,Dasst,numeric-method}
\title{"[<-" method for class \code{\linkS4class{Dasst}}}
\arguments{
  \item{x}{An object of class \code{\linkS4class{Dasst}}.}

  \item{i}{An integer or logical vector. This is the subset
  that will be updated from the whole object.}

  \item{value}{An object of class
  \code{\linkS4class{Dasst}} that will be stored at the
  given subset.}
}
\value{
  The actual object of class \code{\linkS4class{Dasst}}
  that comprises the elements updated from the selected
  subset.
}
\description{
  \code{"[<-"} sets to a subset of an object of class
  \code{\linkS4class{Dasst}} an other object of the same
  class
}
\details{
  This method sets to a subset of an object of class
  \code{\linkS4class{Dasst}} an other object of the same
  class. Shorter objects in the expression are recycled as
  often as need be until they match the length of the
  longest object.
}
\examples{
# Replace position 1 with the contents of position 30.
data(plantGrowth)
plantGrowth[[1]][1:10, 1:15]
plantGrowth[1] <- plantGrowth[30]
plantGrowth[[1]][1:10, 1:15]

# Add a copy of the first order at the end extending the object length
rmax <- length(plantGrowth)
rmax
plantGrowth[rmax+1] <- plantGrowth[1]
length(plantGrowth)

# Copy position 2 into position 31, moving the former position 31 to the 32.
plantgro31 <- plantGrowth[31]
plantGrowth[31] <- plantGrowth[2]
plantGrowth[32] <- plantgro31
}
\seealso{
  \code{\link{[}}
}

