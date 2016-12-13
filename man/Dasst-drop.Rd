\docType{methods}
\name{dropContents<-}
\alias{dropContents<-}
\alias{dropContents<-,Dasst,numeric-method}
\title{Drop contents for class \code{\linkS4class{Dasst}}}
\arguments{
  \item{x}{An object of class \code{\linkS4class{Dasst}}.}

  \item{value}{An integer value. Position where contents
  will be dropped.}
}
\value{
  The actual object of class \code{\linkS4class{Dasst}}.
}
\description{
  \code{dropContents<-} drops the contents of slots from
  the object of class \code{\linkS4class{Dasst}} at
  selected position.
}
\details{
  This method drops the contents of slots from the object
  of class \code{\linkS4class{Dasst}} at selected position.
}
\examples{
data(plantGrowth)
summary(plantGrowth)
dropContents(plantGrowth) <- 10
summary(plantGrowth)
}

