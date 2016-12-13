\docType{methods}
\name{getContents}
\alias{getContents}
\alias{getContents,Dasst,numeric-method}
\title{Get contents for class \code{\linkS4class{Dasst}}.}
\arguments{
  \item{x}{An object of class \code{\linkS4class{Dasst}}.}

  \item{i}{An integer value. Position where to retrieve
  contents.}
}
\value{
  A list with the slot names and its contents at the given
  position.
}
\description{
  \code{getContents} gets the contents of all the slots of
  the object of class \code{\linkS4class{Dasst}} at the
  selected position.
}
\details{
  This method gets the contents of all the slots of the
  object of class \code{\linkS4class{Dasst}} at the
  selected position.
}
\examples{
data(plantGrowth)
plantgro1 <- getContents(plantGrowth, 1)
class(plantgro1)
}

