\docType{methods}
\name{setContents<-}
\alias{setContents<-}
\alias{setContents<-,Dasst,ANY,ANY,list-method}
\title{Set contents for class \code{\linkS4class{Dasst}}.}
\arguments{
  \item{x}{An object of class \code{\linkS4class{Dasst}}.}

  \item{index}{An integer value. Position where to insert
  or update contents.}

  \item{replace}{A logical value. If \code{TRUE} old
  contents are replaced with new ones. Otherwise, new
  contents are inserted at given position, and old contents
  are shifted conveniently to higher positions.}

  \item{value}{A list with contents for each slot.}
}
\value{
  The actual object.
}
\description{
  \code{setContents<-} sets the contents of all the slots
  of the object of class \code{\linkS4class{Dasst}} at the
  selected position.
}
\details{
  This method sets the contents of all the slots of the
  object of class \code{\linkS4class{Dasst}} at the
  selected position.
}
\examples{
data(plantGrowth)
plantgro1 <- getContents(plantGrowth, 1)
setContents(plantGrowth, 2, TRUE) <- plantgro1
}

