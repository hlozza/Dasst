\docType{methods}
\name{[[<-}
\alias{[[<-}
\alias{[[<-,Dasst,numeric-method}
\title{"[[<-" method for class \code{\linkS4class{Dasst}}}
\arguments{
  \item{x}{An object of class \code{\linkS4class{Dasst}}.}

  \item{i}{An integer value. Position where values will be
  updated.}

  \item{value}{Any Values to be stored at the given
  position.}
}
\value{
  The actual object of class \code{\linkS4class{Dasst}}.
}
\description{
  \code{"[[<-"} sets the contents of a table from an object
  of class \code{\linkS4class{Dasst}}.
}
\details{
  This method sets the contents of the selected table
  stored in an object of class \code{\linkS4class{Dasst}}.
  Tables are internally stored and retrieved as
  \code{\link{data.frame}}. Rules for subset can be
  applied.
}
\examples{
# Add a row of NA at the end of the table 1
data(plantGrowth)
rmax <- nrow(plantGrowth[[1]])
plantGrowth[[1]][rmax + 1, ] <- NA

# Edit a subset
plantGrowth[[1]][131:132,2:4]
plantGrowth[[1]][131:132,2:4] <- matrix(rep(100,6),nrow=2)
plantGrowth[[1]][131:132,2:4]

# Remove the last rows
# No need to subset left hand side. Dimension are automatically adjusted.
tail(plantGrowth[[1]])
plantGrowth[[1]] <- plantGrowth[[1]][c(-131,-132), ]
tail(plantGrowth[[1]])

# Column names are also valid
plantGrowth[[1]][129:130,"SNW1C"]
plantGrowth[[1]][129:130,"SNW1C"] <- 1100:1101
plantGrowth[[1]][129:130,"SNW1C"]
}
\seealso{
  \code{\link{[[}}
}

