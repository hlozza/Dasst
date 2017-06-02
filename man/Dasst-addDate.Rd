\docType{methods}
\name{addDate<-}
\alias{addDate<-}
\alias{addDate<-,Dasst-method}
\title{Add date class to objects of class Dasst}
\arguments{
  \item{x}{An object of class \code{\linkS4class{Dasst}}.}

  \item{...}{Other parameters: format, character vector
  encoding the date format;}

  \item{value}{A formula, numeric vector or character
  vector.  Order of the column fields from where dates can
  be composed.}
}
\value{
  The actual object.
}
\description{
  \code{addDate<-} adds a column of class date to tables of
  the object of class \code{\linkS4class{Dasst}}.
}
\details{
  This method adds a column of class date to tables of the
  object of class \code{\linkS4class{Dasst}}. Dates
  expressed as string or integers may be converted and
  stored as date objects in a new column whose name begins
  with "date_" and follows with the names of column fields
  involved in the date extraction.

  So far, the new column will not be saved if the write
  method is invoked.
}
\examples{
data(plantGrowth)
addDate(plantGrowth) <- ~ YEAR + DOY

# or
addDate(plantGrowth) <- c("YEAR", "DOY")

# or
addDate(plantGrowth) <- c(1, 2)

# Only one tables 1 and specifying date format
addDate(plantGrowth, index=c(1,2), format="\%Y\%j") <- ~ YEAR + DOY
}

