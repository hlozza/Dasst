\docType{methods}
\name{as.Date<-}
\alias{as.Date<-}
\alias{as.Date<-,Dasst-method}
\title{Add date class to objects of class Dasst}
\arguments{
  \item{x}{An object of class \code{\linkS4class{Dasst}}.}

  \item{...}{Other parameters: format, character vector
  encoding the date format; index, numeric vector for a
  subset of tables.}

  \item{value}{A formula, numeric vector or character
  vector.  Order of the column fields from where dates can
  be composed.}
}
\value{
  The actual object.
}
\description{
  \code{as.Date<-} adds a column of class date to tables of
  the object of class \code{\linkS4class{Dasst}}.
}
\details{
  This method adds a column of class date to tables of the
  object of class \code{\linkS4class{Dasst}}. Dates
  expressed as string or integers may be converted and
  stored as date objects in a new column whose name begins
  with "date_" and follows with the names of column fields
  involved in the date extraction.
}
\examples{
data(plantGrowth)
as.Date(plantGrowth) <- ~ YEAR + DOY

# or
as.Date(plantGrowth) <- c("YEAR", "DOY")

# or
as.Date(plantGrowth) <- c(1, 2)

# Only one tables 1 and specifying date format
as.Date(plantGrowth, index=c(1,2), format="\%Y\%j") <- ~ YEAR + DOY
}

