\docType{methods}
\name{compute<-}
\alias{compute<-}
\alias{compute<-,Dasst,character,character-method}
\title{Compute within columns from an object of class \code{\linkS4class{Dasst}}.}
\arguments{
  \item{x}{An object of class \code{\linkS4class{Dasst}}.}

  \item{cocol}{A character string. The name of the new
  column field.}

  \item{value}{A character string. An expression to compute
  within column fields.}
}
\value{
  The actual object of class \code{\linkS4class{Dasst}}.
}
\description{
  \code{compute<-} computes an expression using the columns
  of the object of class \code{\linkS4class{Dasst}}.
}
\details{
  This method computes an expression taking the values
  recorded on each column field used in the expression
  belonging to the object of class
  \code{\linkS4class{Dasst}}. The result is stored as a new
  column table.

  So far, the new column will not be saved if the write
  method is invoked.
}
\examples{
data(plantGrowth)
compute(plantGrowth, "date_YEAR_DOY") <-
 "as.Date(paste(YEAR, DOY, sep=\\"\\"), format=\\"\%Y\%j\\")"
}

