\docType{methods}
\name{searchAncillary}
\alias{searchAncillary}
\alias{searchAncillary,Dasst-method}
\title{Search for ancillary data within the \code{\linkS4class{Dasst}} object}
\arguments{
  \item{x}{An object of class \code{\linkS4class{Dasst}}.}

  \item{fileKey}{A character string. Search for this
  pattern within the "filename" slot.}

  \item{secKey}{A character string. Search for this pattern
  within the "section" slot.}

  \item{colKey}{A character string. Search for this pattern
  within the tables column names.}

  \item{...}{Other parameters than may be passed to grepl.}
}
\value{
  An integer representing the table orders whose ancillary
  data satisfied the search criteria.
}
\description{
  \code{searchAncillary} looks for ancillary data that
  satisfies the search criteria and gives the table orders
  in the \code{\linkS4class{Dasst}} object for successful
  results.
}
\details{
  This method searches for character strings or regular
  expressions in the ancillary data of the
  \code{\linkS4class{Dasst}} object. Patterns are sought
  into "fileNames" and "sections" slots, and table column
  names. The corresponding table orders whose ancillary
  data satisfied the search criteria are gathered in a
  vector.
}
\examples{
data(plantGrowth)
searchAncillary(plantGrowth, secKey="run[[:space:]]*1")
searchAncillary(plantGrowth, secKey="run[[:space:]]*1", ignore.case=TRUE)
}

