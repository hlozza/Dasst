\name{buildContents}
\alias{buildContents}
\title{Build contents for an object of class Dasst.}
\usage{
  buildContents(fileName, section, headerLine, dataLine,
    table)
}
\arguments{
  \item{fileName}{A character string.  The file name and
  path corresponding to the generated contents.}

  \item{section}{A character string. The section title
  corresponding to the generated contents.}

  \item{headerLine}{A character string. The header names
  for the table of data included in the generated
  contents.}

  \item{dataLine}{A character string. A typically formatted
  line of data.}

  \item{table}{A data.frame. The records for the table of
  data included in the generated contents are inserted as a
  \code{\link{data.frame}}}
}
\value{
  An object of class \code{\linkS4class{Dasst}}.
}
\description{
  \code{buildContents} generates contents that are inserted
  into an object of class \code{\linkS4class{Dasst}}.
}
\details{
  This function builds the contents that are inserted into
  an object of class \code{\linkS4class{Dasst}} from a
  \code{\link{data.frame}} and ancillary character strings.

  The \code{\link{data.frame}} contains the actual data
  that is stored as a table within the object of class
  \code{\linkS4class{Dasst}}.
}
\examples{
mydf <- data.frame(a=c(1,2,3), b=c("one","two","three"),c=c(1.1,2.2,3.3))
myObj <- Dasst()
myObj[1] <- buildContents("MyTest.OUT","*TestSec",
 "@ID  NAME VALUE","  1   one 1.100", mydf)
}

