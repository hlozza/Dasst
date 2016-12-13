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
  A \code{\link{list}} that can be inserted into an object
  of class \code{\linkS4class{Dasst}}.
}
\description{
  \code{buildContents} generates contents that can be
  inserted into an object of class
  \code{\linkS4class{Dasst}}.
}
\details{
  This function builds the contents that can be inserted
  into an object of class \code{\linkS4class{Dasst}}. The
  result is given as a \code{\link{list}} that can be
  inserted into an object of class
  \code{\linkS4class{Dasst}} using the
  \code{\link{setContents<-}} method.
}
\examples{
mydf <- data.frame(a=c(1,2,3), b=c("one","two","three"),c=c(1.1,2.2,3.3))
mybc <- buildContents("MyTest.OUT","*TestSec",
 "@ID  NAME VALUE","  1   one 1.100", mydf)
myObj <- Dasst()
length(myObj) <- 1
setContents(myObj, 1, TRUE) <- mybc
}

