\docType{class}
\name{Dasst-class}
\alias{Dasst-class}
\title{Dasst class description}
\description{
  An S4 class that stores information recorded on DSSAT
  files.
}
\section{Slots}{
  \describe{ \item{\code{fileNames}:}{A character vector
  containing the names and the paths to the original data
  files.} \item{\code{sections}:}{A character vector
  containing the names for each section within the DSSAT
  file structure.} \item{\code{fields}:}{A list of
  \code{\link{data.frame}} containing the names, modes, and
  widths in characters for each data field.  The number of
  decimal digits are also stored in each numeric field.}
  \item{\code{tables}:}{A list of \code{\link{data.frame}}
  containing the actual values retrieved from the original
  file.} }
}
\seealso{
  \code{\link{show}}, \code{\link{summary}},
  \code{\link{[[}} \code{\link{[}} methods for content
  description, and \code{\link{read.dssat}} function for
  reading files. For other available methods see INDEX
  file.

  An object example is available at
  \code{\link{plantGrowth}}.
}

