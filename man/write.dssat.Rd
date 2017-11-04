\name{write.dssat}
\alias{write.dssat}
\title{Write to a DSSAT file from an object of class Dasst}
\usage{
  write.dssat(object, fnames = character())
}
\arguments{
  \item{object}{An object of class
  \code{\linkS4class{Dasst}}.}

  \item{fnames}{A character vector. An optional parameter
  encoding the paths to the files where the contents of the
  object of class \code{\linkS4class{Dasst}} will be
  stored.}
}
\description{
  \code{write.dssat} writes to a file the contents of an
  object of class \code{\linkS4class{Dasst}}.
}
\details{
  This function writes to a file the contents of an object
  of class \code{\linkS4class{Dasst}} striving to maintain
  compatibility with the DSSAT file format.

  The \code{fnames} vector specifies the paths to the files
  where data will be stored. Each table of the
  \code{\linkS4class{Dasst}} object may be saved in an
  individual file. If the length of \code{fnames} vector is
  shorter than the length of the object, then the paths
  will be recycled as necessary.

  If \code{fnames} is not specified, the paths of the
  originally read files are used. First, the original files
  are saved appending a \file{.bak} extension. Then, the
  \code{\linkS4class{Dasst}} object is saved using these
  paths.
}
\examples{

data(plantGrowth)
length(plantGrowth) <- 1
ffn <- paste(tempdir(), "PlantGro.OUT", sep="/")
write.dssat(plantGrowth, ffn)

}

