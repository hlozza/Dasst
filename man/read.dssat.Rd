\name{read.dssat}
\alias{read.dssat}
\title{Read a DSSAT-style file into an object of class Dasst}
\usage{
  read.dssat(fileVec, fieldVec = character(),
    keyVec = character())
}
\arguments{
  \item{fileVec}{A character vector. The names including
  the paths to the files that will be read.}

  \item{fieldVec}{A character vector. An optional
  parameter.  If it is not specified, all column fields are
  retrieved. Else, the names subset are the only retrieved
  column fields.}

  \item{keyVec}{A character vector. An optional parameter.
  If it is not specified, only sections satisfying this
  condition will be retrieved.}
}
\value{
  A \code{\linkS4class{Dasst}} object with the structure
  and information originally saved in the file using
  DSSAT-style format specifications.
}
\description{
  \code{read.dssat} reads the contents of a file or group
  of files and stores the contents into an object of class
  \code{\linkS4class{Dasst}}.
}
\details{
  This function reads the contents of a file or group of
  files and stores the contents into an object of class
  \code{\linkS4class{Dasst}}.
}
\examples{
dssatfile <- system.file("extdata","PlantGro.OUT",package="Dasst")
dssatfile
plantgro <- read.dssat(dssatfile)
summary(plantgro)
}

