\docType{package}
\name{Dasst-package}
\alias{Dasst-package}
\title{Dasst: a package for reading, processing and writing DSSAT files}
\description{
  The \pkg{Dasst} (Dssat AS Simple Tables) package provides
  methods for reading, processing and writing files
  originally formatted for the DSSAT Crop Simulation
  Models.
}
\details{
  The \pkg{Dasst} package defines the
  \code{\linkS4class{Dasst}} class which enables to store
  the data encoded for the DSSAT Crop Simulation Models as
  an S4 object, both for the inputs and the outputs.
  Several methods are available.
}
\author{
  Homero Lozza, \email{homerolozza@gmail.com}
}
\seealso{
  \linkS4class{Dasst} for class definition,
  \code{\link{show}}, \code{\link{summary}},
  \code{\link{[}} methods for content description, and
  \code{\link{read.dssat}} function for reading files. For
  other available methods see INDEX file.

  An object example is available at
  \code{\link{plantGrowth}}.
}

