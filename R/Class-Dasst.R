### import from methods package
##' @importFrom methods as new slot slot<- slotNames validObject
##' @importFrom methods setClass setGeneric setMethod setReplaceMethod setAs
### import from stat package
##' @importFrom stats model.frame
NULL

##' Dasst class description
##'
##' An S4 class that stores information recorded on DSSAT files. 
##'
##' @section Slots:
##'  \describe{
##'    \item{\code{fileNames}:}{A character vector containing
##'       the names and the paths to the original data files.}
##'    \item{\code{sections}:}{A character vector containing the names 
##'       for each section within the DSSAT file structure.}
##'    \item{\code{fields}:}{A list of \code{\link{data.frame}} containing
##'       the names, modes, and widths in characters for each data field.
##'       The number of decimal digits are also stored in each numeric field.}
##'    \item{\code{tables}:}{A list of \code{\link{data.frame}} containing
##'       the actual values retrieved from the original file.}
##'  }
##'
##' @seealso \code{\link{show}}, \code{\link{summary}}, \code{\link{[[}}
##' \code{\link{[}} methods for content description, and
##' \code{\link{read.dssat}} function
##' for reading files. For other available methods
##' see INDEX file.
##' 
##' An object example is available at \code{\link{plantGrowth}}.
##'
##' @docType class 
##' @name Dasst-class
##' @aliases Dasst-class
##' @rdname Dasst-class
##' @exportClass Dasst
##'
setClass(
         Class = "Dasst",
         representation = representation(
           fileNames   = "character",
           sections    = "character",
           fields      = "list",
           tables      = "list"
           ),
         validity = function(object){
###    cat("~~~ Dasst: inspector ~~~ \n")

           slotLen <- vapply(slotNames(object),function(x) length(slot(object, x)), 1)

           if(all(slotLen == slotLen[1])){
###
           }else{
             stop ("[Dasst: validation error] Slots have not the same length.\n")
           }

           return(TRUE)
         }
         )




