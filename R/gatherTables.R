##' @include Class-Dasst.R Dasst-methods.R
##'
NULL

##' Gather tables of an object of class Dasst.
##'
##' \code{gatherTables} gathers the result of performing
##' a certain operation over the tables of an object
##' of class \code{\linkS4class{Dasst}}.
##'
##' This function gathers the result of performing
##' a certain operation over the tables of an object
##' of class \code{\linkS4class{Dasst}}.
##' The result is given as a \code{\link{data.frame}}.
##'
##' @param object Object of class \code{\linkS4class{Dasst}}.
##' @param coCol A character vector. The field names of those columns
##'  that will be copied identically into the result.
##' @param opCol A character vector. The field names of those columns
##'  that will be gather by means of applying the required operation.
##' @param operation A function. The function name for the required operation.
##'  i.e. \code{\link{mean}}, \code{\link{sum}}, etc.
##' @param ... Other parameters for the \code{\link{apply}}
##'  function as additional arguments for the operation.
##' @return A \code{\link{data.frame}} with the values gathered after
##'  the application of the operator to the required columns.
##'
##' @export
##'
##' @examples
##' 
##' data(plantGrowth)
##' plantgro12 <- gatherTables(plantGrowth[1:10], c("DAP"),
##'    c("SWAD","LWAD","GWAD"), mean)
##'
gatherTables <- function(object, coCol, opCol, operation, ...){

  ## check object and indices type

  if(class(object) != "Dasst"){
    cat("Error: ", deparse(substitute(object)), " is not of class Dasst.\n")
    return(data.frame())
  }
  
### check equal column lengths
### check against common column
### check column names
### check function argument type

  if(length(object) == 0){
    
    return(data.frame())
    
  }else if(length(object) == 1){
    
    return(object[[1]][,c(coCol,opCol)])

  }else{

    ##      Trimm to equal lengths
    col <- opCol[1]
    minLen <- min(vapply(as(object,"list"), function(x) length(x[[col]]), 1))

    redData <- list()
    if(all(nzchar(coCol))){
      
      for(col in coCol){
        redData[[col]] <- object[[1]][[col]][1:minLen]
      }
      
    }
    
    for(col in opCol){
      
      redMat <- sapply(as(object,"list"), function(x){x[[col]][1:minLen]})
      redData[[col]] <- apply(redMat, 1, operation, ...)
      
    }
    
    return(data.frame(redData,stringsAsFactors=FALSE))

  }
}

