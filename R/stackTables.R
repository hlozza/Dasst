##' @include Class-Dasst.R Dasst-methods.R
##'
NULL

##' Stack the tables of an object of class Dasst
##'
##' \code{stackTables} stacks the tables of an object
##' of class \code{\linkS4class{Dasst}}.
##'
##' This function  stacks the tables of an object
##' of class \code{\linkS4class{Dasst}}.
##' The result is given as a \code{\link{data.frame}}.
##'
##' @param object Object of class \code{\linkS4class{Dasst}}.
##' @return A \code{\link{data.frame}} composed of the stacked tables.
##'
##' @export
##'
##' @examples
##' 
##' data(plantGrowth)
##' nrow(plantGrowth[[1]])
##' nrow(plantGrowth[[2]])
##' plantgro12 <- stackTables(plantGrowth[1:2])
##' nrow(plantgro12)
##'
stackTables <- function(object){

  ## check object and indices type

  if(class(object) != "Dasst"){
    cat("Error: ", deparse(substitute(object)), " is not of class Dasst.\n")
    return(data.frame())
  }

  if(length(object) == 0){
    
    return(data.frame())
    
  }else if(length(object) == 1){
    
    return(object[[1]])
    
  }else{
    
    return(do.call(rbind, as(object,"list")))
    
  }
}


