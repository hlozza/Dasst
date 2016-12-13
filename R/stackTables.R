##' @include Class-Dasst.R Dasst-methods.R
##'
NULL

##' Stack tables from object of class Dasst
##'
##' \code{stackTables} stacks the selected tables of an object
##' of class \code{\linkS4class{Dasst}}.
##'
##' This function  stacks the selected tables of an object
##' of class \code{\linkS4class{Dasst}}.
##' The result is given as a \code{\link{data.frame}}.
##'
##' @param object Object of class \code{\linkS4class{Dasst}}.
##' @param indices An integer vector. A vector with the position
##'  of the tables which will be stacked.
##'  Values range form 1 to \code{length(x)}.
##' @return A \code{\link{data.frame}} composed of the stacked tables.
##'
##' @export
##'
##' @examples
##' 
##' data(plantGrowth)
##' nrow(plantGrowth[1])
##' nrow(plantGrowth[2])
##' plantgro12 <- stackTables(plantGrowth, c(1,2))
##' nrow(plantgro12)
##'
stackTables <- function(object, indices){

  ## check object and indices type

  if(class(object) != "Dasst"){
    cat("Error: ", deparse(substitute(object)), " is not of class Dasst.\n")
    return(NULL)
  }

  if(!is.numeric(indices)){
    cat("Error: indices must be integers in the range from 1 to ", length(object@tables), "\n")
    return(NULL)
  }

  if(length(indices) == 1){
    index <- indices[1]
    return(object[index])
  }else if(all(indices > 0) && all(indices <= length(object)) ){
    return(do.call(rbind,as(object,"list")[indices]))
  }else{
    cat("Error: indices must be integers in the range from 1 to ", length(object@tables), "\n")
    return(NULL)
  }
}


