##' @include Dasst.R 
##'
NULL

##' Write a DSSAT file from an object of class Dasst
##'
##' \code{write.dssat} writes the contents of
##' an object of class \code{\linkS4class{Dasst}} to a file.
##'
##' This function writes the contents of
##' an object of class \code{\linkS4class{Dasst}} to a file
##' striving to maintain compatibility with the DSSAT file format.
##'
##' @param object An object of class \code{\linkS4class{Dasst}}.
##'
##' @export
##'
##' @examples
##' \dontrun{
##' 
##' data(plantGrowth)
##' length(plantGrowth) <- 1
##' write.dssat(plantGrowth)
##'
##' }
##'
write.dssat <- function(object){

  ## check object 

  if(class(object) != "Dasst"){
    stop(deparse(substitute(object)), " is not of class Dasst.\n")
  }


  fileName <- ""
  fd <- file()
  prevSection <- ""
  
  for(tix in 1:length(object)){

    if(! identical(fileName,object@fileNames[tix])){

      fileName <- object@fileNames[tix]
      if(isOpen(fd)){
        close(fd)
      }
      if(file.exists(fileName)){
        file.rename(fileName, paste(fileName, ".bak", sep=""))
      }

      fd <- file(fileName, open="w")
      prevSection <- ""
    }

    ## Section
    ## Mark section with *
    line <- object@sections[tix]
    if (! identical(line, prevSection)){
      if(nzchar(prevSection))
        writeLines("", fd)
      prevSection <- line
      line <- sub("^.", "*", line)
      writeLines(line, fd)
    }

    ## Header
    nstdDF <- object@fields[[tix]]
    
    line <- ""
    for(cix in 1:nrow(nstdDF)){
      fstr <- format(nstdDF[cix,"name"], justify="right", width=nstdDF[cix,"size"])
      line <- paste(line, fstr, sep="")
    }
    ## Sign header with @
    line <- sub("^.", "@", line)
    writeLines(line, fd)
    
    ## Values
    valuesDF <- object@tables[[tix]]
    
    if(length(valuesDF) == 0)
      next
    
    for(rix in 1:nrow(valuesDF)){
      line <- ""
      for(cix in 1:nrow(nstdDF)){
        fieldName <- nstdDF[cix,"name"]
        if(nstdDF[cix,"text"]){
          fstr <- valuesDF[rix,fieldName]
        }else{
          value <- round(valuesDF[rix,fieldName], nstdDF[cix,"decimals"])
          fstr <- format(value, justify="right", width=nstdDF[cix,"size"], nsmall=nstdDF[cix,"decimals"])
        }
        line <- paste(line, fstr, sep="")
      }
      writeLines(line, fd)
    }

    ##  Close loop tables    
  }
  
  if(isOpen(fd)){
    close(fd)
  }

}

