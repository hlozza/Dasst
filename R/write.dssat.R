##' @include Dasst.R 
##'
NULL

##' Write to a DSSAT file from an object of class Dasst
##'
##' \code{write.dssat} writes to a file the contents of
##' an object of class \code{\linkS4class{Dasst}}.
##'
##' This function writes to a file the contents of
##' an object of class \code{\linkS4class{Dasst}} 
##' striving to maintain compatibility with the DSSAT file format.
##'
##' The \code{fnames} vector specifies the paths to the files
##' where data will be stored. Each table of the
##' \code{\linkS4class{Dasst}} object may be saved in an individual file.
##' If the length of \code{fnames} vector is shorter than the
##' length of the object, then the paths will be recycled
##' as necessary.
##'
##' If \code{fnames} is not specified, the paths of the originally
##' read files are used. First, the original files are saved appending
##' a \file{.bak} extension. Then, the \code{\linkS4class{Dasst}}
##' object is saved using these paths. 
##'
##' @param object An object of class \code{\linkS4class{Dasst}}.
##' @param fnames A character vector. An optional parameter
##'  encoding the paths to the files where the contents of
##'  the object of class \code{\linkS4class{Dasst}} will be stored.
##'
##' @export
##'
##' @examples
##' 
##' data(plantGrowth)
##' length(plantGrowth) <- 1
##' ffn <- paste(tempdir(), "PlantGro.OUT", sep="/")
##' write.dssat(plantGrowth, ffn)
##'
##'
write.dssat <- function(object, fnames=character()){

  ## check object 

  if(class(object) != "Dasst"){
    stop(deparse(substitute(object)), " is not of class Dasst.\n")
  }

  fileNames <- object@fileNames
  if(length(fnames)){
    fileNames <- rep(fnames, length(object) %/% length(fnames) + 1)
    length(fileNames) <- length(object)
  }

  currentName <- ""
  fd <- file()
  prevSection <- ""
  
  for(tix in 1:length(object)){

    if(! identical(currentName,fileNames[tix])){

      currentName <- fileNames[tix]
      if(isOpen(fd)){
        close(fd)
      }
      if(file.exists(currentName)){
        file.rename(currentName, paste(currentName, ".bak", sep=""))
      }

      fd <- file(currentName, open="w")
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

