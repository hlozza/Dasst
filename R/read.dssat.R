##' @include dasstOptions.R Dasst.R extractHeader.R extractData.R
##'
NULL

##' Read a DSSAT-style file into an object of class Dasst
##'
##' \code{read.dssat} reads the contents of a file or group of files
##' and stores the contents into
##' an object of class \code{\linkS4class{Dasst}}.
##'
##' This function reads the contents of a file or group of files
##' and stores the contents into
##' an object of class \code{\linkS4class{Dasst}}.
##'
##' @param fileVec A character vector. The names including
##'  the paths to the files that will be read. 
##' @param fieldVec A character vector. An optional parameter.
##'  If it is not specified, all column fields are retrieved. Else, 
##'  the names subset are the only retrieved column fields. 
##' @param keyVec A character vector. An optional parameter.
##'  If it is not specified, only sections satisfying
##'  this condition will be retrieved.
##' @return A \code{\linkS4class{Dasst}} object with the structure
##'  and information originally saved in the file using DSSAT-style
##'  format specifications.
##'
##' @export
##'
##' @examples
##' 
##' dssatfile <- system.file("extdata","PlantGro.OUT",package="Dasst")
##' dssatfile
##' plantgro <- read.dssat(dssatfile)
##' summary(plantgro)
##' 
read.dssat <- function(fileVec, fieldVec=character(), keyVec=character()){

  sectionSign <- get("sectionSign", envir = .dasstOptions)
  
  headerSep <- get("headerSep", envir = .dasstOptions)
  headerSign <- get("headerSign", envir = .dasstOptions)

  ignoreSign <- get("ignoreSign", envir = .dasstOptions)
  
  pathSep  <-  get("pathSep", envir = .dasstOptions)

  fileNameS   <- character()
  sectionS    <- character()
  fieldS      <- list()
  tableS      <- list()

  tix <- 1

  for(fileName in fileVec){

    ## Check if file exists
    if(!file.exists(fileName)){
      cat("Error: ",fileName," does not exists.\n")
      next
    }

    ## Open file and prepare for reading

    fd <- file(fileName,open="r")

    ## foundTheSection is always true when we do not look for keywords  
    foundTheSection <- ! as.logical(length(keyVec))

    line <- readLines(fd, n = 1)
    lineNum <- 1
    while(length(line)){

      ## Search for section
      if(grepl(sectionSign, line)){
        
        ## Do we look for a keyword within the section?
        if(length(keyVec)){
          foundTheSection <- any(sapply(keyVec,function(x,y) grepl(x,y), line))
          if(!foundTheSection){
            line <- readLines(fd, n = 1)
            lineNum <- lineNum + 1
            next
          }
        }

        section <- line
        line <- readLines(fd, n = 1)
        lineNum <- lineNum + 1
        next
      }


      ## Search for header
      if(foundTheSection && grepl(headerSign, line)){

        npsDF <- extractHeader(line, headerSep, headerSign, fieldVec)

        ## Do not save the table when npsDF has not any field (empty) 
        if(! as.logical(length(npsDF))){
          line <- readLines(fd, n = 1)
          lineNum <- lineNum + 1
          next
        }

        lval <- extractData(fd, npsDF, patIgnore=ignoreSign, patStop=paste(headerSign, sectionSign, sep="|"))

        fileNameS[[tix]]   <- fileName
        sectionS[[tix]]    <- section
        fieldS[[tix]]      <- lval$nstd
        tableS[[tix]]      <- lval$value
        tix <- tix + 1
        
        line <- lval$line
        lineNum <- lineNum + lval$counter
        next
      }

      line <- readLines(fd, n = 1)
      lineNum <- lineNum + 1

    }

    close(fd)

    ## Close loop in fileVec
  }

  return(new(Class="Dasst", fileNames=fileNameS, sections=sectionS, fields=fieldS, tables=tableS))
}



