##' @include dasstOptions.R extractHeader.R extractData.R
##'
NULL

##' Build contents for an object of class Dasst.
##'
##' \code{buildContents} generates contents that are inserted into   
##' an object of class \code{\linkS4class{Dasst}}.
##'
##' This function builds the contents that are inserted into   
##' an object of class \code{\linkS4class{Dasst}} from a
##' \code{\link{data.frame}} and ancillary character strings.
##'
##' The \code{\link{data.frame}} contains the actual data 
##' that is stored as a table within the object of class
##' \code{\linkS4class{Dasst}}.
##'
##' @param fileName A character string.  The file name and path
##'  corresponding to the generated contents.
##' @param section A character string. The section title
##'  corresponding to the generated contents.
##' @param headerLine A character string. The header names for the table
##'  of data included in the generated contents.
##' @param dataLine A character string. A typically formatted line of data.
##' @param table A data.frame. The records for the table
##'  of data included in the generated contents are inserted
##'  as a \code{\link{data.frame}}
##' @return An object of class \code{\linkS4class{Dasst}}.
##'
##' @export
##'
##' @examples
##' 
##' mydf <- data.frame(a=c(1,2,3), b=c("one","two","three"),c=c(1.1,2.2,3.3))
##' myObj <- Dasst()
##' myObj[1] <- buildContents("MyTest.OUT","*TestSec",
##'  "@@ID  NAME VALUE","  1   one 1.100", mydf)
##'
buildContents <- function(fileName, section, headerLine, dataLine, table){
  
  sectionSign <- get("sectionSign", envir = .dasstOptions)
  
  headerSep <- get("headerSep", envir = .dasstOptions)
  headerSign <- get("headerSign", envir = .dasstOptions)

  ignoreSign <- get("ignoreSign", envir = .dasstOptions)
  

  if(!is.character(fileName) || length(fileName) != 1 || !nzchar(fileName)){
    cat("Error: fileName is a character string.\n")
    return(NULL)
  }
  if(!is.character(section) || length(section) != 1 || !nzchar(section)){
    cat("Error: section is a character string.\n")
    return(NULL)
  }
  if(!is.character(headerLine) || length(headerLine) != 1 || !nzchar(headerLine)){
    cat("Error: headerLine is a character string.\n")
    return(NULL)
  }
  if(!is.character(dataLine) || length(dataLine) != 1 || !nzchar(dataLine)){
    cat("Error: dataLine is a character string.\n")
    return(NULL)
  }
  if(!is.data.frame(table)){
    cat("Error: table is a data frame.\n")
    return(NULL)
  }
  

  fieldVec=character()
  
  npsDF <- extractHeader(headerLine, headerSep, headerSign, fieldVec)

  Tfile <- file()

  cat(paste(dataLine, "\n", sep="", collapse=""), file=Tfile)

  lval <- extractData(Tfile, npsDF, patIgnore=ignoreSign, patStop=paste(headerSign, sectionSign, sep="|"))
  
  close(Tfile)

  if(ncol(table) != nrow(lval$nstd)){
    cat("Error: The columns number in table and header differ.\n")
    return(NULL)
  }
  
  return(new(Class="Dasst", fileNames=fileName, sections=section, fields=list(lval$nstd), tables=list(table)))
}
