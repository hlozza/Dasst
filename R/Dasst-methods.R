##' @include Class-Dasst.R
##'
NULL

##' A constructor for an empty object of class Dasst
##'
##' \code{Dasst} is a constructor for the user that returns an empty
##' object of class \code{\linkS4class{Dasst}}.
##'
##' This function constructs an empty object of class
##' \code{\linkS4class{Dasst}}. No arguments are required.
##'
##' @return An empty object of class \code{\linkS4class{Dasst}}.
##'
##' @seealso \code{\linkS4class{Dasst}} for class definition.
##' 
##' @export
##'
##' @examples
##' 
##' myObj <- Dasst()
##' myObj
##' class(myObj)
##' length(myObj)
##'
##'
Dasst <- function(){
  return(new(Class="Dasst"))
}

##' Print object of class Dasst
##'
##' \code{print.Dasst} prints the contents of an object
##' of class \code{\linkS4class{Dasst}}.
##'
##' This function extends the S3 \code{\link{print}} generic function.
##' It prints the contents of an object of class \code{\linkS4class{Dasst}}.
##'
##' @param x Object of class \code{\linkS4class{Dasst}}.
##' @param ix An integer number. The contents of the first table are
##'  print by default. Others table contents can be display setting
##'  this parameter in the range form 1 to \code{length(x)}.
##' @param ... Other parameters for the \code{\link{print.data.frame}}
##'  function that specify how tables should look.
##' @return An invisible object.
##'
##' @method print Dasst
##' @export
##'
##' @examples
##' 
##' data(plantGrowth)
##' print(plantGrowth)
##'
print.Dasst <- function(x, ix=1, ...){
  cat("* Class Dasst:\n")

  if(length(x@tables) == 0){
    
    cat("* Empty object\n")
    
  }else  if(!is.numeric(ix) || ix < 1|| ix > length(x@tables)){
    
    cat("Error: Use a positive index in the range from 1 to ", length(x@tables), "\n")

  }else{

    cat("* Showing contents for position ", ix, "\n")
    cat("* File    = ", x@fileNames[ix], "\n")
    cat("* Section = ", x@sections[ix], "\n")
    if(is.null(x@tables[[ix]])){
      cat("* Table is null\n")
    }else{
      cat("* Table\n")
      print(x@tables[[ix]], ...)
    }
  }
  
  invisible(x)
}

##' Show method for class \code{\linkS4class{Dasst}}
##'
##' \code{show} shows a few contents of an object
##' of class \code{\linkS4class{Dasst}}.
##'
##' This method shows the contents of the first table stored
##' in an object of class \code{\linkS4class{Dasst}}. It displays values
##' limited to a few records. Use the \code{\link{print}} function
##' for more options.
##'
##' @docType methods
##' @name show
##' @rdname Dasst-show
##' @aliases show,Dasst-method
##'
##' @examples
##' 
##' data(plantGrowth)
##' plantGrowth
##'
setMethod(
          f="show",
          signature="Dasst",
          definition=function(object){
            if(length(object@tables) == 0 || is.null(object@tables[[1]])){
              print.Dasst(object)
            }else{
              print.Dasst(object, 1, max=10*ncol(object@tables[[1]]))
            }
            cat("Contents for other positions can be print using print(object, position).\n")
          }
          )

##' Summary method for class \code{\linkS4class{Dasst}}
##'
##' \code{summary} summarizes the contents
##' of an object of class \code{\linkS4class{Dasst}}.
##'
##' This method summarizes the contents of the object
##' of object of class \code{\linkS4class{Dasst}}. After reading a DSSAT file,
##' summary can give an idea of the volume of information stored in
##' that file.
##'
##' @param object An object of class \code{\linkS4class{Dasst}}.
##' @param ... Arguments that may be passed to other functions.
##' @return An object of class \code{summary.Dasst}
##'
##' @export summary
##' @name summary
##' @docType methods
##' @rdname Dasst-summary
##'
##' @examples
##' 
##' data(plantGrowth)
##' summary(plantGrowth)
##'
###if(!isGeneric("summary")){
setGeneric(name = "summary", def = function(object, ...){standardGeneric("summary")} )
###}

##' @rdname Dasst-summary
##' @aliases summary,Dasst
##' @method summary Dasst
##' @export
##'
summary.Dasst <- function(object, ...){
  obj <- list()
  obj[["class"]] <- class(object)
  if(length(object@tables)){
    obj[["files"]] <- length(levels(factor(object@fileNames)))
    obj[["sections"]] <- length(levels(factor(object@sections)))
    obj[["tables"]] <- length(object@tables)
    obj[["tablesDims"]] <- t(sapply(object@tables,function(x) if(is.null(x)) c(0, 0) else dim(x)))

  }else{
    obj[["files"]] <- 0
    obj[["sections"]] <- 0
    obj[["tables"]] <- 0
    obj[["tablesDims"]] <- c(0, 0)
    
  }
  
  class(obj) <- "summary.Dasst"
  obj
}


##' @rdname Dasst-summary
##' @aliases summary,Dasst-method
##' @exportMethod summary
##'
setMethod(
          f = "summary",
          signature = "Dasst",
          definition = summary.Dasst
          )

##' Print object of class \code{summary.Dasst}
##'
##' \code{print.summary.Dasst} prints the contents of an object
##' of class \code{summary.Dasst}.
##'
##' This function extends the S3 \code{\link{print}} generic function. 
##' It prints the contents of an object of class \code{summary.Dasst}.
##'
##' @param x Object of class \code{summary.Dasst}.
##' @param ... Arguments that may be passed to other functions.
##' @return An invisible object.
##'
##' @method print summary.Dasst
##' @export
##'
##' @examples
##' 
##' data(plantGrowth)
##' summary(plantGrowth)
##'
print.summary.Dasst <- function(x, ...){

  cat("* Object of class = ", x[["class"]], "\n")

  if(x[["tables"]] == 0){

    cat("* Files    = 0\n")
    cat("* Sections = 0\n")
    cat("* Tables   = 0\n")
    cat("* Total records = 0\n")

  }else{
    
    cat("* Files    = ", x[["files"]], "\n")
    cat("* Sections = ", x[["sections"]], "\n")
    cat("* Tables   = ", x[["tables"]], "\n")
    
    prnLt <- 10
    nrowPrint <- min(prnLt, x[["tables"]])

    cat(paste("Table ", (1:nrowPrint), ": ", x[["tablesDims"]][1:nrowPrint,2], "fields and", x[["tablesDims"]][1:nrowPrint,1], "records", sep=" ", collapse="\n"),"\n")

    nonnullTables <- as.logical(apply(x[["tablesDims"]], 1, sum))
    nullTables <- ! nonnullTables
    
    if(any(nullTables[1:nrowPrint])){
      nullTablesCut <- nullTables[1:nrowPrint]
      cat("Null table: ", paste((1:nrowPrint)[nullTablesCut], collapse=", "), "\n") 
    }

    if(nrowPrint < x[["tables"]]){
      cat("\n... Print limited to the first ", prnLt, " tables.\n")
    }

    ## Consider all tables
    if(all(nullTables)){
      cat("* Total records = 0\n")
    }else{
      totalRecs <- sum(x[["tablesDims"]][,1])
      cat("* Total records = ", totalRecs, "\n")
    }
  }
  
  invisible(x)
}

##' Length of an object of class \code{\linkS4class{Dasst}}.
##'
##' \code{length.Dasst} computes the length of an object
##' of class \code{\linkS4class{Dasst}}.
##'
##' This function extends the S3 \code{\link{length}} generic function.
##' It computes the length of an object of class \code{\linkS4class{Dasst}}.
##' The length equals the quantity of stored tables.
##' The empty object has length \code{0}.    
##'
##' @param x Object of class \code{\linkS4class{Dasst}}.
##' @return An integer representing the length of the object.
##'
##' @seealso \code{\link{length<-.Dasst}}
##' 
##' @method length Dasst
##' @export
##'
##' @examples
##' 
##' data(plantGrowth)
##' length(plantGrowth)
##'
length.Dasst <- function(x){ length(x@tables)}

##' Set the length of an object of class \code{\linkS4class{Dasst}}.
##'
##' This sets the length of an object
##' of class \code{\linkS4class{Dasst}}.
##'
##' \code{length<-.Dasst} function extends the S3 \code{\link{length}}
##' generic function.
##' It sets the length of an object of class \code{\linkS4class{Dasst}}.
##' The object can be shrinked or extended adding \code{NULL} or \code{NA}
##' contents.    
##'
##' @param x Object of class \code{\linkS4class{Dasst}}.
##' @param value Integer value. Sets the new length
##'  of the \code{\linkS4class{Dasst}} object.
##' @return An integer value corresponding to the actual length of the object.
##'
##' @seealso \code{\link{length.Dasst}}
##' 
##' @usage \method{length}{Dasst} (x) <- value
##' @method length<- Dasst
##' @export
##'
##' @examples
##' 
##' data(plantGrowth)
##' length(plantGrowth)
##' length(plantGrowth) <- 8
##' length(plantGrowth)
##'
"length<-.Dasst" <- function(x, value){

  if(! is.numeric(value) || value < 0){
    cat("Error: Use a positive integer or zero.\n")
    return(x)
  }

  nameObject <- deparse(substitute(x))

  for(sn in slotNames(x)){
    length(slot(x, sn)) <- floor(value)
  }

  assign(nameObject, x, envir=parent.frame())

  x
}

##' As forces an object of class Dasst to belong to class list
##'
##' Coerces an object of class \code{\linkS4class{Dasst}}
##' to an object of class \code{\link{list}}.
##'
##' This function enables the function \code{\link{as}} to coerce objects
##' of class \code{\linkS4class{Dasst}} to belong to class \code{\link{list}}.  
##'
##' @name as
##'
##' @examples
##' 
##' data(plantGrowth)
##' lplantgro <- as(plantGrowth, "list")
##' class(lplantgro)
##'
setAs("Dasst", "list", function(from) from@tables)

validRange <- function(i, imin, imax){

  if(imax == 0){
    cat("Error: Object of length 0 with no contents.\n")
    return(FALSE)
  }else if(length(i) == 0){
    cat("Error: Index argument out of bounds.\n")
    cat("Choose a table in the range from ", imin, " to ", imax, ".\n")
    return(FALSE)
  }else if(length(i) > 1){
    cat("Error: Vector indexing is not allow.\n")
    cat("Choose a table in the range from ", imin, " to ", imax, ".\n")
    return(FALSE)
  }else if(i >= imin && i <= imax){
    return(TRUE)
  }else{
    cat("Error: Table ", i, "does not exist.\n")
    cat("Choose a table in the range from ", imin, " to ", imax, ".\n")
    return(FALSE)
  }
}


##' "[[" method for class Dasst
##'
##' \code{"[["} gets the contents of a table from
##' an object of class \code{\linkS4class{Dasst}}.
##'
##' This method gets the contents of the selected table stored
##' in an object of class \code{\linkS4class{Dasst}}. Tables are internally
##' stored and retrieved as \code{\link{data.frame}}. Rules for subset
##' can be applied.
##'
##' @param x An object of class \code{\linkS4class{Dasst}}.
##' @param i An integer value. Position where values will be retrieved.
##' @return The values retrieved from the table at position i
##'  as \code{\link{data.frame}}.
##'
##' @seealso \code{\link{[[<-}}
##'
##' @name [[
##' @docType methods
##' @rdname Dasst-getter
##' @aliases [[,Dasst,numeric-method
##' @exportMethod [[
##'
##' @examples
##' 
##' data(plantGrowth)
##' class(plantGrowth[[1]])
##' plantGrowth[[1]]
##' plantGrowth[[1]][1:10,]
##'
setMethod(
          f="[[",
          signature=c(x="Dasst",i="numeric"),
          definition=function(x,i){
            if(validRange(i, 1, length(x@tables))){
              return(x@tables[[i]])
            }else{
              return(data.frame())
            }
          }
          )

##' "[[<-" method for class \code{\linkS4class{Dasst}}
##'
##' \code{"[[<-"} sets the contents of a table
##' from an object of class \code{\linkS4class{Dasst}}.
##'
##' This method sets the contents of the selected table stored
##' in an object of class \code{\linkS4class{Dasst}}. Tables are internally
##' stored and retrieved as \code{\link{data.frame}}. Rules for subset
##' can be applied.
##'
##' @param x An object of class \code{\linkS4class{Dasst}}.
##' @param i An integer value. Position where values will be updated.
##' @param value Any Values to be stored at the given position. 
##' @return The actual object of class \code{\linkS4class{Dasst}}.
##'
##' @seealso \code{\link{[[}}
##'
##' @name [[<-
##' @docType methods
##' @rdname Dasst-setter
##' @aliases [[<-,Dasst,numeric-method
##' @exportMethod [[<-
##'
##' @examples
##' 
##' # Add a row of NA at the end of the table 1
##' data(plantGrowth)
##' rmax <- nrow(plantGrowth[[1]])
##' plantGrowth[[1]][rmax + 1, ] <- NA
##'
##' # Edit a subset
##' plantGrowth[[1]][131:132,2:4]
##' plantGrowth[[1]][131:132,2:4] <- matrix(rep(100,6),nrow=2)
##' plantGrowth[[1]][131:132,2:4]
##'
##' # Remove the last rows
##' # No need to subset left hand side. Dimension are automatically adjusted.
##' tail(plantGrowth[[1]])
##' plantGrowth[[1]] <- plantGrowth[[1]][c(-131,-132), ]
##' tail(plantGrowth[[1]])
##'
##' # Column names are also valid
##' plantGrowth[[1]][129:130,"SNW1C"] 
##' plantGrowth[[1]][129:130,"SNW1C"] <- 1100:1101
##' plantGrowth[[1]][129:130,"SNW1C"] 
##'
setReplaceMethod(
                 f="[[",
                 signature=c(x="Dasst",i="numeric",value="ANY"),
                 definition=function(x,i,value){
                   if(validRange(i, 1, length(x@tables))){
                     x@tables[[i]] <- value
                   }
                   validObject(x)
                   x
                 }
                 )


##' "[" method for class Dasst
##'
##' \code{"["} gets a subset of
##' an object of class \code{\linkS4class{Dasst}}.
##'
##' This method gets a subset of 
##' an object of class \code{\linkS4class{Dasst}}.
##' Shorter objects in the expression are recycled as often as need be
##' until they match the length of the longest object.
##'
##' @param x An object of class \code{\linkS4class{Dasst}}.
##' @param i An integer or logical vector. This is the subset
##' that will be retrieved from the whole object.
##' @return A new object of class \code{\linkS4class{Dasst}} that comprises
##' the elements from the selected subset. 
##'
##' @seealso \code{\link{[<-}}
##'
##' @name [
##' @docType methods
##' @rdname Dasst-getsubsetting
##' @aliases [,Dasst,numeric-method
##' @exportMethod [
##'
##' @examples
##' 
##' data(plantGrowth)
##' length(plantGrowth)
##' plantgro1 <- plantGrowth[1:10]
##' length(plantgro1)
##' class(plantgro1)
##'
##' # Drop contents corresponding to selected orders
##' summary(plantGrowth)
##' plantgro2 <- plantGrowth[-1]
##' summary(plantgro2)
##'
setMethod(
          f="[",
          signature=c(x="Dasst",i="numeric"),
          definition=function(x,i){

            return(new(Class="Dasst", fileNames=x@fileNames[i], sections=x@sections[i], fields=x@fields[i], tables=x@tables[i]))

          }
          )

##' @rdname Dasst-getsubsetting
##' @aliases [,Dasst,logical-method
##' @exportMethod [
##'
setMethod(
          f="[",
          signature=c(x="Dasst",i="logical"),
          definition=function(x,i){

            return(new(Class="Dasst", fileNames=x@fileNames[i], sections=x@sections[i], fields=x@fields[i], tables=x@tables[i]))

          }
          )

##' "[<-" method for class \code{\linkS4class{Dasst}}
##'
##' \code{"[<-"} sets to a subset of
##' an object of class \code{\linkS4class{Dasst}}
##' an other object of the same class 
##'
##' This method sets to a subset of
##' an object of class \code{\linkS4class{Dasst}}
##' an other object of the same class.
##' Shorter objects in the expression are recycled as often as need be
##' until they match the length of the longest object.
##'
##' @param x An object of class \code{\linkS4class{Dasst}}.
##' @param i An integer or logical vector. This is the subset
##' that will be updated from the whole object.
##' @param value An object of class \code{\linkS4class{Dasst}}
##' that will be stored at the given subset. 
##' @return The actual object of class \code{\linkS4class{Dasst}}
##' that comprises the elements updated from the selected subset. 
##'
##' @seealso \code{\link{[}}
##'
##' @name [<-
##' @docType methods
##' @rdname Dasst-setsubsetting
##' @aliases [<-,Dasst,numeric-method
##' @exportMethod [<-
##'
##' @examples
##' 
##' # Replace position 1 with the contents of position 30.
##' data(plantGrowth)
##' plantGrowth[[1]][1:10, 1:15]
##' plantGrowth[1] <- plantGrowth[30]
##' plantGrowth[[1]][1:10, 1:15]
##'
##' # Add a copy of the first order at the end extending the object length
##' rmax <- length(plantGrowth)
##' rmax
##' plantGrowth[rmax+1] <- plantGrowth[1]
##' length(plantGrowth)
##'
##' # Copy position 2 into position 31, moving the former position 31 to the 32.
##' plantgro31 <- plantGrowth[31]
##' plantGrowth[31] <- plantGrowth[2]
##' plantGrowth[32] <- plantgro31
##'
setReplaceMethod(
                 f="[",
                 signature=c(x="Dasst",i="numeric",value="ANY"),
                 definition=function(x,i,value){

                   for(sn in slotNames(x)){
                     slot(x, sn)[i] <- slot(value, sn)
                   }
                   
                   validObject(x)
                   x
                 }
                 )
##' @name [<-
##' @rdname Dasst-setsubsetting
##' @aliases [<-,Dasst,logical-method
##' @exportMethod [<-
##'
setReplaceMethod(
                 f="[",
                 signature=c(x="Dasst",i="logical",value="ANY"),
                 definition=function(x,i,value){

                   for(sn in slotNames(x)){
                     slot(x, sn)[i] <- slot(value, sn)
                   }
                   
                   validObject(x)
                   x
                 }
                 )

##' Add date class to objects of class Dasst
##'
##' \code{addDate<-} adds a column of class date to tables
##' of the object of class \code{\linkS4class{Dasst}}.
##'
##' This method adds a column of class date to tables
##' of the object of class \code{\linkS4class{Dasst}}. Dates
##' expressed as string or integers may be converted and stored as date
##' objects in a new column whose name begins with "date_" and
##' follows with the names of column fields involved in the date extraction.
##'
##' So far, the new column will not be saved if the write method is invoked.
##'
##' @param x An object of class \code{\linkS4class{Dasst}}.
##' @param ... Other parameters:
##'  format, character vector encoding the date format;
##' @param value A formula, numeric vector or character vector.
##'  Order of the column fields from where dates can be composed.
##' @return The actual object.
##'
##' @export addDate<-
##' @name addDate<-
##' @docType methods
##' @rdname Dasst-addDate
##'
##' @examples
##' 
##' data(plantGrowth)
##' addDate(plantGrowth) <- ~ YEAR + DOY
##'
##' # or
##' addDate(plantGrowth) <- c("YEAR", "DOY")
##'
##' # or
##' addDate(plantGrowth) <- c(1, 2)
##'
##' # Only one tables 1 and specifying date format
##' addDate(plantGrowth, index=c(1,2), format="%Y%j") <- ~ YEAR + DOY
##'
###if(!isGeneric("addDate<-")){
setGeneric(name = "addDate<-", def = function(x, ..., value){standardGeneric("addDate<-")} )
###}

##' @rdname Dasst-addDate
##' @name addDate<-
##' @aliases addDate<-,Dasst-method
##' @exportMethod addDate<-
##'
setReplaceMethod(
                 f="addDate",
                 signature=c(x="Dasst", value="ANY"),
                 definition=function(x, ..., value){

                   dateFormat <- "%Y%j"

                   dots <- list(...)
                   if (length(dots)){
                     
                     addedArgs <- names(dots)
                     if (!is.na(match("format", addedArgs)))
                       dateFormat <- dots[["format"]]
                     
                   }

                   datesNumbers <- numeric()
                   for(i in 1:length(x)){
                     
                     if (inherits(value, "formula")) {
                       cc = model.frame(value, x@tables[[i]])
                       if (ncol(cc) == 1) {
                         nm = as.character(as.list(value)[[2]])[1]
                         datesNumbers = match(nm, names(x@tables[[i]]))
                       } else if (ncol(cc) == 2) {
                         nm = as.character(as.list(value)[[2]])[2:3]
                         datesNumbers = match(nm, names(x@tables[[i]]))
                         
                       } else if (dim(cc)[2] == 3) {
                         nm = c(as.character(as.list(as.list(value)[[2]])[[2]])[2:3],
                           as.character(as.list(value)[[2]])[3])
                         datesNumbers = match(nm, names(x@tables[[i]]))
                       }
                       
                     } else if (is.character(value)) {
                       datesNumbers = match(value, names(x@tables[[i]]))
                       naCols <- is.na(datesNumbers)
                       if (any(naCols)) 
                         stop("Columns ", paste(value[naCols], collapse=", "), "are not known.\n")
                       
                     } else if (is.null(dim(value)) && length(value) >= 1) {
                       if (any(value != as.integer(value) || any(value < 1))) 
                         stop("Date columns should be positive integers.\n")
                       if (any(value > ncol(x@tables[[i]])))
                         stop("Value exceeds the number of columns: ", ncol(x@tables[[i]]), ".\n")
                       datesNumbers = value
                     }

                     colName <- paste(names(x@tables[[i]])[datesNumbers], collapse="_")
                     colName <- paste("date", colName, sep="_")
                     
                     if (length(datesNumbers) == 1){
                       x@tables[[i]][[colName]] <- as.Date(as.character(x@tables[[i]][ ,datesNumbers]), format=dateFormat)
                     }else{
                       x@tables[[i]][[colName]] <- as.Date(apply(x@tables[[i]][ ,datesNumbers],1,paste,collapse=""), format=dateFormat)
                     }
                     
                   }

                   validObject(x)
                   x
                 }
                 )


##' Compute within columns from an object of class \code{\linkS4class{Dasst}}.
##'
##' \code{compute<-} computes an expression using the columns
##' of the object of class \code{\linkS4class{Dasst}}.
##'
##' This method computes an expression taking the values recorded
##' on each column field used in the expression belonging
##' to the object of class \code{\linkS4class{Dasst}}.
##' The result is stored as a new column table.
##'
##' So far, the new column will not be saved if the write method is invoked.
##'
##' @param x An object of class \code{\linkS4class{Dasst}}.
##' @param cocol A character string. The name of the new column field.
##' @param value A character string. An expression to compute
##'  within column fields.
##' @return The actual object of class \code{\linkS4class{Dasst}}.
##'
##' @export compute<-
##' @name compute<-
##' @docType methods
##' @rdname Dasst-compute
##'
##' @examples
##' 
##' data(plantGrowth)
##' compute(plantGrowth, "date_YEAR_DOY") <-
##'  "as.Date(paste(YEAR, DOY, sep=\"\"), format=\"%Y%j\")"
##' 
###if(!isGeneric("compute<-")){
setGeneric(name = "compute<-", def = function(x, cocol, value){standardGeneric("compute<-")} )
###}

##' @rdname Dasst-compute
##' @name compute<-
##' @aliases compute<-,Dasst,character,character-method
##' @exportMethod compute<-
##'
setReplaceMethod(
                 f="compute",
                 signature=c(x="Dasst", cocol="character", value="character"),
                 definition=function(x, cocol, value){

                   
                   for(i in 1:length(x@tables)){
                     chrexpr <- parse(text = value)
                     x@tables[[i]][[cocol]] <- eval(chrexpr, x@tables[[i]], parent.frame())
                   }
                   
                   validObject(x)
                   x
                 }
                 )


##' Get ancillary data from an object of
##' class \code{\linkS4class{Dasst}}
##'
##' \code{getAncillary} gets ancillary data from an object
##' of class \code{\linkS4class{Dasst}} connected to the selected
##' table orders.
##'
##' This method gets ancillary data from an object
##' of class \code{\linkS4class{Dasst}} connected to the selected
##' table orders.
##' Values are arranged in tables, and the order is the number
##' assigned successively to each of them after the data
##' have been stored within the \code{\linkS4class{Dasst}} object.
##' \code{getAncillary} provides ancillary data 
##' such as the file name which was originally read, and the section
##' and the header which introduced the values within the file. 
##'
##' @param x An object of class \code{\linkS4class{Dasst}}.
##' @param i An optional integer vector.
##' Orders where to retrieve ancillary data. The default action is
##' to retrieve all the available ancillary data.
##' @return An object of class Ancillary which contains 
##' the retrieved ancillary data for the selected table orders.
##'
##' @export getAncillary
##' @name getAncillary
##' @docType methods
##' @rdname Dasst-getAncillary
##'
##' @examples
##' 
##' data(plantGrowth)
##' getAncillary(plantGrowth, c(1,3,5))
##'
###if(!isGeneric("getAncillary")){
setGeneric(name = "getAncillary", def = function(x, i){standardGeneric("getAncillary")} )
###}

##' @rdname Dasst-getAncillary
##' @aliases getAncillary,Dasst,numeric-method
##' @exportMethod getAncillary
##'
setMethod(
          f="getAncillary",
          signature=c(x="Dasst", i="numeric"),
          definition=function(x, i){

            # Just in case 
            # Restrict indices to actual table orders
            index <- i[i >= 1 & i <= length(x)]
            
            obj <- list()
            obj[["orders"]]   <- index
            obj[["files"]]    <- x@fileNames[index]
            obj[["sections"]] <- x@sections[index]
            obj[["columns"]]  <- vapply(x@tables[index], function(x)
                                        paste(names(x), collapse=" "), "")
            class(obj) <- "Ancillary"
            obj
            
          }
          )


##' @rdname Dasst-getAncillary
##' @aliases getAncillary,Dasst,missing-method
##' @exportMethod getAncillary
##'
setMethod(
          f="getAncillary",
          signature=c(x="Dasst", i="missing"),
          definition=function(x, i){

            return(getAncillary(x,1:length(x)))
          }
          )

##' Print object of class \code{Ancillary}
##'
##' \code{print.Ancillary} prints the contents of an object
##' of class \code{Ancillary}.
##'
##' This function extends the S3 \code{\link{print}} generic function. 
##' It prints the contents of an object of class \code{Ancillary}.
##'
##' @param x Object of class \code{Ancillary}.
##' @param ... Arguments that may be passed to other functions.
##' @return An invisible object.
##'
##' @method print Ancillary
##' @export
##'
##' @examples
##' 
##' data(plantGrowth)
##' getAncillary(plantGrowth, 1:5)
##'
print.Ancillary <- function(x, ...){

  if(length(x[["orders"]]) == 0){

    cat("* No ancillary data for selected table orders.\n")
    
  }else{

    cat("* Showing ancillary data for selected table orders:\n")

    colLimit <- 18
    
    cat(format("Orders:", width=colLimit), " ")
    cat(format("Files:", width=colLimit), " ")
    cat(format("Sections:", width=colLimit), " ")
    cat(format("Columns:", width=colLimit), " \n")

    rowLimit <- 15
    nrow <- min(rowLimit, length(x[["orders"]]))

    for(i in 1:nrow){

      for(cmps in names(x)){

        cval <- x[[cmps]][i]
        # Trim trailing white space
        if(is.character(cval)){
          cval <- sub("\\s+$", "", cval)
        }
        
        if(nchar(cval) <= colLimit){

          cat(format(cval, width=colLimit), " ")
          
        }else{
          
          cat(strtrim(cval, colLimit-3), "... ")
          
        }
        
      }
      
      cat("\n")
      
    }
    
    if(rowLimit < length(x[["orders"]])){
      cat("\n")
      cat("... Print limited to the first ", rowLimit, " rows.\n")
    }
    cat("\n")
    cat("For more, ancillary_object[[<name>]]; ")
    cat("<name>: orders|files|sections|columns.\n")
    
  }
  
  invisible(x)
}


##' Search for ancillary data within the \code{\linkS4class{Dasst}} object
##'
##' \code{searchAncillary} looks for ancillary data that satisfies
##' the search criteria
##' and gives the table orders in the \code{\linkS4class{Dasst}} object
##' for successful results. 
##'
##' This method searches for character strings or regular expressions
##' in the ancillary data of the \code{\linkS4class{Dasst}} object.
##' Patterns are sought into "fileNames" and "sections" slots, and
##' table column names. The corresponding table orders whose ancillary data
##' satisfied the search criteria are gathered in a vector.
##'
##' @param x An object of class \code{\linkS4class{Dasst}}.
##' @param fileKey A character string. Search for this pattern within
##' the "filename" slot.
##' @param secKey A character string. Search for this pattern within
##' the "section" slot.
##' @param colKey A character string. Search for this pattern within
##' the tables column names.
##' @param ... Other parameters than may be passed to grepl.
##' @return An integer representing the table orders whose ancillary data
##' satisfied the search criteria.
##'
##' @export searchAncillary
##' @name searchAncillary
##' @docType methods
##' @rdname Dasst-searchAncillary
##'
##' @examples
##' 
##' data(plantGrowth)
##' searchAncillary(plantGrowth, secKey="run[[:space:]]*1")
##' searchAncillary(plantGrowth, secKey="run[[:space:]]*1", ignore.case=TRUE)
##'
###if(!isGeneric("searchAncillary")){
setGeneric(name = "searchAncillary", def = function(x, fileKey="", secKey="", colKey="", ...){standardGeneric("searchAncillary")} )
###}

##' @rdname Dasst-searchAncillary
##' @aliases searchAncillary,Dasst-method
##' @exportMethod searchAncillary
##'
setMethod(
          f="searchAncillary",
          signature=c(x="Dasst"),
          definition=function(x, fileKey, secKey, colKey, ...){

            if(length(x) == 0){
              cat("Warning: Object of length 0.\n")
              return(numeric(0))
            }
            
            index <- 1:length(x)
            fileOrders <- grepl(fileKey, x@fileNames, ...)
            index <- index[fileOrders]
            
            if(any(fileOrders)){
              
              secOrders <- grepl(secKey, x@sections[fileOrders], ...)
              index <- index[secOrders]
              
              if(any(secOrders)){
                
                colNames <- vapply(x@tables[secOrders],function(x)
                       paste(names(x), collapse=" "), "")
                colOrders <- grepl(colKey, colNames, ...)
                return(index[colOrders])
                
              }
            }
            
            return(numeric(0))
          }
          )

