##' @include dasstOptions.R 
##'
NULL

### Extract text data using position and size 

extractData <- function(fileUnit, npsDF, ctrStop=NA, patIgnore="", patStop=""){

  decimalPoint <- get("decimalPoint", envir = .dasstOptions)
  
  patNumeric <- paste("[^0123456789 ", decimalPoint, "+-]", sep="")

  recordList <- vector("list",nrow(npsDF))

  valuesList <- vector("list",nrow(npsDF))
  names(valuesList) <- npsDF[["name"]]

  startPos <- npsDF[["pos"]]
  endPos <- startPos + npsDF[["size"]] - 1

  cellChrMode <- logical()
  cellNumMode <- numeric()

  line <- readLines(fileUnit, n = 1)
  ctrLine <- 1
  while(length(line)){

    if(! is.na(ctrStop) && ctrLine >= ctrStop){
      break
    }

    if(nzchar(patStop) && grepl(patStop, line)){
      break
    }

    if(nzchar(patIgnore) && grepl(patIgnore, line)){
      line <- readLines(fileUnit, n = 1)
      ctrLine <- ctrLine + 1
      next
    }

    cellStr <- substring(line, startPos, endPos)
    
    ## Test only the first record
    ## for text values and get the number of decimal places
    if(ctrLine == 1){
      cellChrMode <-  grepl(patNumeric,cellStr)
      intdec <- strsplit(cellStr, split=decimalPoint, fixed=TRUE)
      cellNumMode <- vapply(intdec, function(x) if(is.na(x[2])) 0 else nchar(x[2]), 1)
    }

    valuesList[cellChrMode] <- cellStr[cellChrMode]
    valuesList[!cellChrMode] <- as.numeric(cellStr[!cellChrMode])

    recordList <- mapply(c, recordList, valuesList, SIMPLIFY=FALSE)

    line <- readLines(fileUnit, n = 1)
    ctrLine <- ctrLine + 1
  }


  valuesDF <- data.frame(recordList,stringsAsFactors=FALSE)

  ## Expand npsDF to include char mode and number of decimal places
  ## Delete pos information. It will be computed from sizes
  nstdDF <- npsDF
  if(length(valuesDF)){
    nstdDF[["pos"]] <- NULL
    nstdDF[["text"]] <- cellChrMode
    nstdDF[["decimals"]] <- cellNumMode

    ## Assign names. For some unknown reason, it changes '#' chars in header
                                        # to '.' chars. 
    names(valuesDF) <- nstdDF[["name"]]
  }
  
  return(list(values=valuesDF, line=line, counter=ctrLine, nstd=nstdDF))
}
