### Read field names, position and size from file header
### Multiple consecutive headerSep are treated as a single separator  

extractHeader <- function(line, headerSep, headerSign, fieldVec){

  npsDF <- data.frame()

  ## Replace the char headerSign signaling the header line with a headerSep 
  if(nzchar(headerSign)){
    line <- sub(headerSign,headerSep,line)
  }

  ## Extract column order
  alphanums <- strsplit(line, "")[[1]]

  ## bindField is always true when we do not look for specific fields  
  bindField <- ! as.logical(length(fieldVec))
  
  prevcc <- headerSep
  vname <- character()
  vstart <- 1
  vsize <- 0
  for(cc in alphanums){
    if(identical(cc, headerSep) && ! identical(prevcc, headerSep)){

      ## Extract headerSep from field name 
      trimmedvn <- gsub(headerSep,"",vname)

      ## Do we look for specific fields within the header?
      if(length(fieldVec)){
        bindField <- any(sapply(fieldVec,function(x,y) grepl(x,y), trimmedvn))
      }

      if(bindField){
        fields <- list(name=trimmedvn, pos=vstart, size=vsize)
        npsDF <- rbind(npsDF, data.frame(fields,stringsAsFactors=FALSE))
      }
      vname <- character()
      vstart <- vstart + vsize
      vsize <- 0
    }

    prevcc <- cc
    vname <- paste(vname, cc, sep="")
    vsize <- vsize + 1
  }

  ## Extract headerSep from field name 
  trimmedvn <- gsub(headerSep,"",vname)

  ## Do we look for specific fields within the header?
  if(length(fieldVec)){
    bindField <- any(sapply(fieldVec,function(x,y) grepl(x,y), trimmedvn))
  }
  
  if(bindField && nzchar(trimmedvn)){
    fields <- list(name=trimmedvn, pos=vstart, size=vsize)
    npsDF <- rbind(npsDF, data.frame(fields,stringsAsFactors=FALSE))
  }

  return(npsDF)
}


