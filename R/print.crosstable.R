# Method to print a crosstable
print.crosstable <- function(x) {
  invisible(x)
  #-------------------#
  # GENERAL VARIABLES #
  #-------------------#
  tablePrint <- attr(x, "tablePrint")
  arguments <- attr(x, "arguments")
  
  tableDimNames <- dimnames(tablePrint)
  tablePrint.names <- names(dimnames(tablePrint))
  
  if(arguments$stats.on.cols) {
    col.vars <- c(arguments$col.vars, (length(dim(x))+1):length(dim(tablePrint)))
    row.vars <- arguments$row.vars
  }else{
    col.vars <- arguments$col.vars
    row.vars <- c(arguments$row.vars, (length(dim(x))+1):length(dim(tablePrint)))
  }
  
  row.names <- tablePrint.names[row.vars]
  row.dim <- dim(tablePrint)[row.vars]
  col.names <- tablePrint.names[col.vars]
  col.dim <- dim(tablePrint)[col.vars]
  
  #-------------------------#
  # MAKE THE TABLE TO PRINT #
  #-------------------------#
  dimTable <- dim(tablePrint)
  
  # Index of original table to make
  s <- sapply(1:prod(dimTable[(length(dimTable)-1):length(dimTable)]),rep,prod(dimTable[-((length(dimTable)-1):length(dimTable))]))
  dim(s) <- dimTable
  s <- apply(s, col.vars[length(col.vars)], c)
  s <- sort(s, index.return=TRUE)$ix
  
  tablePrint %<>%
    round(arguments$digits) %>%
    apply(col.vars[length(col.vars)], format)
  
  #Now we sort the table using the s index
  tablePrint <- as.vector(tablePrint)[s]
  dim(tablePrint) <- dimTable
  
  f <- arguments$stats %in% c("column","row","total")
  tableFormat <- rep(" ", length(arguments$stats))
  tableFormat[f] <- "%"
  
  # The Sweep function should add % to the cells
  "%p%" <- function(x,y) paste0(x,y)
  tablePrint <- sweep(tablePrint, length(dimTable), tableFormat, "%p%")
  dim(tablePrint) <- dimTable
  
  #colHeaders
  colHeaders <- tableDimNames[col.vars]
  # Table Width for columns based on the length of var labels
  tableWidth <- 0
  for (i in length(colHeaders):1) {
    # Ajustment of the column width by the variable label
    if (nchar(names(colHeaders)[i]) > sum(nchar(colHeaders[[i]]))) {
      dif <- nchar(names(colHeaders)[i]) - sum(nchar(colHeaders[[i]]))
      extraChar <- round(dif*(nchar(colHeaders[[i]])/sum(nchar(colHeaders[[i]]))))
      if (sum(extraChar) < dif)
        extraChar <- extraChar + ceiling((dif-sum(extraChar))*(nchar(colHeaders[[i]])/sum(nchar(colHeaders[[i]]))))
      colWidth <- nchar(colHeaders[[i]]) + extraChar
    } else {
      colWidth <- nchar(colHeaders[[i]])
    }
    # Ajustment of the lower level vars by the upper level vars
    if(sum(colWidth) > sum(tableWidth) & sum(tableWidth) > 0) {
      sapply(1:length(colWidth),
             function(x) {
               if(colWidth[x] > sum(tableWidth)) {
                 dif <- colWidth[x] - sum(tableWidth)
                 extraChar <- round(dif*(tableWidth/sum(tableWidth)))
                 if (sum(extraChar) < dif)
                   extraChar <- extraChar + ceiling((dif-sum(extraChar))*(tableWidth/sum(tableWidth)))
                 tableWidth + extraChar
               } else {
                 tableWidth
               }
             }
      ) -> tableWidth
    } else if (sum(tableWidth) == 0) {
      tableWidth <- colWidth
    } else {
      tableWidth <- rep(tableWidth, length(colWidth))
    }
  }
  
  # Turn table in to ftable and format it to be printed
  tablePrint <- ftable(tablePrint, col.vars = col.vars)
  tableWidth <- pmax(tableWidth, nchar(apply(tablePrint,2,max)))
  tablePrint <- mapply(function(x,y) format(tablePrint[,x], justify ="right", width=y), 1:prod(col.dim), tableWidth)
  dimnames(tablePrint) <- NULL
  
  # A matrix with row headers.
  mapply(
    function(x) {
      rvalue <- character(0)
      for (i in tableDimNames[[row.names[x]]]) {
        rvalue <- c(rvalue,i,rep("", prod(c(row.dim,1)[(x+1):(length(row.dim)+1)])-1))
      }
      rep(rvalue, prod(row.dim[1:x])/row.dim[x])
    }
    , 1:length(row.dim)
  )  -> rowHeaders
  for(i in 1:ncol(rowHeaders)) rowHeaders[,i] <- format(rowHeaders[,i], width=nchar(row.names)[i])
  dim(rowHeaders) <- c(prod(row.dim),length(row.dim))
  rowHeaders.nchar <- nchar(rowHeaders[1,])
  
  # Start of the table
  cat("|", strrep("-", sum(rowHeaders.nchar+1,tableWidth+1)-1), "|\n", sep="")
  
  #Print the table col headers
  colHeaders <- tableDimNames[col.vars]
  for (i in 1:length(col.dim)) {
    iColRep <- prod(c(1,col.dim)[1:i])
    iColDim <- iColRep*length(colHeaders[[i]])
    sapply(
      1:iColRep,
      function(x)
        sum(tableWidth[1:(length(tableWidth)/iColRep)+(length(tableWidth)/iColRep)*(x-1)])
    ) -> iColWidthLab
    sapply(
      1:iColDim,
      function(x)
        sum(tableWidth[1:(length(tableWidth)/iColDim)+(length(tableWidth)/iColDim)*(x-1)])
    ) -> iColWidth
    nSep <- prod(c(col.dim,1)[(i+1):(length(col.dim)+1)])
    
    cat("|", paste0(strrep(" ", c(rowHeaders.nchar)), "|"), sep="")
    cat(paste0(stringr::str_pad(rep(names(colHeaders)[i],iColRep),iColWidthLab+nSep*col.dim[i]-1, "both"), "|"), "\n", sep="")
    cat("|", paste0(strrep(" ", c(rowHeaders.nchar)), "|"), strrep("-", sum(tableWidth+1)-1), "|\n", sep="")
    
    if (i==length(col.dim)) {
      cat("|", paste0(stringr::str_pad(row.names,rowHeaders.nchar,"both", " "),"|"), sep="")
    } else {
      cat("|", paste0(strrep(" ", c(rowHeaders.nchar)), "|"), sep="")
    }
    cat(paste0(stringr::str_pad(rep(colHeaders[[i]],iColRep),iColWidth+nSep-1, "both"), "|"), "\n", sep="")
    if (i<length(col.dim))
      cat("|", paste0(strrep(" ", c(rowHeaders.nchar)), "|"), strrep("-", sum(tableWidth+1)-1), "|\n", sep="")
  }
  
  # Printing the final table
  cat("|", paste0(strrep("-", c(rowHeaders.nchar,tableWidth)), "|"),"\n", sep="")
  for (i in 1:prod(row.dim)) {
    cat("|", paste0(rowHeaders[i,],"|"), sep="")
    cat(paste0(tablePrint[i,], "|"), sep="")
    cat("\n")
  }
  
  # Close of the table
  cat("|", strrep("-", sum(rowHeaders.nchar+1,tableWidth+1)-1), "|\n", sep="")
}
