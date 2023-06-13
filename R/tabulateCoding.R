tabulateCoding <- function(codebook, idCols){
  
  ## Tabulate
  # index of which columns belong to which variables
  variableInd <- unlist(lapply(strsplit(colnames(codebook), "[.]"), function(x) x[[1]]))
  
  # identify variables to tabulate (i.e. not identification columns)
  variables_to_tab <- unique(variableInd)
  variables_to_tab <- variables_to_tab[which(!(variables_to_tab %in% idCols))]
  
  # empty list to fill with tabulated tables (one item for each variable)
  tabulated_variables <- list()
  
  for(v in 1:length(variables_to_tab)){
    colsInd <- grep(variables_to_tab[v], variableInd, ignore.case = TRUE)
    
    if(is.character(codebook[,colsInd])){next} # can't tabulate character vectors
    
    if(!is.null(dim(codebook[,colsInd]))){
      tab <- colSums(codebook[,colsInd], na.rm=TRUE)
      # if there are multiple values for each variable, get the variable name
      if(sum(grepl("[.]", colnames(codebook)[colsInd])) > 0){
        names(tab) <- unlist(lapply(strsplit(colnames(codebook)[colsInd], "[.]"), function(x) x[[2]]))
      }
    }else{
      tab <- tabulate(codebook[,colsInd])
    }
    tabulated_variables[[v]] <- tab
    names(tabulated_variables)[v] <- variables_to_tab[v]
  }
  return(tabulated_variables)
}