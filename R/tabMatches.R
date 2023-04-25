#' tabulate matches of keyword screens for a certain group
#' 
#' @param screenMatrix A matrix of the screened matches with dimensions row = n articles, col = keywords+analysis_id
#' 
#' @param group The group to tabulate keywords by
#' 
#' @param colExclude The name of some columns to exclude if desired
#' 
#' @param keywordGroupLookupTable A lookup table with the keywords (Term) and corresponding groups (Group)


tabMatches <- function(screenMatrix, group, keywordGroupLookupTable, colExclude = NULL){
  
  # find matches
  colNames <- keywordGroupLookupTable$Term[which(keywordGroupLookupTable$Group == group)]
  colNames <- gsub(" ","_", colNames)
  
  colInd <- which(colnames(screenMatrix) %in% colNames)
  
  if(!is.null(colExclude)){
    colExInd <- which(colnames(screenMatrix) %in% colExclude)
    colInd <- colInd[-c(which(colInd %in% colExInd))]
    
    # tabulate matches of these columns to exclude rows which have matches
    if(length(colExInd)>1){
      rmTab <- rowSums(screenMatrix[,colExInd], na.rm = TRUE)
    }else{
      rmTab <- screenMatrix[,colExInd]
    }
    rowExInd <- which(rmTab == 0)
    screenMatrix <- screenMatrix[rowExInd,]
  }
  
  # across all the terms, I only care if there is a match to any term
  if(length(colInd)>1){
    tempTab <- rowSums(screenMatrix[,colInd], na.rm = TRUE)
  }else{
    tempTab <- screenMatrix[,colInd]
  }
  
  screenDf <- data.frame(analysis_id = screenMatrix[,"analysis_id"], n_matches = tempTab)
  
  return(screenDf)
}