#' tabulate matches of keyword screens for a certain group
#' 
#' @param screenMatrix A matrix of the screened matches with dimensions row = n articles, col = keywords+analysis_id
#' 
#' @param group The group to tabulate keywords by
#' 
#' @param keywordGroupLookupTable A lookup table with the keywords (Term) and corresponding groups (Group)


tabMatches <- function(screenMatrix, group, keywordGroupLookupTable){
  
  # find matches
  colInd <- which(colnames(screenMatrix) %in% 
                    keywordGroupLookupTable$Term[which(keywordGroupLookupTable$Group == group)])
  
  # across all the terms, I only care if there is a match to any term
  if(length(colInd)>1){
    tempTab <- rowSums(screenMatrix[,colInd], na.rm = TRUE)
  }else{
    tempTab <- screenMatrix[,colInd]
  }
  
  screenDf <- data.frame(analysis_id = screenMatrix[,"analysis_id"], n_matches = tempTab)
  
  return(screenDf)
}