readPredictions <- function(
    predDir = here::here("data/raw-data/coding-predictions"),
    variable = "",
    boundaries = c("upper","mean","lower")
    ){
  
  ## READ IN ORO ANY PREDICTIONS AND FORMAT
  # Note these predictions were made using the upper confidence limit for ORO_branch
  Files <- dir(predDir)
  Files <- Files[grep(variable, Files)]
  predList <- list()
  for(i in 1:length(Files)){
    temp <- readr::read_csv(
      here::here(predDir,Files[i]),
      show_col_types = FALSE
    )
    colNames <- colnames(temp)
    colNames[1] <- "analysis_id"
    colNames <- gsub(paste0(variable, "."),"",colNames)
    colNames <- gsub(" - ","_", colNames)
    colNames <- gsub("_prediction","", colNames)
    colNames <- gsub("_pred","", colNames)
    colnames(temp) <- colNames

    predList[[i]] <- temp
  }
  ## Make a dataframe of the predictions
  for(i in 1:length(predList)){
    temp <- predList[[i]]
    for(b in 1:length(boundaries)){
      temp_b <- cbind(temp[,1], temp[grep(boundaries[b], colnames(temp))])
      colnames(temp_b) <- gsub(paste0("_",boundaries[b]),"", colnames(temp_b))
      temp_b <- reshape2::melt(temp_b, variable.name = variable, value.name = boundaries[b], 
                               id.vars = c("analysis_id"))
      if(b==1){
        temp_melt <- temp_b
      }else{
        temp_melt <- merge(temp_melt, temp_b, by=c("analysis_id",variable))
      }
    }
    if(i==1)
      predAny <- temp_melt
    else
      predAny <- rbind(predAny, temp_melt)
  }
  rm(predList)
  
  
  return(predAny)
  
}
