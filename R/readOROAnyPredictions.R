readOROAnyPredictions <- function(
    predDir = here::here("data/raw-data/coding-predictions"),
    boundaries = c("upper","mean","lower")
    ){
  
  ## READ IN ORO ANY PREDICTIONS AND FORMAT
  # Note these predictions were made using the upper confidence limit for ORO_branch
  oroAnyFiles <- dir(predDir)
  oroAnyFiles <- oroAnyFiles[grep("oro_any", oroAnyFiles)]
  predOROAnyList <- list()
  for(i in 1:length(oroAnyFiles)){
    temp <- readr::read_csv(
      here::here("data/raw-data/coding-predictions",oroAnyFiles[i]),
      show_col_types = FALSE
    )
    colNames <- colnames(temp)
    colNames[1] <- "analysis_id"
    colNames <- gsub("oro_any.","",colNames)
    colNames <- gsub("M_","", colNames)
    colNames <- gsub("N_","", colNames)
    colNames <- gsub("SA_","", colNames)
    colNames <- gsub(" - ","_", colNames)
    colNames <- gsub("_prediction","", colNames)
    colNames <- gsub("_pred","", colNames)
    colnames(temp) <- colNames
    temp$oro_branch <- gsub("oro_any_","",oroAnyFiles[i])
    temp$oro_branch <- gsub("_predictions.csv","",temp$oro_branch)
    predOROAnyList[[i]] <- temp
  }
  ## Make a dataframe of the predictions
  for(i in 1:length(predOROAnyList)){
    temp <- predOROAnyList[[i]]
    for(b in 1:length(boundaries)){
      temp_b <- cbind(temp[,1], temp$oro_branch, temp[grep(boundaries[b], colnames(temp))])
      colnames(temp_b)[2]<- "oro_branch"
      colnames(temp_b) <- gsub(paste0("_",boundaries[b]),"", colnames(temp_b))
      temp_b <- reshape2::melt(temp_b, variable.name = "oro_any", value.name = boundaries[b], 
                               id.vars = c("analysis_id","oro_branch"))
      if(b==1){
        temp_melt <- temp_b
      }else{
        temp_melt <- merge(temp_melt, temp_b, by=c("analysis_id","oro_branch","oro_any"))
      }
    }
    if(i==1)
      predOROAny <- temp_melt
    else
      predOROAny <- rbind(predOROAny, temp_melt)
  }
  rm(predOROAnyList)
  
  
  return(predOROAny)
  
}
