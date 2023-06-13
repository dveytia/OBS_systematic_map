#' simplifyCodebook
#' @description
#' A function to take a dataframe with columns from output of formatCoding2distilBert.R and simplify some of the variables
#' 
#' @param codebook a data frame with columns indicating a binary 0 1 for each value of each variable
#' 
#' @param idCols The names of the identification columns (never simplified)


simplifyCodebook2 <- function(codebook, idCols){
  
  # seperate out idColumns from the data columns
  idMat <- codebook[,idCols]
  codebook <- codebook[,which(!(colnames(codebook) %in% idCols))]
  
  
  
  ## simplify data columns
  
  # remove unclear categories
  unclearIds <- grep("unclear", colnames(codebook), ignore.case = T)
  codebook <- codebook[,-unclearIds]
  
  
  # oro_development_stage: combine pilot demo into implemented
  colNames <-  c("oro_development_stage.Pilot_demo", "oro_development_stage.Implemented_continued_assessment")
  temp <- rowSums(codebook[,colNames], na.rm=T)
  codebook[,"oro_development_stage.Implemented_continued_assessment"] <- temp; rm(temp)
  codebook[,"oro_development_stage.Pilot_demo"] <- NULL
  
  # climate_threat: combine categories < 25 obsinto one
  colNames <-  c("climate_threat.Sea_ice_changes", "climate_threat.Deoxygenation",
                 "climate_threat.Natural_impacts","climate_threat.Other")
  temp <- rowSums(codebook[,colNames], na.rm=T)
  temp[temp > 1] <- 1
  codebook[,"climate_threat.Other"] <- temp; rm(temp)
  codebook[,colNames[1:3]] <- NULL
  
  # data_type: combine all secondary data types
  colNames <-  c("data_type.Secondary__evidence_synthesis", "data_type.Secondary__other")
  temp <- rowSums(codebook[,colNames], na.rm=T)
  temp[temp > 1] <- 1
  codebook[,"data_type.Secondary__other"] <- temp; rm(temp)
  codebook[,"data_type.Secondary__evidence_synthesis"] <- NULL
  colnames(codebook)[which(colnames(codebook) == "data_type.Secondary__other")] <- "data_type.Secondary"
  
  # remove method_type = "Other" category because there is only 1 entry
  codebook[,"method_type.Other"] <- NULL
  
  # combine estuary into coastal ocean
  colNames <-  c("marine_system.estuary", "marine_system.coastal ocean")
  temp <- rowSums(codebook[,colNames], na.rm=T)
  temp[temp > 1] <- 1
  codebook[,"marine_system.coastal ocean"] <- temp; rm(temp)
  codebook[,"marine_system.estuary"] <- NULL
  
  # ecosystem_type: combine blue carbon other and other
  colNames <-  c("ecosystem_type.Blue_carbon__other", "ecosystem_type.Other")
  temp <- rowSums(codebook[,colNames], na.rm=T)
  temp[temp > 1] <- 1
  codebook[,"ecosystem_type.Other"] <- temp; rm(temp)
  codebook[,"ecosystem_type.Blue_carbon__other"] <- NULL
  
  # time_period: into whether it's forecast or not
  colNames <- grep("time_period", colnames(codebook), ignore.case = T)
  colNames <- colNames[which(!(colnames(codebook)[colNames] %in% "time_period.Forecast"))]
  codebook <- codebook[,-colNames]
  colnames(codebook)[grep("Forecast", colnames(codebook), ignore.case = T)] <- "Forecast"
  
  
  # simplify data_spatial_scale by combining micro and small
  colNames <-  c("data_scale_spatial.Micro", "data_scale_spatial.Small")
  temp <- rowSums(codebook[,colNames], na.rm=T)
  temp[temp > 1] <- 1
  codebook[,"data_scale_spatial.Small"] <- temp; rm(temp)
  codebook[,"data_scale_spatial.Micro"] <- NULL
  

  
  ## output results
  codebook <- cbind(idMat, codebook)
  return(codebook)
  
} # end function