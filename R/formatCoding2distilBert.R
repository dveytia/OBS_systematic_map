#' formatCoding2distilBert
#' @description
#' Function for formatting coding spreadsheet into long format with binary response variables
#' 
#' @param codebookFp The file path of the codebook to be formatted (.csv file)
#' 
#' @param skipLines the number of lines to skip when reading in the file
#' 
#' @param returnVariableString logical -- whether or not to also return a string of just the variable names
#' 
#' @param exclusions TRUE or FALSE of whether to include exclusions

formatCoding2distilBert <- function(codebookFp, skipLines=3,
                                    returnVariableString = FALSE, exclusions=FALSE){
  
  
  
  ## Read in codebook file and get names of variables and variable values
  
  # read in but skip extra header rows
  temp <- readr::read_csv(codebookFp,
                          trim_ws = TRUE, skip = skipLines, skip_empty_rows = TRUE,
                          show_col_types = FALSE)
  
  # Column names
  colnames(temp) <- tolower(colnames(temp))
  colnames(temp) <- gsub(" ","_", colnames(temp))
  colnames(temp) <- gsub("[.]","_", colnames(temp))
  
  # sometimes empty rows are missed when reading in because some columns are autofill
  # remove rows where all other variables are NA
  colInd <- which(!(colnames(temp) %in% c("include_code","row_id"))) # don't count columns which autofill
  temp <- subset(temp, !rowSums(is.na(temp[,colInd])) == length(colInd)) # subset out rows where all values are NA
  
  # change french formatting if needed
  temp[temp == "VRAI"] <- "TRUE"
  temp[temp == "FAUX"] <- "FALSE"
  
  # all the possible values for each variable to search for
  # for each entry, if accidentally duplicated value lists, remove after "|"
  values <- gsub("\\|.*","",temp[1,])
  
  # each variable will be stored in a list -- create empty list
  responseList <- vector("list", length(values))
  names(responseList) <- colnames(temp)
  
  # remove the row that contains all the possible variable values
  temp <- temp[-1,]
  
  # filter out rows where include_code=FALSE if exclusions = FALSE
  if(exclusions == FALSE){
    temp <- subset(temp, include_code == "TRUE")
  }
  
  ## Loop through all the variables and produce indices of presence/absence for each value
  
  for(j in 1:length(values)){
    
    print(paste("j = ", j))
    
    if(grepl("pre-filled",values[j], ignore.case = TRUE)){
      tempMat <- as.data.frame(temp[,j])
      colnames(tempMat) <- colnames(temp)[j]
      tempMat[,1] <- as.character(tempMat[,1])
    }  
    
    if(grepl("text",values[j], ignore.case = TRUE)){
      tempMat <- as.data.frame(temp[,j])
      colnames(tempMat) <- colnames(temp)[j]
      tempMat[,1] <- as.character(tempMat[,1])
    }
    
    
    if(grepl("integer", values[j], ignore.case = TRUE)){
      tempMat <- as.data.frame(temp[,j])
      colnames(tempMat) <- colnames(temp)[j]
      tempMat[,1] <- as.factor(tempMat[,1])
    }
    
    
    if(grepl("automatic fill", values[j], ignore.case = TRUE)){
      tempMat <- as.data.frame(temp[,j])
      colnames(tempMat) <- colnames(temp)[j]
      tempMat[,1] <- as.character(tempMat[,1])
    }
    
    if(grepl("alpha-numeric", values[j], ignore.case = TRUE)){
      tempMat <- as.data.frame(temp[,j])
      colnames(tempMat) <- colnames(temp)[j]
      tempMat[,1] <- as.character(tempMat[,1])
    }
    
    
    if(grepl("boolean", values[j], ignore.case = TRUE)){
      tempMat <- as.data.frame(temp[,j])
      tempMat <- ifelse(tempMat == "TRUE", 1, 0)
      tempMat[is.na(tempMat)] <- 0
      colnames(tempMat) <- colnames(temp)[j]
    }
    
    
    if(grepl("Select", values[j], ignore.case = TRUE)){
      
      if(grepl("Select One", values[j], ignore.case = TRUE)){
        vals <- unlist(strsplit(gsub("Select One: ","", values[j]),","))
      }else{
        vals <- unlist(strsplit(gsub("Select Multiple: ","", values[j]),","))
      }
      
      # remove leading/trailing spaces
      vals<-trimws(vals)
      
      # remove NA
      if("NA" %in% vals){
        vals <- vals[-which(vals == "NA")]
      }
      
      valLabels <- gsub(" ","_",gsub("[[:punct:]]","", vals))
      
      # for some reason grep has trouble with some punctuation so simplify to search for implemented
      if(length(grep("implemented",vals, ignore.case = TRUE))>0){
        vals[grep("implemented",vals, ignore.case = TRUE)] <- "Implemented"
      }
      if(length(grep("tourism",vals, ignore.case = TRUE))>0){
        vals[grep("tourism",vals, ignore.case = TRUE)] <- "tourism"
      }
      if(length(grep("mining",vals, ignore.case = TRUE))>0){
        vals[grep("mining",vals, ignore.case = TRUE)] <- "mining"
      }
      if(length(grep("macroalgae",vals, ignore.case = TRUE))>0){
        vals[grep("macroalgae",vals, ignore.case = TRUE)] <- "macroalgae"
      }
      if(length(grep("science",vals, ignore.case = TRUE))>0){
        vals[grep("science",vals, ignore.case = TRUE)] <- "science"
      }
      
      if(colnames(temp)[j] == c("time_period")){
        vals <- c("Before pre-ind","Pre-industrial", "Modern","Forecast")
        valLabels <- vals # have to re-assign this because otherwise the column order won't match
      }
      if(colnames(temp)[j] == c("marine_system")){
        vals <- c("estuary","land","coastal ocean","open-ocean","deep-ocean","unclear")
        valLabels <- vals
      }
      
      # make empty matrix to fill
      tempMat <- matrix(nrow = nrow(temp), ncol = length(vals))
      colnames(tempMat) <- valLabels
      
      # fill matrix with matches
      for(v in 1:length(vals)){
        tempMat[,v] <- grepl(vals[v], as.matrix(temp[,j]), ignore.case = TRUE)
      }
      tempMat <- as.data.frame(tempMat)
      
    } # end of formatting Select variable types
    
    
    ## Final check to make sure all TRUE/FALSE is 0,1
    tempMat[tempMat==TRUE] <- 1
    tempMat[tempMat==FALSE] <- 0
    
    ## after all the processing, save
    responseList[[j]] <- tempMat
  
  } # end of loop through j
  
  
  # cbind all dataframes together
  out <- do.call(cbind.data.frame, responseList)
  out$row_id <- ifelse(out$row_id == "FALSE", NA,out$row_id)
  
  # because I removed exclusions can get rid of redundant columns
  out <- subset(out, select = -c(include_code,exclude_note,oro_id,row_id))
  
  # prepare opject to return
  if(returnVariableString){
    variables <- colnames(temp)
    return(list(data=out, variables=variables))
  }else{
    return(list(data=out))
  }
  
} # end of function