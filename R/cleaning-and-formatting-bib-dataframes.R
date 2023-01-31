#' Functions for cleaning the wos and scopus records
#' 
#' @description 
#' The WOS and Scopus records are formatted differently with different columns -- 
#' these functions allow the dataframes that are read in from the bib entries to be formatted
#' into a common standard data frame
#' 
#' @author Devi Veytia \email{devi.veytia@fondationbiodiversite.fr}
#' 



## function to calculate scopus eid from url
url2eid <- function(scopusurl){
  # extract the part after eid = 
  temp <- unlist(lapply(strsplit(scopusurl, "eid="), function(x) x[2]))
  # remove the next entry after the eid 
  temp <- unlist(lapply(strsplit(temp, "&"), function(x) x[1]))
  return(temp)
}



## Function to put everything into sentence case
capwords <- function(s, strict = FALSE) {
  cap <- function(s) paste(toupper(substring(s, 1, 1)),
                           {s <- substring(s, 2); if(strict) tolower(s) else s},
                           sep = "", collapse = " " )
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}



## Function to format scopus df to standard column names
scopus2standard <- function(
    # the columns to get the dataframe to conform to
  standardColnames, 
  # the input data frame
  inputDf, 
  search_substring = "General",
  source_database = "Scopus",
  # values to input into standard column names or transformations if not present in input dataframe
  inputvals = data.frame(
    source_database = source_database, 
    search_substring = search_substring,
    database_article_id = url2eid(inputDf$url),
    source_title = inputDf$journal,
    keywords_other = inputDf$author_keywords,
    funding = inputDf$funding_details
  ),
  # Sentence case columns
  sentCaseCol = c("type", "source_title","journal"),
  # lower case columns
  lowerCaseCol = c("keywords","keywords_other")
){
  
  # template dataframe
  tempDf <- matrix(nrow=(nrow(inputDf)), ncol = length(standardColnames))
  colnames(tempDf) <- standardColnames
  tempDf <- as.data.frame(tempDf)
  
  # assign input vals 
  tempDf[,names(inputvals)] <- inputvals
  
  # merge in other columns with identical names
  commonCols <- intersect(colnames(tempDf), colnames(inputDf))
  tempDf[,commonCols] <- inputDf[,commonCols]
  
  # to ensure uniformity, convert to case
  if(length(sentCaseCol) > 0){
    for(i in 1:length(sentCaseCol)){
      tempDf[,sentCaseCol[i]] <- capwords(tempDf[,sentCaseCol[i]], strict=TRUE)
    }
  }
  if(length(lowerCaseCol)>0){
    for(i in 1:length(lowerCaseCol)){
      tempDf[,lowerCaseCol[i]] <- tolower(tempDf[,lowerCaseCol[i]])
    }
  }
  
  return(tempDf)
}


# ## Check
# cleanScopusDf <- scopus2standard(standardColnames=standardColnames, inputDf=scopusFormat, search_substring = "General")
# View(cleanScopusDf)


wos2standard <- function(
    # the columns to get the dataframe to conform to
  standardColnames, 
  # the input data frame
  inputDf,
  search_substring = "General",
  source_database = "Web Of Science",
  # values to input into standard column names or transformations if not present in input dataframe
  inputvals = data.frame(
    source_database = source_database, 
    search_substring = search_substring,
    database_article_id = inputDf$unique_id,
    source_title = ifelse(is.na(inputDf$journal), inputDf$booktitle, inputDf$journal),
    keywords_other = inputDf$keywords_plus,
    funding = inputDf$funding_acknowledgement
  ),
  # Sentence case columns
  sentCaseCol = c("type", "source_title","journal"),
  # lower case columns
  lowerCaseCol = c("keywords","keywords_other")
){
  
  # template dataframe
  tempDf <- matrix(nrow=(nrow(inputDf)), ncol = length(standardColnames))
  colnames(tempDf) <- standardColnames
  tempDf <- as.data.frame(tempDf)
  
  # assign input vals 
  tempDf[,names(inputvals)] <- inputvals
  
  # merge in other columns with identical names
  commonCols <- intersect(colnames(tempDf), colnames(inputDf))
  tempDf[,commonCols] <- inputDf[,commonCols]
  
  # to ensure uniformity, convert to case
  if(length(sentCaseCol) > 0){
    for(i in 1:length(sentCaseCol)){
      tempDf[,sentCaseCol[i]] <- capwords(tempDf[,sentCaseCol[i]], strict=TRUE)
    }
  }
  if(length(lowerCaseCol)>0){
    for(i in 1:length(lowerCaseCol)){
      tempDf[,lowerCaseCol[i]] <- tolower(tempDf[,lowerCaseCol[i]])
    }
  }
  
  return(tempDf)
}

# ## check
# cleanWosDf <- wos2standard(standardColnames=standardColnames, inputDf=wosFormat, search_substring = "General")
# View(cleanWosDf)
