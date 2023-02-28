
## Load data
load("include_text.RData")
nlp_search_terms <- read.csv("keyword-search-tokens.csv")


## source functions
functionsToSource <- c("clean_string.R", "screen.R","tokenization.R","utils.R")
for(i in 1:length(functionsToSource)){
  source(functionsToSource[i])
}



## Process words to search

nlp_search_terms <- na.omit(nlp_search_terms,nlp_search_terms) # remove empty spaces
nlp_search_terms$Term <- textstem::lemmatize_strings(nlp_search_terms$Term) # lemmitize
nlp_search_terms$Term <- clean_string(nlp_search_terms$Term) # remove punctuation and extra spaces
nlp_search_terms <- nlp_search_terms[!duplicated(nlp_search_terms$Term),] # remove any resulting duplicates
nlp_search_terms$Term <- uk2us::convert_uk2us(nlp_search_terms$Term) # transform everything to American spelling

# separate out into single terms and expressions
single_words <- nlp_search_terms$Term[which(nlp_search_terms[,4] == "single")]
expressions <- nlp_search_terms$Term[which(nlp_search_terms[,4] == "expression")]
# name them by their corresponding group
names(single_words) <- nlp_search_terms$`Group name`[which(nlp_search_terms[,4] == "single")]
names(expressions) <- nlp_search_terms$`Group name`[which(nlp_search_terms[,4] == "expression")]

# indices for compound terms
# carbon and removal
carb_ind <- which(nlp_search_terms[,2] == "carbon")
rem_ind <- which(nlp_search_terms[,2] == "removal")
# coasts and communities
coast_ind <- which(nlp_search_terms[,2] == "coasts")
comm_ind <- which(nlp_search_terms[,2] == "communities")
# safe space and fish
rest_ind <- which(nlp_search_terms$Group == "Paper4")
rest_ind <- rest_ind[which(!(rest_ind %in% c(coast_ind, comm_ind)))]



## Screen text

results = parallel::mclapply(1:nrow(my_text), function(i){
  
  
  ## Extract keywords
  # process text
  # group title, abstract together
  text = paste(my_text$title[i], my_text$abstract[i])
  
  # Lemmitization and clean string to remove extra spaces, numbers and punctuation
  text <- clean_string(text)
  text <- textstem::lemmatize_strings(text)
  
  # convert spelling to american
  text <- uk2us::convert_uk2us(text)
  
  # screen for keywords
  screens_swd <- screen(text, single_words)
  screens_expr <- screen(text, expressions)
  
  # order dataset according to spreadsheet
  screens_all <- cbind(screens_expr, screens_swd)
  screens_all <- screens_all[,match(gsub(" ","_", nlp_search_terms$Term), colnames(screens_all))]
  
  #rownames(screens_all) <- my_text$analysis_id[i]
  
  # make sure everything is just a y/n response, to presence/absence
  screens_all <- ifelse(1 <= screens_all, 1, 0)
  
  
  ## Compound terms
  # some concepts require co-occurrences between two terms (i.e. AND statements)
  # so make a new matrix with these values
  complexTerms <- matrix(nrow=1,
                         ncol = 2,
                         dimnames = list(
                           my_text$analysis_id[i],
                           c("carbon removal or storage","coastal communities and safe space or fish")
                         ))
  
  
  # for carbon AND removal
  # a y/n response for whether any of the synonymns are found
  carb <- screens_all[,carb_ind]
  rem <- ifelse(sum(1 <= screens_all[,rem_ind]), 1,0) 
  # whether there are words matching both of the keyword groups for carbon AND removal
  complexTerms[,1] <- ifelse(carb == 1 & rem == 1, 1, 0)
  
  
  
  # for coasts and communities
  coast <- ifelse(sum(1 <= screens_all[,coast_ind]), 1,0)
  comm <- ifelse(sum(1 <= screens_all[,comm_ind]), 1,0)
  coastAndComm <- ifelse(coast == 1 & comm == 1, 1, 0)
  
  # for safe space or fish -- i.e. other Group names that aren't coasts or communities
  rest <- ifelse(sum(1 <= screens_all[,rest_ind]), 1, 0)
  
  # for coasts and communities and safe space or fish
  complexTerms[,2] <- ifelse(coastAndComm ==1 & rest == 1, 1,0)
  
  
  
  ## Bind both together
  screens_all <- cbind(data.frame(analysis_id = my_text$analysis_id[i]),screens_all, as.data.frame(complexTerms))
  
  # return the dataframe
  return(screens_all)
  
}, mc.cores = 20)


## Bind results together
screens <- do.call(rbind.data.frame, results)



## write
db <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = "keyword-matches.sqlite", create=TRUE)
DBI::dbWriteTable(db, "keywordMatches", screens, append=FALSE, overwrite = TRUE)
DBI::dbDisconnect(db)




## TABULATE TOTALS
# Add these new concepts in to the tabulated matrix and convert to data frame
screens$analysis_id <- NULL
ind <- which(!(colnames(screens) %in% nlp_search_terms$Term[c(carb_ind, rem_ind, coast_ind, comm_ind, rest_ind)]))
tab_screens <- colSums(screens[,ind])
tab_screens <- as.data.frame(tab_screens)
colnames(tab_screens) <- c("n_matches")
# new vector of which paper each column belongs to
tab_screens$Topic <- c(nlp_search_terms[rownames(tab_screens) %in% nlp_search_terms$Term,1], "Paper1","Paper4")
tab_screens$Keyword <- rownames(tab_screens)
tab_screens$Keyword_group <- c(
  nlp_search_terms[rownames(tab_screens) %in% nlp_search_terms$Term,2], 
  c("carbon removal or storage","coastal communities and safe space or fish")
)
rownames(tab_screens) <- NULL

save(tab_screens, file = "keyword-matches-tabulated.RData")



