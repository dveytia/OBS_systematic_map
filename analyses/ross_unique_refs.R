library(RSQLite)
library(DBI)
library(dplyr)
library(dplyr)


# open connection to database
dbcon <- DBI::dbConnect(RSQLite::SQLite(), "all-refs_v2_join-duplicate_id.sqlite")

# collect all the records from the databse -- added this
allrefs_join <- DBI::dbGetQuery(dbcon, "SELECT * FROM allrefs_join")

# the number of duplicate ids
unique_ids <- dbGetQuery(dbcon, "SELECT duplicate_id FROM allrefs_join")
unique_ids <- sort(unique(unique_ids[,1]))

# disconnect database
DBI::dbDisconnect(dbcon)

# loop through to extract the most complete reference
results = parallel::mclapply(1:length(unique_ids), function(i){  
  
  #tempDf <-  DBI::dbGetQuery(dbcon, "SELECT * FROM allrefs_join WHERE duplicate_id = ?", params = unique_ids[i])
  tempDf <- subset(allrefs_join, duplicate_id == unique_ids[i])
  
  # if there are duplicates, extract the unique references
  if(nrow(tempDf)>1){
    search_strings <- paste0(unique(tempDf$search_substring), collapse=" | ") # check if only 1 unique works
    databases <- paste0(unique(tempDf$source_database), collapse=" | ")
    tempDf <- revtools::extract_unique_references(tempDf, "duplicate_id")
    tempDf$search_substring <- search_strings
    tempDf$source_database <- databases
    
  }else{ # otherwise add a column saying there were no duplicates
    
    tempDf$n_duplicates <- 0 
  }
  #if(nrow(tempDf) ==0){next}
  
  return(tempDf)
  
}, mc.cores=20)



# write unique results to sqlite database
# rbind results together into a data frame
unique_refs = do.call(rbind.data.frame, results)

# write
db <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = "unique-refs_v2.sqlite", create=TRUE)
DBI::dbWriteTable(db, "uniquerefs", unique_refs, append=FALSE, overwrite = TRUE)
DBI::dbDisconnect(db)



print("done loop")

# # check
db <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = "unique-refs_v2.sqlite")
test <- tbl(db, "uniquerefs")
test %>% summarise(n=n()) %>% print
length(unique_ids) %>% print
DBI::dbDisconnect(db)
