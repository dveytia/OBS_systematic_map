library(RSQLite)
library(DBI)
library(dplyr)


# open connection to database
dbcon <- DBI::dbConnect(RSQLite::SQLite(), "all-refs_v2_join-duplicate_id.sqlite")

# the number of duplicate ids
unique_ids <- dbGetQuery(dbcon, "SELECT duplicate_id FROM allrefs_join")
unique_ids <- sort(unique(unique_ids[,1]))

# loop through to extract the most complete reference
results = parallel::mclapply(1:length(unique_ids), function(i){  
  
  tempDf <-  DBI::dbGetQuery(dbcon, "SELECT * FROM allrefs_join WHERE duplicate_id = ?", params = unique_ids[i])
  
  # if there are duplicates, extract the unique references
  if(nrow(tempDf)>1){
    search_strings <- paste0(unique(tempDf$search_substring), collapse=" | ") # check if only 1 unique works
    databases <- paste0(unique(tempDf$source_database), collapse=" | ")
    tempDf <- revtools::extract_unique_references(tempDf, "duplicate_id")
    tempDf$search_substring <- search_strings
    tempDf$source_database <- databases
  }
  if(nrow(tempDf) ==0){next}
  
  return(tempDf)
  
}, mc.cores=1)

# disconnect database
DBI::dbDisconnect(dbcon)

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
test %>% summarise(n=n())
DBI::dbDisconnect(db)
