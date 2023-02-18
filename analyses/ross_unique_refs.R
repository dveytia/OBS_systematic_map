# open connection to database
dbcon <- dbConnect(RSQLite::SQLite(), "all-refs_v2.sqlite")

# the number of duplicate ids
unique_ids <- dbGetQuery(dbcon, "SELECT duplicate_id FROM refs")
unique_ids <- sort(unique(unique_ids))

# loop through to extract the most complete reference
results = parallel::mclapply(1:length(unique_ids), function(i){
  
  tempDf <- dbGetQuery(dbcon, "SELECT * FROM refs WHERE duplicate_id = ?", params = unique_ids[i])
  
  # if there are duplicates, extract the unique references
  if(nrow(temp)>1){
    search_strings <- paste0(unique(tempDf$search_substring), collapse=" | ") # check if only 1 unique works
    databases <- paste0(unique(tempDf$source_database), collapse=" | ")
    tempDf <- revtools::extract_unique_references(tempDf, "duplicate_id")
    tempDf$search_substring <- search_strings
    tempDf$source_database <- databases
  }
  if(nrow(temp) ==0){next}
  
  # If the first loop, initialize the database, otherwise append
  if(i==1){
    overwritel = TRUE; appendl = FALSE
  }else{
    overwritel = FALSE; appendl = TRUE
  }
  
  # # write to database
  db <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = "deduplicated-refs.sqlite")
  dbWriteTable(db, "dedupRefs", tempDf, append=appendl, overwrite = overwritel)
  dbDisconnect(db)
  
}, mc.cores=20)

print("done loop")