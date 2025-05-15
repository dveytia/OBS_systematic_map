library(RSQLite)
library(DBI)
library(dplyr)
library(dplyr)


# open connection to database
# Get database name
resultsPath <- here::here("data","raw-data")
sqlite_all_ref_file <- file.path(resultsPath,"sql-databases","all-refs_2025.sqlite")
v = "v1"
sqlite_all_ref_file_version <- paste0(gsub(".sqlite","",sqlite_all_ref_file),"_",v,".sqlite")
sqlite_unique_ref_table_name <- "uniquerefs_2025"

# Open connection
dbcon <- DBI::dbConnect(RSQLite::SQLite(), sqlite_all_ref_file_version, create=FALSE)

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
  
}, mc.cores=1)



# write unique results to sqlite database
# rbind results together into a data frame
unique_refs = do.call(rbind.data.frame, results)

# write
dbcon <- DBI::dbConnect(RSQLite::SQLite(), sqlite_all_ref_file_version, create=FALSE)
DBI::dbWriteTable(dbcon, paste0(sqlite_unique_ref_table_name,"_1"), unique_refs, append=FALSE, overwrite = TRUE)
DBI::dbDisconnect(dbcon)



print("done loop")

# # # check
# dbcon <- DBI::dbConnect(RSQLite::SQLite(), sqlite_all_ref_file_version, create=FALSE)
# test <- tbl(db, "uniquerefs")
# test %>% summarise(n=n()) %>% print
# length(unique_ids) %>% print
# DBI::dbDisconnect(dbcon)
