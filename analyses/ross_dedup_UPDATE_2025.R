
resultsPath = "C:/Users/deviv/R-working-folder/OBS_systematic_map/data/raw-data"


load(file.path(resultsPath, "deduplication-files","columns-for-deduplication_UPDATE_13-05-2025.RData"))

years = sort(unique(dedupInfo$'year'))

results = parallel::mclapply(1:length(years), function(i) {
  
  dedupInfo_sub = dedupInfo[dedupInfo$year == years[i], ]
  
  unique_title_id = revtools::find_duplicates(dedupInfo_sub,
                            match_variable = "title", 
                            match_function = "stringdist", 
                            method = "osa", threshold = 5,
                            remove_punctuation = TRUE, to_lower = TRUE)
  
  dedupInfo_sub$duplicate_id <- unique_title_id
  return(dedupInfo_sub)
}, mc.cores = 1)


dedupInfo = do.call(rbind.data.frame, results)

save(dedupInfo, file=file.path(resultsPath,"deduplication-files", "title_dedup_ids_UPDATE_13-05-2025.RData"))
