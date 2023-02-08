load("columns-for-deduplication.RData")

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
}, mc.cores = 20)


dedupInfo = do.call(rbind.data.frame, dedupInfo)

save(dedupInfo, file="title_dedup_ids.RData")
