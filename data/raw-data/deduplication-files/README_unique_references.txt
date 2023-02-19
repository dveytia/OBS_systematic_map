Column names explanation

NB. revtools::extract_unique_references was used to decide which record to retain when duplication was identified. In this case,
the most complete record was retained where duplicates were identified. Therefore in this dataset, the duplicates are removed, 
thus each row represents a unique publication.

NB. the .txt file is tab delimited


analysis_id : The unique record identifier for each unit of publication (i.e. when there are duplicates and before unique
references are extracted, analysis_id > duplicate_id).

duplicate_id : publications identified as duplicates by title and year matching will receive the same duplicate_id

n_duplicates : the number of duplciates that were identified for that publication.

source_database : the citation indexed database the record was retreived from. If the record had duplicates identified from 
multiple databases, these will be separated by " | " (e.g. "Web Of Science | Scopus")

search_substring : the search string used to retreive the record. If multiple search strings returned the same record, numerous entries 
will be separated by " | "

title : the text for the title of the publication

source_title : title of the source of publication (e.g. journal, book)

abstract : the abstract of the publication

keywords : author tagged keywords

keywords_other : other keywords tagged by the database



