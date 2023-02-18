README
column descriptions

sysrev_id : a unique numeric identifier for each article produced in sysrev (the screening software)

title : the title of the article

year : the year of publication

doi : the doi of the publication

source_title : the name of the source the article was published in (most frequenty the journal name)

author : the authors of the publication

screener : the name of the screener of the publication. If >1 screener, seperated by "|||"

include_screen : a boolean variable indicating whether to include (TRUE) or exclude (FALSE) 

multiple_oro_screen : a boolean variable indicating whether multiple OROs were detected in the abstract (TRUE)

outcome_section : a boolean variable indicating whether the relevant outcome was only a section of the article (TRUE)

blue_carbon_flux_storage : a boolean variable indicating whether the article only reported natural carbon fluxes/storage but no
specific intervention

notes : any notes from the screener

sample_screen : how the publication was sampled to be included in the screening 
	random : from each substring, an equal proportion of articles was randomly sampled
	relevance sort : the model predictions for relevance in sysrev were used to preferentially screen for relevant articles
	test list : the article was a part of the test list (94 articles)

reason_excluded_screen : a categorical variable identifying the PICO reason for exclusion


Double_Blind : a boolean variable indicating whether the article was screened double blind (TRUE) or assessed by a single screener (FALSE)

type: the type of publication (e.g. article, book)

...

abstract : the abstract 

publisher : the publisher of the publication

address : the address of the publisher

affiliation : the affiliation of the first author

issn, eissn : other publication identifiers

keywords : author tagged keywords

keywords_plus : web of science keywords_plus

research_areas, web_of_science_categories : web of science tagged metadata corresponding to field of research

...

unique_id : the unique ID from the WOS database

...





