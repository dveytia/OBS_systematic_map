#' Systematic map on ocean-related options
#' 
#' @description 
#' This project documents the steps for searching the literature and evaluating the search string, 
#' training the review team in screening and coding and managing the references from
#' the search results and the screening and coding results
#' 
#' @author Devi Veytia \email{devi.veytia@fondationbiodiversite.fr}
#' 
#' @date 2023/01/24


## Install dependencies

if (!("remotes" %in% installed.packages()[ , "Package"]))
  install.packages("remotes")

remotes::install_deps(upgrade = "never")
# renv::restore()



## Load Project Addins (R Functions and Packages) ----

pkgload::load_all(here::here())


## Run Project ----------

source(knitr::purl(here::here("analyses","01-compile_test_list.Rmd"), output=tempfile()))
source(knitr::purl(here::here("analyses","02-evaluate_comprehensiveness.Rmd"), output=tempfile()))
source(knitr::purl(here::here("analyses","03-review_team_screening_training.Rmd"), output=tempfile()))


