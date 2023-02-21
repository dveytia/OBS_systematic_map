#' Combine expressions in single word
#'
#' @description 
#' Combines expressions in single word. For instance, `climate change` will
#' become `climatechange`.
#' 
#' @param x a vector of characters. Typically a vector of titles/abstracts in 
#'   which expressions listed in `token` must be replaced.
#' 
#' @param token a vector of characters. Words expression to convert in single 
#'   words
#' 
#' @export
#' 
#' @examples 
#' x <- "funrar: An R package to characterize functional rarity"
#' tokenization(x, token = c("R package", "functional rarity"))

tokenization <- function(x, token) {
  
  ## Check arguments ----
  
  is_character(x)
  is_character(token)
  
  
  ## Tokenization ----
  
  for (i in 1:length(token)) {
    x <- gsub(token[i], gsub("\\s", "", token[i]), x, ignore.case = TRUE)  
  }
 
  x   
}
