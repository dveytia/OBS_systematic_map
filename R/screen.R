#' Screen references title/abstract
#'
#' @description 
#' Screens references title/abstract. Detect patterns (argument `pattern`) in a
#' vector of strings (argument `x`).
#' 
#' @param x a vector of characters. Typically a vector of titles/abstracts in 
#'   which `pattern` must be detected.
#' 
#' @param pattern a vector of characters. Patterns to detect in `x`.
#' 
#' @export

screen <- function(x, pattern) {
  
  ## Check arguments ----
  
  is_character(x)
  is_character(pattern)
  
  headers <- gsub("\\s", "_", pattern)
  
  x       <- tokenization(x, pattern)
  pattern <- tokenization(pattern, pattern)
  
  
  ## Detect pattern ----
  
  tokens <- strsplit(x, " ")

  detections <- as.data.frame(matrix(NA, 
                                     nrow = length(x), 
                                     ncol = length(pattern)))
  
  for (i in 1:length(pattern)) {
    detections[ , i] <- unlist(lapply(tokens, function(token) {
      ifelse(sum(grepl(pattern[i], token)) > 0, 1, 0)
    }))
  }

  detections <- as.data.frame(detections)
  colnames(detections) <- headers
  
  detections
}
