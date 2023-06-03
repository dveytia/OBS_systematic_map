kendallModified <- function (ranks_i) 
{
      #ranks_i <- as.matrix(na.omit(ranks_i))
    ns <- nrow(ranks_i)
    nr <- ncol(ranks_i)

    Tj <- 0
    for (i in 1:nr) {
      rater <- table(ranks_i[, i])
      ties <- rater[rater > 1]
      l <- as.numeric(ties)
      Tj <- Tj + sum(l^3 - l)
    }
    coeff.name <- "Wt"
    coeff <- (12 * var(apply(ranks_i, 1, sum)) * (ns - 
                                                         1))/(nr^2 * (ns^3 - ns) - nr * Tj)
  
  Xvalue <- nr * (ns - 1) * coeff
  df1 <- ns - 1
  p.value <- pchisq(Xvalue, df1, lower.tail = FALSE)
  rval <- list(method = paste("Kendall's coefficient of concordance", 
                              coeff.name), subjects = ns, raters = nr, irr.name = coeff.name, 
               value = coeff, stat.name = paste("Chisq(", df1, ")", 
                                                sep = ""), statistic = Xvalue, p.value = p.value,
               Tj = Tj)
  if (!correct && TIES) 
    rval <- c(rval, error = "Coefficient may be incorrect due to ties")
  class(rval) <- "irrlist"
  return(rval)
}
