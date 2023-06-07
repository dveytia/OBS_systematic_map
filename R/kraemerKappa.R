#' kraemerKappa
#' @description
#' An implementation of the kappa statistic for multiple choice selections amongst multiple reviewers, 
#' as presented in Kraemer (1980). 
#' @param ratings A matrix containing categorical ratings of subjects by raters. 
#' The matrix should have dimensions of either number of subjects (ns) x number of categories (nc) x 
#' number of raters (nr) if a multiple choice, or ns*nr if single choice  

kraemerKappa <- function (ratings){
  
  # number of subjects, categories, and raters
  ns <- dim(ratings)[1]
  if(length(dim(ratings))==3){
    nc <- dim(ratings)[2]
    lev <- dimnames(ratings)[[2]]
  }else{
    ratings <- as.matrix(ratings)
    ratings[] <- as.character(ratings)
    lev <- levels(as.factor(ratings))
    nc <- length(lev)
    nr <- dim(ratings)[2]
  }
  
  # create object to hold calculated variables
  Rij <- matrix(nrow=ns, ncol = nc, 
                dimnames = list(subject=NULL, category = lev))
  Ti <- vector("numeric", ns)
  Tj <- vector("numeric", ns)
  mi <- vector("numeric", ns)
  Wi <- vector("numeric", ns)
  ri <- vector("numeric", ns)
  Si <- vector("numeric", ns)
  
  
  # loop through each subject (i)
  
  for (i in 1:ns) {
    
    # for each reviewer, calculate the rankings of the different categories
    # keeping NA values as NA (not assigning a rank)
    if(length(dim(ratings)) == 2){
      ratings_i <- matrix(nrow = nc, ncol = nr,
                          dimnames = list(categories = lev, rater = NULL))
      ratings_i[] <- "No"
      
      for(r in 1:nr){
        ind <- which(rownames(ratings_i) == ratings[i,r])
        ratings_i[ind, r] <- ratings[i,r]
      }
      ranks_i <- apply(ratings_i, 2, rank, na.last = "keep")
    }else{
      ratings_i <- ratings[i,,]
      ranks_i <- apply(ratings_i, 2, rank, na.last = "keep")
    }
    
    # get the number of raters
    nr <- dim(ranks_i)[2]
    
    
    # Ti is the tie correction for the mi observation of subject i
    t <- vector("numeric", nr)
    for (r in 1:nr) {
      rater <- table(ranks_i[, r])
      ties <- rater[rater > 1]
      l <- as.numeric(ties)
      t[r] <- sum(l^3 - l, na.rm=T)
    }
    Ti[i] <- mean(t, na.rm=T)
    
    # calculate the average rank for each category over all the raters for that subject
    Rij[i,] <- apply(ranks_i, 1, mean, na.rm=T)
    
    # the number of observations per subject
    mi[i] <- sum(apply(ranks_i, 2, function(x) length(x)))
    
    # sample variance of Rij
    Si <- var(Rij[i,])

    
    # but when all answers are the same there is no variance, so numerator -> 0
    # to solve this, if the variance is zero, set to very small
    if(Si ==0){
      Si <- 1e-5
    }
    
    # coefficient of concordance for the mi rankings of subject i
    Wi[i] <- 12*(nc-1)*Si/((nc^3)-nc-Ti[i]) 
    
    # if an infinite value returned, set to the max of 1
    if(is.infinite(Wi[i])){
      Wi[i] <- 1
    }
    

    # average spearman rank correlation coefficient
    ri[i] <- ((mi[i]*Wi[i])-1)/(mi[i]-1)
    
    
  } # end of looping through all the subjects
  
  
  
  ## now calculate overall metrics

  # Overall average rank of category j
  Rj <- apply(Rij, 2, mean)
  # Weighted average of the Ti
  weightedTi <- Ti
  for(i in 1:length(Ti)){
    weightedTi[i] <- mi[i]*Ti[i]/sum(mi) 
  }
  weightedTi <- sum(weightedTi)
  # Sample variane of Rj
  St <- var(Rj)
  # overall coefficient of concordance
  WT <- 12*(nc-1)*St/((nc^3)-nc-weightedTi)
  # overall rT
  rT <- ((sum(mi)*WT)-1)/(sum(mi)-1)
  # K0
  k0 <- (mean(ri, na.rm=T)-rT)/(1-rT)
  
  
  ## P value? 
  Xvalue <- nr * (ns - 1) * WT
  df1 <- ns - 1
  p.value <- pchisq(Xvalue, df1, lower.tail = FALSE)
  
  return(list(k0 = k0, pValue = p.value))
  
}
