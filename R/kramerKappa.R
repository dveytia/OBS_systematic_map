kramerKappa <- function (ratings){
  
  # number of subjects, categories, and raters
  ns <- dim(ratings)[1]
  nc <- dim(ratings)[2]
  
  # create object to hold calculated variables
  Rij <- matrix(nrow=ns, ncol = nc, 
                dimnames = list(subject=NULL, category = dimnames(ratings)[[2]]))
  Ti <- vector("numeric", ns)
  Tj <- vector("numeric", ns)
  mi <- vector("numeric", ns)
  Wi <- vector("numeric", ns)
  ri <- vector("numeric", ns)
  Si <- vector("numeric", ns)
  
  for (i in 1:ns) {
    
    # for each reviewer, calculate the ranks
    ratings_i <- ratings[i,,]
    ranks_i <- apply(ratings_i, 2, rank, na.last = "keep") 
    #ranks_i <- t(na.omit(t(ranks_i))) # consider removing
    
    # get the number of raters
    nr <- dim(ranks_i)[2]
    
    
    # Ti is the tie correction for the mi observation of subject i
    #t = apply(ranks_i, 2, function(x) sum(x[x==1], na.rm=T))
    #Ti[i] <- mean((t^3)-t)
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
    #mi[i] <- sum(apply(ranks_i, 2, function(x) length(unique(x))))
    mi[i] <- sum(apply(ranks_i, 2, function(x) length(x)))
    
    # sample variance of Rij
    Si <- var(Rij[i,])
    # R <- (nc+1)/2
    # Si <- vector("numeric", nc)
    # for(j in 1:nc){
    #   Si[j] <- ((Rij[i,j]-R)^2)/(nc-1)
    # }
    # Si <- sum(Si)
    
    
    # coefficient of concordance for the mi rankings of subject i
    Wi[i] <- 12*(nc-1)*Si/((nc^3)-nc-Ti[i]) 
    
    
    # source(here::here("R/kendallModified.R"))
    # WiCorrect <- kendallModified(ranks_i)
    # Wi[i] <- WiCorrect$value
    # Tj[i] <- WiCorrect$Tj
    
    
    # average spearman rank correlation coefficient
    ri[i] <- ((mi[i]*Wi[i])-1)/(mi[i]-1)
    
    
  } # end of looping through all the subjects
  
  
  # now calculate overall metrics
  
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
  WT <- 12*(nc-1)*St/((nc^3)-nc-weightedTj)
  
  # overall rT
  rT <- ((sum(mi)*WT)-1)/(sum(mi)-1)
  
  # K0
  k0 <- (mean(ri)-rT)/(1-rT)
  
  return(k0)
  
}
