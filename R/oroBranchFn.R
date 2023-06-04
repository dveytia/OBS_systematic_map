# function to calculate oro_branch from oro_type
oroBranchFn <- function(x){ # x is a matrix with n pubs x n variables
  
  x2 <- as.numeric(x)
  x2 <- matrix(x2, nrow=nrow(x), ncol=ncol(x),
               dimnames = dimnames(x))
  x <- x2; rm(x2)
  
  out <- array(dim=c(dim(x)[1], 4),
               dimnames = list(publication=NULL,
                               variableValues = paste("oro_branch", 
                                                      c("Mitigation", "Nature",
                                                        "Societal","Unclear"),
                                                      sep=".")))
  
  x2 <- x[,c("oro_type.M_Renewables","oro_type.M_Increase_efficiency",
             "oro_type.M_CO2_removal_or_storage")]
  if(sum(is.na(x2)) == length(c(x2))){
    out[,"oro_branch.Mitigation"] <- rep(0, nrow(x2))
  }else{
    out[,"oro_branch.Mitigation"] <- rowSums(x2, na.rm = TRUE)
  }
  
  
  x2 <- x[,c("oro_type.N_Human_assisted_evolution","oro_type.N_Protection",
             "oro_type.N_Restoration")]
  if(sum(is.na(x2)) == length(c(x2))){
    out[,"oro_branch.Nature"] <- rep(0, nrow(x2))
  }else{
    out[,"oro_branch.Nature"] <- rowSums(x2, na.rm = TRUE)
  }
  
  
  x2 <- x[,c("oro_type.SA_Built_infrastructure_and_technology","oro_type.SA_Socioinstitutional")]
  if(sum(is.na(x2)) == length(c(x2))){
    out[,"oro_branch.Societal"] <- rep(0, nrow(x2))
  }else{
    out[,"oro_branch.Societal"] <- rowSums(x2, na.rm = TRUE)
  }
  
  
  out[,"oro_branch.Unclear"] <- x[,c("oro_type.Unclear")]
  
  
  out[1<out] <- 1
  out2 <- as.character(out)
  out2 <- matrix(out2, nrow=nrow(out), ncol=ncol(out),
                 dimnames = dimnames(out))
  out <- out2; rm(out2)
  return(out)
  
}

