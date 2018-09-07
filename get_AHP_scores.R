# given a vector of attribute values, this function computes the PCM and then the 
# eigenvalues/scores used by the AHP

get_AHP_scores <- function(x){
  
  x <- as.matrix(x, ncol = 1)
  
  # create PCM
  PCM <- x %*% (1 / t(x)) 
  
  ## Compute partial value scores by eigenvector method
  v <- Re(eigen(PCM)$vectors[,1])
  v <- v / sum(v)
  
}