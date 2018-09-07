generate_baseline_evaluations <- function(Ni, J, stddevrange, ...){
  
  lower_bound <- runif(Ni*J, 0, ifelse(stddevrange == 0.04, 1, 0.3))
  upper_bound <- lower_bound + runif(Ni*J, 0, 1 - lower_bound)
  
  # generate unstandardised attribute evaluations between bounds
  Atemp <- matrix(runif(Ni*J, lower_bound, upper_bound), nrow = Ni, ncol = J)
  
  # standardise these base evaluations so that they lie on unit circle, guarantees Pareto
  # optimal within each main future
  A <- Atemp^2 / rowSums(Atemp^2)
  
  return(A)
  
}
