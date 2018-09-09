### utility functions

# standardise x to have min U[min_ub, min_ub] and max U[max_lb, max_ub]

minmax_standardize <- function(x, min_lb, min_ub, max_lb, max_ub){
  min_x <- runif(1, min_lb, min_ub)
  max_x <- runif(1, max_lb, max_ub)
  std_x <- min_x + (max_x - min_x) * ((x - min(x)) / (max(x) - min(x)))
  
  return(std_x)
}