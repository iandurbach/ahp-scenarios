generate_criteria_weights <- function(J, Ns, pNs_poscor_w, poscor_w, min_lb, min_ub, max_lb, max_ub){
  
  # number of positively correlated futures (correlation is poscor_w)
  Ns_poscor_w <- round(pNs_poscor_w * Ns, 0)
  
  # weights correlated over futures
  my_sigma <- rbind(cbind(matrix(poscor_w, nrow = Ns_poscor_w, ncol = Ns_poscor_w), matrix(-poscor_w, nrow = Ns_poscor_w, ncol = Ns - Ns_poscor_w)),
                    cbind(matrix(-poscor_w, nrow = Ns - Ns_poscor_w, ncol = Ns_poscor_w), matrix(poscor_w, nrow = Ns - Ns_poscor_w, ncol = Ns - Ns_poscor_w)))
  diag(my_sigma) <- 1
  
  # check if positive definite, stop if not
  is_pos_def <- prod(eigen(my_sigma)$values > 0)
  try(if(is_pos_def == 0) stop("correlation matrix not positive definite"))
  
  # generate weights with a particular between-future correlation structure
  w <- pnorm(mvrnorm(n = J, mu = rep(0, Ns), Sigma = my_sigma))
  
  # standardise weights to have min U[0.1,0.15] and max U[0.75,0.9]
  # I do this so that the max ratio is somewhere between 6 and 9.
  w <- apply(w, 2, minmax_standardize, min_lb = 0.1, min_ub = 0.15, max_lb = 0.75, max_ub = 0.9)

  # create df containing weights
  w_df <- data.frame(j = rep(1:J, times = Ns),
                     k = rep(1:Ns, each = J),
                     w = as.vector(w))
  
  return(w_df)
  
}