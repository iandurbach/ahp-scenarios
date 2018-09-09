generate_baseline_evaluations <- function(Ni, J, Ns, Ns_poscor, poscor){
  
  # alternative performance is correlated over futures, but not over criteria. Some futures are
  # positively correlated with one another, others are negatively correlated. Correlations need
  # to be checked to see that a cor matrix is possible (matrix is positive definite).
  my_sigma <- rbind(cbind(matrix(poscor, nrow = Ns_poscor, ncol = Ns_poscor), matrix(-poscor, nrow = Ns_poscor, ncol = Ns - Ns_poscor)),
        cbind(matrix(-poscor, nrow = Ns - Ns_poscor, ncol = Ns_poscor), matrix(poscor, nrow = Ns - Ns_poscor, ncol = Ns - Ns_poscor)))
  diag(my_sigma) <- 1
  
  # check if positive definite, stop if not
  is_pos_def <- prod(eigen(my_sigma)$values > 0)
  try(if(is_pos_def == 0) stop("correlation matrix not positive definite"))

  # generate unstandardised attribute evaluations
  
  # generate, for each criterion, evaluations with a particular between-future correlation structure
  A <- list()
  for(j in 1:J){
    A[[j]] <- pnorm(mvrnorm(n = Ni, mu = rep(0, Ns), Sigma = my_sigma))
  }
  
  # reshape this list so that list indexes over futures, and within each list is Ni * J matrix
  # of evaluations
  A_df <- data.frame(i = rep(1:Ni, times = Ns * J), 
                     k = rep(rep(1:Ns, each = Ni), times = J),
                     j = rep(1:J, each = Ni * Ns),
                     a = unlist(A))
  
  A <- list()
  for(s in 1:Ns){
    A[[s]] <- A_df %>% filter(k == s) %>% dplyr::select(-k) %>% 
      spread(key = j, value = a) %>% select(-i) %>% as.matrix()
  }
  
  # standardise the evaluations so that they lie on unit circle, guarantees Pareto
  # optimal within each main future
  for(s in 1:Ns){
    A[[s]] <- sqrt(A[[s]]^2 / rowSums(A[[s]]^2))
  }

  return(A)
  
}

