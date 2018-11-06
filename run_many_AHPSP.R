# function running run_AHPSP many times

run_many_AHPSP <- function(Ni, J, Ns, pNs_poscor, poscor, pcat, stddevbase, stddevrange, 
                           pNs_poscor_w, poscor_w, n_futures_used, ...){
  # run N times 
  res <- map(.x = 1:30, .f = run_AHPSP, Ni = Ni, J = J, Ns = Ns, pNs_poscor = pNs_poscor, poscor = poscor, 
             pcat = pcat, stddevbase = stddevbase, stddevrange = stddevrange, pNs_poscor_w = pNs_poscor_w, 
             poscor_w = poscor_w, n_futures_used = n_futures_used)
  
  print("done combo")
  
  return(res)
}