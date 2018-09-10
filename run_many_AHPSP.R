# function running run_AHPSP many times

run_many_AHPSP <- function(Ni, J, Ns, Ns_poscor, poscor, pcat, stddevbase, stddevrange, 
                           Ns_poscor_w, poscor_w, n_futures_used, ...){
  # run N times 
  res <- map(.x = 1:100, .f = run_AHPSP, Ni = Ni, J = J, Ns = Ns, Ns_poscor = Ns_poscor, poscor = poscor, 
             pcat = pcat, stddevbase = stddevbase, stddevrange = stddevrange, Ns_poscor_w = Ns_poscor_w, 
             poscor_w = poscor_w, n_futures_used = n_futures_used)
  
  print("done combo")
  
  return(res)
}