
# parameter combinations for experiment (check that these are correct!)
# save.image(file = "output/AHPSP_simulation_results_1-10.RData")
pars <- expand.grid(Ni = c(9, 19), J = c(10, 20), Ns = 10, pNs_poscor = 1, poscor = c(0.01),
                    pcat = c(1, 2), stddevbase = 0.01, stdevrange = c(0.01, 0.04, 0.09), 
                    pNs_poscor_w = c(1), poscor_w = c(0.01), n_futures_used = c(1, 3, 5, 10))
pars <- pars %>% filter(!(pNs_poscor != 1 & poscor == 0.01)) %>% filter(!(pNs_poscor_w != 1 & poscor_w == 0.01))
pars <- pars %>% arrange(Ni, J)

# parameter combinations for experiment
# save.image(file = "output/AHPSP_simulation_results_wcor1/2.RData")
pars <- expand.grid(Ni = c(9), J = c(10), Ns = 10, pNs_poscor = 1, poscor = c(0.01),
                    pcat = c(2), stddevbase = 0.01, stdevrange = c(0.04), 
                    pNs_poscor_w = c(1), poscor_w = c(0.33, 0.66, 0.99), n_futures_used = c(1, 3, 5, 10))
pars <- pars %>% filter(!(pNs_poscor != 1 & poscor == 0.01)) %>% filter(!(pNs_poscor_w != 1 & poscor_w == 0.01))
pars <- pars %>% arrange(Ni, J)

# parameter combinations for experiment
# save.image(file = "output/AHPSP_simulation_results_wcor3/4.RData")
pars <- expand.grid(Ni = c(9), J = c(10), Ns = 10, pNs_poscor = 1, poscor = c(0.33, 0.66, 0.99),
                    pcat = c(2), stddevbase = 0.01, stdevrange = c(0.04), 
                    pNs_poscor_w = c(1), poscor_w = c(0.01, 0.33, 0.66, 0.99), n_futures_used = c(1, 3, 5, 10))
pars <- pars %>% filter(!(pNs_poscor != 1 & poscor == 0.01)) %>% filter(!(pNs_poscor_w != 1 & poscor_w == 0.01))
pars <- pars %>% arrange(Ni, J)
