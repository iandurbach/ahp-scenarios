library(MASS)
library(dplyr)
library(tidyr)
library(purrr)

source("run_AHPSP.R")
source("run_many_AHPSP.R")
source("generate_baseline_evaluations.R")
source("generate_state_evaluations.R")
source("get_AHP_scores.R")
source("generate_criteria_weights.R")
source("utils.R")

# run once
# run_AHPSP(Ni = 10, J = 5, Ns = 10, pNs_poscor = 0.8, poscor = 0.6,
#           pcat = 1, stddevbase = 0.01, stddevrange = 0.04, pNs_poscor_w = 1, poscor_w = 0.2,
#           n_futures_used = 3)

# run N times 
# res <- map(.x = 1:10, .f = run_AHPSP, Ni = 10, J = 5, Ns = 10, pNs_poscor = 1, poscor = 0.01, 
#           pcat = 1, stddevbase = 0.01, stddevrange = 0.04, pNs_poscor_w = 1, poscor_w = 0.01, 
#           n_futures_used = 3)

# another way to run N = 100 times
# run_many_AHPSP(Ni = 10, J = 5, Ns = 10, pNs_poscor = 1, poscor = 0.01, 
#                pcat = 1, stddevbase = 0.01, stddevrange = 0.04, pNs_poscor_w = 1, poscor_w = 0.01, 
#                n_futures_used = 3)

# parameter combinations for experiment
pars <- expand.grid(Ni = c(9, 19), J = c(10, 20), Ns = 10, pNs_poscor = 1, poscor = c(0.01),
                    pcat = c(1, 2), stddevbase = 0.01, stdevrange = c(0.01, 0.04, 0.09), 
                    pNs_poscor_w = c(1), poscor_w = c(0.01), n_futures_used = c(1, 3, 5, 10))
pars <- pars %>% filter(!(pNs_poscor != 1 & poscor == 0.01)) %>% filter(!(pNs_poscor_w != 1 & poscor_w == 0.01))
pars <- pars %>% arrange(Ni, J)

# save these to concatenate later with results
save(pars, file = "output/simstudy_pars_XXX.RData")

# run the whole experiment
res <- pmap(list(Ni = pars$Ni, J = pars$J, Ns = pars$Ns, pNs_poscor = pars$pNs_poscor, poscor = pars$poscor, 
           pcat = pars$pcat, stddevbase = pars$stddevbase, stddevrange = pars$stdevrange, 
           pNs_poscor_w = pars$pNs_poscor_w, poscor_w = pars$poscor_w, 
           n_futures_used = pars$n_futures_used), run_many_AHPSP)

# concatenate results. First 100 cols are simulation runs from parameter combo 1, next
# 100 are from parameter combo 2, etc.
res_out <- map(res, . %>% map("out") %>% map_dfc(bind_cols)) %>% map_dfc(bind_cols)

# save results. Results processing loads all files with pattern = "results_", so all results
# files should include that pattern
save.image(file = "output/AHPSP_simulation_results_XXX.RData")
