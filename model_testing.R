# testing simstudy_AHPSP

library(MASS)
library(tidyverse)

source("run_AHPSP.R")
source("run_many_AHPSP.R")
source("generate_baseline_evaluations.R")
source("generate_state_evaluations.R")
source("get_AHP_scores.R")
source("generate_criteria_weights.R")
source("utils.R")

Ni = 10
J = 9
Ns = 10
pNs_poscor = 1
poscor = 0.01
pcat = 2
stddevbase = 0.01 
stddevrange = 0.09
pNs_poscor_w = 1
poscor_w = 0.01
n_futures_used = 1

# testing one run, this works
run_AHPSP(Ni = Ni, J = J, Ns = Ns, pNs_poscor = pNs_poscor, poscor = poscor,
          pcat = pcat, stddevbase = stddevbase, stddevrange = stddevrange, 
          pNs_poscor_w = pNs_poscor_w, poscor_w = poscor_w,
          n_futures_used = n_futures_used)

# testing multiple runs, this works
pars <- expand.grid(Ni = c(9), J = c(10), Ns = 10, pNs_poscor = 1, poscor = c(0.01),
                    pcat = c(2), stddevbase = 0.001, stdevrange = c(0.0001,0.3), 
                    pNs_poscor_w = c(1), poscor_w = c(0.2), n_futures_used = c(1))
pars <- pars %>% filter(!(pNs_poscor != 1 & poscor == 0.01)) %>% filter(!(pNs_poscor_w != 1 & poscor_w == 0.01))
pars <- pars %>% arrange(Ni, J)

res <- pmap(list(Ni = pars$Ni, J = pars$J, Ns = pars$Ns, pNs_poscor = pars$pNs_poscor, poscor = pars$poscor, 
                 pcat = pars$pcat, stddevbase = pars$stddevbase, stddevrange = pars$stdevrange, 
                 pNs_poscor_w = pars$pNs_poscor_w, poscor_w = pars$poscor_w, 
                 n_futures_used = pars$n_futures_used), run_many_AHPSP)

# concatenate results, this works
res_out <- map(res, . %>% map("out") %>% map_dfc(bind_cols)) %>% map_dfc(bind_cols)

# sanity checks on model results

# names of the models used in the experiment
models <- c("allst", "sc1", "sc2", "sc3", "sc4")

# repeat the parameters data frame as many times as there are results files 
pars$id <- 1:nrow(pars)
runs_per_combo <- 30
mypars <- bind_rows(replicate(runs_per_combo, pars, simplify = FALSE)) %>% arrange(id) %>% select(-id)

res_out <- data.frame(t(res_out), row.names = NULL)
colnames(res_out) <- c(paste0("Rtb_", models), paste0("Rmb_", models), paste0("cor_", models))
res_out[,11:15] <- 1 - res_out[,11:15] # make correls into 1 - correls, make range similar to other outcomes
res_all <- cbind.data.frame(mypars, res_out)

# reshape res_all to tidy format
res_all <- res_all %>% gather(out_mod, value, 12:26) 

# split out_mod variable into separate outcome and model variables
res_all <- res_all %>% separate(out_mod, sep = "_", c("outcome", "model"))

# test plots

res_all %>% 
  ggplot(aes(factor(n_futures_used), value)) + 
  stat_summary(fun.data = "mean_cl_normal", geom = "crossbar") +
  facet_grid(outcome ~ model)

res_all %>% filter(outcome == "Rmb") %>%
  ggplot(aes(factor(n_futures_used), value)) + 
  stat_summary(fun.data = "mean_cl_normal", geom = "crossbar") +
  facet_grid(model ~ Ni + J)

res_all %>% filter(outcome == "Rmb") %>%
  ggplot(aes(factor(n_futures_used), value)) + 
  stat_summary(fun.data = "mean_cl_normal", geom = "crossbar") +
  facet_grid(model ~ stdevrange + pcat)