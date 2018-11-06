# concatenate individual results files and save as one big file

library(tidyverse)

# result files from experiment
res_files <- list.files("output", pattern = "cor")

# names of the models used in the experiment
models <- c("allst", "sc1", "sc2", "sc3", "sc4")

# parameter combinations for experiment 1 & 2
pars <- expand.grid(Ni = c(9), J = c(10), Ns = 10, pNs_poscor = 1, poscor = c(0.01),
                    pcat = c(2), stddevbase = 0.01, stdevrange = c(0.04), 
                    pNs_poscor_w = c(1), poscor_w = c(0.33, 0.66, 0.99), n_futures_used = c(1, 3, 5, 10))
pars <- pars %>% filter(!(pNs_poscor != 1 & poscor == 0.01)) %>% filter(!(pNs_poscor_w != 1 & poscor_w == 0.01))
pars <- pars %>% arrange(Ni, J)
#save(pars, file = "output/simstudy_pars.RData")
#load("output/simstudy_pars.RData")

# each results file contains results for N sim runs at all par combos, so 
# repeat the parameters data frame N times
pars$id <- 1:nrow(pars)
mypars12 <- bind_rows(replicate(50, pars, simplify = FALSE)) %>% arrange(id) %>% select(-id)

# parameter combinations for experiment 3 & 4
# save.image(file = "output/AHPSP_simulation_results_wcor3/4.RData")
pars <- expand.grid(Ni = c(9), J = c(10), Ns = 10, pNs_poscor = 1, poscor = c(0.33, 0.66, 0.99),
                    pcat = c(2), stddevbase = 0.01, stdevrange = c(0.04), 
                    pNs_poscor_w = c(1), poscor_w = c(0.01, 0.33, 0.66, 0.99), n_futures_used = c(1, 3, 5, 10))
pars <- pars %>% filter(!(pNs_poscor != 1 & poscor == 0.01)) %>% filter(!(pNs_poscor_w != 1 & poscor_w == 0.01))
pars <- pars %>% arrange(Ni, J)

# each results file contains results for N sim runs at all par combos, so 
# repeat the parameters data frame N times
pars$id <- 1:nrow(pars)
mypars34 <- bind_rows(replicate(50, pars, simplify = FALSE)) %>% arrange(id) %>% select(-id)

# concatenate all the results files, and the parameter values that generated them

# first file (sets up the structure, colnames etc)
i <- 1
x <- load(paste0("output/",res_files[i]))
rm(pars) # x contains a pars df, remove to avoid confusion with above!
res_out <- data.frame(t(res_out), row.names = NULL)
colnames(res_out) <- c(paste0("Rtb_", models), paste0("Rmb_", models), paste0("cor_", models))
res_out[,11:15] <- 1 - res_out[,11:15] # make correls into 1 - correls, make range similar to other outcomes
res_cor <- cbind.data.frame(mypars12, res_out)

i <- 2
x <- load(paste0("output/",res_files[i]))
rm(pars) # x contains a pars df, remove to avoid confusion with above!
res_out <- data.frame(t(res_out), row.names = NULL)
colnames(res_out) <- c(paste0("Rtb_", models), paste0("Rmb_", models), paste0("cor_", models))
res_out[,11:15] <- 1 - res_out[,11:15] # make correls into 1 - correls, make range similar to other outcomes
res_cor <- rbind.data.frame(res_cor, cbind.data.frame(mypars12, res_out))

# rest of the files
for(i in 3:4){
  
  x <- load(paste0("output/",res_files[i]))
  rm(pars) # x contains a pars df, remove to avoid confusion with above!
  res_out <- data.frame(t(res_out), row.names = NULL)
  colnames(res_out) <- c(paste0("Rtb_", models), paste0("Rmb_", models), paste0("cor_", models))
  res_out[,11:15] <- 1 - res_out[,11:15] # make correls into 1 - correls, make range similar to other outcomes
  res_cor <- rbind.data.frame(res_cor, cbind.data.frame(mypars34, res_out))
  
}

# reshape res_all to tidy format
res_cor <- res_cor %>% gather(out_mod, value, 12:26) 

# split out_mod variable into separate outcome and model variables
res_cor <- res_cor %>% separate(out_mod, sep = "_", c("outcome", "model"))

# save all simulation runs
save(res_cor, file = "output/AHPSP_simulation_corresults.rdata")

