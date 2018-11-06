# concatenate individual results files and save as one big file

library(tidyverse)

# result files from experiment
res_files <- list.files("output", pattern = "results_")

# names of the models used in the experiment
models <- c("allst", "sc1", "sc2", "sc3", "sc4")

# parameter combinations for experiment (check that these are correct!)
pars <- expand.grid(Ni = c(9, 19), J = c(10, 20), Ns = 10, pNs_poscor = 1, poscor = c(0.01),
                    pcat = c(1, 2), stddevbase = 0.01, stdevrange = c(0.01, 0.04, 0.09), 
                    pNs_poscor_w = c(1), poscor_w = c(0.01), n_futures_used = c(1, 3, 5, 10))
pars <- pars %>% filter(!(pNs_poscor != 1 & poscor == 0.01)) %>% filter(!(pNs_poscor_w != 1 & poscor_w == 0.01))
pars <- pars %>% arrange(Ni, J)
#save(pars, file = "output/simstudy_pars.RData")
#load("output/simstudy_pars.RData")

# each results file contains results for 10 sim runs at all par combos, so 
# repeat the parameters data frame 10 times
pars$id <- 1:nrow(pars)
mypars <- bind_rows(replicate(10, pars, simplify = FALSE)) %>% arrange(id) %>% select(-id)
  
# concatenate all the results files, and the parameter values that generated them

# first file (sets up the structure, colnames etc)
i <- 1
x <- load(paste0("output/",res_files[i]))
rm(pars) # x contains a pars df, remove to avoid confusion with above!
res_out <- data.frame(t(res_out), row.names = NULL)
colnames(res_out) <- c(paste0("Rtb_", models), paste0("Rmb_", models), paste0("cor_", models))
res_out[,11:15] <- 1 - res_out[,11:15] # make correls into 1 - correls, make range similar to other outcomes
res_all <- cbind.data.frame(mypars, res_out)

# rest of the files
for(i in 2:length(res_files)){
  
  x <- load(paste0("output/",res_files[i]))
  rm(pars) # x contains a pars df, remove to avoid confusion with above!
  res_out <- data.frame(t(res_out), row.names = NULL)
  colnames(res_out) <- c(paste0("Rtb_", models), paste0("Rmb_", models), paste0("cor_", models))
  res_out[,11:15] <- 1 - res_out[,11:15] # make correls into 1 - correls, make range similar to other outcomes
  res_all <- rbind.data.frame(res_all, cbind.data.frame(mypars, res_out))
  
}

# reshape res_all to tidy format
res_all <- res_all %>% gather(out_mod, value, 12:26) 

# split out_mod variable into separate outcome and model variables
res_all <- res_all %>% separate(out_mod, sep = "_", c("outcome", "model"))

# save all simulation runs
save(res_all, file = "output/AHPSP_simulation_allresults.rdata")

