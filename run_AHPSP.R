run_AHPSP <- function(Ni, J, Ns, pNs_poscor, poscor, pcat, stddevbase, stddevrange, 
                      pNs_poscor_w, poscor_w, n_futures_used, ...){
  
  # get the distribution of states over futures
  if (pcat == 1) {
    p <- rep(40, Ns)
  } else {
    p <- sample(c(60, 60, 60, 60, 40, 40, 20, 20, 20, 20), Ns, replace = F)
  }
  
  # generate baseline attribute evaluations in each future
  baseA <- generate_baseline_evaluations(Ni = Ni, J = J, Ns = Ns, 
                                         pNs_poscor = pNs_poscor, poscor = poscor)
  
  # generate state evaluations around each baseline future 
  Z <- generate_state_evaluations(baseA = baseA, p = p, stddevbase = stddevbase, stddevrange = stddevrange)
  
  # generate criterion weights in each future
  W <- generate_criteria_weights(J = J, Ns = Ns, pNs_poscor_w = pNs_poscor_w, poscor_w = poscor_w, 
                                 min_lb = 0.1, min_ub = 0.15, max_lb = 0.75, max_ub = 0.9)
  
  # add AHP scores to attribute evaluations
  Z <- Z %>% group_by(j,k,l) %>% mutate(v = get_AHP_scores(z)) %>% arrange(k,l,j)
  
  # add AHP scores to criteria weights
  W <- W %>% group_by(k) %>% mutate(wv = get_AHP_scores(w)) %>% arrange(k) %>% ungroup()
  # add mean weights over all futures
  W <- W %>% left_join(W %>% group_by(j) %>% summarize(meanwv = mean(wv)), by = "j")
  
  # add criterion weights to Z df so that can work out weighted sums in a bit 
  Z <- Z %>% left_join(W %>% select(-w), by = c("j", "k"))
  
  # true values: full AHP model with future-specific weights
  v_true <- Z %>% 
    # aggregate over all states
    group_by(i,j) %>% summarize(v = mean(wv * v)) %>% ungroup() %>%
    # aggregate over criteria
    group_by(i) %>% summarize(v_true = sum(v)) %>% ungroup()
  
  # full AHP model with mean weights
  # this model assumes that, without a scenario analysis, DM will give weights that are 
  # mentally averaged over futures
  v_allstates <- Z %>% 
    # aggregate over all states
    group_by(i,j) %>% summarize(v = mean(meanwv * v)) %>% ungroup() %>%
    # aggregate over criteria
    group_by(i) %>% summarize(v_allstates = sum(v)) %>% ungroup()
  
  # scenario-based AHP model 1 -- select most likely futures and baseline state in each future 
  # chooses most likely futures
  futures_to_use <- sample(1:Ns, n_futures_used, prob = p^10)
  v_scen_ml_base <- Z %>% 
    # choose futures to use
    filter(k %in% futures_to_use) %>%
    # choose state within each selected future
    filter(l == 1) %>%
    # aggregate over all states
    group_by(i,j) %>% summarize(v = mean(wv * v)) %>% ungroup() %>%
    # aggregate over criteria
    group_by(i) %>% summarize(v_sc1 = sum(v)) %>% ungroup()
  
  # scenario-based AHP model 2 -- select most likely futures and random state in each future 
  # chooses most likely futures
  v_scen_ml_rand <- Z %>% 
    # choose futures to use
    filter(k %in% futures_to_use) %>%
    # choose state within each selected future
    filter(l == 2) %>%
    # aggregate over all states
    group_by(i,j) %>% summarize(v = mean(wv * v)) %>% ungroup() %>%
    # aggregate over criteria
    group_by(i) %>% summarize(v_sc2 = sum(v)) %>% ungroup()
  
  # scenario-based AHP model 3 -- select random futures and baseline state in each future 
  # chooses random futures
  futures_to_use <- sample(1:Ns, n_futures_used)
  v_scen_rand_base <- Z %>% 
    # choose futures to use
    filter(k %in% futures_to_use) %>%
    # choose state within each selected future
    filter(l == 1) %>%
    # aggregate over all states
    group_by(i,j) %>% summarize(v = mean(wv * v)) %>% ungroup() %>%
    # aggregate over criteria
    group_by(i) %>% summarize(v_sc3 = sum(v)) %>% ungroup()
  
  # scenario-based AHP model 4 -- select random futures and random state in each future 
  # chooses random futures
  futures_to_use <- sample(1:Ns, n_futures_used)
  v_scen_rand_rand <- Z %>% 
    # choose futures to use
    filter(k %in% futures_to_use) %>%
    # choose state within each selected future
    filter(l == 2) %>%
    # aggregate over all states
    group_by(i,j) %>% summarize(v = mean(wv * v)) %>% ungroup() %>%
    # aggregate over criteria
    group_by(i) %>% summarize(v_sc4 = sum(v)) %>% ungroup()
  
  # merge values from different models and get the rank order in each model
  v_all <- v_true %>% 
    left_join(v_allstates, by = "i") %>% left_join(v_scen_ml_base, by = "i") %>% 
    left_join(v_scen_ml_rand, by = "i") %>% 
    left_join(v_scen_rand_base, by = "i") %>% left_join(v_scen_rand_rand, by = "i") %>% 
    # rank each model
    mutate_at(-1, .funs = funs((Ni - rank(.)) / (Ni - 1)))
  
  # process results
  
  # rank of the true best alternative in the scenario results 
  rank_tbest_in_model_ro <- v_all %>% select(-i) %>% filter(v_true == 0) %>% select(-v_true) %>% as.numeric()
  # rank of the model best alternative in the true results 
  rank_mbest_in_true_ro <- c(v_all %>% filter(v_allstates == 0) %>% select(v_true) %>% as.numeric(),
                             v_all %>% filter(v_sc1 == 0) %>% select(v_true) %>% as.numeric(),
                             v_all %>% filter(v_sc2 == 0) %>% select(v_true) %>% as.numeric(),
                             v_all %>% filter(v_sc3 == 0) %>% select(v_true) %>% as.numeric(),
                             v_all %>% filter(v_sc4 == 0) %>% select(v_true) %>% as.numeric())
  # rank correlations
  rank_cors <- cor(as.matrix(v_all[,-1]), method = "spearman")[1,][-1]
  
  # combine results
  outcomes <- c(rank_tbest_in_model_ro, rank_mbest_in_true_ro, rank_cors)
  outcomes <- data.frame(outcomes)
  
  return(list(out = outcomes, v_all = v_all))
  
}
