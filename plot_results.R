library(tidyverse)

load("output/AHPSP_simulation_allresults.rdata")
load("output/AHPSP_simulation_corresults.rdata")

res_cor_11 <- res_all %>% filter(Ni == 9, J == 10, pcat == 2, stdevrange == 0.04)
res_cor <- rbind(res_cor, res_cor_11)
rm(res_cor_11)

res_all <- res_all %>%
  mutate(Ni = factor(Ni, levels = c(9, 19),
                         labels = c("9 alts", "19 alts"))) %>%
  mutate(J = factor(J, levels = c(10, 20),
                           labels = c("10 crit", "20 crit"))) %>%
  mutate(pcat = factor(pcat, levels = c(1, 2), 
                        labels = c("Equal future probs", "Unequal future probs"))) %>%
  mutate(stdevrange = factor(stdevrange, levels = c(0.01, 0.04, 0.09),
                             labels = c("Low variability", "Med variability", "High variability")))



res_all_NiJ <- res_all %>% filter(model != "allst") %>%
  group_by(outcome, n_futures_used, Ni, J) %>%
  summarize(v_mean = mean(1-value),
            v_lcl = mean(1-value) - 1.96 * sd(1-value) / sqrt(n()),
            v_ucl = mean(1-value) + 1.96 * sd(1-value) / sqrt(n()))

p_ps <- res_all_NiJ %>% filter(outcome == "Rtb") %>%
  ggplot(aes(x = n_futures_used / 10)) + 
  geom_line(aes(y = v_mean)) +
  geom_point(aes(y = v_mean)) +
  geom_errorbar(aes(ymin = v_lcl, ymax = v_ucl), width = 0.01) +
  facet_grid(. ~ Ni + J) +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=14), strip.text = element_text(size = 14)) +
  xlab("Scenario coverage") + ylab("Rank correlation")

ggsave("results/problemsize.png", p_ps, width = 12, height = 4, dpi = 300)

res_all <- res_all %>% 
  mutate(meanscen = ifelse(str_detect(model, "[13]"), "Mean scenarios", "Random scenarios"))

res_all_Zvar <- res_all %>% filter(model != "allst") %>%
  group_by(outcome, n_futures_used, meanscen, stdevrange, pcat) %>%
  summarize(v_mean = mean(1-value),
            v_lcl = mean(1-value) - 1.96 * sd(1-value) / sqrt(n()),
            v_ucl = mean(1-value) + 1.96 * sd(1-value) / sqrt(n()))

p_var <- res_all_Zvar %>% filter(outcome == "Rtb") %>%
  ggplot(aes(x = n_futures_used / 10, colour = meanscen)) + 
  geom_line(aes(y = v_mean)) +
  geom_errorbar(aes(ymin = v_lcl, ymax = v_ucl), width = 0.01) +
  geom_point(aes(y = v_mean)) +
  facet_grid(. ~ pcat + stdevrange) +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=14), strip.text = element_text(size = 14)) +
  xlab("Scenario coverage") + ylab("Rank correlation") +
  theme(legend.position = "bottom", legend.text=element_text(size=14),
        legend.title=element_blank(), legend.spacing.x = unit(0.5, "cm"), legend.key.width = unit(2,"cm")) 

ggsave("results/variability.png", p_var, width = 12, height = 4, dpi = 300)

##

res_cor_summary <- res_cor %>% 
  group_by(outcome, n_futures_used, poscor, poscor_w, model) %>%
  summarize(v_mean = mean(value),
            v_lcl = mean(value) - 1.96 * sd(value) / sqrt(n()),
            v_ucl = mean(value) + 1.96 * sd(value) / sqrt(n())) %>%
  ungroup()

res_cor_summary <- res_cor_summary %>%
  mutate(poscor = factor(poscor, levels = c(0.99, 0.66, 0.33, 0.01),
                         labels = c("rho^w == 0.99", "rho^w == 0.66", "rho^w == 0.33", "rho^w == 0.01"))) %>%
  mutate(poscor_w = factor(poscor_w, levels = c(0.99, 0.66, 0.33, 0.01),
                         labels = c("rho^z == 0.99", "rho^z == 0.66", "rho^z == 0.33", "rho^z == 0.01"))) %>%
  mutate(model = factor(model, levels = c("sc1", "sc2", "sc3", "sc4", "allst"), 
                        labels = c("ML-Mean", "ML-Random", "Random-Mean", "Random-Random", "Average~weights")))

my.labs <- list(bquote(rho^w == "0.99"), bquote(rho^w == "0.66"), bquote(rho^w == "0.33"), 
                       bquote(rho^w == "0.01"))

p1 <- res_cor_summary %>% filter(outcome == "cor") %>%
  ggplot(aes(x = n_futures_used / 10, color = poscor, shape = poscor, group = poscor)) + 
  geom_line(aes(y = 1 - v_mean)) +
  geom_point(aes(y = 1 - v_mean), size = 2) +
  geom_errorbar(aes(ymin = 1 - v_ucl, ymax = 1 - v_lcl), width = 0.01) +
  facet_grid(poscor_w ~ model, labeller = label_parsed) +
  scale_color_discrete(labels = my.labs) +
  scale_shape_discrete(labels = my.labs) +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=14), strip.text = element_text(size = 14)) +
  theme(legend.position = "bottom", legend.text=element_text(size=14),
        legend.title=element_blank(), legend.spacing.x = unit(0.5, "cm"), legend.key.width = unit(2,"cm")) + 
  xlab("Scenario coverage") + ylab("Rank correlation")

ggsave("results/scenario_models.png", p1, width = 12, height = 9, dpi = 300)



