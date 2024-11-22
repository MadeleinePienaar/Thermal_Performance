setwd("Documents/R/Locomotion performance/Maximum speed/A25/Data/")

# load packages
library(boot)
library(car)
library(rTPC)
library(nls.multstart)
library(broom)
library(tidyverse)
library(patchwork)
library(minpack.lm)
library(nlstools)

# load in data
df25 <- read.csv("Acclimation_25_Max_speed_plus_anchor.csv", header = TRUE, sep= ";")

ggplot(df25, aes(Temp, Max_speed)) +
  geom_point() +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (?C)',
       y = 'Max speed (mm/s)',
       title = 'Max speed across temperatures')

dev.off()
# fit Guassian model
d_fit_25 <- nest(df25, data = c(Temp, Max_speed)) %>%
  mutate(ratkowsky = map(data, ~nls_multstart(Max_speed~ratkowsky_1983(temp = Temp, tmin, tmax, a, b),
                                              data = .x,
                                              iter = c(4,4,4,4),
                                              start_lower = get_start_vals(.x$Temp, .x$Max_speed, model_name = 'ratkowsky_1983') - 10,
                                              start_upper = get_start_vals(.x$Temp, .x$Max_speed, model_name = 'ratkowsky_1983') + 10,
                                              lower = get_lower_lims(.x$Temp, .x$Max_speed, model_name = 'ratkowsky_1983'),
                                              upper = get_upper_lims(.x$Temp, .x$Max_speed, model_name = 'ratkowsky_1983'),
                                              supp_errors = 'Y',
                                              convergence_count = FALSE)),
         # create new temperature data
         new_data = map(data, ~tibble(Temp = seq(min(.x$Temp), max(.x$Temp), length.out = 100))),
         # predict over that data,
         preds =  map2(ratkowsky, new_data, ~augment(.x, newdata = .y)))


# unnest predictions
d_preds <- select(d_fit_25, preds) %>%
  unnest(preds)

# plot data and predictions
ggplot() +
  geom_line(aes(Temp, .fitted), d_preds, col = 'blue') +
  geom_point(aes(Temp, Max_speed), df25, size = 2, alpha = 0.5) +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (?C)',
       y = 'Max walking speed (mm/s)',
       title = 'Max walking speed across temperatures')


# refit model using nlsLM
fit_nlsLM <- minpack.lm::nlsLM(Max_speed~ratkowsky_1983(temp = Temp, tmin, tmax, a, b),
                               data = df25,
                               start = coef(d_fit_25$ratkowsky[[1]]),
                               lower = get_lower_lims(df25$Temp, df25$Max_speed, model_name = 'ratkowsky_1983'),
                               upper = get_upper_lims(df25$Temp, df25$Max_speed, model_name = 'ratkowsky_1983'),
                               weights = rep(1, times = nrow(df25)))

# bootstrap using case resampling
boot1 <- Boot(fit_nlsLM, method = 'case')

# create predictions of each bootstrapped model
boot1_preds <- boot1$t %>%
  as.data.frame() %>%
  drop_na() %>%
  mutate(iter = 1:n()) %>%
  group_by_all() %>%
  do(data.frame(Temp = seq(min(df25$Temp), max(df25$Temp), length.out = 100))) %>%
  ungroup() %>%
  mutate(pred = ratkowsky_1983(Temp, tmin, tmax, a, b))

# calculate bootstrapped confidence intervals
boot1_conf_preds <- group_by(boot1_preds, Temp) %>%
  summarise(conf_lower = quantile(pred, 0.025),
            conf_upper = quantile(pred, 0.975)) %>%
  ungroup()

# plot bootstrapped CIs
p1 <- ggplot() +
  geom_line(aes(Temp, .fitted), d_preds, col = 'blue') +
  geom_ribbon(aes(Temp, ymin = conf_lower, ymax = conf_upper), boot1_conf_preds, fill = 'blue', alpha = 0.3) +
  geom_point(aes(Temp, Max_speed), df25, size = 2, alpha = 0.5) +
  theme_bw(base_size = 12) +
  scale_x_continuous(minor_breaks= c(8,13,18,23,28,33,38), breaks=c(8,13,18,23,28,33,38), labels = c("8", "13", "18", "23", "28", "33", "38"))+ 
  labs(x = 'Temperature (?C)',
       y = 'Max walking speed (mm/s)',
       title = 'Bootstrapped confidence intervals: Max walking speed \n across temperatures of 25?C acclimated PSHB')
p1

dev.off()
# plot bootstrapped predictions
p2 <- ggplot() +
  geom_line(aes(Temp, .fitted), d_preds, col = 'blue') +
  geom_line(aes(Temp, pred, group = iter), boot1_preds, col = 'blue', alpha = 0.007) +
  geom_point(aes(Temp, Max_speed), df25, size = 2, alpha = 0.5) +
  theme_bw(base_size = 12) +
  scale_x_continuous(minor_breaks= c(8,13,18,23,28,33,38), breaks=c(8,13,18,23,28,33,38), labels = c("8", "13", "18", "23", "28", "33", "38"))+ 
  labs(x = 'Temperature (?C)',
       y = 'Max walking speed (mm/s)',
       title = 'Bootstrapped predictions: Max walking speed \n across temperatures of 25?C acclimated PSHB')

p2
p1 + p2


# Calculating confidence intervals of estimated and calculated parameters

# get parameters of fitted model
param_bact <- broom::tidy(fit_nlsLM) %>%
  select(param = term, estimate)

# calculate confidence intervals of models
ci_bact1 <- nlstools::confint2(fit_nlsLM, method = 'asymptotic') %>%
  as.data.frame() %>%
  rename(conf_lower = 1, conf_upper = 2) %>%
  rownames_to_column(., var = 'param') %>%
  mutate(method = 'asymptotic')
ci_bact2 <- confint(fit_nlsLM) %>%
  as.data.frame() %>%
  rename(conf_lower = 1, conf_upper = 2) %>%
  rownames_to_column(., var = 'param') %>%
  mutate(method = 'profile')
#> Waiting for profiling to be done...

# CIs from case resampling
ci_bact3 <- confint(boot1, method = 'bca') %>%
  as.data.frame() %>%
  rename(conf_lower = 1, conf_upper = 2) %>%
  rownames_to_column(., var = 'param') %>%
  mutate(method = 'case bootstrap')

# CIs from residual resampling
ci_bact4 <- Boot(fit_nlsLM, method = 'residual') %>%
  confint(., method = 'bca') %>%
  as.data.frame() %>%
  rename(conf_lower = 1, conf_upper = 2) %>%
  rownames_to_column(., var = 'param') %>%
  mutate(method = 'residual bootstrap')

ci_bact <- bind_rows(ci_bact1, ci_bact2, ci_bact3, ci_bact4) %>%
  left_join(., param_bact)
#> Joining, by = "param"

# plot
ggplot(ci_bact, aes(forcats::fct_relevel(method, c('profile', 'asymptotic')), estimate, col = method)) +
  geom_hline(aes(yintercept = conf_lower), linetype = 2, filter(ci_bact, method == 'profile')) +
  geom_hline(aes(yintercept = conf_upper), linetype = 2, filter(ci_bact, method == 'profile')) +
  geom_point(size = 4) +
  geom_linerange(aes(ymin = conf_lower, ymax = conf_upper)) +
  theme_bw() +
  facet_wrap(~param, scales = 'free') +
  scale_x_discrete('', labels = function(x) stringr::str_wrap(x, width = 10)) +
  labs(title = 'Calculation of confidence intervals for model parameters',
       subtitle = 'Dashed lines are CI of profiling method')


# bootstrap confidence intervals for the extra parameters

extra_params <- calc_params(fit_nlsLM) %>%
  pivot_longer(everything(), names_to =  'param', values_to = 'estimate')

ci_extra_params <- Boot(fit_nlsLM, f = function(x){unlist(calc_params(x))}, labels = names(calc_params(fit_nlsLM)), R = 200, method = 'case') %>%
  confint(., method = 'bca') %>%
  as.data.frame() %>%
  rename(conf_lower = 1, conf_upper = 2) %>%
  rownames_to_column(., var = 'param') %>%
  mutate(method = 'case bootstrap')

ci_extra_params <- left_join(ci_extra_params, extra_params)
#> Joining, by = "param"

ggplot(ci_extra_params, aes(param, estimate)) +
  geom_point(size = 4) +
  geom_linerange(aes(ymin = conf_lower, ymax = conf_upper)) +
  theme_bw() +
  facet_wrap(~param, scales = 'free') +
  scale_x_discrete('') +
  labs(title = 'Calculation of confidence intervals for extra parameters',
       subtitle = 'PSHB TPC; using case resampling')

ci_extra_params
write.csv(ci_extra_params, "Params_CI_Max_speed_25.csv", sep = ",")
