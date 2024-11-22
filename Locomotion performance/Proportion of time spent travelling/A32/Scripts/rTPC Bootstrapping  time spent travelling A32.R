setwd("Documents/R/Locomotion performance/Proportion of time spent travelling/A32/Data/")

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
df32 <- read.csv("Time_travelling_A32 - log(time+1) - plus anchors.csv", header = TRUE, sep= ";")

#Detect outliers
boxplot(df32$Time_travelling)$out
outliers <- boxplot(df32$Time_travelling, plot=FALSE)$out
print(outliers)

#Remove outliers
x<- df32
x<- x[-which(x$Time_travelling %in% outliers),]
x
df32 <- x

ggplot(df32, aes(Temp, Time_travelling)) +
  geom_point() +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (?C)',
       y = 'Time spent travelling (mins/30 mins)',
       title = 'Time spent travelling across temperatures')

dev.off()
# fit thomas2 model
d_fit_32 <- nest(df32, data = c(Temp, Time_travelling)) %>%
  mutate(thomas1 = map(data, ~nls_multstart(Time_travelling~thomas_2012(temp = Temp, a,b,c,topt),
                                            data = .x,
                                            iter = c(4,4,4,4),
                                            start_lower = get_start_vals(.x$Temp, .x$Time_travelling, model_name = 'thomas_2012') - 1,
                                            start_upper = get_start_vals(.x$Temp, .x$Time_travelling, model_name = 'thomas_2012') + 2,
                                            lower = get_lower_lims(.x$Temp, .x$Time_travelling, model_name = 'thomas_2012'),
                                            upper = get_upper_lims(.x$Temp, .x$Time_travelling, model_name = 'thomas_2012'),
                                            supp_errors = 'Y',
                                            convergence_count = FALSE)),
         # create new temperature data
         new_data = map(data, ~tibble(Temp = seq(min(.x$Temp), max(.x$Temp), length.out = 100))),
         # predict over that data,
         preds =  map2(thomas1, new_data, ~augment(.x, newdata = .y)))

# unnest predictions
d_preds <- select(d_fit_32, preds) %>%
  unnest(preds)

# plot data and predictions
ggplot() +
  geom_line(aes(Temp, .fitted), d_preds, col = 'blue') +
  geom_point(aes(Temp, Time_travelling), df32, size = 2, alpha = 0.5) +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (?C)',
       y = 'Time spent travelling (log(time+1))',
       title = 'A32: Time spent travelling across temperatures')


# refit model using nlsLM
fit_nlsLM <- minpack.lm::nlsLM(Time_travelling~thomas_2012(temp = Temp, a,b,c,topt),
                               data = df32,
                               start = coef(d_fit_32$thomas1[[1]]),
                               lower = get_lower_lims(df32$Temp, df32$Time_travelling, model_name = 'thomas_2012'),
                               upper = get_upper_lims(df32$Temp, df32$Time_travelling, model_name = 'thomas_2012'),
                               weights = rep(1, times = nrow(df32)))

# bootstrap using case resampling
boot1 <- Boot(fit_nlsLM, method = 'case')

# create predictions of each bootstrapped model
boot1_preds <- boot1$t %>%
  as.data.frame() %>%
  drop_na() %>%
  mutate(iter = 1:n()) %>%
  group_by_all() %>%
  do(data.frame(Temp = seq(min(df32$Temp), max(df32$Temp), length.out = 100))) %>%
  ungroup() %>%
  mutate(pred = thomas_2012(Temp, a,b,c,topt))

# calculate bootstrapped confidence intervals
boot1_conf_preds <- group_by(boot1_preds, Temp) %>%
  summarise(conf_lower = quantile(pred, 0.025),
            conf_upper = quantile(pred, 0.975)) %>%
  ungroup()

# plot bootstrapped CIs
p1 <- ggplot() +
  geom_line(aes(Temp, .fitted), d_preds, col = 'blue') +
  geom_ribbon(aes(Temp, ymin = conf_lower, ymax = conf_upper), boot1_conf_preds, fill = 'blue', alpha = 0.3) +
  geom_point(aes(Temp, Time_travelling), df32, size = 2, alpha = 0.5) +
  theme_bw(base_size = 12) +
  scale_x_continuous(minor_breaks= c(8,13,18,23,28,33,38), breaks=c(8,13,18,23,28,33,38), labels = c("8", "13", "18", "23", "28", "33", "38"))+ 
  labs(x = 'Temperature (?C)',
       y = 'Time spent travelling (mins/30mins)',
       title = 'Bootstrapped confidence intervals: Time spent travelling \n across temperatures of 32 ?C acclimated PSHB')
p1

dev.off()
# plot bootstrapped predictions
p2 <- ggplot() +
  geom_line(aes(Temp, .fitted), d_preds, col = 'blue') +
  geom_line(aes(Temp, pred, group = iter), boot1_preds, col = 'blue', alpha = 0.007) +
  geom_point(aes(Temp, Time_travelling), df32, size = 2, alpha = 0.5) +
  theme_bw(base_size = 12) +
  scale_x_continuous(minor_breaks= c(8,13,18,23,28,33,38), breaks=c(8,13,18,23,28,33,38), labels = c("8", "13", "18", "23", "28", "33", "38"))+ 
  labs(x = 'Temperature (?C)',
       y = 'Time spent travelling (min/30 mins)',
       title = 'Bootstrapped predictions: Time spent travelling \n across temperatures of 32 ?C acclimated PSHB')

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

ci_bact <- bind_rows(ci_bact1) %>%
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

write.csv(ci_extra_params, "Params_CI_Time_travelling_A32_plus_anchors_thomas1.csv", sep = ",")
