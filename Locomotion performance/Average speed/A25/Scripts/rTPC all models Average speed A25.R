getwd()
setwd("Documents/R/Locomotion performance/Average speed/A25/Data/")

# load packages
library(rTPC)
library(nls.multstart)
library(broom)
library(tidyverse)
library(ggrepel)

# load in data
df25 <- read.csv("Average_speed_A25.csv", header = TRUE, sep= ";")

ggplot(df25, aes(Temp, Average_speed)) +
  geom_point() +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (?C)',
       y = 'Average speed (mm/s)',
       title = '25?C acclimated PSHB average walking speed across temperatures')

dev.off()

# fit every model formulation in rTPC

d_fits_25 <- nest(df25, data = c(Temp, Average_speed)) %>%
  mutate(beta = map(data, ~nls_multstart(Average_speed~beta_2012(temp = Temp, a, b, c, d, e),
                                         data = .x,
                                         iter = c(6,6,6,6,6),
                                         start_lower = get_start_vals(.x$Temp, .x$Average_speed, model_name = 'beta_2012') - 10,
                                         start_upper = get_start_vals(.x$Temp, .x$Average_speed, model_name = 'beta_2012') + 10,
                                         lower = get_lower_lims(.x$Temp, .x$Average_speed, model_name = 'beta_2012'),
                                         upper = get_upper_lims(.x$Temp, .x$Average_speed, model_name = 'beta_2012'),
                                         supp_errors = 'Y',
                                         convergence_count = FALSE)),
         boatman = map(data, ~nls_multstart(Average_speed~boatman_2017(temp = Temp, rmax, tmin, tmax, a,b),
                                            data = .x,
                                            iter = c(4,4,4,4,4),
                                            start_lower = get_start_vals(.x$Temp, .x$Average_speed, model_name = 'boatman_2017') - 10,
                                            start_upper = get_start_vals(.x$Temp, .x$Average_speed, model_name = 'boatman_2017') + 10,
                                            lower = get_lower_lims(.x$Temp, .x$Average_speed, model_name = 'boatman_2017'),
                                            upper = get_upper_lims(.x$Temp, .x$Average_speed, model_name = 'boatman_2017'),
                                            supp_errors = 'Y',
                                            convergence_count = FALSE)),
         briere2 = map(data, ~nls_multstart(Average_speed~briere2_1999(temp = Temp, tmin, tmax, a,b),
                                            data = .x,
                                            iter = c(4,4,4,4),
                                            start_lower = get_start_vals(.x$Temp, .x$Average_speed, model_name = 'briere2_1999') - 10,
                                            start_upper = get_start_vals(.x$Temp, .x$Average_speed, model_name = 'briere2_1999') + 10,
                                            lower = get_lower_lims(.x$Temp, .x$Average_speed, model_name = 'briere2_1999'),
                                            upper = get_upper_lims(.x$Temp, .x$Average_speed, model_name = 'briere2_1999'),
                                            supp_errors = 'Y',
                                            convergence_count = FALSE)),
         delong = map(data, ~nls_multstart(Average_speed~delong_2017(temp = Temp, c, eb, ef, tm, ehc),
                                           data = .x,
                                           iter = c(4,4,4,4,4),
                                           start_lower = get_start_vals(.x$Temp, .x$Average_speed, model_name = 'delong_2017') - 10,
                                           start_upper = get_start_vals(.x$Temp, .x$Average_speed, model_name = 'delong_2017') + 10,
                                           lower = get_lower_lims(.x$Temp, .x$Average_speed, model_name = 'delong_2017'),
                                           upper = get_upper_lims(.x$Temp, .x$Average_speed, model_name = 'delong_2017'),
                                           supp_errors = 'Y',
                                           convergence_count = FALSE)),
         flinn = map(data, ~nls_multstart(Average_speed~flinn_1991(temp = Temp, a, b, c),
                                          data = .x,
                                          iter = c(5,5,5),
                                          start_lower = get_start_vals(.x$Temp, .x$Average_speed, model_name = 'flinn_1991') - 10,
                                          start_upper = get_start_vals(.x$Temp, .x$Average_speed, model_name = 'flinn_1991') + 10,
                                          lower = get_lower_lims(.x$Temp, .x$Average_speed, model_name = 'flinn_1991'),
                                          upper = get_upper_lims(.x$Temp, .x$Average_speed, model_name = 'flinn_1991'),
                                          supp_errors = 'Y',
                                          convergence_count = FALSE)),
         gaussian = map(data, ~nls_multstart(Average_speed~gaussian_1987(temp = Temp, rmax, topt, a),
                                             data = .x,
                                             iter = c(4,4,4),
                                             start_lower = get_start_vals(.x$Temp, .x$Average_speed, model_name = 'gaussian_1987') - 10,
                                             start_upper = get_start_vals(.x$Temp, .x$Average_speed, model_name = 'gaussian_1987') + 10,
                                             lower = get_lower_lims(.x$Temp, .x$Average_speed, model_name = 'gaussian_1987'),
                                             upper = get_upper_lims(.x$Temp, .x$Average_speed, model_name = 'gaussian_1987'),
                                             supp_errors = 'Y',
                                             convergence_count = FALSE)),
         hinshelwood = map(data, ~nls_multstart(Average_speed~hinshelwood_1947(temp = Temp, a, e, b, eh),
                                                data = .x,
                                                iter = c(5,5,5,5),
                                                start_lower = get_start_vals(.x$Temp, .x$Average_speed, model_name = 'hinshelwood_1947') - 1,
                                                start_upper = get_start_vals(.x$Temp, .x$Average_speed, model_name = 'hinshelwood_1947') + 1,
                                                lower = get_lower_lims(.x$Temp, .x$Average_speed, model_name = 'hinshelwood_1947'),
                                                upper = get_upper_lims(.x$Temp, .x$Average_speed, model_name = 'hinshelwood_1947'),
                                                supp_errors = 'Y',
                                                convergence_count = FALSE)),
         joehnk = map(data, ~nls_multstart(Average_speed~joehnk_2008(temp = Temp, rmax, topt, a, b, c),
                                           data = .x,
                                           iter = c(4,4,4,4, 4),
                                           start_lower = get_start_vals(.x$Temp, .x$Average_speed, model_name = 'joehnk_2008') - 10,
                                           start_upper = get_start_vals(.x$Temp, .x$Average_speed, model_name = 'joehnk_2008') + 10,
                                           lower = get_lower_lims(.x$Temp, .x$Average_speed, model_name = 'joehnk_2008'),
                                           upper = get_upper_lims(.x$Temp, .x$Average_speed, model_name = 'joehnk_2008'),
                                           supp_errors = 'Y',
                                           convergence_count = FALSE)),
         kamykowski = map(data, ~nls_multstart(Average_speed~kamykowski_1985(temp = Temp, tmin, tmax, a, b, c),
                                               data = .x,
                                               iter = c(4,4,4,4,4),
                                               start_lower = get_start_vals(.x$Temp, .x$Average_speed, model_name = 'kamykowski_1985') - 10,
                                               start_upper = get_start_vals(.x$Temp, .x$Average_speed, model_name = 'kamykowski_1985') + 10,
                                               lower = get_lower_lims(.x$Temp, .x$Average_speed, model_name = 'kamykowski_1985'),
                                               upper = get_upper_lims(.x$Temp, .x$Average_speed, model_name = 'kamykowski_1985'),
                                               supp_errors = 'Y',
                                               convergence_count = FALSE)),
         lactin2 = map(data, ~nls_multstart(Average_speed~lactin2_1995(temp = Temp, a, b, tmax, delta_t),
                                            data = .x,
                                            iter = c(4,4,4,4),
                                            start_lower = get_start_vals(.x$Temp, .x$Average_speed, model_name = 'lactin2_1995') - 10,
                                            start_upper = get_start_vals(.x$Temp, .x$Average_speed, model_name = 'lactin2_1995') + 10,
                                            lower = get_lower_lims(.x$Temp, .x$Average_speed, model_name = 'lactin2_1995'),
                                            upper = get_upper_lims(.x$Temp, .x$Average_speed, model_name = 'lactin2_1995'),
                                            supp_errors = 'Y',
                                            convergence_count = FALSE)),
         modifiedgaussian = map(data, ~nls_multstart(Average_speed~modifiedgaussian_2006(temp = Temp, rmax, topt, a, b),
                                                     data = .x,
                                                     iter = c(4,4,4,4),
                                                     start_lower = get_start_vals(.x$Temp, .x$Average_speed, model_name = 'modifiedgaussian_2006') - 10,
                                                     start_upper = get_start_vals(.x$Temp, .x$Average_speed, model_name = 'modifiedgaussian_2006') + 10,
                                                     lower = get_lower_lims(.x$Temp, .x$Average_speed, model_name = 'modifiedgaussian_2006'),
                                                     upper = get_upper_lims(.x$Temp, .x$Average_speed, model_name = 'modifiedgaussian_2006'),
                                                     supp_errors = 'Y',
                                                     convergence_count = FALSE)),
         oneill = map(data, ~nls_multstart(Average_speed~oneill_1972(temp = Temp, rmax, ctmax, topt, q10),
                                           data = .x,
                                           iter = c(4,4,4,4),
                                           start_lower = get_start_vals(.x$Temp, .x$Average_speed, model_name = 'oneill_1972') - 10,
                                           start_upper = get_start_vals(.x$Temp, .x$Average_speed, model_name = 'oneill_1972') + 10,
                                           lower = get_lower_lims(.x$Temp, .x$Average_speed, model_name = 'oneill_1972'),
                                           upper = get_upper_lims(.x$Temp, .x$Average_speed, model_name = 'oneill_1972'),
                                           supp_errors = 'Y',
                                           convergence_count = FALSE)),
         pawar = map(data, ~nls_multstart(Average_speed~pawar_2018(temp = Temp, r_tref, e, eh, topt, tref = 15),
                                          data = .x,
                                          iter = c(4,4,4,4),
                                          start_lower = get_start_vals(.x$Temp, .x$Average_speed, model_name = 'pawar_2018') - 10,
                                          start_upper = get_start_vals(.x$Temp, .x$Average_speed, model_name = 'pawar_2018') + 10,
                                          lower = get_lower_lims(.x$Temp, .x$Average_speed, model_name = 'pawar_2018'),
                                          upper = get_upper_lims(.x$Temp, .x$Average_speed, model_name = 'pawar_2018'),
                                          supp_errors = 'Y',
                                          convergence_count = FALSE)),
         quadratic = map(data, ~nls_multstart(Average_speed~quadratic_2008(temp = Temp, a, b, c),
                                              data = .x,
                                              iter = c(4,4,4),
                                              start_lower = get_start_vals(.x$Temp, .x$Average_speed, model_name = 'quadratic_2008') - 0.5,
                                              start_upper = get_start_vals(.x$Temp, .x$Average_speed, model_name = 'quadratic_2008') + 0.5,
                                              lower = get_lower_lims(.x$Temp, .x$Average_speed, model_name = 'quadratic_2008'),
                                              upper = get_upper_lims(.x$Temp, .x$Average_speed, model_name = 'quadratic_2008'),
                                              supp_errors = 'Y',
                                              convergence_count = FALSE)),
         ratkowsky = map(data, ~nls_multstart(Average_speed~ratkowsky_1983(temp = Temp, tmin, tmax, a, b),
                                              data = .x,
                                              iter = c(4,4,4,4),
                                              start_lower = get_start_vals(.x$Temp, .x$Average_speed, model_name = 'ratkowsky_1983') - 10,
                                              start_upper = get_start_vals(.x$Temp, .x$Average_speed, model_name = 'ratkowsky_1983') + 10,
                                              lower = get_lower_lims(.x$Temp, .x$Average_speed, model_name = 'ratkowsky_1983'),
                                              upper = get_upper_lims(.x$Temp, .x$Average_speed, model_name = 'ratkowsky_1983'),
                                              supp_errors = 'Y',
                                              convergence_count = FALSE)),
         rezende = map(data, ~nls_multstart(Average_speed~rezende_2019(temp = Temp, q10, a,b,c),
                                            data = .x,
                                            iter = c(4,4,4,4),
                                            start_lower = get_start_vals(.x$Temp, .x$Average_speed, model_name = 'rezende_2019') - 10,
                                            start_upper = get_start_vals(.x$Temp, .x$Average_speed, model_name = 'rezende_2019') + 10,
                                            lower = get_lower_lims(.x$Temp, .x$Average_speed, model_name = 'rezende_2019'),
                                            upper = get_upper_lims(.x$Temp, .x$Average_speed, model_name = 'rezende_2019'),
                                            supp_errors = 'Y',
                                            convergence_count = FALSE)),
         sharpeschoolhigh = map(data, ~nls_multstart(Average_speed~sharpeschoolhigh_1981(temp = Temp, r_tref,e,eh,th, tref = 15),
                                                     data = .x,
                                                     iter = c(4,4,4,4),
                                                     start_lower = get_start_vals(.x$Temp, .x$Average_speed, model_name = 'sharpeschoolhigh_1981') - 10,
                                                     start_upper = get_start_vals(.x$Temp, .x$Average_speed, model_name = 'sharpeschoolhigh_1981') + 10,
                                                     lower = get_lower_lims(.x$Temp, .x$Average_speed, model_name = 'sharpeschoolhigh_1981'),
                                                     upper = get_upper_lims(.x$Temp, .x$Average_speed, model_name = 'sharpeschoolhigh_1981'),
                                                     supp_errors = 'Y',
                                                     convergence_count = FALSE)),
                  spain = map(data, ~nls_multstart(Average_speed~spain_1982(temp = Temp, a,b,c,r0),
                                          data = .x,
                                          iter = c(4,4,4,4),
                                          start_lower = get_start_vals(.x$Temp, .x$Average_speed, model_name = 'spain_1982') - 1,
                                          start_upper = get_start_vals(.x$Temp, .x$Average_speed, model_name = 'spain_1982') + 1,
                                          lower = get_lower_lims(.x$Temp, .x$Average_speed, model_name = 'spain_1982'),
                                          upper = get_upper_lims(.x$Temp, .x$Average_speed, model_name = 'spain_1982'),
                                          supp_errors = 'Y',
                                          convergence_count = FALSE)),
         thomas1 = map(data, ~nls_multstart(Average_speed~thomas_2012(temp = Temp, a,b,c,topt),
                                            data = .x,
                                            iter = c(4,4,4,4),
                                            start_lower = get_start_vals(.x$Temp, .x$Average_speed, model_name = 'thomas_2012') - 1,
                                            start_upper = get_start_vals(.x$Temp, .x$Average_speed, model_name = 'thomas_2012') + 2,
                                            lower = get_lower_lims(.x$Temp, .x$Average_speed, model_name = 'thomas_2012'),
                                            upper = get_upper_lims(.x$Temp, .x$Average_speed, model_name = 'thomas_2012'),
                                            supp_errors = 'Y',
                                            convergence_count = FALSE)),
         thomas2 = map(data, ~nls_multstart(Average_speed~thomas_2017(temp = Temp, a,b,c,d,e),
                                            data = .x,
                                            iter = c(3,3,3,3,3),
                                            start_lower = get_start_vals(.x$Temp, .x$Average_speed, model_name = 'thomas_2017') - 10,
                                            start_upper = get_start_vals(.x$Temp, .x$Average_speed, model_name = 'thomas_2017') + 10,
                                            lower = get_lower_lims(.x$Temp, .x$Average_speed, model_name = 'thomas_2017'),
                                            upper = get_upper_lims(.x$Temp, .x$Average_speed, model_name = 'thomas_2017'),
                                            supp_errors = 'Y',
                                            convergence_count = FALSE)),
         weibull = map(data, ~nls_multstart(Average_speed~weibull_1995(temp = Temp, a,topt,b,c),
                                            data = .x,
                                            iter = c(4,4,4,4),
                                            start_lower = get_start_vals(.x$Temp, .x$Average_speed, model_name = 'weibull_1995') - 10,
                                            start_upper = get_start_vals(.x$Temp, .x$Average_speed, model_name = 'weibull_1995') + 10,
                                            lower = get_lower_lims(.x$Temp, .x$Average_speed, model_name = 'weibull_1995'),
                                            upper = get_upper_lims(.x$Temp, .x$Average_speed, model_name = 'weibull_1995'),
                                            supp_errors = 'Y',
                                            convergence_count = FALSE)))


glimpse(select(d_fits_25, 1:22))
d_fits_25$beta[[1]]

# stack models
d_stack <- select(d_fits_25, -data) %>%
  pivot_longer(., names_to = 'model_name', values_to = 'fit', beta:weibull)

# get parameters using tidy
params <- d_stack %>%
  mutate(., est = map(fit, tidy)) %>%
  select(-fit) %>%
  unnest(est)

# get predictions using augment
newdata <- tibble(Temp = seq(min(df25$Temp), max(df25$Temp), length.out = 100))
d_preds <- d_stack %>%
  mutate(., preds = map(fit, augment, newdata = newdata)) %>%
  select(-fit) %>%
  unnest(preds)

# write function to label ggplot2 panels
label_facets_num <- function(string){
  len <- length(string)
  string = paste('(', 1:len, ') ', string, sep = '')
  return(string)
}


# plot all models 
ggplot(d_preds, aes(Temp, Average_speed)) +
  geom_point(aes(Temp, Average_speed), df25) +
  geom_line(aes(Temp, .fitted), col = 'blue') +
  facet_wrap(~model_name, labeller = labeller(model_name = label_facets_num), scales = 'free', ncol = 4) +
  theme_bw(base_size = 12) +
  theme(legend.position = 'none',
        strip.text = element_text(hjust = 0),
        strip.background = element_blank()) +
  labs(x = 'Temperature (?C)',
       y = 'Average speed (mm/s)',
       title = 'Fits of every model available in rTPC') +
  geom_hline(aes(yintercept = 0), linetype = 2)

dev.off()
# plot all on same graph 

# take a random point from each model for labelling
d_labs <- filter(d_preds, Temp < 30) %>%
  group_by(., model_name) %>%
  sample_n(., 1) %>%
  ungroup()



ggplot(d_preds, aes(Temp, .fitted)) +
  geom_line(aes(col = model_name)) +
  geom_label_repel(aes(Temp, .fitted, label = model_name, col = model_name), fill = 'white', nudge_y = 0.8, segment.size = 0.2, segment.colour = 'grey50', d_labs) +
  geom_point(aes(Temp, Average_speed), df25) +
  theme_bw(base_size = 12) +
  theme(legend.position = 'none') +
  labs(x = 'Temperature (?C)',
       y = 'Average speed (mm/s)',
       title = 'Average speed across temperatures') +
  geom_hline(aes(yintercept = 0), linetype = 2) +
  scale_color_brewer(type = 'qual', palette = 2)

#Model selection 

library(MuMIn)

d_ic <- d_stack %>%
  mutate(., info = map(fit, glance),
         AICc =  map_dbl(fit, MuMIn::AICc)) %>%
  select(-fit) %>%
  unnest(info) %>%
  select(model_name, sigma, AIC, AICc, BIC, df.residual)

d_ic

write.csv(d_ic, "d_ic_average_speed_A25.csv", sep = ",")

# filter for best model()
best_model = filter(d_ic, AICc == min(AICc)) %>% pull(model_name)
best_model

# get colour code
col_best_mod = RColorBrewer::brewer.pal(n = 6, name = "Dark2")[6]

# plot
ggplot(d_preds, aes(Temp, .fitted)) +
  geom_line(aes(group = model_name), col = 'grey50', alpha = 0.5) +
  geom_line(data = filter(d_preds, model_name == best_model), col = col_best_mod) +
  geom_label_repel(aes(Temp, .fitted, label = model_name), fill = 'white', nudge_y = 0.8, segment.size = 0.2, segment.colour = 'grey50', data = filter(d_labs, model_name == best_model), col = col_best_mod) +
  geom_point(aes(Temp, Average_speed), df25) +
  theme_bw(base_size = 12) +
  theme(legend.position = 'none') +
  labs(x = 'Temperature (?C)',
       y = 'Average speed (mm/s)',
       title = 'Average speed across temperatures',
       subtitle= 'The Guassian model is the best model') +
  geom_hline(aes(yintercept = 0), linetype = 2) 

