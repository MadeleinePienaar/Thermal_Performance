setwd("Documents/R/Locomotion performance/Average speed/A25/Data/")

#load packages 
library(lsr)
library(ggplot2)
library(psych)
library(gplots)
library(dplyr)
library(rTPC)
library(nls.multstart)
library(broom)
library(tidyverse)

#Load data 
df25 <- read.csv("Average_speed_A25.csv", header= TRUE, sep= ";")

# show the data
ggplot(df25, aes(x = df25$Temp, y = df25$Average_speed))+ 
  geom_point() +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (?C)',
       y = 'Average walking speed (mm/s)',
       title = 'Average walking speed across temperatures')

get_model_names()
# choose model
mod = 'gaussian_1987'

# get start vals
start_vals <- get_start_vals(x = df25$Temp, y = df25$Average_speed, model_name = 'gaussian_1987')

# get limits 
lower_limits <- get_lower_lims(x = df25$Temp, y = df25$Average_speed, model_name = 'gaussian_1987')
upper_limits <- get_upper_lims(x = df25$Temp, y = df25$Average_speed, model_name = 'gaussian_1987')

start_vals
lower_limits
upper_limits

# fit model
fit <- nls_multstart(Average_speed~gaussian_1987(temp = Temp, rmax, topt, a),data = df25,iter = 500, start_lower = start_vals - 10, start_upper = start_vals + 10, lower = lower_limits, upper = upper_limits, supp_errors = 'Y')

fit 

# calculate additional traits
calc_params(fit) %>%
  # round for easy viewing
  mutate_all(round, 2)


# predict new data
new_data <- data.frame(Temp = seq(min(df25$Temp), max(df25$Temp), 0.5))
preds <- augment(fit, newdata = new_data)

# plot data and model fit1
ggplot(df25, aes(Temp, df25$Average_speed)) +
  geom_point() +
  geom_line(aes(Temp, .fitted), preds, col = 'blue') +
  theme_bw(base_size = 12) +
  scale_x_continuous(minor_breaks= c(8,13,18,23,28,33,38), breaks=c(8,13,18,23,28,33,38), labels = c("8", "13", "18", "23", "28", "33", "38"))+ 
  labs(x = 'Temperature (?C)',
       y = 'Average walking speed (mm/s)',
       title = '25?C Acclimated PSHB average walking speed across temperatures')

