setwd("Documents/R/Locomotion performance/Raw data and best models/Time spent travelling/Data/")

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
library(patchwork)

#Load data for average speed acclimation 18
df18 <- read.csv("Time_spent_travelling_A18 - log(time+1) + anchors.csv", header= TRUE, sep= ";")

#changing column names from Temp to Temp18, etc. 
colnames(df18)[which(names(df18) == "Temp")] <- "Temp18"

# show the data
A18_raw <- ggplot(df18, aes(x = df18$Temp18, y = df18$Time_travelling))+ 
  geom_point() +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (?C)',
       y = 'Proportion of time spent travelling (%)',
       title = 'Time spent travelling across temperatures')

A18_raw

get_model_names()
# choose model
mod = 'thomas_2017'

# get start vals
start_vals18 <- get_start_vals(x = df18$Temp18, y = df18$Time_travelling, model_name = 'thomas_2017')

# get limits 
lower_limits18 <- get_lower_lims(x = df18$Temp18, y = df18$Time_travelling, model_name = 'thomas_2017')
upper_limits18 <- get_upper_lims(x = df18$Temp18, y = df18$Time_travelling, model_name = 'thomas_2017')

start_vals18
lower_limits18
upper_limits18

# fit model
fit18 <- nls_multstart(Time_travelling~thomas_2017(temp = Temp18, a,b,c,d,e),data = df18,iter = 500, start_lower = start_vals18 - 10, start_upper = start_vals18 + 10, lower = lower_limits18, upper = upper_limits18, supp_errors = 'Y')

fit18

# calculate additional traits
calc_params(fit18) %>%
  # round for easy viewing
  mutate_all(round, 2)


# predict new data
new_data18 <- data.frame(Temp = seq(min(df18$Temp18), max(df18$Temp18), 0.5))
preds18 <- augment(fit18, new_data = new_data18)

# plot data and model fit
A18_fit <- ggplot(df18, aes(Temp18, Time_travelling)) +
  geom_point() +
  geom_smooth(aes(Temp18, .fitted), preds18, col = 'blue') +
  theme_bw(base_size = 12) +
  ylim(0,1.5)+
  theme(plot.margin = unit(c(5.5, 0.8, 5.5, 0), "pt"))+
  theme(axis.ticks.y = element_blank(), axis.text.y=element_blank())+
  scale_x_continuous(minor_breaks= c(8,13,18,23,28,33,38,45), breaks=c(8,13,18,23,28,33,38,45), labels = c("8","13", "18", "23", "28", "33", "38","45"))+ 
  labs(x = '',
       y = 'Proportion of time spent travelling \n(log(minutes travelling+1))',
       title = '')

A18_fit

# OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO
# OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO


#Load data for average speed acclimation 25
df25 <- read.csv("Time_spent_travelling_A25 - log(time+1) + anchors.csv", header= TRUE, sep= ";")

colnames(df25)[which(names(df25) == "Temp")] <- "Temp25"

# show the data
A25_raw <- ggplot(df25, aes(x = df25$Temp25, y = df25$Time_travelling))+ 
  geom_point() +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (?C)',
       y = 'Time travel (%)',
       title = 'Time travelling across temperatures')

A25_raw

get_model_names()
# choose model
mod = 'thomas_2012'

# get start vals
start_vals25 <- get_start_vals(x = df25$Temp25, y = df25$Time_travelling, model_name = 'thomas_2012')

# get limits 
lower_limits25 <- get_lower_lims(x = df25$Temp25, y = df25$Time_travelling, model_name = 'thomas_2012')
upper_limits25 <- get_upper_lims(x = df25$Temp25, y = df25$Time_travelling, model_name = 'thomas_2012')

start_vals25
lower_limits25
upper_limits25

# fit model
fit25 <- nls_multstart(Time_travelling~thomas_2012(temp = Temp25, a, b, c, topt),data = df25,iter = 500, start_lower = start_vals25 - 10, start_upper = start_vals25 + 10, lower = lower_limits25, upper = upper_limits25, supp_errors = 'Y')

fit25 

# calculate additional traits
calc_params(fit25) %>%
  # round for easy viewing
  mutate_all(round, 2)


# predict new data
new_data25 <- data.frame(Temp = seq(min(df25$Temp25), max(df25$Temp25), 0.5))
preds25 <- augment(fit25, new_data = new_data25)

# plot data and model fit1
A25_fit <-ggplot(df25, aes(df25$Temp25, df25$Time_travelling)) +
  geom_point() +
  geom_smooth(aes(Temp25, .fitted), preds25, col = 'green') +
  theme_bw(base_size = 12) +
  ylim(0,1.5)+
  theme(plot.margin = unit(c(5.5, 0.5, 5.5, 0), "pt"))+
  theme(axis.ticks.y = element_blank(), axis.text.y=element_blank())+
  scale_x_continuous(minor_breaks= c(8,13,18,23,28,33,38, 45), breaks=c(8,13,18,23,28,33,38, 45), labels = c("8", "13", "18", "23", "28", "33", "38", "45"))+ 
  labs(x = 'Temperature (?C)',
       y = '',
       title = '')


A25_fit

# oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo


#Load data for average speed acclimation 32
df32 <- read.csv("Time_spent_travelling_A32 - log(time+1) + anchors.csv", header= TRUE, sep= ";")

colnames(df32)[which(names(df32) == "Temp")] <- "Temp32"

# show the data
A32_raw <- ggplot(df32, aes(x = df32$Temp32, y = df32$Time_travelling))+ 
  geom_point() +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (?C)',
       y = 'Proportion of time spent travelling (log(minutes travelling+1))',
       title = 'Time spent travelling across temperatures')

A32_raw

get_model_names()
# choose model
mod = 'thomas_2012'

# get start vals
start_vals32 <- get_start_vals(x = df32$Temp32, y = df32$Time_travelling, model_name = 'thomas_2012')

# get limits 
lower_limits32 <- get_lower_lims(x = df32$Temp32, y = df32$Time_travelling, model_name = 'thomas_2012')
upper_limits32 <- get_upper_lims(x = df32$Temp32, y = df32$Time_travelling, model_name = 'thomas_2012')

start_vals32
lower_limits32
upper_limits32

# fit model
fit32 <- nls_multstart(Time_travelling~thomas_2012(temp = Temp32, a, b, c, topt),data = df32,iter = 500, start_lower = start_vals32 - 12, start_upper = start_vals32 + 10, lower = lower_limits32, upper = upper_limits32, supp_errors = 'Y')

fit32

# calculate additional traits
calc_params(fit32) %>%
  # round for easy viewing
  mutate_all(round, 2)


# predict new data
new_data32 <- data.frame(Temp = seq(min(df32$Temp32), max(df32$Temp32), 0.5))
preds32 <- augment(fit32, new_data = new_data32)

# plot data and model fit1
A32_fit <- ggplot(df32, aes(df32$Temp32, df32$Time_travelling)) +
  geom_point() +
  geom_smooth(aes(Temp32, .fitted), preds32, col = 'red') +
  theme_bw(base_size = 12) +
  theme(plot.margin = unit(c(5.5, 0.5, 5.5, 0), "pt"))+
  theme(axis.ticks.y = element_blank(), axis.text.y=element_blank())+
  scale_x_continuous(minor_breaks= c(8,13,18,23,28,33,38, 45), breaks=c(8,13,18,23,28,33,38,45), labels = c("8","13", "18", "23", "28", "33", "38", "45"))+ 
  labs(x = '',
       y = '',
       title = '')

A32_fit

# OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO
# OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO

# Overlay A18, A25 and A32


ggplot() +
  geom_point(data=df18, aes(df18$Temp, df18$Time_travelling, col= "blue"))+
  geom_point(data=df25, aes(df25$Temp, df25$Time_travelling, col= "green"))+
  geom_point(data=df32, aes(df32$Temp, df32$Time_travelling, col= "red"))+
  scale_color_identity(guide = "legend", name = "Acclimation", label = c("18 ?C", "25 ?C", "32 ?C"))+
  geom_smooth(aes(Temp18, .fitted), preds18, col = "royalblue2")+
  geom_smooth(aes(Temp25, .fitted), preds25, col = "limegreen")+
  geom_smooth(aes(Temp32, .fitted), preds32, col = "red1") +
  theme_bw(base_size = 12)+
  scale_x_continuous(minor_breaks= c(8,13,18,23,28,33,38,45), breaks=c(8,13,18,23,28,33,38,45), labels = c("8", "13", "18", "23", "28", "33", "38","45"))+
  labs(x = 'Temperature (?C)',
       y = 'Proportion of time spent travelling (%)',
       title = 'Best model fit on raw data of PSHB time spent travelling across three acclimations')

#save size: width: 800, breadth: 600

#Separate panes A18, A25, A32


A18_fit
A25_fit
A32_fit

A18_fit+A25_fit+A32_fit+ plot_layout(nrow = 1, ncol = 3)

#Saved at width: 1200; height: 400
