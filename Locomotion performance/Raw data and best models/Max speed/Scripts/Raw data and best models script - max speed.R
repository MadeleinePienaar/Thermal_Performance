setwd("Documents/R/Locomotion performance/Raw data and best models/Maximum speed/Data/")

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

#Load data for average speed acclimation 18
df18 <- read.csv("Acclimation_25_Max_speed.csv", header= TRUE, sep= ";")

#changing column names from Temp to Temp18, etc. 
colnames(df18)[which(names(df18) == "Temp")] <- "Temp18"

# show the data
A18_raw <- ggplot(df18, aes(x = df18$Temp18, y = df18$Max_speed))+ 
  geom_point() +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (?C)',
       y = 'Maximum walking speed (mm/s)',
       title = 'Maximum walking speed across temperatures')

A18_raw

get_model_names()
# choose model
mod = 'ratkowsky_1983'

# get start vals
start_vals18 <- get_start_vals(x = df18$Temp18, y = df18$Max_speed, model_name = 'ratkowsky_1983')

# get limits 
lower_limits18 <- get_lower_lims(x = df18$Temp18, y = df18$Max_speed, model_name = 'ratkowsky_1983')
upper_limits18 <- get_upper_lims(x = df18$Temp18, y = df18$Max_speed, model_name = 'ratkowsky_1983')

start_vals18
lower_limits18
upper_limits18

# fit model
fit18 <- nls_multstart(Max_speed~ratkowsky_1983(temp = Temp18, tmin, tmax, a, b),data = df18,iter = 500, start_lower = start_vals18 - 10, start_upper = start_vals18 + 10, lower = lower_limits18, upper = upper_limits18, supp_errors = 'Y')

fit18

# calculate additional traits
calc_params(fit18) %>%
  # round for easy viewing
  mutate_all(round, 2)


# predict new data
new_data18 <- data.frame(Temp = seq(min(df18$Temp18), max(df18$Temp18), 0.5))
preds18 <- augment(fit18, new_data = new_data18)

# plot data and model fit
A18_fit <- ggplot(df18, aes(Temp18, Max_speed)) +
  geom_point() +
  geom_smooth(aes(Temp18, .fitted), preds18, col = 'blue') +
  theme_bw(base_size = 12) +
  theme(plot.margin = unit(c(5.5, 0, 5.5, 0), "pt"))+
  theme(axis.ticks.y = element_blank(), axis.text.y=element_blank())+
  scale_x_continuous(minor_breaks= c(8,13,18,23,28,33,38), breaks=c(8,13,18,23,28,33,38), labels = c("8", "13", "18", "23", "28", "33", "38"))+ 
  labs(x = 'Temperature (?C)',
       y = 'Maximum walking speed (mm/s)',
       title = 'Maximum walking speed across temperatures')

A18_fit

# OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO
# OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO


#Load data for average speed acclimation 25
df25 <- read.csv("Acclimation_32_Max_speed.csv", header= TRUE, sep= ";")

colnames(df25)[which(names(df25) == "Temp")] <- "Temp25"

# show the data
A25_raw <- ggplot(df25, aes(x = df25$Temp25, y = df25$Max_speed))+ 
  geom_point() +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (?C)',
       y = 'Maximum walking speed (mm/s)',
       title = 'Maximum walking speed across temperatures')

A25_raw

get_model_names()
# choose model
mod = 'ratkowsky_1983'

# get start vals
start_vals25 <- get_start_vals(x = df25$Temp25, y = df25$Max_speed, model_name = 'ratkowsky_1983')

# get limits 
lower_limits25 <- get_lower_lims(x = df25$Temp25, y = df25$Max_speed, model_name = 'ratkowsky_1983')
upper_limits25 <- get_upper_lims(x = df25$Temp25, y = df25$Max_speed, model_name = 'ratkowsky_1983')

start_vals25
lower_limits25
upper_limits25

# fit model
fit25 <- nls_multstart(Max_speed~ratkowsky_1983(temp = Temp25, tmin, tmax, a, b),data = df25,iter = 500, start_lower = start_vals25 - 10, start_upper = start_vals25 + 10, lower = lower_limits25, upper = upper_limits25, supp_errors = 'Y')

fit25 

# calculate additional traits
calc_params(fit25) %>%
  # round for easy viewing
  mutate_all(round, 2)


# predict new data
new_data25 <- data.frame(Temp = seq(min(df25$Temp25), max(df25$Temp25), 0.5))
preds25 <- augment(fit25, new_data = new_data25)

# plot data and model fit1
A25_fit <-ggplot(df25, aes(Temp25, df25$Max_speed)) +
  geom_point() +
  geom_smooth(aes(Temp25, .fitted), preds25, col = 'green') +
  theme_bw(base_size = 12) +
  theme(plot.margin = unit(c(5.5, 0, 5.5, 0), "pt"))+
  theme(axis.ticks.y = element_blank(), axis.text.y=element_blank())+
  scale_x_continuous(minor_breaks= c(8,13,18,23,28,33,38), breaks=c(8,13,18,23,28,33,38), labels = c("8", "13", "18", "23", "28", "33", "38"))+ 
  labs(x = 'Temperature (?C)',
       y = '',
       title = '')

A25_fit

# oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo


#Load data for average speed acclimation 32
df32 <- read.csv("Acclimation_32_Max_speed_plus_anchor.csv", header= TRUE, sep= ";")

colnames(df32)[which(names(df32) == "Temp")] <- "Temp32"

# show the data
A32_raw <- ggplot(df32, aes(x = df32$Temp32, y = df32$Max_speed))+ 
  geom_point() +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (?C)',
       y = 'Max walking speed (mm/s)',
       title = 'Max walking speed across temperatures')

A32_raw

get_model_names()
# choose model
mod = 'ratkowsky_1983'

# get start vals
start_vals32 <- get_start_vals(x = df32$Temp32, y = df32$Max_speed, model_name = 'ratkowsky_1983')

# get limits 
  lower_limits32 <- get_lower_lims(x = df32$Temp32, y = df32$Max_speed, model_name = 'ratkowsky_1983')
  upper_limits32 <- get_upper_lims(x = df32$Temp32, y = df32$Max_speed, model_name = 'ratkowsky_1983')

start_vals32
lower_limits32
upper_limits32

# fit model
fit32 <- nls_multstart(Max_speed~ratkowsky_1983(temp = Temp32, tmin, tmax, a, b),data = df32,iter = 500, start_lower = start_vals32 - 10, start_upper = start_vals32 + 10, lower = lower_limits32, upper = upper_limits32, supp_errors = 'Y')

fit32

# calculate additional traits
calc_params(fit32) %>%
  # round for easy viewing
  mutate_all(round, 2)


# predict new data
new_data32 <- data.frame(Temp = seq(min(df32$Temp32), max(df32$Temp32), 0.5))
preds32 <- augment(fit32, new_data = new_data32)

# plot data and model fit1
A32_fit <- ggplot(df32, aes(Temp32, df32$Max_speed)) +
  geom_point() +
  geom_smooth(aes(Temp32, .fitted), preds32, col = 'blue') +
  theme_bw(base_size = 12) +
  scale_x_continuous(minor_breaks= c(8,13,18,23,28,33,38), breaks=c(8,13,18,23,28,33,38), labels = c("8", "13", "18", "23", "28", "33", "38"))+ 
  labs(x = 'Temperature (?C)',
       y = 'Max walking speed (mm/s)',
       title = 'Max walking speed across temperatures')
 
A32_fit

# OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO
# OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO

# Overlay A18, A25 and A32


ggplot() +
  geom_point(data=df18, aes(df18$Temp, df18$Max_speed, col= "blue"))+
  geom_point(data=df25, aes(df25$Temp, df25$Max_speed, col= "green"))+
  geom_point(data=df32, aes(df32$Temp, df32$Max_speed, col= "red"))+
  scale_color_identity(guide = "legend", name = "Acclimation", label = c("18 ?C", "25 ?C", "32 ?C"))+
  geom_smooth(aes(Temp18, .fitted), preds18, col = "royalblue2")+
  geom_smooth(aes(Temp25, .fitted), preds25, col = "limegreen")+
  geom_smooth(aes(Temp32, .fitted), preds32, col = "red1") +
  theme_bw(base_size = 12)+
  scale_x_continuous(minor_breaks= c(8,13,18,23,28,33,38), breaks=c(8,13,18,23,28,33,38), labels = c("8", "13", "18", "23", "28", "33", "38"))+
  labs(x = 'Temperature (?C)',
     y = 'Maximum walking speed (mm/s)',
     title = 'Ratkowsky model fit on raw data of PSHB maximum speed across three acclimations')
  
#save size: width: 800, breadth: 600


