setwd("Documents/R/Locomotion performance/Confidence intervals/Data")

#Load packages
library(ggplot2)
library(ggthemes)
library(multcompView)
library(dplyr)

#Distance travelled 
# load in data 
dt <- read.csv("all parameters_distance travelled.csv", header = TRUE, sep= ";")

dt$Acclimation <- as.character(dt$Acclimation)


p1 <- ggplot(dt) + 
  
  geom_errorbar(aes(y = Acclimation, xmin = lowerTmax, xmax = upperTmax, col = 'Red'),size = 0.5, width = 0.4) +
  geom_point(aes(y = Acclimation, x = Tmax, size = 0.01, col = 'Red')) +
  geom_errorbar(aes(y = Acclimation, xmin = lowerTopt, xmax = upperTopt, col = 'Green'), size = 0.5, width = 0.4) +
  geom_point(aes(y = Acclimation, x = Topt, size = 0.01, col = 'Green')) + 
  geom_errorbar(aes(y = Acclimation, xmin = lowerTmin, xmax = upperTmin, col = 'Blue'), size = 0.5, width = 0.4) +
  geom_point(aes(y = Acclimation, x = Tmin, size = 0.01, col = 'Blue')) + 
  labs(x = 'Temperature (?C)', 
       y = 'Acclimation \n      temperature (?C)',
       title = 'Distance travelled')  +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=20))+
  theme(legend.position = "right")+
  theme(title = element_text(size=22))+
  theme(legend.title = element_blank())+ 
  scale_size(guide='none')+
  scale_color_manual(labels = c("CTmin", "Topt", "CTmax"), values = c("Blue", "Green", "Red")) +
  theme(legend.text = element_text(size=18))+
  geom_text(aes(y= Acclimation, x = 20, label = ''))+
  geom_text(aes(y= Acclimation, x = 33.5, label = ''))+
  geom_text(aes(y= 3, x = 57, label = ''))+
  geom_text(aes(y= 2, x = 57, label = ''))+
  geom_text(aes(y= 1, x = 57, label = ''))+
  geom_line(aes(x=x, y=y), data=tibble(x=20, y=c(0.8, 3.2)))+
  geom_line(aes(x=x, y=y), data=tibble(x=33.3, y=c(0.8, 3.2)))+
  geom_line(aes(x=x, y=y), data=tibble(x=57, y=c(0.8, 2.2)))+
  geom_text(aes(x=x,y=y),data=tibble(x=22,y=2),label="NS")+
  geom_text(aes(x=x,y=y),data=tibble(x=35.3,y=2),label="NS")+
  geom_text(aes(x=x,y=y),data=tibble(x=59,y=1.5),label="*", size=6)
  
p1

#Tbr 

p2 <- ggplot(dt) + 
  geom_errorbar(aes(y = Acclimation, xmin = lowerTbr, xmax = upperTbr, size = 0.005), width = 0.4) +
  geom_point(aes(y = Acclimation, x = Tbr, size= 0.01)) + 
  labs(x = 'Tbr (?C)', 
       y = 'Acclimation \n      temperature (?C)', 
       title = 'Distance travelled')+ 
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=20))+
  theme(legend.position = "none")+
  theme(title = element_text(size=22))+
  geom_text(aes(y= 3, x = 12.5, label = ''))+
  geom_text(aes(y= 2, x = 12.5, label = ''))+
  geom_text(aes(y= 1, x = 12.5, label = ''))+
  geom_line(aes(x=x, y=y), data=tibble(x=12.5, y=c(0.8, 2.2)))+
  geom_text(aes(x=x,y=y),data=tibble(x=13,y=1.5),label="*", size=6)

p2
# Rmax 


p3 <- ggplot(dt) + 
  geom_errorbar(aes(y = Acclimation, xmin = lowerRmax, xmax = upperRmax, size = 0.005), width = 0.4) +
  geom_point(aes(y = Acclimation, x = Rmax, size= 0.01)) + 
  labs(x = 'Umax: Distance travelled (mm)', 
       y = 'Acclimation \n      temperature (?C)', 
       title = 'Distance travelled')+ 
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=20))+
  theme(legend.position = "none")+
  theme(title = element_text(size=22))+
  geom_text(aes(y= Acclimation, x = 5200, label = ''))+
  geom_line(aes(x=x, y=y), data=tibble(x=5200, y=c(0.8, 3.2)))+
  geom_text(aes(x=x,y=y),data=tibble(x=5350,y=2),label="NS")
  

p3

#Average speed 
# load in data (average speed)
as <- read.csv("all parameters_average_speed.csv", header = TRUE, sep= ";")

as$Acclimation <- as.character(as$Acclimation)


p4 <- ggplot(as) + 
  
  geom_errorbar(aes(y = Acclimation, xmin = lowerTmax, xmax = upperTmax, col = 'Red'),size = 0.5, width = 0.4) +
  geom_point(aes(y = Acclimation, x = Tmax, size = 0.01, col = 'Red')) +
  geom_errorbar(aes(y = Acclimation, xmin = lowerTopt, xmax = upperTopt, col = 'Green'), size = 0.5, width = 0.4) +
  geom_point(aes(y = Acclimation, x = Topt, size = 0.01, col = 'Green')) + 
  geom_errorbar(aes(y = Acclimation, xmin = lowerTmin, xmax = upperTmin, col = 'Blue'), size = 0.5
                , width = 0.4) +
  geom_point(aes(y = Acclimation, x = Tmin, size = 0.01, col = 'Blue')) + 
  labs(x = 'Temperature (?C)', 
       y = 'Acclimation \n      temperature (?C)',
       title = 'Average speed')  +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=20))+
  theme(legend.position = "right")+
  theme(title = element_text(size=22))+
  theme(legend.title = element_blank())+ 
  scale_size(guide='none')+
  scale_color_manual(labels = c("CTmin", "Topt", "CTmax"), values = c("Blue", "Green", "Red")) +
  theme(legend.text = element_text(size=18))+
  geom_text(aes(y= 3, x = 18, label = ''))+
  geom_text(aes(y= 2, x = 18, label = ''))+
  geom_text(aes(y= 1, x = 18, label = ''))+
  geom_text(aes(y= Acclimation, x = 33, label = ''))+
  geom_text(aes(y= 3, x = 58, label = ''))+
  geom_text(aes(y= 2, x = 58, label = ''))+
  geom_text(aes(y= 1, x = 58, label = ''))+
  geom_line(aes(x=x, y=y), data=tibble(x=18, y=c(0.8, 2.2)))+
  geom_line(aes(x=x, y=y), data=tibble(x=33.3, y=c(0.8, 3.2)))+
  geom_line(aes(x=x, y=y), data=tibble(x=58, y=c(0.8, 2.2)))+
  geom_text(aes(x=x,y=y),data=tibble(x=20,y=1.5),label="*", size=6)+
  geom_text(aes(x=x,y=y),data=tibble(x=35.3,y=2),label="NS")+
  geom_text(aes(x=x,y=y),data=tibble(x=60,y=1.5),label="*", size=6)

p4

#Tbr 

p5 <- ggplot(as) + 
  geom_errorbar(aes(y = Acclimation, xmin = lowerTbr, xmax = upperTbr, size = 0.005), width = 0.4) +
  geom_point(aes(y = Acclimation, x = Tbr, size= 0.01)) + 
  labs(x = 'Tbr (?C)', 
       y = 'Acclimation \n      temperature (?C)', 
       title = 'Average speed')+ 
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=20))+
  theme(legend.position = "none")+
  theme(title = element_text(size=22))+
  geom_text(aes(y= 3, x = 13.5, label = ''))+
  geom_text(aes(y= 2, x = 13.5, label = ''))+
  geom_text(aes(y= 1, x = 13.5, label = ''))+
  geom_line(aes(x=x, y=y), data=tibble(x=13.6, y=c(0.8, 2.2)))+
  geom_text(aes(x=x,y=y),data=tibble(x=14.1,y=1.5),label="*", size=6)


p5

# Rmax 


p6 <- ggplot(as) + 
  geom_errorbar(aes(y = Acclimation, xmin = lowerRmax, xmax = upperRmax, size = 0.005), width = 0.4) +
  geom_point(aes(y = Acclimation, x = Rmax, size= 0.01)) + 
  labs(x = 'Umax: Average speed (mm/s)', 
       y = 'Acclimation \n      temperature (?C)', 
       title = 'Average speed')+ 
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=20))+
  theme(legend.position = "none")+
  theme(title = element_text(size=22))+
  geom_text(aes(y=Acclimation, x= 2.4, label=''))+
  geom_line(aes(x=x, y=y), data=tibble(x=2.44, y=c(0.8, 3.2)))+
  geom_text(aes(x=x,y=y),data=tibble(x=2.52,y=2),label="NS")

p6


#size = 1500 

#Time spent travelling
# load in data (time spent travelling)
tt <- read.csv("all_parameters_time_travelling.csv", header = TRUE, sep= ";")

tt$Acclimation <- as.character(tt$Acclimation)


p7 <- ggplot(tt) + 
  
  geom_errorbar(aes(y = Acclimation, xmin = lowerTmax, xmax = upperTmax, col = 'Red'),size = 0.5, width = 0.4) +
  geom_point(aes(y = Acclimation, x = Tmax, size = 0.01, col = 'Red')) +
  geom_errorbar(aes(y = Acclimation, xmin = lowerTopt, xmax = upperTopt, col = 'Green'), size = 0.5, width = 0.4) +
  geom_point(aes(y = Acclimation, x = Topt, size = 0.01, col = 'Green')) + 
  geom_errorbar(aes(y = Acclimation, xmin = lowerTmin, xmax = upperTmin, col = 'Blue'), size = 0.5, width = 0.4) +
  geom_point(aes(y = Acclimation, x = Tmin, size = 0.01, col = 'Blue')) + 
  labs(x = 'Temperature (?C)', 
       y = 'Acclimation \n      temperature (?C)',
       title = 'Proportion of time spent travelling')  +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=20))+
  theme(legend.position = "right")+
  theme(title = element_text(size=22))+
  theme(legend.title = element_blank())+ 
  scale_size(guide='none')+
  scale_color_manual(labels = c("CTmin", "Topt", "CTmax"), values = c("Blue", "Green", "Red")) +
  theme(legend.text = element_text(size=18))+
  geom_text(aes(y= 3, x = 15, label = ''))+
  geom_text(aes(y= 2, x = 15, label = ''))+
  geom_text(aes(y= 1, x = 15, label = ''))+
  geom_text(aes(y= 3, x = 34, label = ''))+
  geom_text(aes(y= 2, x = 34, label = ''))+
  geom_text(aes(y= 1, x = 34, label = ''))+
  geom_text(aes(y= 3, x = 50, label = ''))+
  geom_text(aes(y= 2, x = 50, label = ''))+
  geom_text(aes(y= 1, x = 50, label = ''))+
  
    geom_line(aes(x=x, y=y), data=tibble(x=15, y=c(0.8, 2.2)))+
  geom_line(aes(x=x, y=y), data=tibble(x=34, y=c(1.8, 3.2)))+
  geom_line(aes(x=x, y=y), data=tibble(x=37.5, y=c(0.8, 3.2)))+
  geom_line(aes(x=x, y=y), data=tibble(x=49, y=c(0.8, 2.2)))+
  geom_line(aes(x=x, y=y), data=tibble(x=52, y=c(0.8, 3.2)))+
  
  geom_text(aes(x=x,y=y),data=tibble(x=17,y=1.5),label="*", size=6)+
  geom_text(aes(x=x,y=y),data=tibble(x=36,y=2.5),label="*", size=6)+
  geom_text(aes(x=x,y=y),data=tibble(x=39.5,y=2),label="*", size=6)+
  geom_text(aes(x=x,y=y),data=tibble(x=51,y=1.5),label="*", size=6)+
  geom_text(aes(x=x,y=y),data=tibble(x=54,y=2),label="*", size=6)

p7

#Tbr 

p8 <- ggplot(tt) + 
  geom_errorbar(aes(y = Acclimation, xmin = lowerTbr, xmax = upperTbr, size = 0.005), width = 0.4) +
  geom_point(aes(y = Acclimation, x = Tbr, size= 0.1)) + 
  labs(x = 'Tbr (?C)', 
       y = 'Acclimation \n      temperature (?C)', 
       title = 'Proportion of time spent\ntravelling')+ 
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=20))+
  theme(legend.position = "none")+
  theme(title = element_text(size=22))+
  geom_text(aes(y= 3, x = 24.2, label = ''))+
  geom_text(aes(y= 2, x = 24.2, label = ''))+
  geom_text(aes(y= 1, x = 24.2, label = ''))+
  geom_line(aes(x=x, y=y), data=tibble(x=24.2, y=c(0.8, 3.2)))+
  geom_text(aes(x=x,y=y),data=tibble(x=24.7,y=2),label="*", size=6)


p8

# Rmax 

p9 <- ggplot(tt) + 
  geom_errorbar(aes(y = Acclimation, xmin = lowerRmax, xmax = upperRmax, size = 0.005), width = 0.4) +
  geom_point(aes(y = Acclimation, x = Rmax, size= 0.01)) + 
  labs(x = 'Umax: Proportion of time spent travelling \n (log(minutes travelling+1)', 
       y = 'Acclimation \n      temperature (?C)', 
       title = 'Proportion of time spent\ntravelling')+ 
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=20))+
  theme(legend.position = "none")+
  theme(title = element_text(size=22))+
  geom_text(aes(y= 3, x = 1.38, label = ''))+
  geom_text(aes(y= 2, x = 1.38, label = ''))+
  geom_text(aes(y= 1, x = 1.38, label = ''))+
  geom_line(aes(x=x, y=y), data=tibble(x=1.38, y=c(1.8, 3.2)))+
  geom_text(aes(x=x,y=y),data=tibble(x=1.4,y=2.5),label="*", size=6)

p9


#Max speed
# load in data
ms <- read.csv("all_parameters_max_speed.csv", header = TRUE, sep= ";")

ms$Acclimation <- as.character(ms$Acclimation)


p10 <- ggplot(ms) + 
  
  geom_errorbar(aes(y = Acclimation, xmin = lowerTmax, xmax = upperTmax, col = 'Red'),size = 0.5, width = 0.4) +
  geom_point(aes(y = Acclimation, x = Tmax, size = 0.01, col = 'Red')) +
  geom_errorbar(aes(y = Acclimation, xmin = lowerTopt, xmax = upperTopt, col = 'Green'), size = 0.5, width = 0.4) +
  geom_point(aes(y = Acclimation, x = Topt, size = 0.01, col = 'Green')) + 
  geom_errorbar(aes(y = Acclimation, xmin = lowerTmin, xmax = upperTmin, col = 'Blue'), size = 0.5, width = 0.4) +
  geom_point(aes(y = Acclimation, x = Tmin, size = 0.01, col = 'Blue')) + 
  labs(x = 'Temperature (?C)', 
       y = 'Acclimation \n      temperature (?C)',
       title = 'Maximum speed')  +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=20))+
  theme(legend.position = "right")+
  theme(title = element_text(size=22))+
  theme(legend.title = element_blank())+ 
  scale_size(guide='none')+
  scale_color_manual(labels = c("CTmin", "Topt", "CTmax"), values = c("Blue", "Green", "Red")) +
  theme(legend.text = element_text(size=18))+
  geom_text(aes(y= Acclimation, x = 16.5, label = ''))+
  geom_text(aes(y= Acclimation, x = 38, label = ''))+
  geom_text(aes(y= Acclimation, x = 46.8, label = ''))+
  
  geom_line(aes(x=x, y=y), data=tibble(x=16, y=c(0.8, 3.2)))+
  geom_line(aes(x=x, y=y), data=tibble(x=38, y=c(0.8, 3.2)))+
  geom_line(aes(x=x, y=y), data=tibble(x=46.5, y=c(0.8, 3.2)))+
  
  geom_text(aes(x=x,y=y),data=tibble(x=18,y=2),label="NS")+
  geom_text(aes(x=x,y=y),data=tibble(x=40,y=2),label="NS")+
  geom_text(aes(x=x,y=y),data=tibble(x=48.5,y=2),label="NS")
  

p10

#Tbr 

p11 <- ggplot(ms) + 
  geom_errorbar(aes(y = Acclimation, xmin = lowerTbr, xmax = upperTbr, size = 0.005), width = 0.4) +
  geom_point(aes(y = Acclimation, x = Tbr, size= 0.01)) + 
  labs(x = 'Tbr (?C)', 
       y = 'Acclimation \n      temperature (?C)', 
       title = 'Maximum speed')+ 
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=20))+
  theme(legend.position = "none")+
  theme(title = element_text(size=22))+
  geom_text(aes(y= Acclimation, x = 13.3, label = ''))+
  geom_line(aes(x=x, y=y), data=tibble(x=13.4, y=c(0.8, 3.2)))+
  geom_text(aes(x=x,y=y),data=tibble(x=13.9,y=2),label="NS")
  


p11

# Rmax 

p12 <- ggplot(ms) + 
  geom_errorbar(aes(y = Acclimation, xmin = lowerRmax, xmax = upperRmax, size = 0.005), width = 0.4) +
  geom_point(aes(y = Acclimation, x = Rmax, size= 0.01)) + 
  labs(x = 'Umax: Maximum speed (mm/s)', 
       y = 'Acclimation \n      temperature (?C)', 
       title = 'Maximum speed')+ 
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=20))+
  theme(legend.position = "none")+
  theme(title = element_text(size=22))+
  geom_text(aes(y= 3, x = 12.6, label = ''))+
  geom_text(aes(y= 2, x = 12.6, label = ''))+
  geom_text(aes(y= 1, x = 12.6, label = ''))+
  geom_line(aes(x=x, y=y), data=tibble(x=12.6, y=c(1.8, 3.2)))+
  geom_text(aes(x=x,y=y),data=tibble(x=12.8,y=2.5),label="*", size=6)

p12


library(patchwork)

#To save graph with Tmin, Topt and Tmax for each parameter:
p1+p4+p10+p7+ plot_layout(nrow = 4, ncol = 1)

#To save graph with Tbr and Rmax for each parameter 
p2+p3+p5+p6+p11+p12+p8+p9+ plot_layout(nrow = 4, ncol = 2)


+plot_layout(nrow = 3, ncol = 1)


p3+p6+p9+plot_layout(nrow = 3, ncol = 1)


#size = 1500 x 1000




