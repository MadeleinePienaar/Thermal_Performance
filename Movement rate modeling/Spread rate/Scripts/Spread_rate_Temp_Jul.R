setwd("Documents/R/Movement rate modeling/Spread rate/Data/")
getwd()

#Install Packages
install.packages("ggplot2")

#Load Packages
library(ggplot2)

#Load data

df <- read.csv("Spread_rate_Jul.csv", header = TRUE, sep=";")

df$Acclimation <- as.character(df$Acclimation)  

df18 <- subset(df, Acclimation == 18)

df25 <- subset(df, Acclimation == 25)

df32 <- subset(df, Acclimation == 32)

#Plot data
p3 <- ggplot()+
  geom_line(data=df32, aes(x=Time, y=Distance_travelled_acc, color="Acclimation 32"))+
  geom_line(data=df25, aes(x=Time, y=Distance_travelled_acc, color="Acclimation 25"))+
  geom_line(data=df18, aes(x=Time, y=Distance_travelled_acc, color="Acclimation 18"))+
  labs(y = 'Distance travelled (m)',
       x= 'Time (hours)',
       color = "")+
  scale_colour_manual(values = c("blue", "green", "red"))+
theme(legend.title = element_text("Acclimation"))+
  ylim(0, 1100000)

#View plot
p3

#Plot data
p4 <- ggplot()+
  geom_line(data=df, aes(x=Time, y=Temp))+
  labs(y = 'Temperature (?C)',
       x= 'Time (hours)')+
  ylim(0,35)

#View plot
p4

#Combine plots
p4+p3

#Combine plots
p4 + p3

#Save plots: Size 850 width, do not keep aspect ra