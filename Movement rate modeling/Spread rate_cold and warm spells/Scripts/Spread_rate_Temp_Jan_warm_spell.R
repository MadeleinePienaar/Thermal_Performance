setwd("Documents/R/Movement rate modeling/Spread rate_cold and warm spells/Data/")

#Install Packages 
install.packages("ggplot2")
install.packages("patchwork")

#Load Packages 
library(ggplot2)
library(patchwork)

#Load January warm spell data 
df <- read.csv("Spread_rate_Jan_warm_spell.csv", header = TRUE, sep=";")

df$Acclimation <- as.character(df$Acclimation)  

df18 <- subset(df, Acclimation == 18)

df25 <- subset(df, Acclimation == 25)

df32 <- subset(df, Acclimation == 32)

#Plot January with warm spell distance travelled data (Grabouw)

p1 <- ggplot()+
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
p1

#Plot temperature over time data for January with warm spell (Grabouw)
p2 <- ggplot()+
  geom_line(data=df, aes(x=Time, y=Temp))+
  labs(y = 'Temperature (?C)',
       x= 'Time (hours)')+
  ylim(0,35)

#View plot 
p2

#Combine plots 
p1+p2

#Save plots: Size 850 width, do not keep aspect ratio


  
