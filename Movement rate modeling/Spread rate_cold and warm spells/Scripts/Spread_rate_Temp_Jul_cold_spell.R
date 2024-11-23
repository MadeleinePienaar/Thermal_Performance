setwd("Documents/R/Movement rate modeling/Spread rate_cold and warm spells/Data/")

#Install Packages 
install.packages("ggplot2")
install.packages("patchwork")

#Load Packages 
library(ggplot2)
library(patchwork)

#Load July cold spell data 
df <- read.csv("Spread_rate_Jul_cold_spell.csv", header = TRUE, sep=";")

df$Acclimation <- as.character(df$Acclimation)  

df18 <- subset(df, Acclimation == 18)

df25 <- subset(df, Acclimation == 25)

df32 <- subset(df, Acclimation == 32)

#Plot July with cold spell distance travelled data (Grabouw)

p7 <- ggplot()+
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
p7

#Plot temperature over time data for July with cold spell (Grabouw)
p8 <- ggplot()+
  geom_line(data=df, aes(x=Time, y=Temp))+
  labs(y = 'Temperature (?C)',
       x= 'Time (hours)')+
  ylim(0,35)

#View plot 
p8

#Combine plots 
p7+p8

#Save plots: Size 850 width, do not keep aspect ratio