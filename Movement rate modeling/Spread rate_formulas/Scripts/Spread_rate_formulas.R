getwd()
setwd("Documents/R/Movement rate modeling/Spread rate_formulas/Data/")

# Install Packages 
install.packages("ggplot2")

#Load Packages
library(ggplot2)

#Acclimation 18 - Derive formula equation coefficients for movement/spread rate

#Load data 
df18 <- read.csv("Spread_rate_formula_A18.csv", header = TRUE, sep=";")

spread_rate18 <- df18$Spread_rate
temperature18 <- df18$Temp

# Fit a second-order polynomial regression
model18 <- lm(spread_rate18 ~ temperature18 + I(temperature18^2), data = df18)

# Show the summary of the model
summary(model18)

# Create scatter plot with fitted curve
ggplot(df18, aes(x = temperature18, y = spread_rate18)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE)+
  labs(title="Acclimation 18 Spread rate model", 
       x= "Temperature", 
       y= "Spread rate (Distance travelled x % of beetles moving")

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
# Acclimation 25 - Derive formula equation coefficients for movement/spread rate

# Load data 
  df25 <- read.csv("Spread_rate_formula_A25.csv", header = TRUE, sep=";")

spread_rate25 <- df25$Spread_rate
temperature25 <- df25$Temp

# Fit a second-order polynomial regression
model25 <- lm(spread_rate25 ~ temperature25 + I(temperature25^2), data = df18)

# show the summary of the model
summary(model25)

#Show model shape 

ggplot(df25, aes(x = temperature25, y = spread_rate25)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE)+
  labs(title="Acclimation 25 Spread rate model", 
       x= "Temperature", 
       y= "Spread rate (Distance travelled x % of beetles moving")

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Acclimation 32 - Derive formula equation coefficients for movement/spread rate
  
# Load data 
  
df32 <- read.csv("Spread_rate_formula_A32.csv", header = TRUE, sep=";")

spread_rate32 <- df32$Spread_rate
temperature32 <- df32$Temp

# fit a second-order polynomial regression
model32 <- lm(spread_rate32 ~ temperature32 + I(temperature32^2), data = df18)

# show the summary of the model
summary(model32)

#Show model shape 

ggplot(df32, aes(x = temperature32, y = spread_rate32)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE)+
  labs(title="Acclimation 32 Spread rate model", 
       x= "Temperature", 
       y= "Spread rate (Distance travelled x % of beetles moving")

