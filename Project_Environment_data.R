library(dplyr)
library(car)
library(GGally)
library(ggplot2)

WBData <- read.csv(file.choose(), header=TRUE)
str(WBData)
#View(WBData)

# Attach data set 
attach(WBData)

# Log Transform all Y and Xs 
logCO2.per.capita <- log(CO2.per.capita)
logGDP.per.capita <- log(GDP.per.capita)
logEnergy.use.per.capita <- log(Energy.use.per.capita)
logElectric.power.per.capita <- log(Electric.power.per.capita)
logGNI.per.capita <- log(GNI.per.capita)

# Create a new data frame: log 
logWBData <- data.frame(logCO2.per.capita, logGDP.per.capita, 
                        logEnergy.use.per.capita, logElectric.power.per.capita, 
                        logGNI.per.capita) 
str(logWBData)
#View(logWBData)

# Attach data set 
attach(logWBData)

# Descriptive Statistics: original scale, log 
summary(CO2.per.capita)
summary(GDP.per.capita)
summary(Energy.use.per.capita)
summary(Electric.power.per.capita)
summary(WBData$GNI.per.capita)

summary(logCO2.per.capita)
summary(logGDP.per.capita)
summary(logEnergy.use.per.capita)
summary(logElectric.power.per.capita)
summary(logGNI.per.capita)

# Pairwise Scatterplots using ggplot2
ggpairs(WBData[, -1]) + ggtitle("Pairwise Scatterplots")

# Pairwise Scatterplots using ggplot2: log
ggpairs(logWBData) + ggtitle("Pairwise Scatterplots: Log")

# Fit simple linear regression model: log  
lmX1L <- lm(logCO2.per.capita~logGDP.per.capita)
summary(lmX1L)
lmX2L <- lm(logCO2.per.capita~logEnergy.use.per.capita)
summary(lmX2L)
lmX3L <- lm(logCO2.per.capita~logElectric.power.per.capita)
summary(lmX3L)
lmX4L <- lm(logCO2.per.capita~logGNI.per.capita)
summary(lmX4L)

# Diagnostic plots
par(mfrow=c(2, 2))
plot(lmX1L)
plot(lmX2L)
plot(lmX3L)
plot(lmX4L)

# Part I analysis 
# Fit multiple linear regression model: log, w/ interaction 
lmMInteract <- lm(logCO2.per.capita~logGDP.per.capita*
                  logEnergy.use.per.capita*logElectric.power.per.capita)
summary(lmMInteract)
plot(lmMInteract)

# Fit multiple linear regression model: log, no interaction
#lmM <- lm(logCO2.per.capita~logGDP.per.capita+logEnergy.use.per.capita
#        +logElectric.power.per.capita)
#summary(lmM)
#plot(lmM)

# Fit multiple linear regression model: log, drop logGDP.per.capita
#lmM2 <- lm(logCO2.per.capita~logEnergy.use.per.capita+logElectric.power.per.capita)
#summary(lmM2)
#plot(lmM2)

# Part II analysis 
# Fit linear, quadratic, and cubic reg model: test the EKC hypotheis
par(mfrow=c(1, 1))
plot(CO2.per.capita~GDP.per.capita, 
     main="The EKC hypothesis") 

lmEKC1 <- lm(CO2.per.capita~GDP.per.capita)
summary(GDP.per.capita)
Xnew <- seq(from=273.5, to=185152.5) 
Yhat1 <- predict(lmEKC1, list(GDP.per.capita=Xnew)) 
lines(Yhat1~Xnew)

lmEKC2 <- lm(CO2.per.capita~GDP.per.capita+I(GDP.per.capita^2))
Xnew <- seq(from=273.5, to=185152.5) 
Yhat2 <- predict(lmEKC2, list(GDP.per.capita=Xnew)) 
lines(Yhat2~Xnew)

lmEKC3 <- lm(CO2.per.capita~GDP.per.capita+I(GDP.per.capita^2)++I(GDP.per.capita^3))
Xnew <- seq(from=273.5, to=185152.5)
Yhat3 <- predict(lmEKC3, list(GDP.per.capita=Xnew)) 
lines(Yhat3~Xnew)

#summary(lmEKC1)
#summary(lmEKC2)
#summary(lmEKC3)

#par(mfrow=c(2, 2))
#plot(lmEKC1)
#plot(lmEKC2)
#plot(lmEKC3)

#Fit linear, quadratic, and cubic reg model: test the EKC hypotheis, log 
par(mfrow=c(1, 1))
plot(logCO2.per.capita~logGDP.per.capita, 
     main="The EKC hypothesis: Log Transform") 

lmEKC1Log <- lm(logCO2.per.capita~logGDP.per.capita)
summary(logGDP.per.capita)
Xnew <- seq(from=5.611, to=12.129) 
Yhat1 <- predict(lmEKC1Log, list(logGDP.per.capita=Xnew)) 
lines(Yhat1~Xnew)

lmEKC2Log <- lm(logCO2.per.capita~logGDP.per.capita+I(logGDP.per.capita^2))
Xnew <- seq(from=5.611, to=12.129) 
Yhat2 <- predict(lmEKC2Log, list(logGDP.per.capita=Xnew)) 
lines(Yhat2~Xnew)

lmEKC3Log <- lm(logCO2.per.capita~logGDP.per.capita+I(logGDP.per.capita^2)++I(logGDP.per.capita^3))
Xnew <- seq(from=5.611, to=12.129) 
Yhat3 <- predict(lmEKC3Log, list(logGDP.per.capita=Xnew)) 
lines(Yhat3~Xnew)

summary(lmEKC1Log)
summary(lmEKC2Log)
summary(lmEKC3Log)

par(mfrow=c(2, 2))
plot(lmEKC1Log)
plot(lmEKC2Log)
plot(lmEKC3Log)
