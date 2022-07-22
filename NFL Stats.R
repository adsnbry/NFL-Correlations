rm(list = ls())
NFL_2019 <- read.csv("NFL_2003-2019.csv")
summary(NFL_2019)
attach(NFL_2019)
library(tidyverse)

NFL_2019 <- transform(NFL_2019, Pythagorean_Expectation = 1/(1+(PA/PF)^2.37))
NFL_2019 <- transform(NFL_2019, Expected_Win = Pythagorean_Expectation*16)
NFL_2019 <- transform(NFL_2019, Last_Season_Pythagorean_Expectation = 1/(1+(Last_Season_PA/Last_Season_PF)^2.37))
attach(NFL_2019)

NFL_train <- NFL_2019[97:544,]
NFL_test <- NFL_2019[1:96,]

Data.fit <- lm(NFL_train$PD ~ NFL_train$Last_Season_PD)
summary(Data.fit)

plot(NFL_train$Last_Season_Pythagorean_Expectation, NFL_train$Pythagorean_Expectation)
abline(Data.fit)

anova(Data.fit)

library(ggplot2)
Data.fit.log <- glm(W.L. ~ ((Last_Season_PF)/(Last_Season_PF)+(Last_Season_PA)))
summary(Data.fit.log)
plot(Data.fit.log)

for (i in 32){
  Teams_2019[i,] <- NFL_2019[i,]
}

Arizona_Cardinals <- NFL_2019[seq(1, nrow(NFL_2019), 32),]
Atlanta_Falcons <- NFL_2019[seq(2, nrow(NFL_2019), 32),]
Baltimore_Ravens <- NFL_2019[seq(3, nrow(NFL_2019), 32),]
Buffalo_Bills <- NFL_2019[seq(4, nrow(NFL_2019), 32),]
Carolina_Panthers <- NFL_2019[seq(5, nrow(NFL_2019), 32),]
Chicago_Bears <- NFL_2019[seq(6,nrow(NFL_2019), 32),]
Cincinnati_Bengals <- NFL_2019[seq(7,nrow(NFL_2019), 32),]
Cleveland_Browns <- NFL_2019[seq(8,nrow(NFL_2019), 32),]
Dallas_Cowboys <- NFL_2019[seq(9,nrow(NFL_2019), 32),]
Denver_Broncos <- NFL_2019[seq(10, nrow(NFL_2019), 32),]
Detroit_Lions <- NFL_2019[seq(11, nrow(NFL_2019),32),]
Greenbay_Packers <- NFL_2019[seq(12,nrow(NFL_2019), 32),]
Houston_Texans <- NFL_2019[seq(13,nrow(NFL_2019), 32),]
Indianapolis_Colts <- NFL_2019[seq(14,nrow(NFL_2019), 32),]
Jacksonville_Jaguars <- NFL_2019[seq(15,nrow(NFL_2019), 32),]
KansasCity_Chiefs <- NFL_2019[seq(16,nrow(NFL_2019), 32),]
Miami_Dolphins <- NFL_2019[seq(17,nrow(NFL_2019), 32),]
Minnesota_Vikings <- NFL_2019[seq(18,nrow(NFL_2019), 32),]
NewEngland_Patriots <- NFL_2019[seq(19,nrow(NFL_2019), 32),]
NewOrleans_Saints <- NFL_2019[seq(20,nrow(NFL_2019), 32),]
NewYork_Giants <- NFL_2019[seq(21,nrow(NFL_2019), 32),]
NewYork_Jets <- NFL_2019[seq(22,nrow(NFL_2019), 32),]
Oakland_Raiders <- NFL_2019[seq(23,nrow(NFL_2019), 32),]
Philadelphia_Eagles <- NFL_2019[seq(24,nrow(NFL_2019), 32),]
Pittsburgh_Steelers <- NFL_2019[seq(25,nrow(NFL_2019), 32),]
SanDiego_Chargers <- NFL_2019[seq(26,nrow(NFL_2019), 32),]
SanFrancisco_49ers <- NFL_2019[seq(27,nrow(NFL_2019), 32),]
Seattle_Seahawks <- NFL_2019[seq(28,nrow(NFL_2019), 32),]
StLouis_Rams <- NFL_2019[seq(29,nrow(NFL_2019), 32),]
TampaBay_Buccaneers <- NFL_2019[seq(30,nrow(NFL_2019), 32),]
Tenessee_Titans <- NFL_2019[seq(31,nrow(NFL_2019), 32),]
Washington_Redskins <- NFL_2019[seq(32,nrow(NFL_2019), 32),]


NFL_2019 <- transform(NFL_2019, Pytha_Ratio = (Expected_Win)/(W))
plot(NFL_2019$Last_Season_PF, NFL_2019$Pytha_Ratio)
