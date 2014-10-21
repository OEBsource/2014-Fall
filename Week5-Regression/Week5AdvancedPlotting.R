#Advanced plotting
#Setwd
#Bring in my Lizard data

Lizards<-read.csv("anolisData.csv",
                  na.strings="?")

head(Lizards)

MyColors<-c("red", "blue", "black", "green")
MyColors[unclass(Lizards$Island)]

MySymbols<-c(0, 4, 6, 1, 20, 19, 15)
MySymbols[unclass(Lizards$ecomorph)]

plot(Lizards$SVL, Lizards$PCI_limbs,
     pch=MySymbols[unclass(Lizards$ecomorph)],
     col=MyColors[unclass(Lizards$Island)],
     main="Lizards Head and Limbs")

#Linear model function
Fit1<-lm(Lizards$PCI_limbs~Lizards$SVL)

summary(Fit1)
attributes(Fit1)
str(Fit1)

#Look at our residuals
Fit1$residuals

#Plot our regression line
abline(Fit1)
abline(Fit1, col='red')

#Adding residuals as an extra column
Lizards$Residuals <- Fit1$residuals
head(Lizards)

#Regression subsets
#CG, TG
levels(Lizards$ecomorph)

#Subset Trunk Ground
TrunkG <- subset(Lizards,
                 subset=Lizards$ecomorph=='TG')

#Quicker way to make a subset
TrunkG <- subset(Lizards, ecomorph=='TG')

#Regression on Trunk Ground
FitTG <- lm(TrunkG$PCI_limbs~TrunkG$SVL)
summary(FitTG)

abline(FitTG, col='blue')

#Regession on Crown Giant
CrownG <- subset(Lizards, ecomorph=='CG')

#Regression on Crown Giant
FitCG <- lm(CrownG$PCI_limbs~CrownG$SVL)
summary(FitCG)

abline(FitCG, col='green')

#Correlations
cor(CrownG$SVL, CrownG$PCI_limbs)
cor(TrunkG$SVL, TrunkG$PCI_limbs)

#Getting the R value for Crown Giant
sqrt(0.6247)

#Poly regression
TrunkGPoly<-TrunkG$SVL^2
FitTGPoly<-lm(TrunkG$PCI_limbs~TrunkG$SVL+TrunkGPoly)
summary(FitTGPoly)

Vec<-seq(3, 6, by=0.1)
#Get the numbers from your summary estimates
lines(Vec, (-159.18)+(79.26*Vec)+(-10.02*(Vec^2)))

#Now for all Lizards
LizP <- Lizards$SVL^2
FitPoly <- lm(Lizards$PCI_limbs~Lizards$SVL+LizP)

#Can also use the I term
#FitPoly <- lm(Lizards$PCI_limbs~Lizards$SVL+I(Lizards$SVL^2))

summary(FitPoly)

lines(Vec, (135.860) + (-63.173*Vec) + (7.234*(Vec^2)),
      col='purple')
