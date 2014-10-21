##YEAH!!## 10/10/2014
#Plotting :)

#seq
#rev
#rep

?seq

X<-seq(1,10,1)
X

A<-seq(10,100,10)
A

Aa<-c(10,20,30)

?rev

Xx<-rev(X)
Xx
X

?rep

Aa<-rep(50,10)
Aa

XX<-rep(X,10)

?matrix
matrixX<-matrix(XX, 10, 10)
matrixX

Testmatrix<-matrix(XX, 11, 9)

TmatrixX<-t(matrixX)
TmatrixX

write.csv(TmatrixX, "MatrixTranspos.csv")

X
Y<-seq(1,10,1)

#General plot
plot(X,Y)

#setting limits
plot(X,Y, xlim=c(0,20), ylim=c(0,20))

#adding title/text
plot(X,Y, xlim=c(0,20), ylim=c(0,20),
     main="SOURCE Data", xlab="Variable X",
     ylab="Variable Y")

#changing plotting features
#type = p (points)
#type = l (lines)
#type = b (both lines and points)
plot(X,Y, xlim=c(0,20), ylim=c(0,20),
     main="SOURCE Data", xlab="Variable X",
     ylab="Variable Y", type="l")

plot(X,Y, xlim=c(0,20), ylim=c(0,20),
     main="SOURCE Data", xlab="Variable X",
     ylab="Variable Y", type="b")

#Changing symbols
#pch
plot(X,Y, xlim=c(0,20), ylim=c(0,20),
     main="SOURCE Data", xlab="Variable X",
     ylab="Variable Y", type="p", pch=c(1:10))

c(1:10)

#Changing colours
#col
plot(X,Y, xlim=c(0,20), ylim=c(0,20),
     main="SOURCE Data", xlab="Variable X",
     ylab="Variable Y", type="p", pch=c(1:10),
     col="blue")

#Adding data to plots
#points
#lines
#note: a plot already needs to exist for lines and
#points to work
points(rev(X),Y, col="red")
lines(X, rev(Y), col="black")

#multipl plots one window
par(mfrow=c(2,2))
plot(X,Y, col="black")
plot(X,Y, col="blue")
plot(X,Y, col="red")
plot(X,Y, col="green")

#need to go back to single plot, single window
par(mfrow=c(1,1))

#real data
#setwd
Lizards<-read.csv("anolisData.csv",
                  na.string="?")

head(Lizards)

#Real data plotting
plot(Lizards$SVL, Lizards$PCI_limbs)
plot(Lizards$SVL, Lizards$PCI_limbs, pch=19)

levels(Lizards$Island)

#Using unclass
MyColors<-c("red", "blue", "black", "green")
unclass(Lizards$Island)
table(Lizards$Island)

MyColors[unclass(Lizards$Island)]

plot(Lizards$SVL, Lizards$PCI_limbs, pch=19,
     col=MyColors[unclass(Lizards$Island)])

levels(Lizards$ecomorph)

MySymbols<-c(0, 4, 6, 1, 20, 19, 15)
MySymbols[unclass(Lizards$ecomorph)]




plot(Lizards$SVL, Lizards$PCI_limbs,
     pch=MySymbols[unclass(Lizards$ecomorph)],
     col=MyColors[unclass(Lizards$Island)])


Fit1<-lm(Lizards$PCI_limbs~Lizards$SVL)
abline(Fit1)
Fit1
summary(Fit1)
str(Fit1)
Fit1$residuals

Lizards$Resids<-Fit1$residuals

CrownG<-subset(Lizards, subset=Lizards$ecomorph=='CG')
FitCB<-lm(CrownG$PCI_limbs~CrownG$SVL)

abline(FitCB, col="red")

levels(Lizards$ecomorph)
table(Lizards$ecomorph)


TrunkG<-subset(Lizards, subset=Lizards$ecomorph=='TG')
FitTG<-lm(TrunkG$PCI_limbs~TrunkG$SVL)

abline(FitTG, col="blue")

summary(FitCB)
summary(FitTG)

cor(CrownG$SVL, CrownG$PCI_limbs)
cor(TrunkG$SVL, TrunkG$PCI_limbs)

TrunkG<-subset(Lizards, subset=Lizards$ecomorph=='TG')
TrunkP<-TrunkG$SVL^2
FitTGPoly<-lm(TrunkG$PCI_limbs~TrunkG$SVL+TrunkP)
summary(FitTGPoly)

vec <- seq(3.5, 5, by=0.1)
lines(vec, -159.18 + 79.26*vec + -10.02*vec^2 )


LizP<-Lizards$SVL^2
Fitpoly<-lm(Lizards$PCI_limbs~Lizards$SVL+LizP)
summary(Fitpoly)

vec <- seq(3.5, 5, by=0.1)
lines(vec, 135.860 + -63.173*vec + 7.234*vec^2 )
lines(vec, -159.18 + 79.26*vec + -10.02*vec^2 )