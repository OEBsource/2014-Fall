#ggplot2 
library(ggplot2)

####price vs. carat####
#native R version
head(diamonds)
plot(x=diamonds$carat,y=diamonds$price)

#ggplot version
ggplot(data=diamonds,aes(x=carat,y=price))

#Layers?
ggplot(data=diamonds,aes(x=carat,y=price))+geom_point()

#Distinguish the data points by factor 
ggplot(data=diamonds,aes(x=carat,y=price, color=cut))+geom_point()

#Store a plot
MyPlot<-ggplot(data=diamonds,aes(x=carat,y=price, color=cut))+geom_point()
MyPlot
#You can also add aes on the end, for example
NoColorPlot<-ggplot(data=diamonds,aes(x=carat,y=price))+geom_point()
NoColorPlot
ColorPlot<-NoColorPlot+aes(color=cut)
ColorPlot

#Let's make a histogram
ggplot(data=diamonds, aes(x=carat))+geom_histogram(binwidth=0.5)

#Since we can store part of a plot, we can make a shortcut...
DmondHist<-ggplot(data=diamonds, aes(x=carat))
DmondHist+geom_histogram(binwidth=0.5)

#Using the Iris dataset
#Currently no layers, so I will save it to make it quicker
IrisPlot<-ggplot(data=iris, aes(x=Sepal.Length, y=Sepal.Width, color=Species))

#Let's store the layer as well
IrisLayer<-IrisPlot+geom_point()

#Place a regression line through it
IrisLayer+geom_smooth(method="lm",se=F,size=2)

#We can see the standard error for this data
IrisLayer+geom_smooth(method="lm",se=T,size=2)

#I want to make regression lines through some other data
RegLine<-geom_smooth(method="lm",se=T,size=2)
BigPoints<-geom_point(size=2)
PetalPlot<-ggplot(data=iris, aes(x=Petal.Length, y=Petal.Width, color=Species))

#Plot them all togther
PetalPlot+BigPoints+RegLine


#Boxplots#
IrisBox<-ggplot(data=iris, aes(y=Petal.Width, x=Species))
IrisBox+geom_boxplot()

head(ToothGrowth)
ToothBox<-ggplot(data=ToothGrowth, aes(y=len, x=supp))
ToothBox+geom_boxplot(aes(fill=as.factor(dose)))


#Lines
#ggplot doesn't like times series data :(
##Lets make a work-around
LynxTime<-seq(1821, 1934, 1)
LynxTrap<-as.numeric(lynx)
Lynx<-as.data.frame(cbind(LynxTime, LynxTrap))

LynxLine<-ggplot(data=Lynx, aes(x=LynxTime, y=LynxTrap))
LynxLine+geom_line()

LynxLine+geom_line()+theme_bw()