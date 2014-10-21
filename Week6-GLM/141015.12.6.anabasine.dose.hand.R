#2014.10.15
#experiment testing effects of 3 anabasine doses
#on growth of Crithidia bombi (a bee parasite)
#2 analyses
#first on data from spectrophotometer readings
#second on data from microscope counts
#######this is for the HAND COUNTS
#clear memory
rm(list=ls())
#set wd

#read in data
Ana.Hand<-read.csv('141015.12.6.anabasine.dose.csv')
str(Ana.Hand)
#change time to factor
Ana.Hand$time<-as.factor(Ana.Hand$time)
str(Ana.Hand$time) 
#change conc to numeric (cell concentration)
Ana.Hand$conc<-as.numeric(paste(Ana.Hand$conc))
str(Ana.Hand$conc)
Ana.Hand$conc #good; note "paste" command
#I never understand why there is 1 extra factor level
#replacements from previous script for plate reader
##"plate>"Hand"
##"a620">"conc"

#make a subset to remove wells with media only
Ana.Hand.analysis<-subset(Ana.Hand, !strain=="control")
head(Ana.Hand.analysis)
Ana.Hand.analysis$conc #these are cell counts
######need to replace missing values and "#DIV/0!"
Ana.Hand.analysis.no.na<-subset(Ana.Hand.analysis, !conc=="NA")
Ana.Hand.analysis.no.na$conc

#remove time 0 for analysis
Hand.analysis<-subset(Ana.Hand.analysis.no.na, !time=="0")
Hand.analysis$conc
str(Hand.analysis$conc) #good; these are counts, should be numeric
#for compatibility with script as written, reassign:
Ana.Hand.analysis.no.na<-Hand.analysis
#assess distribution of response variable
hist(Ana.Hand.analysis.no.na$conc) #right skew
#these are counts, so my inclination is to use GLM
glm.hand<-glm( conc~anabasine*time,
               data=Ana.Hand.analysis.no.na,
               family="poisson")
glm.hand
plot(glm.hand) #model is atrocious fit to assumptions
#poor homogeneity!
#terrible normality of residuals!
#any advice?
#transformation?
library(car)
Anova(glm.hand) #a very strange result!
#what about negative binomial error distribution
library(MASS)
#sample: nbinom <- glm.nb(daysabs ~ math + prog, data = dat))
model.nb<-glm.nb( conc~anabasine*time,
                 data=Ana.Hand.analysis.no.na)
anova(model.nb) #effects of time, not anabasine
plot(model.nb) #seems ok?
#homo
#norm
#scale
#leverage
#model simplification sample
#m2 <- update(m1, . ~ . - prog)
#compare to poisson model
#pchisq(2 * (logLik(glm.hand) - logLik(model.nb)), df = 1, lower.tail = FALSE)
pchisq(2 * (logLik(model.nb) - logLik(glm.hand)), df = 1, lower.tail = FALSE)
#p-value of 0??
#below is not working
glm.nbinom<-glm( conc~anabasine*time,
               data=Ana.Hand.analysis.no.na,
               family="nbinom")
glm.hand
plot(glm.hand)

#note: glm does not account for repeated measures design
#explore distributions
hist(sqrt(Ana.Hand.analysis.no.na$conc)) #not guassian
hist(log(Ana.Hand.analysis.no.na$conc))
#log transformation seems most appropriate
#if we want to run an ANOVA
Hand.analysis$log.conc<-log(Ana.Hand.analysis.no.na$conc)
Hand.analysis$log.conc
Hand.analysis<-Ana.Hand.analysis.no.na
Hand.analysis$conc
#run an repeated measures analysis of variance
#test effects of different anabasine concentrations on cell growth
#we will model conc~anabasine*time, with "sample" as the subject
#coerce time to factor
Hand.analysis$time<-as.factor(Hand.analysis$time)
rm.anova.log<-aov(log.conc~anabasine*time + Error (sample), 
                  data=Hand.analysis)
summary(rm.anova.log) #no effect of anabasine
#I can only do diagnostic plots with regular ANOVA
anova.log<-aov(log.conc~anabasine*time , 
               data=Hand.analysis)
summary(anova.log)
plot(anova.log) #beware, this line will clog your script
#next plot
#next plot
#next plot
#if you try to run the whole script to get to some
#line towards the bottom
#OK homogeneity of variance?
#fit to normal-- not so splendid
#observations 40,43,65 seem to be outliers

#I think there is also a way to do repeated measures ANOVA 
#with lme & lme4
#we can also do pairwise comparisons with Tukey's test

?TukeyHSD
TukeyHSD(anova.log, which='anabasine')
#no pairwise differences

######################use ggplot2 to graph means and standard errors

library(bear)
library(ggplot2)
#thanks to moira for this link on plotting
#http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/
#2 helpful functions
?summarySE
?summarySEwithin #for repeated-measures designs
##########summarySEwithin uses the following arguments
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   betweenvars: a vector containing names of columns that are between-subjects variables
##   withinvars: a vector containing names of columns that are within-subjects variables
##   idvar: the name of a column that identifies each subject (or matched subjects)
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
Hand.analysis$log.conc
Hand.summary<-summarySEwithin(data=Hand.analysis,
                measurevar="log.conc",
                betweenvars="anabasine",
                withinvars="time",
                idvar="sample")
Hand.summary #exquisite!
#now can we plot this?
#sample
#ggplot(dfc, aes(x=dose, y=len, colour=supp)) + 
#geom_errorbar(aes(ymin=len-se, ymax=len+se), width=.1) +
  #geom_line() +
 # geom_point()

#???why do counts go down??

pd <- position_dodge(.1) # move error bars .05 to the left and right
#rerun above
ggplot(Hand.summary, aes(x=time, y=log.conc, colour=anabasine)) + 
  geom_errorbar(aes(ymin=log.conc-se, ymax=log.conc+se), width=.1, 
                position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd)
#no evidence that anabasine inhibits growth!!
# Use 95% confidence interval (column df$ci) instead of SEM
ggplot(Hand.summary, aes(x=time, y=log.conc, colour=anabasine)) + 
  geom_errorbar(aes(ymin=log.conc-ci, ymax=log.conc+ci), width=.1, 
                position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd)
#Superior niftiness





#######now analyze the hand counts




#plotting the hand counts
