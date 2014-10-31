#2014.10.15
#experiment testing effects of 3 anabasine doses
#on growth of Crithidia bombi (a bee parasite)
#2 analyses
#first on data from spectrophotometer readings
#second on data from microscope counts

#clear memory
rm(list=ls())
#set wd

#read in data
Ana.plate<-read.csv('141015.plate.reader.csv')
str(Ana.plate)
#change time to factor
Ana.plate$time<-as.factor(Ana.plate$time)
str(Ana.plate$time) 
#I never understand why there is 1 extra factor level


#make a subset to remove wells with media only
Ana.plate.analysis<-subset(Ana.plate, !strain=="control")
head(Ana.plate.analysis)
Ana.plate.analysis$a620 #these are absorbance values
Ana.plate.analysis.no.na<-subset(Ana.plate.analysis, !a620=="NA")
Ana.plate.analysis.no.na$a620

#assess distribution of response variable
hist(Ana.plate.analysis.no.na$a620)
hist(asin(Ana.plate.analysis.no.na$a620))
hist(sqrt(Ana.plate.analysis.no.na$a620))
hist(log(Ana.plate.analysis.no.na$a620))
#log transformation seems most appropriate
#if we want to run an ANOVA
Ana.plate.analysis.no.na$log.a620<-log(Ana.plate.analysis.no.na$a620)
Plate.analysis<-Ana.plate.analysis.no.na

#run an repeated measures analysis of variance
#test effects of different anabasine concentrations on cell growth
#we will model a620~anabasine*time, with "sample" as the subject
rm.anova.log<-aov(log.a620~anabasine*time + Error (sample), 
                  data=Plate.analysis)
summary(rm.anova.log) #nicotine p=0.705, nicotine*strain p=0.590
#claims there is highly significant effect of anabasine

#I can only do diagnostic plots with regular ANOVA
anova.log<-aov(log.a620~anabasine*time , 
               data=Plate.analysis)
summary(anova.log)
plot(anova.log) #beware, this line will clog your script
#if you try to run the whole script to get to some
#line towards the bottom
#exquisite homogeneity of variance!
#fit to normal-- not so splendid
#observations 363 & 419 seem to be outliers
#but there is an effect of anabasine anyway

#I think there is also a way to do repeated measures ANOVA 
#with lme & lme4
#we can also do pairwise comparisons with Tukey's test
?TukeyHSD
TukeyHSD(anova.log, which='anabasine')
#5 ppm and 50 ppm were different from control and 0.5 ppm



######################use ggplot2 to graph means and standard errors
install.packages("bear")
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
Plate.summary<-summarySEwithin(data=Plate.analysis,
                measurevar="log.a620",
                betweenvars="anabasine",
                withinvars="time",
                idvar="sample")
Plate.summary #exquisite!

#now can we plot this?
#sample
#ggplot(dfc, aes(x=dose, y=len, colour=supp)) + 
#geom_errorbar(aes(ymin=len-se, ymax=len+se), width=.1) +
  #geom_line() +
 # geom_point()
ggplot(Plate.summary, aes(x=time, y=log.a620, colour=anabasine)) + 
  geom_errorbar(aes(ymin=log.a620-se, ymax=log.a620+se), width=.1) +
  geom_line() +
  geom_point()

pd <- position_dodge(.1) # move error bars .05 to the left and right
#rerun above
ggplot(Plate.summary, aes(x=time, y=log.a620, colour=anabasine)) + 
  geom_errorbar(aes(ymin=log.a620-se, ymax=log.a620+se), width=.1, 
                position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd)
#results are contrary to hypothesis 
#that anabasine inhibits growth!!
# Use 95% confidence interval (column df$ci) instead of SEM
ggplot(Plate.summary, aes(x=time, y=log.a620, colour=anabasine)) + 
  geom_errorbar(aes(ymin=log.a620-ci, ymax=log.a620+ci), width=.1, 
                position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd)
#Superior niftiness





#######now analyze the hand counts




#plotting the hand counts
