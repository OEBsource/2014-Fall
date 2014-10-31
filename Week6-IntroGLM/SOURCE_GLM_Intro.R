#OEB SOURCE - Intro to Generalized Linear Models (GLMs)
#October 31, 2014
#Prepared by Laura Doubleday

#There is a lot of information in this script, and not too many R commands.
#I hope this SOURCE session will provide participants with a good introduction to GLMs and make them comfortable enough to continue to explore GLMs beyond the binomial model we will work through together.

#GLMs are incredibly versatile. They can handle non-constant variance and non-normal errors. They can be used with presence-absence data, count data, and proportions (as well as the usual continuous and categorical suspects).
#GLMs are parametric models, with a probability distribution specified for the response variable (and thus specified for the model's error terms).

#GLM Ingredients:
#Data - including values of response and predictor(s)
#Error family - what probability distribution best describes our data's errors?
#Common error families include normal, poisson, and binomial
#Link function - what function should we use to "link" the expected value of the response to a linear equation of predictors? In R, there are default link functions associated with each error family.
#Common links include identity (for normal), log (for poisson), and logit (for binomial)
#The best link function results in the smallest residual deviance

#GLMs boil down to three components:
# 1. A probability distribution
# 2. A linear predictor
# 3. An error family and corresponding link function

#Maximum likelihood
#Probability of a hypothesis given some data.
#Data are fixed, which could have been produced by any of a variety of hypotheses.
#Maximum likelihood seeks hypothesis that maximizes data's likelihood.
#(Compare with probability, where hypothesis is fixed and data are variable.)

#Error structure
#In R, we specify the "family" that we think best describes the distrubtion of our errors
#Each family has a "canonical" link function (the default), which will be applied unless we say otherwise.

#The link function
#The link relates the linear predictor to the probability distribution
#Some examples of distributions and their canonical link functions:
#Normal & identity link
#Poisson & log link
#Binomial & logit link

#Deviance
#Based on maximized value of model's log-likelihood
#Provides measure of model's fit to the data (smaller deviance = better fit)
#GLM equivalent of residual SS
#Deviance is estimated in different ways for different error families when using glm() in R

#Overdispersion
#Can come up with Poisson or binomial errors
#Overdispersion is when observed conditoinal variance of response is larger than variation implied by distribution used in fitting the model
#Could occur because an important factor has been left out of the model (or maybe not even measured!)
#Could also result from underlying distribution being non-Poisson or non-binomial

#Quasi-likelihood - one way to handle overdispersion
#Quasi-likelihood only uses information about the response variable's variance-mean relationship
#In R, you can specify "quasipoisson" as the family for your GLM, for example

#It's very easy to run GLMs in R!
#Let's use German Rodriguez's contraceptive use dataset to explore logistic regression in a glm framework
#This dataset shows the distribution of 1607 currently married and fecund women interviewed in the Fiji Fertility Survey, according to age, education, desire for more children, and current use of contraception.

#Let's load the data:
Data<-read.table("http://data.princeton.edu/wws509/datasets/cuse.dat", header=TRUE)
Data #make sure it all looks good

#Attach the dataset, so we don't have to type as much
attach(Data)
#Contraceptive use is our response, possible predictors are age, education, and desire for more children.

#Let's try a simple additive model, where contraceptive use depends on age, education, and desire for more children:
lrfit<-glm(cbind(using, notUsing)~age+education+wantsMore, family=binomial) 
#We pass in two columns for our response because we have one column of the number of "successes" and another column of the number of "failures." R will add the two columns together to produce the correct binomial denominator. cbind creates a matrix by binding the column vectors of the numbers of women using and not using contraception.

#Let's see the results of our model
lrfit

#Let's define indicator variables for women with high education and women who want no more children. This will give us our results with "wants more" and low education as the reference.
noMore<-wantsMore == "no"
hiEduc<-education == "high"

glm(cbind(using, notUsing)~age + hiEduc + noMore, family=binomial)
#we can calculate a p-value for our model's residual deviance like this:
1-pchisq(29.92,10) #highly significant! we have more residual deviance than we should for this number of degrees of freedom, which means that our model is not doing a good job of fitting the data. we are just subtracting the chi sq probability for residual deviance of 29.92 and 10 residual df (given to us in our model output) from 1. Because it is significant, we know that our model is not doing too well.
#Residual deviance is too large - this is bad, it's not a good fit

#Let's try introducing an interaction between age and desire for no more children.
lrfit<-glm(cbind(using,notUsing)~age*noMore+hiEduc, family=binomial)
lrfit
1-pchisq(12.63,7) #not significant at alpha = 0.05 level, so we have no evidence against this model
summary(lrfit)

#Here is the 'full' model, with all of the interaction terms, including a 3-way interaction:
fullmod<-glm(cbind(using,notUsing)~age*noMore+noMore*hiEduc+age*hiEduc, family=binomial)
fullmod
summary(fullmod)
1-pchisq(2.441,3) #this model is doing a good job
#Are all of the interactions significant?
library(car)
Anova(fullmod, type=c("III")) #interaction between desire and education not significant
#Type used depends on whether you have a balanced design 
##Equal numbers in each category, very different numbers of women in each category

mod2<-glm(cbind(using,notUsing)~age*noMore+age*hiEduc, family=binomial)
Anova(mod2, type=c("III")) #now interaction between age and education isn't significant - the desire*education interaction was making it significant

mod3<-glm(cbind(using,notUsing)~age*noMore+hiEduc, family=binomial)
Anova(mod3, type=c("III")) #interaction between age and desire is significant and main effects of age and education are significant. The main effect of desire for more children is not significant, but have to keep it if we want the desire*age interaction term in the model.

#Diagnostic plots
plot(mod3)

residualPlots(mod3, layout=c(1,3)) #for some reason, the Pearson residuals are only being plotted against the predictor age - will have to look into this further 
#lack of fit tests are only provided for numerical predictors, not categorical ones like we have

library(boot)
glm.diag.plots(mod3) #the plot in the bottom left shows points with high leverage that may be having undue influence on the results - these points would be above a horizontal line and/or to the right of a vertical line, but because we don't have any points with high leverage those lines are not shown.

influenceIndexPlot(mod3, vars=c("Cook", "hat"), id.n=3)
#could then update model to check if more influential points are having undue influence on results

#See also Chapter 6 of Fox & Weisberg's An R Companion to Applied Regression (pdf googleable).

