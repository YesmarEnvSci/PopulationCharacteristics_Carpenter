# Annika Putt
# Created August 28, 2014
# fishidlengthweight.r
# Generally followign fishR methods!

# load libraries
library(ggplot2)
library(scales) 
library(FSA)

# source the import file to obtain "fishid" data frane
source("fishidimport.r")
cl <- subset(fishid,waterbody=="cl")
clbt <- subset(cl,species=="bt")
clmw <- subset(cl,species=="mw")
clrb <- subset(cl,species=="rb")

##########
# Pick an option
#cllw <- clbt
#cllw <- clmw
#cllw <- clrb

##########
# Run a linear regression on Bull Trout lengths and weights (log)
lvsw <- function(sp) {
  temp <- subset(fishid,species==sp & waterbody=="cl")
  ggplot(data=temp,aes(x=log(lengthmm), y=log(weightg), color=date)) + geom_point()
}

lvsw(sp="bt")
lvsw(sp="rb")
lvsw(sp="mw")

# Model
cllw$logweight <- log(cllw$weightg)
cllw$loglength <- log(cllw$lengthmm)
fitbt <- lm(logweight~loglength,data=cllw)
coef(fitbt)
confint(fitbt)

# Hypothesis test for whether the model with the explanitory varible explains significantly more of the variability than 
 # a model without the explanatory variable
anova(fitbt) # has a massive f and tiny p, so yes.
summary(fitbt)

# Create a conversion factor based on the model sigma for back transforming
syx <- summary(fitbt)$sigma
cf <- exp((syx^2)/2)

# Assumption checking
r <- residuals(fitbt)
fv <- fitted(fitbt)
plot(r~fv,xlab="Fitted Values",ylab="Residuals")
hist(r,xlab="Residuals")

# Plot the fit
plot(logweight~loglength,data=cllw,pch=16,col=rgb(0,0,0,1/4),
     ylab="log Weight",xlab="log Fork Length") # Note the shading allows you to see where points overlie
#abline(fitbt,col="gray20",lwd=2,lty="solid") # OR predict and plot
x <- seq(min(cllw$loglength,na.rm=TRUE),max(cllw$loglength,na.rm=TRUE),length.out=99)
y <- predict(fitbt,data.frame(loglength=x),interval="prediction")
lines(y[,"fit"]~x,col="gray20",lwd=2,lty="solid")
lines(y[,"lwr"]~x,col="gray20",lwd=2,lty="dashed")
lines(y[,"upr"]~x,col="gray20",lwd=2,lty="dashed")

# Superimpose on the original data
plot(weightg~lengthmm,data=cllw,pch=16,col=rgb(0,0,0,1/4),
       ylab="Weight (g)",xlab="Fork Length (mm)")
x <- seq(min(cllw$lengthmm,na.rm=TRUE),max(cllw$lengthmm,na.rm=TRUE),length.out=99)
y <- exp(predict(fitbt,data.frame(loglength=log(x)),interval="prediction"))*cf
lines(y[,"fit"]~x,col="gray20",lwd=2,lty="solid")
lines(y[,"lwr"]~x,col="gray20",lwd=2,lty="dashed")
lines(y[,"upr"]~x,col="gray20",lwd=2,lty="dashed")

# Compare length-weigth relationships between 2013 and 2014
cllw$fYear <- factor(cllw$y)
fitbtyears <- lm(logweight~loglength*fYear,data=cllw) # loglength is now considered a covariate
# short hand for response ~ covariate + factor + factor:covariate
anova(fitbtyears)
# p value for the interaction is very low, so the slopes of the two lines are statistically different
cbind(coef(fitbtyears),confint(fitbtyears)) # the bottom left number tells how different the two slopes are

# Plot the fit from the two years
plot(weightg~lengthmm,data=cllw,col="white",ylab="Weight (g)",xlab="Fork Length (mm)") # set up plot

cllw2013 <- Subset(cllw,fYear==2013)
points(weightg~lengthmm,data=cllw2013,pch=16,col=rgb(0,0,0,1/3),cex=0.6)
x2013 <- seq(min(cllw2013$lengthmm,na.rm=TRUE),max(cllw2013$lengthmm,na.rm=TRUE),length.out=99)
y2013 <- exp(predict(fitbtyears,data.frame(loglength=log(x2013),fYear=factor(2013))))
lines(y2013~x2013,col="black",lwd=2)

cllw2014 <- Subset(cllw,fYear==2014)
points(weightg~lengthmm,data=cllw2014,pch=16,col=rgb(1,0,0,1/3),cex=0.6)
x2014 <- seq(min(cllw2014$lengthmm,na.rm=TRUE),max(cllw2014$lengthmm,na.rm=TRUE),length.out=99)
y2014 <- exp(predict(fitbtyears,data.frame(loglength=log(x2014),fYear=factor(2014))))
lines(y2014~x2014,col="red",lwd=2)
# add a legend
legend("topleft",c("2013","2014"),lwd=2,pch=16,col=c("black","red"),bty="n",cex=0.9)

