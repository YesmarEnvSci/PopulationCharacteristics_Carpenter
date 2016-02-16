# Annika Putt
# Created August 28, 2014
# fishidlengthweight.r
# Generally followign fishR methods!


source("fishidimport.r")
head(agetab.all) # This is all fish from carpenter. Ages have been added but unaged fish are still there

# Set the species
#SP <- "bt"
#SP <- "rb"
SP <- "ko"
#SP <- "mw"

# Separate into species
all  <- subset(agetab.all,species==SP)
all  <- subset(all,y != "2012")
all$y <- factor(all$y)

####################
# Plotting #########
####################

# 1 Length frequency plots
windows()
ggplot(all,aes(x=lengthmm)) + 
  geom_histogram(binwidth=25,colour="black",fill="grey") +
  facet_grid(y ~ .) +
  labs(x="Length (mm)",y="Count") +
  theme_bw()

# 2 Cumulative density functions
windows()
ggplot(all,aes(x=lengthmm, colour=y)) + 
  labs(x="Length (mm)",y="Proportion") +
  stat_ecdf(size=1.2) +
  theme_bw() +
  theme(legend.title=element_blank(), legend.key=element_rect(colour=NA)) 

###############################################
# Compare Length Distributions between Groups #
###############################################

# Komogorov Smirnov Test
# Two sample test that determines whether ECDFs from different years are the same
ks.test(subset(all,y==2013)$lengthmm,subset(all,y==2014)$lengthmm)
ks.test(subset(all,y==2013)$lengthmm,subset(all,y==2015)$lengthmm)
ks.test(subset(all,y==2014)$lengthmm,subset(all,y==2015)$lengthmm)

# Chi-square tests for determining if size structure varies by year, gear, etc
# High p values suggest that distributions of frequencies don't differ between categories
# Create lcat. Check the ALF func if you want the categories to be the same.
all <- mutate(all,lcat=lencat(lengthmm,w=50))
yearFreq <- xtabs(~y+lcat,data=all)
chisq.test(yearFreq)
# Table of rwo percentages can help see where differences are 
round(prop.table(yearFreq,margin=1)*100,1)
 
# Note that the chi.square test won't work when there are zeros...
gearFreq <- xtabs(~gear+lcat,data=all)
chisq.test(gearFreq)
round(prop.table(gearFreq,margin=1)*100,1)
# Can compare each gear, get a p value, then adjust the p value for multiple comparisons
gearpvals <- c(chisq.test(gearFreq[2:3,])$p.value,
               chisq.test(gearFreq[c(2,4),])$p.value,
               chisq.test(gearFreq[3:4,])$p.value)
p.adjust(gearpvals)

# Two sample t test for difference in population means
y2013 <- subset(all,y==2013)
y2014 <- subset(all,y==2014)
y2015 <- subset(all,y==2015)
t.test(y2013$lengthmm,y2014$lengthmm)
t.test(y2013$lengthmm,y2015$lengthmm)
t.test(y2014$lengthmm,y2015$lengthmm)

# Logistic regression
summary(glm(lengthmm~y,data=all))
summary(glm(lengthmm~gear,data=all))
summary(glm(lengthmm~y+gear,data=all))
summary(glm(lengthmm~y:gear,data=all))

#bestmod <- glm(lengthmm~gear,data=all)


###########################
# Compare LvsW by Year ###
##########################

# Log-linear Model
all$logweight <- log(all$weightg)
all$loglength <- log(all$lengthmm)
fitbt <- lm(logweight~loglength,data=all)
coef(fitbt)
confint(fitbt)

# Hypothesis test for whether the model with the explanitory varible explains significantly more of the variability than 
# a model without the explanatory variable
anova(fitbt) # has a massive f and tiny p, so yes.
summary(fitbt)

# Create a conversion factor based on the model sigma for back transforming
syx <- summary(fitbt)$sigma
cf  <- exp((syx^2)/2)

# Assumption checking
r  <- residuals(fitbt)
fv <- fitted(fitbt)
windows()
plot(r~fv,xlab="Fitted Values",ylab="Residuals",main="Fitted vs Residuals of Log-linear LxW Model")
abline(h=0)
#hist(r,xlab="Residuals")

# Plot the fit
windows()
plot(logweight~loglength,data=all,pch=16,col=rgb(0,0,0,1/4),
     ylab="log(Weight)",xlab="log(Fork Length)",las=1,main="Log-Linear Model of Weight vs Length") # Note the shading allows you to see where points overlie
#abline(fitbt,col="gray20",lwd=2,lty="solid") # OR predict and plot
x <- seq(min(all$loglength,na.rm=TRUE),max(all$loglength,na.rm=TRUE),length.out=99)
y <- predict(fitbt,data.frame(loglength=x),interval="prediction")
lines(y[,"fit"]~x,col="gray20",lwd=2,lty="solid")
lines(y[,"lwr"]~x,col="gray20",lwd=2,lty="dashed")
lines(y[,"upr"]~x,col="gray20",lwd=2,lty="dashed")
mtext(sprintf("log(W) = %s*log(L) %s: R-squared %s",round(fitbt$coef[2],2),round(fitbt$coef[1],2),round(summary(fitbt)$r.squared,2)), adj=0, padj=0)

# Superimpose on the original data
windows()
plot(weightg~lengthmm,data=all,pch=16,col=rgb(0,0,0,1/4),
       ylab="Weight (g)",xlab="Fork Length (mm)",las=1,main="Log-Linear Model of Weigth vs Length")
x <- seq(min(all$lengthmm,na.rm=TRUE),max(all$lengthmm,na.rm=TRUE),length.out=99)
y <- exp(predict(fitbt,data.frame(loglength=log(x)),interval="prediction"))*cf
lines(y[,"fit"]~x,col="gray20",lwd=2,lty="solid")
lines(y[,"lwr"]~x,col="gray20",lwd=2,lty="dashed")
lines(y[,"upr"]~x,col="gray20",lwd=2,lty="dashed")

# Compare length-weigth relationships between years
all$fYear  <- factor(all$y)
fitbtyears <- lm(logweight~loglength*fYear,data=all) # loglength is now considered a covariate
# short hand for response ~ covariate + factor + factor:covariate
anova(fitbtyears)
# p value for the interaction is very low, so the slopes of the two lines are statistically different
cbind(coef(fitbtyears),confint(fitbtyears)) # the bottom left number tells how different the two slopes are

# Plot the fit from the multiple years
# Copy the ggplot colours for consistency
gg_color_hue <- function(n) {
  hues = seq(15, 375, length=n+1)
  hcl(h=hues, l=65, c=100)[1:n]
}
ggcolours <- gg_color_hue(3)

windows()
newpred <- seq(0,1000,length.out=99)
plot(weightg~lengthmm,data=all,col="white",ylab="Weight (g)",xlab="Fork Length (mm)",las=1) # set up plot

points(weightg~lengthmm,data=Subset(all,fYear==2013),pch=16,col=ggcolours[1],cex=0.6)
y2013 <- exp(predict(fitbtyears,data.frame(loglength=log(newpred),fYear=factor(2013))))
lines(y2013~newpred,col=ggcolours[1],lwd=2)

points(weightg~lengthmm,data=Subset(all,fYear==2014),pch=16,col=ggcolours[2],cex=0.6)
y2014 <- exp(predict(fitbtyears,data.frame(loglength=log(newpred),fYear=factor(2014))))
lines(y2014~newpred,col=ggcolours[2],lwd=2)

points(weightg~lengthmm,data=Subset(all,fYear==2015),pch=16,col=ggcolours[3],cex=0.6)
y2015 <- exp(predict(fitbtyears,data.frame(loglength=log(newpred),fYear=factor(2015))))
lines(y2015~newpred,col=ggcolours[3],lwd=2)
# add a legend
legend("topleft",c("2013","2014","2015"),lwd=2,pch=16,col=c(ggcolours[1],ggcolours[2],ggcolours[3]),bty="n",cex=0.9)

