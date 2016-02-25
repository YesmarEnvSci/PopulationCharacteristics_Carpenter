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

####################################
# Are Lengths Equal Between Years? #
####################################
# compare mean length by year
all$yearfactor <- factor(all$y)

windows()
boxplot(lengthmm~yearfactor,data=all,las=1,ylab="Length (mm)")

# Check assumptions of normality
bartlett.test(all$lengthmm,all$yearfactor) # Variances are homogenous if the p value is > 0.5
meanfit <- lm(formula = all$lengthmm~all$yearfactor)
anova(meanfit) # If the p-value is >0.05 we can accept the null that the means are equal
# If p-val is <0.05 we can conclude that there is a relationship between year and length

# If the means are NOT equal, we can check to see where the differences are with t-tests
y2013 <- subset(all,y==2013)
y2014 <- subset(all,y==2014)
y2015 <- subset(all,y==2015)
t.test(y2013$lengthmm,y2014$lengthmm)
t.test(y2013$lengthmm,y2015$lengthmm)
t.test(y2014$lengthmm,y2015$lengthmm)

# Do the same for gear; first need to clean up a bit
all.gear <- subset(all,gear != "")
all.gear$gear <- factor(all.gear$gear)
all.gear <- subset(all.gear,!is.na(lengthmm))
boxplot(lengthmm~gear,data=all.gear,las=1,ylab="Length (mm)")

meanfit.gear <- lm(formula=all.gear$lengthmm~all.gear$gear)
anova(meanfit.gear)

# If the means are NOT equal, we can check to see where the differences are with t-tests
gn <- subset(all.gear,gear=="gn")
an <- subset(all.gear,gear=="an")
ef <- subset(all.gear,gear=="ef")
t.test(gn$lengthmm,ef$lengthmm)
t.test(an$lengthmm,ef$lengthmm)
t.test(an$lengthmm,gn$lengthmm)


#####################################
# Are groups from the same cdf? #####
#####################################
# Komogorov Smirnov Test
# Two sample test that determines whether ECDFs from different years are the same
# Large p-val means accept the null that the groups are from the same cdf
ks.test(subset(all,y==2013)$lengthmm,subset(all,y==2014)$lengthmm)
ks.test(subset(all,y==2013)$lengthmm,subset(all,y==2015)$lengthmm)
ks.test(subset(all,y==2014)$lengthmm,subset(all,y==2015)$lengthmm)

# Doesn't seem to be working for gear. Maybe because it's a categorical factor?
# ks.test(subset(all.gear,gear=="gn")$lengthmm,subset(all.gear,gear=="gn")$lengthmm)
# ks.test(subset(all.gear,gear=="an")$lengthmm,subset(all.gear,gear=="an")$lengthmm)
# ks.test(subset(all.gear,gear=="ef")$lengthmm,subset(all.gear,gear=="ef")$lengthmm)

###############################################
# Are Gear and Year Predictors of Length? #####
###############################################
# Check out the chi-squared test in the aiffd or ogle book. Took it out because it wasn't doing much

# Logistic regression
mod1 <- glm(lengthmm~1,data=all.gear)
mod2 <- glm(lengthmm~y,data=all.gear)
mod3 <- glm(lengthmm~gear,data=all.gear)
mod4 <- glm(lengthmm~y+gear,data=all.gear)
mod5 <- glm(lengthmm~y:gear,data=all.gear)

mod1aic <- extractAIC(mod1)
mod2aic <- extractAIC(mod2)
mod3aic <- extractAIC(mod3)
mod4aic <- extractAIC(mod4)
mod5aic <- extractAIC(mod5)

AICTab <- data.frame(Model = c("1","2","3","4","5"), AIC = c(mod1aic[2], mod2aic[2], mod3aic[2], mod4aic[2], mod5aic[2]))
bestmod <- AICTab[which.min(AICTab$AIC),]

#################################################
# Does Length to Weight Ratio Differ by Year? ###
#################################################

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
# If the p value for the interaction is <0.05, the slopes of the two lines are statistically different
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

