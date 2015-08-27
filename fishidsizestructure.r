# Annika Putt
# Created August 28, 2014
# fishidsizestructure.r

# load libraries
library(ggplot2)
library(scales)
library(FSA)
library(plyr)
library(Matching)


# source the import file to obtain "fishid" data frane
source("fishidimport.r")

cl <- subset(fishid,waterbody=="cl")
clbt <- subset(cl,species=="bt")
clmw <- subset(cl,species=="mw")
clrb <- subset(cl,species=="rb")

##########
# Pick an option
#clsize <- clbt
#clsize <- clmw
clsize <- clrb

##########

# Create length categories
clsize <- mutate(clsize,lcat50=lencat(lengthmm,w=50,as.fact=TRUE))

# Create a length freqency table for the species
btFreq50 <- xtabs(~lcat50,data=clsize)
# Convert to percentages
btPerc50 <- round(prop.table(btFreq50)*100,1)

# Create histograms
hist(~lengthmm,data=clsize,xlab="Fork Length mm",freq=TRUE)
hist(lengthmm~y,data=clsize,xlab="Fork Length mm",same.ylim=FALSE)

# Adjust histograms a bit
# Plot the frequency of percent for each year
clsize2013 <- subset(clsize,y==2013)
clsize2014 <- subset(clsize,y==2014)

par(mfrow = c(1,2),oma=c(0.5,0,0,0))

if (clsize$species[1]=="rb") (mybreaks <- seq(from=100,to=500,by=30))
if (clsize$species[1]=="mw") (mybreaks <- seq(from=0,to=400,by=30))
if (clsize$species[1]=="bt") (mybreaks <- seq(from=150,to=600,by=50))

h2013 <- hist(~lengthmm,data=clsize2013,breaks=mybreaks,plot=FALSE)
  h2013$counts=h2013$counts/sum(h2013$counts)
  plot(h2013,xlab="",ylim=c(0,0.45),main="2013",ylab="Proportion of Sample",col="grey")
h2014 <- hist(~lengthmm,data=clsize2014,breaks=mybreaks,plot=FALSE)
  h2014$counts=h2014$counts/sum(h2014$counts)
  plot(h2014,xlab="",ylim=c(0,0.45),main="2014",ylab="",col="grey")
mtext(side=1,line=-2,text="Fork Length mm", outer=TRUE)

# Cumulative frequencies
# empiracle cumulative distribution function: prop of fish that are less 
# than each observed length

plot(ecdf(clsize$lengthmm),xlab="Fork Length mm",do.points=FALSE,verticals=TRUE,main="")

plot(ecdf(subset(clsize,y==2013)$lengthmm),xlab="Fork Length mm",do.points=FALSE,verticals=TRUE,main="")
plot(ecdf(subset(clsize,y==2014)$lengthmm),add=TRUE,xlab="Fork Length mm",do.points=FALSE,verticals=TRUE,main="",col="gray50")
legend("bottomright",c("2013","2014"),col=c("black","gray50"),lty=1,bty="n",cex=0.75)


# Komogorov Smirnov Test
# Two sample test that determines whether ECDFs from different years are the same
ks.test(subset(clsize,y==2013)$lengthmm,subset(clsize,y==2014)$lengthmm)
# Can use the bootstrap method because we don't have continuous data
ks.boot(subset(clsize,y==2013)$lengthmm,subset(clsize,y==2014)$lengthmm,nboots=5000)


# Chi-square tests for determining if size structure varies by year, gear, etc
# High p values suggest that distributions of frequencies don't differ between categories
yearFreq <- xtabs(~y+lcat50,data=clsize)
chisq.test(yearFreq)
# Table of rwo percentages can help see where differences are 
round(prop.table(yearFreq,margin=1)*100,1)

gearFreq <- xtabs(~gear+lcat50,data=clsize)
chisq.test(gearFreq)
round(prop.table(gearFreq,margin=1)*100,1)
# Can compare each gear, get a p value, then adjust the p value for multiple comparisons
gearpvals <- c(chisq.test(gearFreq[2:3,])$p.value,
               chisq.test(gearFreq[c(2,4),])$p.value,
               chisq.test(gearFreq[3:4,])$p.value)
p.adjust(gearpvals)


# Two sample t test for difference in population means
y2013 <- subset(clsize,y==2013)
y2014 <- subset(clsize,y==2014)
t.test(y2013$lengthmm,y2014$lengthmm)

# Logistic regression
summary(glm(lengthmm~y,data=clsize))
summary(glm(lengthmm~gear,data=clsize))
summary(glm(lengthmm~y+gear,data=clsize))
summary(glm(lengthmm~y:gear,data=clsize))

bestmod <- glm(lengthmm~gear,data=clsize)
