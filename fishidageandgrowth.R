############################
# fishidgrowthandage.R
# PopulationCharacteristics_Carpenter.Rproj

# Looks at length frequency, length vs weight, ages, etc. 
# Closely follows chapter 5 of AIFFD

# Files sourced:
# Files produced:

# Created February 5, 2016
# A Putt
#############################

source("fishidimport.r")
head(agetab.all) # ages merged with fishid
head(ages) # raw age table

# Set the species
#SP <- "bt"
#SP <- "rb"
#SP <- "ko"
SP <- "mw"

# Separate into species
all  <- subset(agetab.all,species==SP)
aged <- subset(all,!is.na(ifrage))

##########################
# Summarize Aging     ####
##########################

StructuresAged <- ddply(ages,.(species),summarize,scale=sum(!is.na(scaleage)),otolith=sum(!is.na(otoage)))

##########################
# Von Bert estimation ####
##########################

# Use a hand-made func with optim
theta <- c(450, 0.2, -3)
SSQ       <- function(theta, data) {
  Linf    <- theta[1]
  K       <- theta[2]
  t0      <- theta[3]
  epsilon <- rep(0, length(data$ifrage))
  lpred   <- rep(0, length(data$ifrage))
  for (i in 1:length(data$ifrage)) {
    lpred[i]   <- Linf * (1 - exp(-K * (data$ifrage[i] - t0)))
    epsilon[i] <- (data$lengthmm[i] - lpred[i])^2
  }
  ssq <- sum(epsilon)
  return(ssq)
}

out   <- optim(theta, fn = SSQ, method = "BFGS", data=aged[c("ifrage","lengthmm")],hessian=TRUE)
out$V <- solve(out$hessian)  # solve the hessian
out$S <- sqrt(diag(out$V))  # Standard Error
out$R <- out$V/(out$S %o% out$S)  #Correlation

#Plot
lp <- out$par[1] * (1 - exp(-out$par[2] * (aged$ifrage - out$par[3])))
windows()
plot(aged$ifrage, aged$lengthmm, xlab = "Age (yrs)", ylab = "Length (mm)",las=1,pch=19,main="Von Bertalanffy Model of Length and Age")
lines(sort(aged$ifrage), sort(lp), col = "black", lwd = 2)

# Try FSA Method (see IFAR book for more detail)
FSAtheta <- vbStarts(lengthmm~ifrage,data=aged) # Find starting values
FSAfunc  <- function(age,Linf,K,t0) Linf*(1-exp(-K*(age-t0))) # Create von bert func to feed to nls
FSAfunc  <- vbFuns() # This allows for easier parameterization of the function...
FSAfit   <- nls(lengthmm~FSAfunc(ifrage,Linf,K,t0),data=aged,start=FSAtheta,control=list(maxiter=500,trace=TRUE))
FSAcoef  <- coef(FSAfit) # Pull the coefficients
FSAconf  <- confint(FSAfit) # Pull the confidence intervals via profile likelihood
FSAsummary <- summary(FSAfit,correlation=TRUE) # These stats are generally not useful to VB fit because they depend on linear approximations of the model
bootFSA  <- nlsBoot(FSAfit) # Do a boostrapping method to pull confidence intervals as well
confint(bootFSA,plot=TRUE) # I think that a skewed parameter distribution isn't a good sign
newdata  <- seq(0,10,length.out=300)  
FSApred  <- FSAfunc(newdata,Linf=coef(FSAfit)) # Predict the best fit line for plot. THe weird function intake is due to the vbFuns used previously
windows()
plot(lengthmm~ifrage,data=aged,xlab="Age",ylab="Fork Length (mm)",pch=19,col=rgb(0,0,0,1/3))
lines(FSApred~newdata,lwd=2)
# Create confidence intervals
LCI <- UCI <- numeric(length(newdata))
for(i in 1:length(newdata)) {
  tmp    <- apply(bootFSA$coefboot,MARGIN=1,FUN=FSAfunc,t=newdata[i]) 
  LCI[i] <- quantile(tmp,0.025) 
  UCI[i] <- quantile(tmp,0.975) }
lines(UCI~newdata,lwd=2,lty="dashed")
lines(LCI~newdata,lwd=2,lty="dashed")
# Take a look at residuals as well
windows()
residPlot(FSAfit)
# If there is a lot of heteroskedasticity the model can be fit with multiplicative error (i.e., log)
# aged$logl <- log(aged$lengthmm)
# FSAlog <- nls(logl~log(FSAfunc(ifrage,Linf,K,t0)),data=aged,start=FSAtheta)

#################
# Age-Length Key#
#################

# Create a function to run age-length keys
ALKfunc <- function(speciesdata,width) {
  addlength  <- mutate(speciesdata,lcat=lencat(lengthmm,w=width,as.fact=TRUE)) # Create length intervals and append to data frames
  aged       <- subset(addlength,!is.na(ifrage))
  unaged     <- subset(addlength,is.na(ifrage))
  alk.freq   <- xtabs(~lcat+ifrage,data=aged) # Determine frequency in each category
  alk        <- prop.table(alk.freq,margin=1) # Determine the conditional proportions
  
  # Or do a smoothed/modeled age-length key 
  # If you have enough ages you could do this for each year and then do an anova to compare
  mlr        <- multinom(ifrage~lengthmm,data=aged,maxit=500)
  lengthmm   <- seq(min(as.numeric(levels(aged$lcat))),max(as.numeric(levels(aged$lcat))),width)
  alk.mlr    <- predict(mlr,data.frame(lcat=lengthmm),type="probs")
  
  # Visualizing
  windows()
  alkPlot(alk,pal="grey",xlab="Fork Length mm")
  windows()  
  alkPlot(alk,pal="grey",xlab="Fork Length mm",type="bubble")
  windows()
  alkPlot(alk,pal="grey",xlab="Fork Length mm",type="area",showLegend=TRUE)
  
  # Apply the key
  len.n <- xtabs(~lcat,data=addlength)
  tmp   <- sweep(alk,MARGIN=1,FUN="*",STATS=len.n) # Apply the alk to the whole table of ages 
  ad1   <- colSums(tmp,na.rm=TRUE) # Determine the total number of fish at each age
  ad2   <- round(prop.table(ad1),3) # Determine the proportion of fish at each age
  propse <- alkAgeDist(alk,lenA.n=rowSums(alk.freq),len.n=len.n)
  
  # Age all of the unaged fish based on the ALK
  unagedsub <- subset(unaged,lengthmm>min(aged$lengthmm) & lengthmm<max(aged$lengthmm)) # remove length below the key
  newages   <- alkIndivAge(alk,ifrage~lengthmm,data=unagedsub)
  allaged   <- rbind(aged,newages) # Combine all fish aged and unaged within the ALK size range
  
  # return an output
  listout <- list(alk.freq=alk.freq,alk.prop=alk,samplesize=nrow(aged),samplesizeunaged=nrow(unagedsub),numatage=ad1,propatage=ad2,propse=propse,newages=newages,allaged=allaged)
}

allalk <- ALKfunc(all,50)
allaged <- allalk$allaged
allalk$alk.prop
allalk$samplesize
allalk$numatage
# allalk$propatage
allalk$samplesizeunaged
allalk$propse
allalk$alk.freq

################################
# Von Bert with Estimated Ages #
################################

# Re-run the VB using the entire population (with lengths within those that were aged)
starts <- list(Linf=out$par[1],K=out$par[2],t0=out$par[3])# Find starting values

FSAfitall   <- nls(lengthmm~FSAfunc(ifrage,Linf,K,t0),data=allaged,start=starts,control=list(maxiter=500))
bootFSAall  <- nlsBoot(FSAfitall) # Do a boostrapping method to pull confidence intervals as well
confint(bootFSA,plot=TRUE) # I think that a skewed parameter distribution isn't a good sign
FSApredall  <- FSAfunc(newdata,Linf=coef(FSAfitall)) # Predict the best fit line for plot. THe weird function intake is due to the vbFuns used previously
windows()
plot(lengthmm~ifrage,data=allaged,xlab="Age",ylab="Fork Length (mm)",pch=19,col=rgb(0,0,0,1/3))
lines(FSApredall~newdata,lwd=2)
# Create confidence intervals
LCI2 <- UCI2 <- numeric(length(newdata))
for(i in 1:length(newdata)) {
  tmp    <- apply(bootFSAall$coefboot,MARGIN=1,FUN=FSAfunc,t=newdata[i]) 
  LCI2[i] <- quantile(tmp,0.025) 
  UCI2[i] <- quantile(tmp,0.975) }
lines(UCI2~newdata,lwd=2,lty="dashed")
lines(LCI2~newdata,lwd=2,lty="dashed")
# Take a look at residuals as well
windows()
residPlot(FSAfitall)

###########################
# Histogram of all Ages ###
###########################

allaged$agefactor <- factor(allaged$ifrage)
windows()
ggplot(allaged, aes(x=lengthmm, color=agefactor)) + geom_density()

windows()
ggplot(allaged, aes(x=lengthmm, color=agefactor, fill=agefactor)) + 
  geom_histogram(alpha=0.5,position="identity") +
  labs(x="Length (mm)",y="Count") +
  theme_bw() +
  theme(legend.title=element_blank(), legend.key=element_rect(colour=NA))

