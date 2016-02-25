############################
# markrecap_2015.R
# PopulationCharacteristics_Carpenter.Rproj

# Runs a closed mark recapture for Carpenter Reservoir in 2015

# Files sourced:
# Files produced:

# Created February 17, 2016
# A Putt
#############################

source("fishidimport.r")

# Closed Peterson Mark Recapture
# N̂=((n_1+1)(n_2+1))/((m_2+1))-1
# Var(N ̂ )=(n_1+1)(n_2+1)(n_1-m_2 )(n_2-m_2 )/((m_2+1)^2 (m_2+2) )

# 1. Pull only the data from the 2015 mark recapture period
# This was fish caught between July 1 to July 7 and also between July 27 and July 30
fishid <- subset(fishid,waterbody=="cl")
fishid <- subset(fishid,species=="bt")
head(fishid)

cmr2015 <- subset(fishid,date  > "2015-06-30" & date < "2015-08-01")
period1 <- subset(cmr2015,date > "2015-06-30" & date < "2015-07-08")
period2 <- subset(cmr2015,date > "2015-07-07" & date < "2015-08-01")

# Check to make sure that all fish during were caught within the study area
print(levels(as.factor(period1$sitedescription)))
print(levels(as.factor(period2$sitedescription)))

# Remove unwanted sites (outside of the m-r boundary)
period2 <- subset(period2, sitedescription != "0543913/5627014; creek south of falls")
period2 <- subset(period2, sitedescription != "middle bridge river")
period2 <- subset(period2, sitedescription != "keary creek confluence")

# 2. Determine the number of fish tagged in period 1

# Remove any mortalities from period1
period1.mortalities <- subset(period1, mort.==TRUE)
period1 <- subset(period1, is.na(mort.))

# 3. Create variables necessary for the mark-recapture model

# n1: the number of Bull Trout captured and marked in the first period
n1.fish <- subset(period1,!is.na(pitlast6))
n1      <- nrow(n1.fish)

# n2: number of fish captured in the second period
n2 <- nrow(period2)

# m2: the number of fish captured in the second period that were marked in the first period
recaptures <- intersect(period1$pitlast6,period2$pitlast6) # Looks for common elements in the two pit vectors
(duplicated(recaptures)) # Make sure that none of these recaptures were caught more than once. Not sure if intersect cleans them out.
m2         <- length(recaptures)

# 4. Run the chapman estimate and the variance
N    <- ( ((n1+1)*(n2+1)) / (m2+1) ) -1
varN <- ( (n1+1)*(n2+1)*(n2-m2)*(n1-m2) ) / ( ((m2+1)^2)*(m2+2) )
stdE <- sqrt(varN)

###################################
# FSA Package Method ##############
###################################

fsaN       <- mrClosed(M=n1,n=n2,m=m2,method="Chapman")
fsaSummary <- summary(fsaN,incl.SE=TRUE)
fsaConf    <- confint(fsaN,verbose=TRUE) # Poisson distribution is used because m < 50 (Seber 2002)

###################################
# CPUE Calculation ################
###################################

# I have created a data frame with just the effort data form dates and sites 
# relating to the mark recapture

effort <- read.csv("Data/MarkRecaptureEffort_2015.csv",head=TRUE,na.strings=c("","NA"))

# Change all effort colums to seconds
effort$gnsoakseconds   <- effort$gnsoakhours
effort$secondsangling  <- effort$hoursangling
effort$efseconds <- effort$efseconds/60/60

# for all counts, change NAs to 0
effort$bt[is.na(effort$bt)] <- 0
effort$rb[is.na(effort$rb)] <- 0
effort$ko[is.na(effort$ko)] <- 0
effort$mw[is.na(effort$mw)] <- 0

library(dplyr)
cpueFunc   <- function(onespecies) {
  gn       <- subset(onespecies,gear=="gn")
  an       <- subset(onespecies,gear=="an")
  ef       <- subset(onespecies,gear=="ef")
  gn$cpue  <- gn$count/gn$gnsoakseconds # CPUE per hour for each event
  an$cpue  <- an$count/an$secondsangling
  ef$cpue  <- ef$count/ef$efseconds
  all      <- rbind(an,gn,ef)
  allgears <- data.frame(gear="all",samples=nrow(all),fish=sum(all$count),mean=mean(all$cpue),sd=sd(all$cpue),se=sd(all$cpue)/sqrt(nrow(all)))
  summary  <- all %>% group_by(gear) %>% summarize(samples=n(),fish=sum(count), mean=mean(cpue),sd=sd(cpue), 
                                                   se=sd/sqrt(samples)) %>% as.data.frame()
  final    <- rbind(summary,allgears)
  return(final)
}
   
bulltrout <- data.frame(count=effort$bt,gear=effort$gear,gnsoakseconds=effort$gnsoakseconds,secondsangling=effort$secondsangling,efseconds=effort$efseconds)
rainbow   <- data.frame(count=effort$rb,gear=effort$gear,gnsoakseconds=effort$gnsoakseconds,secondsangling=effort$secondsangling,efseconds=effort$efseconds)
kokanee   <- data.frame(count=effort$ko,gear=effort$gear,gnsoakseconds=effort$gnsoakseconds,secondsangling=effort$secondsangling,efseconds=effort$efseconds)
whitefish <- data.frame(count=effort$mw,gear=effort$gear,gnsoakseconds=effort$gnsoakseconds,secondsangling=effort$secondsangling,efseconds=effort$efseconds)

btCpue <- data.frame(cpueFunc(onespecies=bulltrout),species="bt")
rbCpue <- data.frame(cpueFunc(onespecies=rainbow),species="rb")
koCpue <- data.frame(cpueFunc(onespecies=kokanee),species="ko")
mwCpue <- data.frame(cpueFunc(onespecies=whitefish),species="mw")

CPUE   <- rbind(btCpue,rbCpue,koCpue,mwCpue)
write.csv(file="Results/MarkRecapTable_2015.csv",CPUE,row.names=FALSE)
