# Annika Putt
# Created October 22, 2014
# fishidrecaps.r

# source the import file to obtain "fishid" data frane
source("fishidimport.r")

cl <- subset(fishid,waterbody=="cl")
  
# create a recap table
recaps <- subset(fishid,recap==TRUE)

##########
# total number of recaps
recap    <- recaps
recap    <- subset(recap,waterbody=="cl")
nonrecap <- subset(fishid,recap==FALSE)
nonrecap <- subset(nonrecap,waterbody=="cl")
recap["count"] <- 1
nonrecap["count"] <- 1

recaps.count    <- aggregate(recap[c("count")], by=list(species=recap$species), FUN=sum, na.rm=FALSE)
nonrecaps.count <- aggregate(nonrecap[c("count")], by=list(species=nonrecap$species), FUN=sum, na.rm=FALSE)
nonrecaps.count <- subset(nonrecaps.count,species %in% c("bt","mw","rb"))

# Determine the number of duplicate recaps
recap.n_occur <- data.frame(table(recap$pitlast6)) # gives you a data frame with a list of ids and the number of times they occurred.
recap.dup     <- recap.n_occur[recap.n_occur$Freq > 1,] # tells you which ids occurred more than once

##########
# Determine the percentage of recaps for rainbows and bull trout for a couple different sessions

RecapFunc <- function(Funcdata,startdate,enddate) { # dates must be in the format "yyyy=mm=dd"
  data <- subset(Funcdata,species=="bt") # I haven't figured out how to make this an option yet
  data["count"] <- 1
  recapdata <- subset(data,recap==TRUE)
  nonrecapdata <- subset(data,recap==FALSE)
  marked <- subset(nonrecapdata,pitlast6!="NA") # of non recaps, which actually had a pit implanted?
  availablemarks <- subset(marked,date<startdate) # of marked fish, which were available prior to the selected period
  studyperiod <- subset(recapdata,date>=startdate & date<=enddate) # select all recaps in the study period
  # for this we ignore any recaps that were tagged and caught during the study period #
  # Note if any fish were marked and recaught during the study period
   all.studyperiod <- subset(data,date>=startdate & date<=enddate)
   dup.1 <- data.frame(table(all.studyperiod$pitlast6))
   dup.2 <- dup.1[dup.1$Freq>1,]
   if (nrow(dup.2)==0) {
     print("No fish marked and recaught during study period")
     } else {
       print("Fish caught and marked during study period")}
  count.available <- nrow(availablemarks)
  count.recap     <- nrow(studyperiod)
  total.caught    <- nrow(all.studyperiod)
  percent.recap   <- count.recap/total.caught
  list(Start=startdate,End=enddate,AvailableMarks=count.available,TotalCaught=total.caught,
       Recaptures=count.recap,RecapPercentage_InSample=percent.recap)
}

Spring2014EF <- RecapFunc(cl,"2014-06-6","2014-06-8")
August2014AN <- RecapFunc(cl,"2014-08-12","2014-08-13")
Fall2014GN   <- RecapFunc(cl,"2014-10-16","2014-10-16")
Fall2014EF   <- RecapFunc(cl,"2014-10-08","2014-10-08")  
