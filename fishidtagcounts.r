# Annika Putt
# Created October 22, 2014
# fishidtagcounts.r

# source the import file to obtain "fishid" data frane
source("fishidimport.r")

##########
# Make a summary table of the total numbers of fish caught and tagged split into the two years and the total

# create a blank template
Species   <- c("Bull Trout","Rainbow Trout","Mountain Whitefish","Bridgelip Sucker","Kokanee")

# create a function to count fishid
countfunc <- function(fishdata) {
  funcdata     <- subset(fishdata,waterbody=="cl")
  # remove recaps
  funcdata     <- subset(funcdata,recap!=TRUE)
  funcdata["count"] <- 1 # create a column to sum for counts
  totals       <- aggregate(funcdata[c("count")], by=list(year=funcdata$y, species=funcdata$species), FUN=sum, na.rm=FALSE)
  allyrs       <- aggregate(funcdata[c("count")], by=list(species=funcdata$species), FUN=sum, na.rm=FALSE)
  totalstagged <- aggregate(subset(funcdata,pitlast6!="NA")[c("count")], by=list(year=subset(funcdata,pitlast6!="NA")$y, species=subset(funcdata,pitlast6!="NA")$species), FUN=sum, na.rm=FALSE)
  allyrstagged <- aggregate(subset(funcdata,pitlast6!="NA")[c("count")], by=list(species=subset(funcdata,pitlast6!="NA")$species), FUN=sum, na.rm=FALSE)
  
  specieslist  <<- c("bt","rb","mw","ko")
  rowlist      <- list()
  for (i in 1:length(specieslist)) {
    temp    <- c(subset(totals,year=="2013" & species==specieslist[i])$count,
                 subset(totalstagged,year=="2013" & species==specieslist[i])$count,
                 subset(totals,year=="2014" & species==specieslist[i])$count,
                 subset(totalstagged,year=="2014" & species==specieslist[i])$count,
                 subset(totals,year=="2015" & species==specieslist[i])$count,
                 subset(totalstagged,year=="2015" & species==specieslist[i])$count,
                 subset(allyrs,species==specieslist[i])$count,
                 subset(allyrstagged,species==specieslist[i])$count)
    rowlist[[i]] <- temp
  }
  # Add in zeros for kokanee
  ko <- rowlist[[4]]
  ko <- c(ko[1],0,ko[2:5])
  rowlist[[4]] <- ko
  return(rowlist)
}


capturetab <- data.frame(do.call("rbind",countfunc(fishdata=fishid)))
colnames(capturetab) <- c("total2013","tagged2013","total2014","tagged2014","total2015","tagged2015","total","totaltagged")
capturetab$species <- specieslist
print(capturetab)
