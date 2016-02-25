############################
# fishidcountsummaries.R
# PopulationCharacteristics_Carpenter.Rproj

# Creates a summary table of tagging data 

# Files sourced:
# Files produced:

# Created October 20, 2014
# A Putt
#############################

# source the import file to obtain "fishid" data frane
source("fishidimport.r")

fishid <- subset(fishid,waterbody=="cl") # Select only the Carpenter fish data
fishid["count"] <- 1 # create a column to sum for counts

#############################
# Create Tables of Counts ###
#############################

# Make a summary table of the total numbers of fish caught and tagged split into by years and the total

# Create a bunch of tables to pull data from depending on the category of the fish
fishtagged    <- subset(fishid,recap!=TRUE)
fishtagged    <- subset(fishtagged,!is.na(pitlast6)) # Fish tagged for the first time
fishrecap     <- subset(fishid,recap==TRUE) # Fish recaptured
fishhandled   <- subset(fishid,is.na(pitlast6)) # Fish handled but never tagged
fishhandled   <- subset(fishhandled,recap!=TRUE)

# 1. Total fish in Carpenter
# Count the total number of non-recap fish, separated by species and year
totals       <- aggregate(fishid[c("count")], by=list(year=fishid$y, species=fishid$species), FUN=sum, na.rm=FALSE)
# Count the total number of non-recap fish, separated by species only
allyrs       <- aggregate(fishid[c("count")], by=list(species=fishid$species), FUN=sum, na.rm=FALSE)

# 2. Total fish tagged for the first time
totalstagged <- aggregate(fishtagged[c("count")], by=list(year=fishtagged$y, species=fishtagged$species), FUN=sum, na.rm=FALSE)
allyrstagged <- aggregate(fishtagged[c("count")], by=list(species=fishtagged$species), FUN=sum, na.rm=FALSE)
  
# 3. Total fish recaptured
totalsrecap <- aggregate(fishrecap[c("count")], by=list(year=fishrecap$y, species=fishrecap$species), FUN=sum, na.rm=FALSE)
allyrsrecap <- aggregate(fishrecap[c("count")], by=list(species=fishrecap$species), FUN=sum, na.rm=FALSE)

# 4. Total fish only handled but never had a tag
totalshandled <- aggregate(fishhandled[c("count")], by=list(year=fishhandled$y, species=fishhandled$species), FUN=sum, na.rm=FALSE)
allyrshandled <- aggregate(fishhandled[c("count")], by=list(species=fishhandled$species), FUN=sum, na.rm=FALSE)

# Run a check to make sure that all fish are covered by the different categories
sprintf("Total Carpenter Fish = %s, Sum Tagged, Recap, Handled = %s",nrow(fishid),
        sum(nrow(fishtagged),nrow(fishrecap),nrow(fishhandled)))

############################
# Examine Recaps ###########
############################

# Determine the number of multiple recaps
recap.n_occur  <- data.frame(table(fishrecap$pitlast6)) # gives you a data frame with a list of ids and the number of times they occurred.
recap.multiple <- recap.n_occur[recap.n_occur$Freq > 1,] # tells you which ids occurred more than once
