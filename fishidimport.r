# Annika Putt
# Created August 28, 2014
# fishidimport.r

source("fishidlibraries.r")

# Read in the fishid table. It must be saved in the working directory as brgmonfishid_current.csv.
fishid <- read.csv("brgmonfishid_current.csv",head=TRUE, colClasses=c("fishid"="character","sitedescription"="character","pit"="character","structurecode"="character","age"="character",
                                "spawningcondition"="character","fishidcomments"="character","tissue.sediment"="character","recap"="logical",
                                "radio.acoustic"="character","mort."="logical","y"="factor"))

# Remove any blank columns imported in excel mishaps
fishid <- fishid[,1:28]

# List column classes
sapply(fishid,class)

# Turn the date into a Date object
# If NAs are appearing, highlight the date column in excel and format as dd-mmm-yy
fishid$date <- as.POSIXlt(fishid$date,format="%d-%b-%y")

# Assign fasle to recap logical column
fishid$recap[is.na(fishid$recap)] <- "FALSE"

# paste the factor levels to make sure that there are no weird excel spacing issues
print("Look for duplicate factor levels indicating space issues in the database")
print(levels(fishid$species))
print(levels(fishid$waterbody))
print(levels(fishid$gear))
print(levels(fishid$sex))
print(levels(fishid$monitor))

###############
# Add in Age Data

# Read in the age table
ages    <- read.csv("brgmonages_current.csv",head=TRUE,na.strings="")
onlyage <- ages[c("fishid","age")]
colnames(onlyage) <- c("fishid","ifrage")

# Merge the ages into the fishid table, using the instream ages over Gene's ages whenever possible
mergeages <- merge(onlyage,fishid,by=c("fishid"),all.y=TRUE)
mergeages  <- subset(mergeages,waterbody=="cl")

agetab.all <- subset(mergeages,y %in% c(2013,2014,2015)) 
agetab     <- subset(agetab.all,ifrage != "NA")
