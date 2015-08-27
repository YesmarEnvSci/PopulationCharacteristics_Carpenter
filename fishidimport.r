# Annika Putt
# Created August 28, 2014
# fishidimport.r

# Read in the fishid table. It must be saved in the working directory as brgmonfishid_current.csv.
fishid <- read.csv("brgmonfishid_current.csv",head=TRUE,
                   colClasses=c("fishid"="character","sitedescription"="character","pit"="character","structurecode"="character","age"="character",
                                "spawningcondition"="character","fishidcomments"="character","tissue.sediment"="character","recap"="logical",
                                "radio"="character"))

# Remove any blank columns imported in excel mishaps
fishid <- fishid[,1:28]

# List column classes
sapply(fishid,class)

# Turn the date into a Date object
# If NAs are appearing, highlight the date column in excel and format as dd-mmm-yy
fishid$date <- as.Date(fishid$date,format="%d/%b/%y")

# Assign fasle to recap logical column
fishid$recap[is.na(fishid$recap)] <- "FALSE"

# paste the factor levels to make sure that there are no weird excel spacing issues
print("Look for duplicate factor levels indicating space issues in the database")
print(levels(fishid$species))
print(levels(fishid$waterbody))
print(levels(fishid$gear))
print(levels(fishid$sex))
print(levels(fishid$monitor))
