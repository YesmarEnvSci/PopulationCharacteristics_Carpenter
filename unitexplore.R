# Annika Putt
# Created October 20, 2014
# unitexplore.r

# load libraries
library(ggplot2)
library(scales) 

# source the import file to obtain "fishid" data frane
source("fishidimport.r")

##########
# turn date into a factor
fishid$date2 <- as.factor(fishid$date)

# Create length histograms for all fish
lengthhist <- function(sp) {
  temp <- subset(fishid,species==sp & waterbody=="cl")
  ggplot(temp,aes(lengthmm)) + geom_histogram()
}

lengthhist(sp="mw")
lengthhist(sp="bt")

# Create length histograms for all fish coloured by date
lengthbydate <- function(sp) {
  temp <- subset(fishid,species==sp & waterbody=="cl")
  ggplot(temp,aes(lengthmm,fill=date2)) + geom_histogram()
}

lengthbydate(sp="mw")
lengthbydate(sp="bt")

# create weigth histograms for all fish
weighthhist <- function(sp) {
  temp <- subset(fishid,species==sp & waterbody=="cl")
  ggplot(temp,aes(weightg)) + geom_histogram()
}

weighthhist(sp="mw")
weighthhist(sp="bt")

# create weight histogram coloured by date
weightbydate <- function(sp) {
  temp <- subset(fishid,species==sp & waterbody=="cl")
  ggplot(temp,aes(weightg,fill=date2)) + geom_histogram()
}

weightbydate(sp="mw")
weightbydate(sp="bt")

# plot length vs weight
lvsw <- function(sp) {
  temp <- subset(fishid,species==sp & waterbody=="cl")
  ggplot(data=temp,aes(x=lengthmm, y=weightg, color=date2)) + geom_point()
}

lvsw(sp="mw")
lvsw(sp="bt")
