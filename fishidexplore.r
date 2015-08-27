# Annika Putt
# Created August 28, 2014
# fishidexplore.r

# load libraries
library(ggplot2)
library(scales) 

# source the import file to obtain "fishid" data frane
source("fishidimport.r")

# create a recap table
recaps <- subset(fishid,recap==TRUE)

##########
# create a column with season 
# note that the seasons are not lunar, but just roughly based on month
getSeason <- function(months) {
  ifelse (months > 11  | months < 3, "winter",
          ifelse (months >= 3 & months < 6, "spring",
                  ifelse (months >= 6 & months < 9, "summer", "fall")))
}

seasons <- getSeason(months=fishid$m)
fishid$season <- as.factor(seasons)

##########
# plot the number of fish caught by location for each species in Carpenter Res
# Separate bars by season as well
numberbyloc <- function(sp) {
  temp <- subset(fishid,species==sp & waterbody=="cl")
  ggplot(temp,aes(sitecode,fill=season)) + geom_bar()
}

numberbyloc(sp="bt")
numberbyloc(sp="mw")
numberbyloc(sp="rb")
numberbyloc(sp="ko")

# create one for all fish separated by panels
numbylocall <- ggplot(data=subset(fishid, species %in% c("bt","ko","mw","rb") & waterbody=="cl"), aes(sitecode,fill=season))+geom_bar()+facet_wrap(~species, nrow=2)
numbylocall + theme(axis.text.x=element_text(angle=90, vjust=0.5))

##########
# Create length histograms for all fish
lengthhist <- function(sp) {
  temp <- subset(fishid,species==sp & waterbody=="cl")
  ggplot(temp,aes(lengthmm)) + geom_histogram()
}

lengthhist(sp="bt")
lengthhist(sp="mw")
lengthhist(sp="rb")
lengthhist(sp="ko")

# create one for all fish separated by panels
ggplot(data=subset(fishid, species %in% c("bt","ko","mw","rb") & waterbody=="cl"), aes(lengthmm))+geom_bar()+facet_wrap(~species, nrow=2)

##########
# create length histograms only for those fish for which we have scales
scaleshist <- function(sp) {
  temp <- subset(fishid,species==sp & waterbody=="cl" & structure %in% c("s","f"))
  ggplot(temp,aes(lengthmm)) + geom_histogram()
}

scaleshist(sp="bt")
scaleshist(sp="mw")
scaleshist(sp="rb")
scaleshist(sp="ko")

# create one for all fish separated by panels
ggplot(data=subset(fishid, species %in% c("bt","ko","mw","rb") & waterbody=="cl" & structure %in% c("s","f")), aes(lengthmm))+geom_bar()+facet_wrap(~species, nrow=2)

##########
# create weight histograms for all of the species
weighthhist <- function(sp) {
  temp <- subset(fishid,species==sp & waterbody=="cl")
  ggplot(temp,aes(weightg)) + geom_histogram()
}

weighthhist(sp="bt")
weighthhist(sp="mw")
weighthhist(sp="rb")
weighthhist(sp="ko")

# create one for all fish separated by panels
ggplot(data=subset(fishid, species %in% c("bt","ko","mw","rb") & waterbody=="cl"), aes(weightg))+geom_bar()+facet_wrap(~species, nrow=2)

##########
# create plots of length vs weight for all of the species
lvsw <- function(sp) {
  temp <- subset(fishid,species==sp & waterbody=="cl")
  ggplot(data=temp,aes(x=lengthmm, y=weightg)) + geom_point()
}

lvsw(sp="bt")
lvsw(sp="mw")
lvsw(sp="rb")
lvsw(sp="ko")

# create a length vs weight plot with all species
ggplot(data=subset(fishid, species %in% c("bt","ko","mw","rb") & waterbody=="cl"), aes(x=lengthmm, y=weightg,  group=species, colour=species)) + geom_point()

# create a length vs weight plot with all species as their own panel
lvswall <- ggplot(data=subset(fishid, species %in% c("bt","ko","mw","rb") & waterbody=="cl"), aes(x=lengthmm, y=weightg))+geom_point(color="aquamarine4")+facet_wrap(~species, nrow=2)
lvswall

# create the same plot but use a log axis
lvswall + scale_y_continuous(trans=log10_trans())

##########
# create length histograms for bt and rb separated by method and then by year

ggplot(data=subset(fishid, species=="bt" & waterbody=="cl"),aes(lengthmm))+geom_bar()+facet_wrap(~gear,nrow=2)
ggplot(data=subset(fishid, species=="bt" & waterbody=="cl"),aes(lengthmm))+geom_bar()+facet_wrap(~y,nrow=1)
ggplot(data=subset(fishid, species=="rb" & waterbody=="cl"),aes(lengthmm))+geom_bar()+facet_wrap(~gear,nrow=2)
ggplot(data=subset(fishid, species=="rb" & waterbody=="cl"),aes(lengthmm))+geom_bar()+facet_wrap(~y,nrow=1)