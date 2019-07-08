#=============================================================================
#
# Script for formatting Care Home Statistics for Council Areas
# into statistics.gov.scot upload format (tidy data).
# source data from :
# https://www.opendata.nhs.scot/dataset/care-home-census
#
# Data, Statistics and Outcomes
# Scottish Government
# July 2019 Liam Cavin x44092
#
# Contents:
# 
# 1. Number and rate of care home places 
# 2. Number care homes
# 3. 
#
#=============================================================================
#*****************************************************************************
#=============================================================================

# next jobs needing attention: create files for upload for datasets covering:
#
# - health characteristics of care home residents (% and no)
# - occupancy rate of care homes
# - admissions, discharges and deaths of residents
# - demographic characteristics of residents and average age (% and no)
# - type and length (& average length) of stay
# - soures of funding and weekly charges
# 

# start with a clean slate
#=========================

rm(list=ls())

# load in the approriate package
# and any others that are needed 
# for this script.
#===============================

# the tidyverse
install.packages("tidyverse")
library("stringr")





#===========================================================================

#===========================================================================
#
# 1. Number and rate of care home places 
#
#===========================================================================

#===========================================================================

# read in the data from NHS CKAN website
numberplaces <- read.csv("https://www.opendata.nhs.scot/dataset/75cca0a9-780d-40e0-9e1f-5f4796950794/resource/04958b74-a351-4dc0-b8e4-cbc369372804/download/file14_nos_registered_places_2007_2017.csv")
rateplaces <- read.csv("https://www.opendata.nhs.scot/dataset/75cca0a9-780d-40e0-9e1f-5f4796950794/resource/d2f8b247-1b0d-40e1-92f8-df8cd21d5a17/download/file15_rate_registered_places_2007_2017.csv")

# and have a peek at it
head(numberplaces)
dim(numberplaces)
str(numberplaces)
unique(numberplaces[,9])

head(rateplaces)
dim(rateplaces)
str(rateplaces)
unique(rateplaces[,7])

# create function to reformat data into statistics.gov.scot upload format
places.format <- function(x,y) {
  pipe <- data.frame(str_sub(x[,"CA2011"]))  
  names(pipe) <- "GeographyCode"      
  pipe$DateCode <-  x[,"ï..Date"]             
  pipe$Measurement <- x[,"Unit"]
  pipe$Units <- x[,"KeyStatistic"]
  pipe$Value <- x[,"Value"]                   
  pipe$ClientGroup <- x[,"MainClientGroup"]        
  pipe$Sector <- x[,"Sector"]
  return(pipe)
}

# run reformating function on datasets
placesODPP  <- rbind(places.format(numberplaces), places.format(rateplaces))

# remove NAs and duplicates
placesODPP <- placesODPP[complete.cases(placesODPP), ]
placesODPP <- unique(placesODPP)

#review output
head(placesODPP)
dim(placesODPP)
str(placesODPP)
typeof(placesODPP)
summary(placesODPP)
unique(placesODPP[,1])
unique(placesODPP[,2])
unique(placesODPP[,3])
unique(placesODPP[,4])
unique(placesODPP[,5])
unique(placesODPP[,6])
unique(placesODPP[,7])

# Edit the headers and text strings 
colnames(placesODPP)[6] <- "Care Home Client Group"
colnames(placesODPP)[7] <- "Care Home Sector"
placesODPP[,2] <- str_sub(placesODPP[,2], 1, 4)
placesODPP[,3] <- str_replace_all(placesODPP[,3], fixed("Number"), "Count")
placesODPP[,3] <- str_replace_all(placesODPP[,3], fixed("Rate"), "Ratio")
placesODPP[,4] <- str_replace_all(placesODPP[,4], fixed("Number of Registered Places for Older People per 1000 population"), 
                                                        "Number of Registered Places per 1000 population")

# Finally, export the dataset, ready for upload to statistics.gov.scot 
# my local directory, but you can change this to yours
# setwd("//scotland.gov.uk//dc1//fs3_home//u441625")
# setwd("C:/Users/augno/Documents/connecting-open-data-portals")

write.csv(placesODPP, "care_home_places.csv", row.names=FALSE)

# yaldi


#===========================================================================

#===========================================================================
#
# 2. Number care homes
#
#===========================================================================

#===========================================================================

# read the data from the NHS CKAN website
numberhomes <- read.csv("https://www.opendata.nhs.scot/dataset/75cca0a9-780d-40e0-9e1f-5f4796950794/resource/29f79bd7-9810-436d-9b29-2ede440adc87/download/file13_carehomes_2007_2017.csv")

# and have a peek at it
head(numberhomes)
dim(numberhomes)
str(numberhomes)
unique(numberhomes[,9])

# create function to reformat data into statistics.gov.scot upload format
homes.format <- function(x,y) {
  pipe <- data.frame(str_sub(x[,"CA2011"]))  
  names(pipe) <- "GeographyCode"      
  pipe$DateCode <-  x[,"ï..Date"]             
  pipe$Measurement <- x[,"Unit"]
  pipe$Units <- x[,"KeyStatistic"]
  pipe$Value <- x[,"Value"]                   
  pipe$ClientGroup <- x[,"MainClientGroup"]        
  pipe$Sector <- x[,"Sector"]
  return(pipe)
}

# run reformating function on datasets
homesODPP  <- homes.format(numberhomes)

# remove any NAs and duplicates
homesODPP <- homesODPP[complete.cases(homesODPP), ]
homesODPP <- unique(homesODPP)

#review output
head(homesODPP)
dim(homesODPP)
str(homesODPP)
typeof(homesODPP)
summary(homesODPP)
unique(homesODPP[,1])
unique(homesODPP[,2])
unique(homesODPP[,3])
unique(homesODPP[,4])
unique(homesODPP[,5])
unique(homesODPP[,6])
unique(homesODPP[,7])

# Edit the headers and text strings 
colnames(homesODPP)[colnames(homesODPP)=="ClientGroup"] <- "Care Home Client Group"
colnames(homesODPP)[colnames(homesODPP)=="Sector"] <- "Care Home Sector"
# amend previous dataset code to replicate this ^
homesODPP[,2] <- str_sub(homesODPP[,2], 1, 4)
homesODPP[,3] <- str_replace_all(homesODPP[,3], fixed("Number"), "Count")
homesODPP[,3] <- str_replace_all(homesODPP[,3], fixed("Rate"), "Ratio")
homesODPP[,4] <- str_replace_all(homesODPP[,4], fixed("Number of Registered Places for Older People per 1000 population"), 
                                  "Number of Registered Places per 1000 population")


# Finally, export the dataset, ready for upload to statistics.gov.scot 
# my local directory, but you can change this to yours
# setwd("//scotland.gov.uk//dc1//fs3_home//u441625")
# setwd("C:/Users/augno/Documents/connecting-open-data-portals")

write.csv(homesODPP, "number_care_homes.csv", row.names=FALSE)

# yaldi

