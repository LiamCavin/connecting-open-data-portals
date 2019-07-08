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
# 3. Occupancy rate
# 4. Health Characteristics
# 5. Demographic Characteristics
#
#=============================================================================
#*****************************************************************************
#=============================================================================

# next jobs needing attention: create files for upload for datasets covering:
#
# - admissions, discharges and deaths of residents
# - demographic characteristics of residents and average age (% and no) LC-WIP
# - type and length (& average length) of stay
# - soures of funding and weekly charges
# 

# load in the approriate package
# and any others that are needed 
# for this script.
install.packages("tidyverse")
library("stringr")



#===========================================================================

#===========================================================================
#
# 1. Number and rate of care home places 
#
#===========================================================================

#===========================================================================


# start with a clean slate
rm(list=ls())

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
placesODPP <- placesODPP[complete.cases(placesODPP),]
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
colnames(placesODPP)[colnames(placesODPP)=="ClientGroup"] <- "Care Home Client Group"
colnames(placesODPP)[colnames(placesODPP)=="Sector"] <- "Care Home Sector"
placesODPP[,"DateCode"] <- str_sub(placesODPP[,"DateCode"], 1, 4)
placesODPP[,"Measurement"] <- str_replace_all(placesODPP[,"Measurement"], fixed("Number"), "Count")
placesODPP[,"Measurement"] <- str_replace_all(placesODPP[,"Measurement"], fixed("Rate"), "Ratio")
placesODPP[,"Units"] <- str_replace_all(placesODPP[,"Units"], fixed("Number of Registered Places for Older People per 1000 population"), 
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

# start with a clean slate
rm(list=ls())

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
homesODPP <- homesODPP[complete.cases(homesODPP),]
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
homesODPP[,"DateCode"] <- str_sub(homesODPP[,"DateCode"], 1, 4)
homesODPP[,"Measurement"] <- str_replace_all(homesODPP[,"Measurement"], fixed("Number"), "Count")
homesODPP[,"Measurement"] <- str_replace_all(homesODPP[,"Measurement"], fixed("Rate"), "Ratio")
homesODPP[,"Units"] <- str_replace_all(homesODPP[,"Units"], fixed("Number of Registered Places for Older People per 1000 population"), 
                                  "Number of Registered Places per 1000 population")

# Finally, export the dataset, ready for upload to statistics.gov.scot 
# my local directory, but you can change this to yours
# setwd("//scotland.gov.uk//dc1//fs3_home//u441625")
# setwd("C:/Users/augno/Documents/connecting-open-data-portals")

write.csv(homesODPP, "number_care_homes.csv", row.names=FALSE)

# yaldi



#===========================================================================

#===========================================================================
#
# 3. Occupancy rate in care homes
#
#===========================================================================

#===========================================================================

# start with a clean slate
rm(list=ls())

# read the data from the NHS CKAN website
occupancy <- read.csv("https://www.opendata.nhs.scot/dataset/75cca0a9-780d-40e0-9e1f-5f4796950794/resource/e5e5bd8f-a2c9-4898-bbb0-21488e7433f2/download/file6_occupancy_2007_2017.csv")

# and have a peek at it
head(occupancy)
dim(occupancy)
str(occupancy)
unique(occupancy[,7])

# create function to reformat data into statistics.gov.scot upload format
occupancy.format <- function(x,y) {
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
occupancyODPP  <- occupancy.format(occupancy)

# remove any NAs and duplicates
occupancyODPP <- occupancyODPP[complete.cases(occupancyODPP),]
occupancyODPP <- unique(occupancyODPP)

#review output
head(occupancyODPP)
dim(occupancyODPP)
str(occupancyODPP)
typeof(occupancyODPP)
summary(occupancyODPP)
unique(occupancyODPP[,1])
unique(occupancyODPP[,2])
unique(occupancyODPP[,3])
unique(occupancyODPP[,4])
unique(occupancyODPP[,5])
unique(occupancyODPP[,6])
unique(occupancyODPP[,7])

# Edit the headers and text strings 
colnames(occupancyODPP)[colnames(occupancyODPP)=="ClientGroup"] <- "Care Home Client Group"
colnames(occupancyODPP)[colnames(occupancyODPP)=="Sector"] <- "Care Home Sector"
occupancyODPP[,"DateCode"] <- str_sub(occupancyODPP[,"DateCode"], 1, 4)
occupancyODPP[,"Measurement"] <- str_replace_all(occupancyODPP[,"Measurement"], fixed("Percentage"), "Percent")


# Finally, export the dataset, ready for upload to statistics.gov.scot 
# my local directory, but you can change this to yours
# setwd("//scotland.gov.uk//dc1//fs3_home//u441625")
# setwd("C:/Users/augno/Documents/connecting-open-data-portals")

write.csv(occupancyODPP, "occupancy_rate.csv", row.names=FALSE)

# yaldi



#===========================================================================

#===========================================================================
#
# 4. Health Characteristics of Care Home Residents
#
#===========================================================================

#===========================================================================

# start with a clean slate
rm(list=ls())

# read the data from the NHS CKAN website
number.health <- read.csv("https://www.opendata.nhs.scot/dataset/75cca0a9-780d-40e0-9e1f-5f4796950794/resource/92ebf3df-2af4-4d73-9397-f5d6a6778da7/download/file10_nos_health_characteristics_2007_2017.csv")
percent.health <- read.csv("https://www.opendata.nhs.scot/dataset/75cca0a9-780d-40e0-9e1f-5f4796950794/resource/9bf418aa-c54d-45d3-8306-023e81f49f60/download/file8_perc_health_characteristics_2007_2017.csv")


# and have a peek at them
head(number.health)
dim(number.health)
str(number.health)
unique(number.health[,7])
colnames(number.health)[colnames(number.health)=="Country"] <- "CA2011"


head(percent.health)
dim(percent.health)
str(percent.health)
unique(percent.health[,7])
colnames(percent.health)[colnames(percent.health)=="ï..Date"] <- "Date"

# create function to reformat data into statistics.gov.scot upload format
health.format <- function(x,y) {
  pipe <- data.frame(str_sub(x[,"CA2011"])) 
  names(pipe) <- "GeographyCode"      
  pipe$DateCode <-  x[,"Date"]             
  pipe$Measurement <- x[,"Unit"]
  pipe$Units <- x[,"Unit"]
  pipe$HealthChar <- x[,"KeyStatistic"]
  pipe$Value <- x[,"Value"]                   
  pipe$ClientGroup <- x[,"MainClientGroup"]        
  pipe$Sector <- x[,"Sector"]
  return(pipe)
}

# run reformating function on datasets
healthODPP  <- rbind(health.format(percent.health), health.format(number.health))

# remove any NAs and duplicates
healthODPP <- healthODPP[complete.cases(healthODPP),]
healthODPP <- unique(healthODPP)

#review output
head(healthODPP)
dim(healthODPP)
str(healthODPP)
typeof(healthODPP)
summary(healthODPP)
unique(healthODPP[,1])
unique(healthODPP[,2])
unique(healthODPP[,3])
unique(healthODPP[,4])
unique(healthODPP[,5])
unique(healthODPP[,6])
unique(healthODPP[,7])

# Edit the headers and text strings 
colnames(healthODPP)[colnames(healthODPP)=="ClientGroup"] <- "Care Home Client Group"
colnames(healthODPP)[colnames(healthODPP)=="HealthChar"] <- "Health Characteristic of Residents"

healthODPP[,"DateCode"] <- str_sub(healthODPP[,"DateCode"], 1, 4)
healthODPP[,"Measurement"] <- str_replace_all(healthODPP[,"Measurement"], fixed("Percentage"), "Percent")
healthODPP[,"Measurement"] <- str_replace_all(healthODPP[,"Measurement"], fixed("Number"), "Count")
healthODPP[,"Units"] <- str_replace_all(healthODPP[,"Units"], fixed("Percentage"), "Percentage of Long Stay Residents")
healthODPP[,"Units"] <- str_replace_all(healthODPP[,"Units"], fixed("Number"), "People")

healthODPP[,"Health Characteristic of Residents"] <- str_replace_all(healthODPP[,"Health Characteristic of Residents"], 
                                                                     fixed("Percentage of Long Stay Residents "), "")
healthODPP[,"Health Characteristic of Residents"] <- str_replace_all(healthODPP[,"Health Characteristic of Residents"], 
                                                                     fixed("Number of Long Stay Residents "), "")
healthODPP[,"Health Characteristic of Residents"] <- str_replace_all(healthODPP[,"Health Characteristic of Residents"], 
                                                                     fixed("with "), "")
healthODPP[,"Health Characteristic of Residents"] <- str_replace_all(healthODPP[,"Health Characteristic of Residents"], 
                                                                     fixed("Disability"), "Disabilities")
# only one sector in this dataset, so drop it
healthODPP <- healthODPP[, !(names(healthODPP) %in% "Sector")]
healthODPP[,"Value"] <- round(healthODPP[,"Value"])

# Finally, export the dataset, ready for upload to statistics.gov.scot 
# my local directory, but you can change this to yours
# setwd("//scotland.gov.uk//dc1//fs3_home//u441625")
# setwd("C:/Users/augno/Documents/connecting-open-data-portals")

write.csv(healthODPP, "health.csv", row.names=FALSE)

# yaldi



#===========================================================================

#===========================================================================
#
# 5. Demographic Characteristics of Care Home Residents
#
#===========================================================================

#===========================================================================

# start with a clean slate
rm(list=ls())

# read the data from the NHS CKAN website
number.dem <- read.csv("https://www.opendata.nhs.scot/dataset/75cca0a9-780d-40e0-9e1f-5f4796950794/resource/39d2b480-2990-46a2-bd58-96aac41a032a/download/file11_nos_sex_age_2007_2017.csv")
percent.dem <- read.csv("https://www.opendata.nhs.scot/dataset/75cca0a9-780d-40e0-9e1f-5f4796950794/resource/f2f376d8-f101-41f5-adb0-3249ed31cce0/download/file9_perc_sex_age_2007_2017.csv")


# and have a peek at them
head(number.dem)
dim(number.dem)
str(number.dem)
unique(number.dem[,5])
colnames(number.dem)[colnames(number.dem)=="Country"] <- "CA2011"

head(percent.dem)
dim(percent.dem)
str(percent.dem)
unique(percent.dem[,5])


# create function to reformat data into statistics.gov.scot upload format
dem.format <- function(x,y) {
  pipe <- data.frame(str_sub(x[,"CA2011"])) 
  names(pipe) <- "GeographyCode"      
  pipe$DateCode <-  x[,"ï..Date"]             
  pipe$Measurement <- x[,"Unit"]
  pipe$Units <- x[,"Unit"]
  pipe$Age <- x[,"KeyStatistic"]
  pipe$Sex <- x[,"KeyStatistic"]
  pipe$Value <- x[,"Value"]                   
  pipe$ClientGroup <- x[,"MainClientGroup"]        
  return(pipe)
}

# run reformating function on datasets
demODPP  <- rbind(dem.format(number.dem), dem.format(percent.dem))

# remove any NAs and duplicates
demODPP <- demODPP[complete.cases(demODPP),]
demODPP <- unique(demODPP)

#review output
head(demODPP)
dim(demODPP)
str(demODPP)
typeof(demODPP)
summary(demODPP)
unique(demODPP[,1])
unique(demODPP[,2])
unique(demODPP[,3])
unique(demODPP[,4])
unique(demODPP[,5])
unique(demODPP[,6])
unique(demODPP[,7])
unique(demODPP[,8])

# Edit the headers and text strings 
colnames(demODPP)[colnames(demODPP)=="ClientGroup"] <- "Care Home Client Group"
demODPP[,"DateCode"] <- str_sub(demODPP[,"DateCode"], 1, 4)
demODPP[,"Measurement"] <- str_replace_all(demODPP[,"Measurement"], fixed("Percentage"), "Percent")
demODPP[,"Measurement"] <- str_replace_all(demODPP[,"Measurement"], fixed("Number"), "Count")
demODPP[,"Units"] <- str_replace_all(demODPP[,"Units"], fixed("Percentage"), "Percentage of Long Stay Residents")
demODPP[,"Units"] <- str_replace_all(demODPP[,"Units"], fixed("Number"), "People")

agetext2remove <- c("Number of ", "Percentage of", "Long Stay Residents ", "Aged", "Male ", "Female ", "and ")

demODPP[,"Age"] <- demODPP[,"Age"] %>%
                   str_replace_all(c("Number of " = "", "Percentage of" = "", "Long Stay Residents " = "",
                                     "Aged" = "", "Male " = "", "Female " = "", "and " = ""))


demODPP[,"Age"] <- str_replace_all(demODPP[,"Age"], agetext2remove,"")
demODPP[,"Age"] <- str_replace_all(demODPP[,"Age"], fixed("Number of Long Stay Residents "), "")
demODPP[,"Age"] <- str_replace_all(demODPP[,"Age"], fixed("with "), "")
demODPP[,"Age"] <- str_replace_all(demODPP[,"Age"], fixed("Disability"), "Disabilities")

demODPP[,"Value"] <- round(demODPP[,"Value"])

# Finally, export the dataset, ready for upload to statistics.gov.scot 
# my local directory, but you can change this to yours
# setwd("//scotland.gov.uk//dc1//fs3_home//u441625")
# setwd("C:/Users/augno/Documents/connecting-open-data-portals")

write.csv(demODPP, "demography.csv", row.names=FALSE)

# yaldi