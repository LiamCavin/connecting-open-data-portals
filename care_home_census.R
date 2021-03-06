#=============================================================================#
#
# 0. Information -------------------------------------------------------------
#
#=============================================================================#
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
# 6. Admissions, discharges and deaths of residents
# 7. Average age of residents
# 8. Type of stay
# 9. Length of stay
# 10. Sources of funding and weekly charges
#
#=============================================================================#
#*****************************************************************************#
#=============================================================================#

# next jobs needing attention: create files for upload for datasets covering:
#
# - soures of funding and weekly charges
# 

# load in the approriate package
# and any others that are needed 
# for this script.
library("tidyverse")
library("stringr")
library(httr) #Version 1.4.0
library(jsonlite) #Version 1.6


#===========================================================================#

#===========================================================================#
#
# 1. Number and rate of care home places -----------------------------------
#
#===========================================================================#

#===========================================================================#


# start with a clean slate
rm(list=ls())

# read the data from the NHS CKAN website. API download limit = 32000 rows
url  <- "https://www.opendata.nhs.scot"
path.number <- "/api/3/action/datastore_search?resource_id=04958b74-a351-4dc0-b8e4-cbc369372804&limit=32000"
path.rate <- "/api/3/action/datastore_search?resource_id=d2f8b247-1b0d-40e1-92f8-df8cd21d5a17&limit=32000"

raw.number <- GET(url = url, path = path.number)
raw.rate <- GET(url = url, path = path.rate)

# code 200 is ok
raw.number$status_code 
raw.rate$status_code 

# Translates it into text and parse character string containing JSON file into something R can work with
number.content <- fromJSON(rawToChar(raw.number$content))
rate.content <- fromJSON(rawToChar(raw.rate$content))

# Should be a list with 3 elements - 3rd element contains the data and notes
numberplaces <- number.content[[3]]$records
rateplaces <- rate.content[[3]]$records

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
  pipe$DateCode <-  x[,"Date"]             
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
placesODPP$Value <- as.numeric(placesODPP[,"Value"])
placesODPP <- data.frame(na.omit(placesODPP))

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
colnames(placesODPP)[colnames(placesODPP)=="ClientGroup"] <- "Main Client Group in Care Home"
colnames(placesODPP)[colnames(placesODPP)=="Sector"] <- "Care Home Sector"
placesODPP[,"DateCode"] <- str_sub(placesODPP[,"DateCode"], 1, 4)
placesODPP[,"Measurement"] <- str_replace_all(placesODPP[,"Measurement"], fixed("Number"), "Count")
placesODPP[,"Measurement"] <- str_replace_all(placesODPP[,"Measurement"], fixed("Rate"), "Ratio")
placesODPP[,"Units"] <- str_replace_all(placesODPP[,"Units"], fixed("Number of "), "")

# Finally, export the dataset, ready for upload to statistics.gov.scot 
# my local directory, but you can change this to yours
# setwd("//scotland.gov.uk//dc1//fs3_home//u441625")
# setwd("C:/Users/augno/Documents/connecting-open-data-portals")

write.csv(placesODPP, "care_home_places.csv", row.names=FALSE)

# yaldi


#===========================================================================#

#===========================================================================#
#
# 2. Number care homes -----------------------------------------------------
#
#===========================================================================#

#===========================================================================#

# start with a clean slate
rm(list=ls())

# read the data from the NHS CKAN website. API download limit = 32000 rows
url  <- "https://www.opendata.nhs.scot"
path <- "/api/3/action/datastore_search?resource_id=29f79bd7-9810-436d-9b29-2ede440adc87&limit=32000"

raw.homes <- GET(url = url, path = path)

# code 200 is ok
raw.homes$status_code 

# Translates it into text and parse character string containing JSON file into something R can work with
homes.content <- fromJSON(rawToChar(raw.homes$content))

# Should be a list with 3 elements - 3rd element contains the data and notes
numberhomes <- homes.content[[3]]$records

# and have a peek at it
head(numberhomes)
dim(numberhomes)
str(numberhomes)
unique(numberhomes[,9])

# create function to reformat data into statistics.gov.scot upload format
homes.format <- function(x,y) {
  pipe <- data.frame(str_sub(x[,"CA2011"]))  
  names(pipe) <- "GeographyCode"      
  pipe$DateCode <-  x[,"Date"]             
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
homesODPP$Value <- as.numeric(homesODPP[,"Value"])
homesODPP <- data.frame(na.omit(homesODPP))

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
colnames(homesODPP)[colnames(homesODPP)=="ClientGroup"] <- "Main Client Group in Care Home"
colnames(homesODPP)[colnames(homesODPP)=="Sector"] <- "Care Home Sector"
homesODPP[,"DateCode"] <- str_sub(homesODPP[,"DateCode"], 1, 4)
homesODPP[,"Measurement"] <- str_replace_all(homesODPP[,"Measurement"], fixed("Number"), "Count")
homesODPP[,"Units"] <- str_replace_all(homesODPP[,"Units"], fixed("Number of "), "")

# Finally, export the dataset, ready for upload to statistics.gov.scot 
# my local directory, but you can change this to yours
# setwd("//scotland.gov.uk//dc1//fs3_home//u441625")
# setwd("C:/Users/augno/Documents/connecting-open-data-portals")

write.csv(homesODPP, "number_care_homes.csv", row.names=FALSE)

# yaldi



#===========================================================================#

#===========================================================================#
#
# 3. Occupancy rate in care homes ------------------------------------------
#
#===========================================================================#

#===========================================================================#

# start with a clean slate
rm(list=ls())

# read the data from the NHS CKAN website. API download limit = 32000 rows
url  <- "https://www.opendata.nhs.scot"
path <- "/api/3/action/datastore_search?resource_id=e5e5bd8f-a2c9-4898-bbb0-21488e7433f2&limit=32000"

raw.occupancy <- GET(url = url, path = path)

# code 200 is ok
raw.occupancy$status_code 

# Translates it into text and parse character string containing JSON file into something R can work with
occupancy.content <- fromJSON(rawToChar(raw.occupancy$content))

# Should be a list with 3 elements - 3rd element contains the data and notes
occupancy<- occupancy.content[[3]]$records

# and have a peek at it
head(occupancy)
dim(occupancy)
str(occupancy)
unique(occupancy[,7])

# create function to reformat data into statistics.gov.scot upload format
occupancy.format <- function(x,y) {
  pipe <- data.frame(str_sub(x[,"CA2011"]))  
  names(pipe) <- "GeographyCode"      
  pipe$DateCode <-  x[,"Date"]             
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
occupancyODPP$Value <- as.numeric(occupancyODPP[,"Value"])
occupancyODPP <- data.frame(na.omit(occupancyODPP))

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
colnames(occupancyODPP)[colnames(occupancyODPP)=="ClientGroup"] <- "Main Client Group in Care Home"
colnames(occupancyODPP)[colnames(occupancyODPP)=="Sector"] <- "Care Home Sector"
occupancyODPP[,"DateCode"] <- str_sub(occupancyODPP[,"DateCode"], 1, 4)
occupancyODPP[,"Measurement"] <- str_replace_all(occupancyODPP[,"Measurement"], fixed("Percentage"), "Percent")


# Finally, export the dataset, ready for upload to statistics.gov.scot 
# my local directory, but you can change this to yours
# setwd("//scotland.gov.uk//dc1//fs3_home//u441625")
# setwd("C:/Users/augno/Documents/connecting-open-data-portals")

write.csv(occupancyODPP, "occupancy_rate.csv", row.names=FALSE)

# yaldi



#===========================================================================#

#===========================================================================#
#
# 4. Health Characteristics of Care Home Residents -------------------------
#
#===========================================================================#

#===========================================================================#

# start with a clean slate
rm(list=ls())

# read the data from the NHS CKAN website. API download limit = 32000 rows
url  <- "https://www.opendata.nhs.scot"
path.number <- "/api/3/action/datastore_search?resource_id=92ebf3df-2af4-4d73-9397-f5d6a6778da7&limit=32000"
path.percent <- "/api/3/action/datastore_search?resource_id=9bf418aa-c54d-45d3-8306-023e81f49f60&limit=32000"

raw.number <- GET(url = url, path = path.number)
raw.percent <- GET(url = url, path = path.percent)

# code 200 is ok
raw.number$status_code 
raw.percent$status_code 

# Translates it into text and parse character string containing JSON file into something R can work with
number.content <- fromJSON(rawToChar(raw.number$content))
percent.content <- fromJSON(rawToChar(raw.percent$content))

# Should be a list with 3 elements - 3rd element contains the data and notes
number.health <- number.content[[3]]$records
percent.health <- percent.content[[3]]$records

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
healthODPP$Value <- as.numeric(healthODPP[,"Value"])
healthODPP <- data.frame(na.omit(healthODPP))

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
colnames(healthODPP)[colnames(healthODPP)=="ClientGroup"] <- "Main Client Group in Care Home"
colnames(healthODPP)[colnames(healthODPP)=="HealthChar"] <- "Health Characteristic of Residents"

healthODPP[,"DateCode"] <- str_sub(healthODPP[,"DateCode"], 1, 4)
healthODPP[,"Measurement"] <- str_replace_all(healthODPP[,"Measurement"], fixed("Percentage"), "Percent")
healthODPP[,"Measurement"] <- str_replace_all(healthODPP[,"Measurement"], fixed("Number"), "Count")
healthODPP[,"Units"] <- str_replace_all(healthODPP[,"Units"], fixed("Percentage"), "Percentage of Long Stay Residents")
healthODPP[,"Units"] <- str_replace_all(healthODPP[,"Units"], fixed("Number"), "Long Stay Residents")

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
# and round values
healthODPP[,"Value"] <- round(healthODPP[,"Value"])
healthODPP[,"Value"] <- round(as.numeric(healthODPP[,"Value"]))

# Finally, export the dataset, ready for upload to statistics.gov.scot 
# my local directory, but you can change this to yours
# setwd("//scotland.gov.uk//dc1//fs3_home//u441625")
# setwd("C:/Users/augno/Documents/connecting-open-data-portals")

write.csv(healthODPP, "health.csv", row.names=FALSE)

# yaldi



#===========================================================================#

#===========================================================================#
#
# 5. Demographic Characteristics of Care Home Residents --------------------
#
#===========================================================================#

#===========================================================================#

# start with a clean slate
rm(list=ls())

# read the data from the NHS CKAN website. API download limit = 32000 rows
url  <- "https://www.opendata.nhs.scot"
path.number <- "/api/3/action/datastore_search?resource_id=39d2b480-2990-46a2-bd58-96aac41a032a&limit=32000"
path.percent <- "/api/3/action/datastore_search?resource_id=f2f376d8-f101-41f5-adb0-3249ed31cce0&limit=32000"

raw.number <- GET(url = url, path = path.number)
raw.percent <- GET(url = url, path = path.percent)

# code 200 is ok
raw.number$status_code 
raw.percent$status_code 

# Translates it into text and parse character string containing JSON file into something R can work with
number.content <- fromJSON(rawToChar(raw.number$content))
percent.content <- fromJSON(rawToChar(raw.percent$content))

# Should be a list with 3 elements - 3rd element contains the data and notes
number.dem <- number.content[[3]]$records
percent.dem <- percent.content[[3]]$records

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
  pipe$DateCode <-  x[,"Date"]
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

# remove any missing values, NAs and duplicates
demODPP <- demODPP[complete.cases(demODPP),]
demODPP <- unique(demODPP)
demODPP$Value <- as.numeric(demODPP[,"Value"])
demODPP <- data.frame(na.omit(demODPP))

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
colnames(demODPP)[colnames(demODPP)=="ClientGroup"] <- "Main Client Group in Care Home"

demODPP[,"DateCode"] <- str_sub(demODPP[,"DateCode"], 1, 4)

demODPP[,"Measurement"] <- str_replace_all(demODPP[,"Measurement"], fixed("Percentage"), "Percent")
demODPP[,"Measurement"] <- str_replace_all(demODPP[,"Measurement"], fixed("Number"), "Count")

demODPP[,"Units"] <- str_replace_all(demODPP[,"Units"], fixed("Percentage"), "Percentage of Long Stay Residents")
demODPP[,"Units"] <- str_replace_all(demODPP[,"Units"], fixed("Number"), "Long Stay Residents")

demODPP[,"Age"] <-  str_replace_all(demODPP[,"Age"], c("Number of " = "", "Percentage of " = "", "Long Stay Residents " = "",
                                     "Aged " = "", "Male " = "", "and Female " = "", "Female " = "", "Long Stay Residents " = "",
                                     "Long Stay Residents" = "All", "and" = "And"))

demODPP[,"Sex"] <- str_replace_all(demODPP[,"Sex"], c("Male and Female" = "All", "Long" = 'All',
                                                      "Percentage of " = "", "Number of " = ""))
demODPP[,"Sex"] <- str_sub(demODPP[,"Sex"], 1, 4)
demODPP[,"Sex"] <- str_replace_all(demODPP[,"Sex"], "Fema", "Female")
# round values
demODPP[,"Value"] <- round(as.numeric(demODPP[,"Value"]))

# Finally, export the dataset, ready for upload to statistics.gov.scot 
# my local directory, but you can change this to yours
# setwd("//scotland.gov.uk//dc1//fs3_home//u441625")
# setwd("C:/Users/augno/Documents/connecting-open-data-portals")

write.csv(demODPP, "demography.csv", row.names=FALSE)

# yaldi




#===========================================================================#

#===========================================================================#
#
# 6. Admission, Discharge and Death of Care Home Residents -----------------
#
#===========================================================================#

#===========================================================================#

# start with a clean slate
rm(list=ls())

# read the data from the NHS CKAN website. API download limit = 32000 rows
url  <- "https://www.opendata.nhs.scot"
path <- "/api/3/action/datastore_search?resource_id=aa3b2b55-9a30-4c7c-ae4b-33bd5a75ab03&limit=32000"
raw.result <- GET(url = url, path = path)

# code 200 is ok
raw.result$status_code 

#Translates it into text and parse character string containing JSON file into something R can work with
this.content <- fromJSON(rawToChar(raw.result$content))

# Should be a list with 3 elements - 3rd element contains the data and notes
ADDraw <- this.content[[3]]$records

# and have a peek at them
head(ADDraw)
dim(ADDraw)
str(ADDraw)
unique(ADDraw[,3])
unique(ADDraw[,4])
unique(ADDraw[,5])
unique(ADDraw[,6])
unique(ADDraw[,7])
unique(ADDraw[,8])

# create function to reformat data into statistics.gov.scot upload format
ADD.format <- function(x,y) {
  pipe <- data.frame(str_sub(x[,"CA2011"])) 
  names(pipe) <- "GeographyCode"      
  pipe$DateCode <-  x[,"Date"] 
  pipe$Units <- "People"
  pipe$Measurement <- "Count"
  pipe$ADD <- x[,"KeyStatistic"]
  pipe$Value <- x[,"Value"]                   
  pipe$ClientGroup <- x[,"MainClientGroup"]        
  return(pipe)
}

# run reformating function on datasets
addODPP  <- ADD.format(ADDraw)

# remove any NAs and duplicates
addODPP <- addODPP[complete.cases(addODPP),]
addODPP <- unique(addODPP)
# missing values encoded as " ", this line finds and removes these instances
addODPP <- addODPP[!(addODPP$Value == " "),]

#review output
head(addODPP)
dim(addODPP)
str(addODPP)
typeof(addODPP)
summary(addODPP)
unique(addODPP[,1])
unique(addODPP[,2])
unique(addODPP[,3])
unique(addODPP[,4])
unique(addODPP[,5])
unique(addODPP[,6])

# Edit the headers and text strings 
colnames(addODPP)[colnames(addODPP)=="ADD"] <- "Admissions, Discharges and Deaths"
colnames(addODPP)[colnames(addODPP)=="ClientGroup"] <- "Main Client Group in Care Home"
addODPP[,"Admissions, Discharges and Deaths"] <- str_replace_all(addODPP[,"Admissions, Discharges and Deaths"], fixed("Number of "), "")
addODPP[,"Measurement"] <- str_replace_all(addODPP[,"Measurement"], fixed("Number"), "Count")

# Finally, export the dataset, ready for upload to statistics.gov.scot 
# my local directory, but you can change this to yours
# setwd("//scotland.gov.uk//dc1//fs3_home//u441625")
# setwd("C:/Users/augno/Documents/connecting-open-data-portals")

write.csv(addODPP, "Admissions_discharges_deaths.csv", row.names=FALSE)

# yaldi





#===========================================================================#

#===========================================================================#
#
# 7. Average age of residents ----------------------------------------------
#
#===========================================================================#

#===========================================================================#

# start with a clean slate
rm(list=ls())

# read the data from the NHS CKAN website. API download limit = 32000 rows
url  <- "https://www.opendata.nhs.scot"
path <- "/api/3/action/datastore_search?resource_id=139f61d8-a87d-419d-b7af-31f555a60c89&limit=32000"
raw.result <- GET(url = url, path = path)

# code 200 is ok
raw.result$status_code 

#Translates it into text and parse character string containing JSON file into something R can work with
this.content <- fromJSON(rawToChar(raw.result$content))

# Should be a list with 3 elements - 3rd element contains the data and notes
ageraw <- this.content[[3]]$records

# and have a peek at them
head(ageraw)
dim(ageraw)
str(ageraw)
unique(ageraw[,3])
unique(ageraw[,4])
unique(ageraw[,5])
unique(ageraw[,6])
unique(ageraw[,7])
unique(ageraw[,8])

# create function to reformat data into statistics.gov.scot upload format
age.format <- function(x,y) {
  pipe <- data.frame(str_sub(x[,"CA2011"])) 
  names(pipe) <- "GeographyCode"      
  pipe$DateCode <-  x[,"Date"] 
  pipe$Units <- "Years"
  pipe$Measurement <- x[,"KeyStatistic"]
  pipe$AgeOfResidents <- x[,"KeyStatistic"]
  pipe$ClientGroup <- x[,"MainClientGroup"]
  pipe$Value <- x[,"Value"]                   
  return(pipe)
}

# run reformating function on datasets
ageODPP  <- age.format(ageraw)

# remove any NAs and duplicates
ageODPP <- ageODPP[complete.cases(ageODPP),]
ageODPP <- unique(ageODPP)
# missing values encoded as " ", this line finds and removes these instances
ageODPP <- ageODPP[!(ageODPP$Value == " "),]

#review output
head(ageODPP)
dim(ageODPP)
str(ageODPP)
typeof(ageODPP)
summary(ageODPP)
unique(ageODPP[,1])
unique(ageODPP[,2])
unique(ageODPP[,3])
unique(ageODPP[,4])
unique(ageODPP[,5])
unique(ageODPP[,6])
unique(ageODPP[,7])


# Edit the headers and text strings 
ageODPP[,"DateCode"] <- str_sub(ageODPP[,"DateCode"], 1, 4)
colnames(ageODPP)[colnames(ageODPP)=="AgeOfResidents"] <- "Age Of Residents"
colnames(ageODPP)[colnames(ageODPP)=="ClientGroup"] <- "Main Client Group in Care Home"
ageODPP[,"Age Of Residents"] <- str_replace_all(ageODPP[,"Age Of Residents"], 
                                                c("Mean " = "", "Median " = ""))
ageODPP[,"Measurement"] <- gsub("Mean.*", "Mean", ageODPP[,"Measurement"])
ageODPP[,"Measurement"] <- gsub("Median.*", "Median", ageODPP[,"Measurement"])

# Finally, export the dataset, ready for upload to statistics.gov.scot 
# my local directory, but you can change this to yours
# setwd("//scotland.gov.uk//dc1//fs3_home//u441625")
# setwd("C:/Users/augno/Documents/connecting-open-data-portals")

write.csv(ageODPP, "average_age.csv", row.names=FALSE)

# yaldi






#===========================================================================#

#===========================================================================#
#
# 8. Type of stay ----------------------------------------------------------
#
#===========================================================================#

#===========================================================================#

# start with a clean slate
rm(list=ls())

# read the data from the NHS CKAN website. API download limit = 32000 rows
url  <- "https://www.opendata.nhs.scot"
t.path <- "/api/3/action/datastore_search?resource_id=940176d7-b164-4ddc-b839-b0325eafb591&limit=32000"

type.result <- GET(url = url, path = t.path)

# code 200 is ok
type.result$status_code 

#Translates it into text and parse character string containing JSON file into something R can work with
type.content <- fromJSON(rawToChar(type.result$content))

# Should be a list with 3 elements - 3rd element contains the data and notes
typeraw <- type.content[[3]]$records

# and have a peek at them
head(typeraw)
dim(typeraw)
str(typeraw)
unique(typeraw[,3])
unique(typeraw[,4])
unique(typeraw[,6])
unique(typeraw[,7])
unique(typeraw[,8])

# create function to reformat data into statistics.gov.scot upload format
type.format <- function(x,y) {
  pipe <- data.frame(str_sub(x[,"CA2011"])) 
  names(pipe) <- "GeographyCode"      
  pipe$DateCode <-  x[,"Date"] 
  pipe$Units <- "People"
  pipe$Measurement <- "Count"
  pipe$TypeOfStay <- x[,"KeyStatistic"]
  pipe$ClientGroup <- x[,"MainClientGroup"]
  pipe$Sector <- x[,"Sector"]
  pipe$Value <- x[,"Value"]                   
  return(pipe)
}

# run reformating function on datasets
typeODPP  <- type.format(typeraw)

# remove any NAs and duplicates
typeODPP <- typeODPP[complete.cases(typeODPP),]
typeODPP <- unique(typeODPP)
# missing values encoded as " ", this line finds and removes these instances
typeODPP <- typeODPP[!(typeODPP$Value == " "),]

#review output
head(typeODPP)
dim(typeODPP)
str(typeODPP)
typeof(typeODPP)
summary(typeODPP)
unique(typeODPP[,1])
unique(typeODPP[,2])
unique(typeODPP[,3])
unique(typeODPP[,4])
unique(typeODPP[,5])
unique(typeODPP[,6])
unique(typeODPP[,7])
unique(typeODPP[,8])

# Edit the headers and text strings 
typeODPP[,"DateCode"] <- str_sub(typeODPP[,"DateCode"], 1, 4)
colnames(typeODPP)[colnames(typeODPP)=="TypeOfStay"] <- "Type Of Care Home Stay"
colnames(typeODPP)[colnames(typeODPP)=="ClientGroup"] <- "Main Client Group in Care Home"
colnames(typeODPP)[colnames(typeODPP)=="Sector"] <- "Care Home Sector"

typeODPP[,"Type Of Care Home Stay"] <- str_replace_all(typeODPP[,"Type Of Care Home Stay"], 
                                                "Number of ", "")


# Finally, export the dataset, ready for upload to statistics.gov.scot 
# my local directory, but you can change this to yours
# setwd("//scotland.gov.uk//dc1//fs3_home//u441625")
# setwd("C:/Users/augno/Documents/connecting-open-data-portals")

write.csv(typeODPP, "type_of_stay.csv", row.names=FALSE)

# yaldi







#===========================================================================#

#===========================================================================#
#
# 9. Length of stay ---------------------------------------------------------
#
#===========================================================================#

#===========================================================================#

# start with a clean slate
rm(list=ls())

# read the data from the NHS CKAN website. API download limit = 32000 rows
url  <- "https://www.opendata.nhs.scot"
percent.path <- "/api/3/action/datastore_search?resource_id=55b0199f-1bb6-45ab-af56-20b58e652e9e&limit=32000"
average.path <- "/api/3/action/datastore_search?resource_id=d78d65cd-697d-4c73-9078-9dd788bf239c&limit=32000"

percent.result <- GET(url = url, path = percent.path)
average.result <- GET(url = url, path = average.path)

# code 200 is ok
percent.result$status_code 
average.result$status_code 

#Translates it into text and parse character string containing JSON file into something R can work with
percent.content <- fromJSON(rawToChar(percent.result$content))
average.content <- fromJSON(rawToChar(average.result$content))

# Should be a list with 3 elements - 3rd element contains the data and notes
percentraw <- percent.content[[3]]$records
averageraw <- average.content[[3]]$records

# and have a peek at them
head(percentraw)
dim(percentraw)
str(percentraw)
unique(percentraw[,3])
unique(percentraw[,4])
unique(percentraw[,5])
unique(percentraw[,6])
unique(percentraw[,7])
unique(percentraw[,8])

head(averageraw)
dim(averageraw)
str(averageraw)
unique(averageraw[,3])
unique(averageraw[,4])
unique(averageraw[,5])
unique(averageraw[,6])
unique(averageraw[,7])
unique(averageraw[,8])

# create function to reformat data into statistics.gov.scot upload format
percent.format <- function(x,y) {
  pipe <- data.frame(str_sub(x[,"CA2011"])) 
  names(pipe) <- "GeographyCode"      
  pipe$DateCode <-  x[,"Date"] 
  pipe$Units <- "Percentage Of Long Stay Residents"
  pipe$Measurement <- "Percent"
  pipe$Length <- x[,"KeyStatistic"]
  pipe$Complete <- x[,"KeyStatistic"]
  pipe$ClientGroup <- x[,"MainClientGroup"]
  pipe$Value <- x[,"Value"]                   
  return(pipe)
}

average.format <- function(x,y) {
  pipe <- data.frame(str_sub(x[,"CA2011"])) 
  names(pipe) <- "GeographyCode"      
  pipe$DateCode <-  x[,"Date"] 
  pipe$Units <- "Years"
  pipe$Measurement <- x[,"KeyStatistic"]
  pipe$Length <- "All Lengths of Stay"
  pipe$Complete <- x[,"KeyStatistic"]
  pipe$ClientGroup <- x[,"MainClientGroup"]
  pipe$Value <- x[,"Value"]                   
  return(pipe)
}

# run reformating function on datasets
lengthODPP  <- rbind(percent.format(percentraw), average.format(averageraw))
 

# remove any NAs and duplicates
lengthODPP <- lengthODPP[complete.cases(lengthODPP),]
lengthODPP <- unique(lengthODPP)
# missing values encoded as " ", this line finds and removes these instances
lengthODPP <- lengthODPP[!(lengthODPP$Value == " "),]

#review output
head(lengthODPP)
dim(lengthODPP)
str(lengthODPP)
typeof(lengthODPP)
summary(lengthODPP)
unique(lengthODPP[,1])
unique(lengthODPP[,2])
unique(lengthODPP[,3])
unique(lengthODPP[,4])
unique(lengthODPP[,5])
unique(lengthODPP[,6])
unique(lengthODPP[,7])
unique(lengthODPP[,8])


# Edit the headers and text strings 
lengthODPP[,"DateCode"] <- str_sub(lengthODPP[,"DateCode"], 1, 4)
lengthODPP[,"Measurement"] <- gsub("Mean.*", "Mean", lengthODPP[,"Measurement"])
lengthODPP[,"Measurement"] <- gsub("Median.*", "Median", lengthODPP[,"Measurement"])
colnames(lengthODPP)[colnames(lengthODPP)=="Length"] <- "Length Of Care Home Stay"
colnames(lengthODPP)[colnames(lengthODPP)=="ClientGroup"] <- "Main Client Group in Care Home"
colnames(lengthODPP)[colnames(lengthODPP)=="Complete"] <- "Care Home Stay Complete"

lengthODPP[,"Length Of Care Home Stay"] <- str_replace_all(lengthODPP[,"Length Of Care Home Stay"], 
                                                       c("Complete Length of " = "", "Incomplete Length of " = ""))
lengthODPP[,"Care Home Stay Complete"] <- gsub(".*Complete.*", "Complete", lengthODPP[,"Care Home Stay Complete"])
lengthODPP[,"Care Home Stay Complete"] <- gsub(".*Incomplete.*", "Incomplete", lengthODPP[,"Care Home Stay Complete"])
lengthODPP[,"Value"] <- round(as.numeric(lengthODPP[,"Value"]),1)


# Finally, export the dataset, ready for upload to statistics.gov.scot 
# my local directory, but you can change this to yours
# setwd("//scotland.gov.uk//dc1//fs3_home//u441625")
# setwd("C:/Users/augno/Documents/connecting-open-data-portals")

write.csv(lengthODPP, "length_of_stay.csv", row.names=FALSE)

# yaldi

#===========================================================================#
#
#===========================================================================#
#
# 10. Sources of Funding and Weekly Charges ================================
# `
#===========================================================================#
#
#===========================================================================#

# start with a clean slate
rm(list=ls())

# read the data from the NHS CKAN website. API download limit = 32000 rows
url  <- "https://www.opendata.nhs.scot"
path.charges <- "/api/3/action/datastore_search?resource_id=4ee7dc84-ca65-455c-9e76-b614091f389f&limit=32000"
path.funding <- "/api/3/action/datastore_search?resource_id=53f1af96-9a94-4ac0-a6b1-aeeea6ab111d&limit=32000"

raw.charges <- GET(url = url, path = path.charges)
raw.funding <- GET(url = url, path = path.funding)

# code 200 is ok
raw.charges$status_code 
raw.funding$status_code

# Translates it into text and parse character string containing JSON file into something R can work with
charges.content <- fromJSON(rawToChar(raw.charges$content))
funding.content <- fromJSON(rawToChar(raw.funding$content))

# Should be a list with 3 elements - 3rd element contains the data and notes
charges <- charges.content[[3]]$records
funding <- funding.content[[3]]$records

# and have a peek at it
head(charges)
dim(charges)
str(charges)
unique(charges[,3])
unique(charges[,5])
unique(charges[,6])
unique(charges[,7])

head(funding)
dim(funding)
str(funding)
unique(funding[,3])
unique(funding[,5])
unique(funding[,6])

# create function to reformat data into statistics.gov.scot upload format
charges.format <- function(x,y) {
  pipe <- data.frame(str_sub(x[,"CA2011"])) 
  names(pipe) <- "GeographyCode"      
  pipe$DateCode <-  x[,"Date"] 
  pipe$Units <- "Pounds (GBP)"
  pipe$Measurement <- "Count"
  pipe$AverageWeeklyCharge <- x[,"KeyStatistic"]
  pipe$SourceFunding <- x[,"KeyStatistic"]
  pipe$Value <- x[,"Value"]                   
  return(pipe)
}

funding.format <- function(x,y) {
  pipe <- data.frame(str_sub(x[,"CA2011"])) 
  names(pipe) <- "GeographyCode"      
  pipe$DateCode <-  x[,"Date"] 
  pipe$Units <- "Long Stay Residents"
  pipe$Measurement <- "Percent"
  pipe$AverageWeeklyCharge <- x[,"KeyStatistic"]
  pipe$SourceFunding <- x[,"KeyStatistic"]
  pipe$Value <- x[,"Value"]                   
  return(pipe)
}

# run reformating function on datasets
financeODPP  <- rbind(charges.format(charges), funding.format(funding))

# remove any NAs and duplicates
financeODPP <- financeODPP[complete.cases(financeODPP),]
financeODPP <- unique(financeODPP)
# missing values encoded as " ", this line finds and removes these instances
financeODPP <- financeODPP[!(financeODPP$Value == " "),]


#review output
head(financeODPP)
dim(financeODPP)
str(financeODPP)
typeof(financeODPP)
summary(financeODPP)
unique(financeODPP[,1])
unique(financeODPP[,2])
unique(financeODPP[,3])
unique(financeODPP[,4])
unique(financeODPP[,5])
unique(financeODPP[,6])


# Edit the headers and text strings 
financeODPP[,"DateCode"] <- str_sub(financeODPP[,"DateCode"], 1, 4)

colnames(financeODPP)[colnames(financeODPP)=="AverageWeeklyCharge"] <- "Gross Average Weekly Charge Category"
colnames(financeODPP)[colnames(financeODPP)=="SourceFunding"] <- "Source of Funding"

financeODPP[,"Gross Average Weekly Charge Category"] <- str_replace_all(financeODPP[,"Gross Average Weekly Charge Category"],
                                                                 c("Average Gross Weekly Charge for All Self Funders" = "All",
                                                                   "Average Gross Weekly Charge for Self Funders " = "",
                                                                   "Average Gross Weekly Charge for All Funding " = "",
                                                                   "Average Gross Weekly Charge for Publicly Funded " = "",
                                                                   "Percentage of Long Stay Residents Mainly Or Wholly Funded by Local Authority or NHS" ="All",
                                                                   "Percentage of Long Stay Residents Mainly Or Wholly Self Funded" = "All"
                                                                   ))

financeODPP[,"Source of Funding"] <- str_replace_all(financeODPP[,"Source of Funding"],
                                                                  c("Average Gross Weekly Charge for All Self Funders" = "Self Funded",
                                                                    "Average Gross Weekly Charge for Self Funders With Nursing Care" = "Self Funded",
                                                                    "Average Gross Weekly Charge for Self Funders Without Nursing Care" = "Self Funded",
                                                                    "Average Gross Weekly Charge for All Funding With Nursing Care" = "All",
                                                                    "Average Gross Weekly Charge for All Funding Without Nursing Care" = "All",
                                                                    "Average Gross Weekly Charge for Publicly Funded With Nursing Care" = "Publicly Funded",
                                                                    "Average Gross Weekly Charge for Publicly Funded Without Nursing Care" = "Publicly Funded",
                                                                    "Percentage of Long Stay Residents Mainly Or Wholly Funded by Local Authority or NHS" ="Publicly Funded",
                                                                    "Percentage of Long Stay Residents Mainly Or Wholly Self Funded" = "Self Funded"
                                                                  ))

# Finally, export the dataset, ready for upload to statistics.gov.scot 
setwd("C:/Users/dsap01/connecting-open-data-portals/")
write.csv(financeODPP, "financeODPP.csv", row.names = F)

# yaldi

# END