#=============================================================================
#
# Script for formatting Alcohol Related Hospital Statistics for Council Areas
# into statistics.gov.scot upload format (tidy data).
# source data from :
# https://www.opendata.nhs.scot/dataset/c4db1692-fa02-4a1c-af4c-6039c74633ea/resource/b0b520e8-3507-46cd-a9b5-cff03007bb57/download/arhs_councilarea_21_11_2017.csv
# and
# https://www.opendata.nhs.scot/dataset/c4db1692-fa02-4a1c-af4c-6039c74633ea/resource/1cad0a45-a9ce-43f8-9aec-df0aac122764/download/arhs_hbt2014_21_11_2017.csv
#
#
# Data, Statistics and Outcomes
# Scottish Government
# October 2018 Liam Cavin x44092
#
#=============================================================================
#*****************************************************************************
#=============================================================================


# start with a clean slate
#=========================

rm(list=ls())


# load in the approriate package
# and any others that are needed 
# for this script.
#===============================

# the tidyverse
install.packages("tidyverse")


# Load in the data
# from NHS ISD Alcohol Related Hospital Statistics (ARHS)
#========================================================

arhs.ca <- read.csv("https://www.opendata.nhs.scot/dataset/c4db1692-fa02-4a1c-af4c-6039c74633ea/resource/b0b520e8-3507-46cd-a9b5-cff03007bb57/download/arhs_councilarea_21_11_2017.csv")
arhs.hb <- read.csv("https://www.opendata.nhs.scot/dataset/c4db1692-fa02-4a1c-af4c-6039c74633ea/resource/1cad0a45-a9ce-43f8-9aec-df0aac122764/download/arhs_hbt2014_21_11_2017.csv")

# and have a peek at it
head(arhs.hb)
dim(arhs.hb)
str(arhs.hb)


# create function to reformat data into statistics.gov.scot upload format
#========================================================================

library("stringr")

arhs.format <- function(x,y) {
  pipe <- data.frame(str_sub(x[,6]))  # fetch the geography data from the arhs object
  names(pipe) <- "GeographyCode"      # and name it appropriately
  pipe$DateCode <-  x[,5]             # fetch the reference period data
  pipe$Measurement <- ifelse(colnames(x)[y]=="EASRStays", "Ratio",                 # nested ifelse statements to assign the correct measurement type to the various types of arhs data
                             (ifelse(colnames(x)[y]=="EASRPatients", "Ratio",
                             (ifelse(colnames(x)[y]=="EASRNewPatients", "Ratio",
                             (ifelse(colnames(x)[y]=="AverageNumberOfStaysPerPatient", "Ratio",
                             "Count")))))))
  pipe$Units <- ifelse(colnames(x)[y]=="EASRStays", "Stays Per 100,00 Population",        # nested ifelse statements to assign the correct measurement units to the various types of arhs data
                       (ifelse(colnames(x)[y]=="EASRPatients", "Patients Per 100,00 Population",
                       (ifelse(colnames(x)[y]=="EASRNewPatients", "Patients Per 100,00 Population",
                       (ifelse(colnames(x)[y]=="NumberOfStays", "Stays",
                       (ifelse(colnames(x)[y]=="NumberOfPatients", "Patients",
                       (ifelse(colnames(x)[y]=="NumberOfNewPatients", "Patients",
                       "Average Stays Per Patient")))))))))))
  pipe$Value <- x[,y]                   # fetch the observations
  pipe$AlcoholCondition <- x[,1]        # fetch the alcohol condition descriptive text, with no reformatting
  pipe$AlcoholRelatedStay <- ifelse(colnames(x)[y]=="EASRStays", "Stays",            # nested ifelse statements to assign an appropriate name to the arhs data types 
                                    (ifelse(colnames(x)[y]=="EASRNewPatients", "New Patients",
                                    (ifelse(colnames(x)[y]=="NumberOfStays", "Stays",
                                    (ifelse(colnames(x)[y]=="NumberOfPatients", "Patients",
                                    (ifelse(colnames(x)[y]=="NumberOfNewPatients", "New Patients",
                                    "Average Stays Per Patient")))))))))
  pipe$TypeOfHospital <- ifelse(x[,4] == "SMR01", "General Acute Hospital",         # nested ifelse statements to assign hospital type from SMR code
                                (ifelse(x[,4] == "SMR04", "Psychiatric Hospital", "All")))
  colnames(pipe)[8] <- "Type Of Hospital"                    # rename columns
  colnames(pipe)[7] <- "Alcohol Related Hospital Activity"       
  colnames(pipe)[6] <- "Alcohol Condition"
  return(pipe)
}



# run reformating function on dataset
#=====================================

# apply function to first column of data in council area data
arhs.odpp <- arhs.format(arhs.ca,8)
head(arhs.odpp)
dim(arhs.odpp)
str(arhs.odpp)

# apply function to all subsequent relevant columns in council area data
a <- c(9,10,12,14,16,18)
for (i in unique(a)){
  arhs.odpp <- rbind(arhs.odpp, arhs.format(arhs.ca,i))
}

# apply function to all  relevant columns in health board data
b <- c(8,9,10,12,14,16,18)
for (i in unique(b)){
  arhs.odpp <- rbind(arhs.odpp, arhs.format(arhs.hb,i))
}

# remove NAs and duplicates

arhs.odpp <- arhs.odpp[complete.cases(arhs.odpp), ]
arhs.odpp <- unique(arhs.odpp)

#review output
head(arhs.odpp)
dim(arhs.odpp)
str(arhs.odpp)
typeof(arhs.odpp)
summary(arhs.odpp)
unique(arhs.odpp[,1])
unique(arhs.odpp[,7])
unique(arhs.odpp[,6])
unique(arhs.odpp[,4])


# Last years ARHS data contains superceded geography codes
# Fix this
#=========================================================

arhs.odpp[,1] <-  str_replace_all(arhs.odpp[,1], fixed("S12000015"), "S12000047")
arhs.odpp[,1] <-  str_replace_all(arhs.odpp[,1], fixed("S12000024"), "S12000048")
arhs.odpp[,1] <-  str_replace_all(arhs.odpp[,1], fixed("S08000018"), "S08000029")
arhs.odpp[,1] <-  str_replace_all(arhs.odpp[,1], fixed("S08000027"), "S08000030")



# Finally, export the dataset, ready for upload to statistics.gov.scot 
#=====================================================================

# my local directory, but you can change this to yours
setwd("//scotland.gov.uk//dc1//fs3_home//u441625")

write.csv(arhs.odpp, "arhs_with_geography_for_odpp.csv", row.names=FALSE)


# yaldi
