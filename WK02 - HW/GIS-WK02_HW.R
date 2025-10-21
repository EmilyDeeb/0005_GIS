# Homework WK02 
# The task is to join some non spatial data 
# to some spatial data and wrangle it.
# You need calcuate the average percent of science students (in all) 
# grades per county meeting the required standards and produce a map 
# to show where the Country averages are above or below the State of 
# Washington average.

# Tip do not try and pull out the number from the % symbol, 
# you have the count of students who expected to test and 
# the count who met standard, so you can calculate the percentage.

# thought process clean excel > find common id > dyplr the shit out of it
# calculate the average of all the rest of the countries
# what is Washington Average
# produce a map to show where the country averages 
# are above or below the state of Washington

# Download Files + Unzip
# Set Directory
setwd()

# Read CSV file 
library(readr)
SchoolDataUS<- read.csv("DATA/2018-19_School_Year.csv", 
                          header = TRUE, 
                          sep = ",",  
                          encoding = "latin1")
# Check Class
class(SchoolDataUS)

# check if it has been read corrcitly 
library(tidyverse)
library(dplyr)

Datatypelist <- SchoolDataUS %>% 
  summarise_all(class) %>%
  pivot_longer(everything(), 
               names_to="All_variables", 
               values_to="Variable_class")

Datatypelist

# Lets edit the data
SchoolDataUSED <- edit(SchoolDataUS) # Why? Something is missing here

# Summary of Data
summary(df)


# Top 5 of the Data
SchoolDataUSED%>%
  colnames()%>%
  # just look at the head, top5
  head()

# Filter with the word Science 
library(stringr)
SchoolDataUSED<- SchoolDataUS  %>% 
  filter(str_detect(`TestSubject`, "^Science"))

# Access Column
SchoolDataUSED $TestSubject

# Removes Repeated Rows
SchoolDataUSED2<-SchoolDataUSED %>%
  distinct()

library(dplyr)
library (janitor)
# if you Want to remove all capital letter
SchoolDataUSED2 <- SchoolDataUSED2 %>%
#here the ., means all data
  clean_names(., case="big_camel")

# 
#select only columns we want
SchoolDataUSED3 <- SchoolDataUSED2 %>%
  dplyr::select(DistrictCode,
              DistrictName,
              County,
              DistrictOrganizationId, 
              SchoolName, 
              GradeLevel,
              TestSubject, 
              CountOfStudentsExpectedToTest, 
              CountMetStandard)

#top of data
slice_head(SchoolDataUSED3, n=5)

#bottom of data
slice_tail(SchoolDataUSED3,n=5)

# Sum of Students Expected to Test per District
# view(SchoolDataUSED3)

str(SchoolDataUSED3)

# Turn Data to numeric 
SchoolDataUSED3 <- SchoolDataUSED3 %>%
  mutate(
    CountOfStudentsExpectedToTest = as.numeric(CountOfStudentsExpectedToTest),
    CountMetStandard = as.numeric(CountMetStandard)
  )

# 

SchoolDataMerged <- SchoolDataUSED3 %>% # Create a new Excel
  group_by(County) %>% # that is joined by ID
  summarise(
    SchoolsPerCounty = n(),  # counts rows (schools) per County
    SumExpected = sum(CountOfStudentsExpectedToTest, na.rm = TRUE), # Sum
    SumMet = sum(CountMetStandard, na.rm = TRUE), # Sum
   AveragePerCounty = (SumMet/SumExpected)*100 # Percentage
  )


# ---- Dealing With The Map ------
library(tmap)
library(tmaptools)
library(sf)
USCountyMap<- st_read(here::here("DATA/Washington_Counties_with_Natural_Shoreline___washsh_area",
                         "Washington_Counties_with_Natural_Shoreline___washsh_area.shp"))

#plot it using the qtm function
qtm(USCountyMap)

# join attributes to boundries
library(janitor)

# Join types always best to use left join, 
# first make all CapsLock

SchoolDataMerged <- SchoolDataMerged %>%
  mutate(County = toupper(County))

SchoolDataMerged <- SchoolDataMerged %>%
  rename(CountySchool=County)

library(dplyr)

# Join Left
CountyDataMap <- USCountyMap %>%
  left_join(SchoolDataMerged, 
            by = c("COUNTY" = "CountySchool"))

# Plot
tmap_mode("plot")
qtm(CountyDataMap, 
    fill = "AveragePerCounty")

# ----- Compare Above Average or Below -----

# Produce a map that shows the county
# Average above Washington or Below

KingAvg <- CountyDataMap %>%
  filter(COUNTY == "KING") %>%
  pull(AveragePerCounty)

CountyDataMap <- CountyDataMap %>%
  mutate(
    ComparedToKing = ifelse(
      AveragePerCounty >= KingAvg,
      "Above King Average",
      "Below King Average"
    )
  )


tmap_mode("plot")
qtm(CountyDataMap,
    fill = "ComparedToKing",
    fill.palette = c("blue", "pink"),
    title = "Counties Above or Below King County Average")

           