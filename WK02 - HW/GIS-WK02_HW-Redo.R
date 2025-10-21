# Homework WK02 
# calculate the average percent of science students (in all) grades per county meeting the required standards

# We have:
# Count met standard
# Count expected to test 
# test subject

# thought process clean excel > find common id > dyplr the shit out of it
# calculate the average of all the rest of the countries
# what is Washington Average
# produce a map to show where the country averages 
# are above or below the state of Washington

# Download Files + Unzip
# Set Directory

setwd()
library(tidyverse)
library(here)
library(sf)
library(janitor)
library(readr)
library(dplyr)
library(stringr)
library(tmap)
library(tmaptools)

# Read CSV file 
report<- read.csv(here::here("DATA","2018-19_School_Year.csv"), 
                          header = TRUE, 
                          sep = ",",  
                          encoding = "latin1")

# Reading Map Shp
shape<- st_read(here::here("DATA","Washington_Counties_with_Natural_Shoreline___washsh_area",
                                 "Washington_Counties_with_Natural_Shoreline___washsh_area.shp"))


# ---- Task ----
# Filter For Science Students and Mutate to 
# to workout % that have met

# Check Class
class(report)

# check if it has been read correctly 

Datatypelist <- report %>% 
  summarise_all(class) %>%
  pivot_longer(everything(), 
               names_to="All_variables", 
               values_to="Variable_class")

Datatypelist

# Clean Data Frame Of CapsLock and make is with underscore
report <- report %>%
  janitor::clean_names()

# Make New DataFrame that will only contain 
# these:
county_only <- report %>%
  clean_names() %>%
  select(county, organization_level, test_subject, count_met_standard, 
         count_of_students_expected_to_test, grade_level)%>%
  
  # the != means don't select this, but select everything else
  # i could also filter on where 
  filter(county != "Multiple")%>%
  filter(organization_level == "School")%>%
  filter(test_subject == "Science")%>%
  filter(grade_level=="All Grades")%>%
  group_by(county)%>%
  
  # we need to remove NAs - note we can use this function or do it within the summarise below with the argument na.rm=T they do the same thing!
  na.omit()%>%
  # na.rm = T means remove missing values from the data
  # could also use na.omit or filter greater than 0
  
  # Mutate is to numeric
  mutate(
    count_met_standard = as.numeric(count_met_standard),
    count_of_students_expected_to_test = as.numeric(count_of_students_expected_to_test)
  ) %>%
 # Group by and summrize
   group_by(county) %>%
  summarise(
    total_county_met_standard = sum(count_met_standard, na.rm = TRUE),
    total_county_to_test = sum(count_of_students_expected_to_test, na.rm = TRUE)
  ) %>%
 
  # Calculate Percentage of who met / numbers that where tested
   mutate(percent_met_per_county = (total_county_met_standard / total_county_to_test) * 100)
  
# Top 5 of the Data
county_only%>%
  colnames()%>%
  # just look at the head, top5
  head()

# We now have the percent that me from the counties and and 
# need to work out what the state average is


state_average <- county_only%>%
  summarise(state_average= mean(percent_met_per_county))%>%
  pull() # Pulls Value

# or use $ to acess value of columns
state_average2 <- mean(county_only$percent_met_per_county)

#or using summerize for state average
state_average3 <- county_only%>%
  select(percent_met_per_county)%>%
  summarise(state_average=mean(percent_met_per_county))%>%
  pull()

# Ok, now we need to make a column that compares each county 
# value to the state average and some text to say if it is above or below...

county_only_above_below_state <- county_only %>%
  mutate(difference_to_state=(percent_met_per_county-state_average))%>%
  mutate(across(difference_to_state, ~ round(.x, 0)))%>%
  mutate(above_below = case_when(difference_to_state<0 ~ "below",
                                 difference_to_state>0 ~ "above",
                                 difference_to_state==0 ~ "equal"
  ))

#Join to our spatial data....
joined_data <- shape %>% 
  clean_names(.) %>%
  left_join(., 
            county_only_above_below_state,
            by = c("countylabe" = "county"))

# --- Let's map ---

bbox_county <- joined_data %>%
  st_bbox(.) %>% 
  tmaptools::read_osm(., type = "osm", zoom = NULL)

tm_shape(bbox_county)+
  tm_rgb()+

  tm_shape(joined_data) + 
  tm_polygons(fill="above_below",
              fill.scale = tm_scale_categorical(
                values="brewer.bu_pu"),
              fill_alpha=0.5)+
  tm_compass(position = c("left", "bottom"),type = "arrow")+
  tm_scalebar(position = c("left", "bottom")) +
  tm_title_out("Counties above or below state avearge for science in all grades", 
           position=tm_pos_out("center", "top"))
