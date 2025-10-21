setwd()

library(sf)
library(here)
# Read Data with st_layers
st_layers(here("DATA", "gadm41_AUS.gpkg"))

# Create data Frame of the polygon of whole of Aus
Ausoutline <- st_read(here("DATA", "gadm41_AUS.gpkg"), 
                      layer='ADM_ADM_0')

# Check Coordinate refrence Systems 
print(Ausoutline) # Our projection is Geographic 

# Another way to check our CRS
st_crs(Ausoutline)$proj4string #WGS84

# Set the CRS to the world EPSG System
Ausoutline <- Ausoutline %>%
  st_set_crs(., 4326)

# ---- if there is no CRS ---- 
#Ausoutline <- st_read(here("prac3_data", "gadm41_AUS.gpkg"), 
#                     layer='ADM_ADM_0') %>% 
#  st_set_crs(4326)
# ---------------------------



# Transform for Local Data > Projected Meters
AusoutlinePROJECTED <- Ausoutline %>%
  st_transform(.,3112)

print(AusoutlinePROJECTED)

# Convert sf to sp 
AusoutlineSP <- Ausoutline %>%
as(., "Spatial")

#Conver sp to sf
AusoutlineSF <- AusoutlineSP %>%
  st_as_sf()

# ---- Loading Raster of Minimum Temp ----
# we downloaded data at 5 min second resultion

library(raster)
library(terra)
jan<-terra::rast(here("DATA/wc2.1_5m_tmin", "wc2.1_5m_tmin_01.tif"))

# have a look at the raster layer jan
jan

plot(jan)

# Project the raster to the new grid 
# We want to project it to mollweide projection
# set the proj 4 to a new object
pr1 <- terra::project(jan, "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
plot(pr1)

# Now project back to WGS84
pr1 <- pr1 %>%
  terra::project(., "+proj=longlat +datum=WGS84 +no_defs +type=crs")
plot(pr1)

# ---- Lets Wrangle that DATA -----

# List all file in directory
# look in our folder, find the files that end with .tif and 
library(fs)
dir_info("DATA/wc2.1_5m_tmin/") #dir_info for knowing whats in the file

# now we want to select the files that we want
library(tidyverse)
listfiles<-dir_info("DATA/wc2.1_5m_tmin/") %>%
  filter(str_detect(path, ".tif")) %>%
  dplyr::select(path)%>%
  pull() # pull is the same as $ but here the file name as characters

# have a look at the file names 
listfiles

# Load the Data (the tiff files) into SpatRaster, which is a collection of Raster Layers
worldclimtemp <- listfiles %>%
  terra::rast()

#have a look at the raster stack
worldclimtemp # in nyler (number of layers) we can see it contains 12 layers

# we can sccess January Layer
worldclimtemp[[1]] # cheatsheet df[[""]]

# Rename our Layer Within the Stack
# using the combine function
month <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
               "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

names(worldclimtemp) <- month # rename the names column to the months

# access the data just for january using $ which is used for the column
worldclimtemp$Jan

# Using the stack we can extract data > lets make a data frame of samle sites
site <- c("Brisbane", "Melbourne", "Perth", "Sydney", "Broome", "Darwin", "Orange", 
              "Bunbury", "Cairns", "Adelaide", "Gold Coast", "Canberra", "Newcastle", 
              "Wollongong", "Logan City" )
lon <- c(153.03, 144.96, 115.86, 151.21, 122.23, 130.84, 149.10, 115.64, 145.77, 
         138.6, 153.43, 149.13, 151.78, 150.89, 153.12)
lat <- c(-27.47, -37.91, -31.95, -33.87, 17.96, -12.46, -33.28, -33.33, -16.92, 
         -34.93, -28, -35.28, -32.93, -34.42, -27.64)

#Put all of this inforamtion into one list 
samples <- data.frame(site, lon, lat, row.names="site")

# Extract the data from the Rasterstack for all points 
AUcitytemp<- terra::extract(worldclimtemp, samples)

# add the names of cities to the rows of Aucitytemp
Aucitytemp2 <- AUcitytemp %>% 
  as_tibble()%>% ####
  add_column(Site = site, .before = "Jan") ###

# ---- Descriptive Statstics ----
# lets take perth as an Example by using filter
Perthtemp <- Aucitytemp2 %>%
  filter(site=="Perth")

# Or by Row Location
Perthtemp <- Aucitytemp2[3,]

# Make a Histogram
# histogram lets us see the frequency of distribution of our data

# Make a histogram of Perth Temp
# The tibble stores the data as double we need it as numeric
hist(as.numeric(Perthtemp))

# x is the temperature 
# y frequency of occurrence

## Another Way to Histogram

library(tidyverse)
#define where you want the breaks in the historgram
userbreak<-c(8,10,12,14,16,18,20,22,24,26)

# remove the ID and site columns
Perthtemp <- Aucitytemp2 %>%
  filter(site=="Perth")

t<-Perthtemp %>%
  dplyr::select(Jan:Dec)

hist((as.numeric(t)), 
     breaks=userbreak, 
     col="red", 
     main="Histogram of Perth MIN Temperature", 
     xlab="Temperature", 
     ylab="Frequency")

histinfo

# Information about the histogram R Genarated
histinfo <- as.numeric(t) %>%
  as.numeric()%>%
  hist(.)

histinfo

# plot Geom
plot(Ausoutline$geom)

# Simplify the Geom
AusoutSIMPLE <- Ausoutline %>%
  st_simplify(., dTolerance = 1000) %>%
  st_geometry()%>%
  plot()

# Make sure we are in the coordinates system
print(Ausoutline)

# For Raster
crs(worldclimtemp)

# crop our map extent 
Austemp <- Ausoutline %>%
  # now crop our temp data to the extent
  terra::crop(worldclimtemp,.)

# plot the output
plot(Austemp)

# Use Mask in order for just the shape of austreila
exactAus<-terra::mask(Austemp, Ausoutline)
plot(exactAus)

# Histogram for just one month
#subset using the known location of the raster
hist(exactAus[[3]], col="blue", main ="March temperature")

# Histogram With GGplot2
exactAusdf <- exactAus %>%
  as.data.frame()

library(ggplot2)
# set up the basic histogram
gghist <- ggplot(exactAusdf, 
                 aes(x=Mar)) + 
  geom_histogram(color="black", 
                 fill="white")+
  labs(title="Ggplot2 histogram of Australian March temperatures", 
       x="Temperature", 
       y="Frequency")
# add a vertical line to the hisogram showing mean tempearture
gghist + geom_vline(aes(xintercept=mean(Mar, 
                                        na.rm=TRUE)),
                    color="blue", 
                    linetype="dashed", 
                    size=1)+
  theme(plot.title = element_text(hjust = 0.5))

# We cam also make a histogram combinign two
# Using Pivot Longer 
squishdata<-exactAusdf%>%
  pivot_longer(
    cols = 1:12,
    names_to = "Month",
    values_to = "Temp"
  )

# choose the two months using filter
twomonths <- squishdata %>%
  # | = OR
  filter(., Month=="Jan" | Month=="Jun")

# get the mean the average gor each months with groupby and summarize
meantwomonths <- twomonths %>%
  group_by(Month) %>%
  summarise(mean=mean(Temp, na.rm=TRUE))

meantwomonths

# plot the two months 
ggplot(twomonths, aes(x=Temp, color=Month, fill=Month)) +
  geom_histogram(position="identity", alpha=0.5)+
  geom_vline(data=meantwomonths, 
             aes(xintercept=mean, 
                 color=Month),
             linetype="dashed")+
  labs(title="Ggplot2 histogram of Australian Jan and Jun
       temperatures",
       x="Temperature",
       y="Frequency")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))


### Drop all the NA 
### Decending Order
### Change Bin Size

data_complete_cases <- squishdata %>%
  drop_na()%>% 
  mutate(Month = factor(Month, levels = c("Jan","Feb","Mar",
                                          "Apr","May","Jun",
                                          "Jul","Aug","Sep",
                                          "Oct","Nov","Dec")))

# Plot faceted histogram
ggplot(data_complete_cases, aes(x=Temp, na.rm=TRUE))+
  geom_histogram(color="black", binwidth = 5)+
  labs(title="Ggplot2 faceted histogram of Australian temperatures", 
       x="Temperature",
       y="Frequency")+
  facet_grid(Month ~ .)+
  theme(plot.title = element_text(hjust = 0.5))

### Interactive Histogram with plotly
library(plotly)
# split the data for plotly based on month

jan <- squishdata %>%
  drop_na() %>%
  filter(., Month=="Jan")

jun <- squishdata %>%
  drop_na() %>%
  filter(., Month=="Jun")

# give axis titles
x <- list (title = "Temperature")
y <- list (title = "Frequency")

# set the bin width
xbinsno<-list(start=0, end=40, size = 2.5)

# plot the histogram calling all the variables we just set
ihist<-plot_ly(alpha = 0.6) %>%
  add_histogram(x = jan$Temp,
                xbins=xbinsno, name="January") %>%
  add_histogram(x = jun$Temp,
                xbins=xbinsno, name="June") %>% 
  layout(barmode = "overlay", xaxis=x, yaxis=y)

ihist

# Other Statistics we want to use
# mean per month
meanofall <- squishdata %>%
  group_by(Month) %>%
  summarise(mean = mean(Temp, na.rm=TRUE))

# print the top 1
head(meanofall, n=1)

# standard deviation per month
sdofall <- squishdata %>%
  group_by(Month) %>%
  summarize(sd = sd(Temp, na.rm=TRUE))

# maximum per month
maxofall <- squishdata %>%
  group_by(Month) %>%
  summarize(max = max(Temp, na.rm=TRUE))

# minimum per month
minofall <- squishdata %>%
  group_by(Month) %>%
  summarize(min = min(Temp, na.rm=TRUE))

# Interquartlie range per month
IQRofall <- squishdata %>%
  group_by(Month) %>%
  summarize(IQR = IQR(Temp, na.rm=TRUE))

# perhaps you want to store multiple outputs in one list..
lotsofstats <- squishdata %>%
  group_by(Month) %>%
  summarize(IQR = IQR(Temp, na.rm=TRUE), 
            max=max(Temp, na.rm=T))

# or you want to know the mean (or some other stat) 
#for the whole year as opposed to each month...

meanwholeyear=squishdata %>%
  summarize(meanyear = mean(Temp, na.rm=TRUE))
