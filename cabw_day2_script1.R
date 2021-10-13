
# Load packages and data --------------------------------------------------

library(tidyverse)
library(openxlsx)
library(readxl)

# load data
messy <- readxl::read_excel(path = "data/csci_spreadsheet.xlsx", sheet = 1)

#load cleaned data
csci <- readxl::read_excel(path = "data/csci_spreadsheet.xlsx", sheet = 2)
sites <- readxl::read_excel(path = "data/csci_spreadsheet.xlsx", sheet = 3)

#inspect data
glimpse(sites)
#can also use head(), str(), dim(), names(), etc.

# summary gives a quick assessment of numeric/date/factor data classes.
summary(csci[,c(3:7)])


# Clean data --------------------------------------------------------------

csci_clean <- dplyr::filter(csci, !is.na(CSCI)) 

dim(csci_clean) # how many rows did we drop?

sites_clean <- dplyr::rename(sites, lat = New_Lat, lon = New_Long)
head(sites_clean)

#Save data as csv
readr::write_csv(csci_clean, path="data/csci_clean.csv")
readr::write_csv(sites_clean, path="data/sites_clean.csv")

#Save as R data format (use load() to read back into R)
save(csci_clean, sites_clean, file = "data/csci_sites_clean.rda")

#Save as RDS (like RDA but single objects only)
saveRDS(csci_clean, file="data/csci_clean.rds")
saveRDS(sites_clean, file="data/sites_clean.rds")
# we use readRDS() to read this back into R with any name we want

#Save as XLSX file (using openxlsx package) 

wb <- createWorkbook() # first create the workbook

# then create the worksheets we want to write data to
addWorksheet(wb, sheetName = "csci_clean", gridLines = TRUE)
addWorksheet(wb, sheetName = "sites_clean", gridLines = TRUE)

# now we add our cleaned data
writeDataTable(wb=wb, sheet=1, x = csci_clean, withFilter = FALSE)
writeDataTable(wb=wb, sheet=2, x = sites_clean, withFilter = FALSE)

# now we save our data 
saveWorkbook(wb, "data/csci_sites_clean_spreadsheet.xlsx", overwrite = TRUE)


# Joining Data ------------------------------------------------------------

load("data/csci_sites_clean.rda")

#Left, Inner, Full, Semi (Filtering), Anti (Filtering) joins

#These are in the dplyr package (loaded as part of tidyverse)

# join data by common column: same name
csci_sites_match <- inner_join(csci_clean, sites_clean)
#doesn't work because there are no common column names

# join data by common column: different name
csci_sites_match <- inner_join(csci_clean, sites_clean,
                                 # now the by, remember x col first, then y
                                 by=c("StationCode"="StationID"))

# now we have lat and lon in our dataframe with the data!
str(csci_sites_match)

#Anti-join to figure out what didn't match - list longer frame and column first
anti_join(sites_clean, csci_clean,
          by=c("StationID"="StationCode"))

#Save joined file as RData
# list our data object, and then the filepath and name
# remember to use a relative path!
save(csci_sites_match, file = "data/csci_sites_match.rda")


# Using sf ----------------------------------------------------------------

# install packages if you haven't already
#install.packages(c("sf","mapview", "tmap", "USAboundaries"))

# load packages or "libraries"
library(tidyverse) # wrangling/plotting tools
library(viridis) # nice color palette
library(sf) # "simple features" spatial package for vector based data
library(mapview) # interactive web mapping
library(tmap) # static/interactive mapping
library(USAboundaries) # data for USA boundaries

# Load data - notice the relative path
load(file = "data/csci_sites_match.rda")

#Summarize data
summary(csci_sites_match)

#Make sure all your longitudes are negative (for data on N Amer continent) 
# if not you can correct it like this
  #  csci_sites_match$lon <- abs(csci_sites_match$lon) * -1 # make all values negative
  #  range(csci_sites_match$lon)

# make data sf object: 
df_sf <- st_as_sf(csci_sites_match,
                  coords = c("lon", "lat"), # note we put lon (or X) first!
                  #coords = c(10, 9), # can use numbers here too
                  remove = F, # don't remove these lat/lon cols from the dataframe
                  crs = 4326) # add projection (this is WGS84)


#Transform to different projection?
# check CRS first:
st_crs(df_sf) #CRS is EPSG:4326 WGS 84

# change CRS using st_transform
df_sf_albers <- st_transform(df_sf, crs=3310)

# check that it changed
st_crs(df_sf_albers) #CRS is now EPSG:3310 NAD83 California Albers

## Reading in Spatial Data

## NOTE - need to create spatial data first using USAboundaries pkg or 
# download from Resources tab

## Read in Spatial Data
states <- st_read("data/states_boundaries.shp",
                  stringsAsFactors = FALSE, 
                  as_tibble = TRUE)
# note, read_sf is the same as above, but it is "quiet" 
# so it doesn't print out the details, and uses the other arguments above as defaults
# read_sf("data/states_boundaries.shp)
# check CRS
st_crs(states)

counties <- st_read("data/ca_counties_boundaries.geojson",
                    stringsAsFactors = FALSE, 
                    as_tibble = TRUE)
# check the CRS
st_crs(counties)

plot(df_sf)
# specify a single layer or plot() will do every single facet of dataset
plot(df_sf$geometry) # same as plot(st_coordinates(df_sf))

# this is better
plot(df_sf$geometry, 
     pch=16, 
     # purple dots are CSCI > 0.75, yellow are <0.75
     col=ifelse(df_sf$CSCI>0.75, adjustcolor("purple4", alpha=0.7), "gold"), 
     cex=1.5, 
     xlab = "Longitude", ylab="Latitude")
# add a title
graphics::title("CSCI ( >0.75=purple, <0.75=yellow)")

### Spatial Operations
#Crop points to a single county with st_intersection()

eldor_co <- filter(counties, name=="El Dorado")
# compare rows in the counties object with the rows in the eldor_co...
# we list the thing we want to crop first, then what we crop by second
eldor_pts <- st_intersection(df_sf, eldor_co)

# plot to check
plot(eldor_co$geometry)
plot(df_sf$geometry, add=T, bg="gray", pch=21) # all the points
plot(eldor_pts$geometry, add=T, bg ="purple", pch=21) # just the points we cropped

#Buffer a polygon
# double check CRS
st_crs(df_sf_albers)
# transform the county to same CRS
eldor_co_albers <- st_transform(eldor_co, crs = st_crs(df_sf_albers))

# now buffer by 5 kilometers! (remember, units are in meters)
eldor_co_buff_5km <- st_buffer(eldor_co_albers, dist = 5000)

#Let’s plot this to see how it looks.
plot(eldor_co_buff_5km$geometry, col="skyblue", border="steelblue")
plot(eldor_co_albers$geometry, lty=2, add=T) #original

eldor_pts_5km <- st_intersection(df_sf_albers, eldor_co_buff_5km)

plot(eldor_pts_5km$geometry, add = T)
# look at number of rows in new - old
nrow(eldor_pts_5km) - nrow(eldor_pts)

#Save and export
save(df_sf_albers, df_sf, eldor_co, eldor_co_albers, eldor_co_buff_5km, eldor_pts_5km, eldor_pts, file = "data/m2_3_out_eldorado_sf.rda")


# Making Maps -------------------------------------------------------------

# This is points and El Dorado County Data
load("data/m2_3_out_eldorado_sf.rda")

# this is state boundaries
states <- st_read("data/states_boundaries.shp",
                  stringsAsFactors = FALSE, 
                  as_tibble = TRUE)
# filter to just CA
CA <- filter(states, name=="California")

#Interactive mapping with mapview
# check data is in sf format?
class(df_sf)
# make a map with mapview
mapview::mapview(df_sf, layer="CSCI Sites")

#Customize
#Can view in web browser by clicking the pop out button
mapview(df_sf, 
        col.regions="salmon", 
        cex=3, 
        layer.name="CSCI Sites")

#Plotting with ggplot

library(ggspatial)

nicemap<-
  ggplot() + # set up the framework
  
  # use GEOM_SF
  geom_sf(data = CA, color="gray", lwd=2) + # add the state outline using geom_sf
  
  # use GEOM_POINT: note, we could use geom_sf instead here too!
  geom_point(data=df_sf, aes(x=lon, y=lat), fill="orange", 
             pch=21, alpha=0.7, size=2)+
  
  # scale bar & north arrow
  ggspatial::annotation_north_arrow(location="tr") +
  ggspatial::annotation_scale() +
  # formatting
  labs(x="Longitude (WGS84)", y="Latitude", title="Map of CSCI Sites") + 
  theme_bw() # change this to sans if it doesn't plot

nicemap
# To save plot
# ggsave(filename = "figures/site_map_ggplot.png", width = 8, height = 6, units = "in", dpi = 300)

### Make an interactive plot that shows CSCI scores
mapview(eldor_pts, zcol="CSCI", layer="CSCI")

# add another layer by linking with "+"
mapview(eldor_co, layer="El Dorado County") +
  mapview(eldor_pts, zcol="CSCI", layer="CSCI")

#Challenge: replicate the static ggplot from the website

csci_sub <- csci_clean[,1:2]
csci_sub$newdat <- rep("Aa", nrow(csci_clean))

joindoub <- inner_join(csci_sub, csci_clean)
