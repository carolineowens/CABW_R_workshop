# Load Data and Packages --------------------------------------------------

#Install packages
# https://ucd-cws.github.io/CABW2020_R_training/m1_1_using_R.html#Installing_packages

#Load libraries
library(tidyverse)

#Load data
cscidat <- read_csv('data/cscidat.csv')
ascidat <- read_csv('data/ascidat.csv')

#examine with View(), str(), head(), dim(), names()


# Vectors and Dataframes --------------------------------------------------

#Create vectors (4 types):
## double (numeric) vector
dbl_var <- c(1, 2.5, 4.5)

## integer vector
int_var <- c(1L, 6L, 10L)

## logical vector
log_var <- c(TRUE, FALSE, TRUE, FALSE)

## character vector
chr_var <- c("a", "b", "c")

#Explore vector properties with class(), length()

# take the mean of a character vector
  mean(chr_var)

# adding two numeric vectors of different lengths
vec1 <- c(1, 2, 3, 4)
vec2 <- c(2, 3, 5)
vec1 + vec2

#2-dimensional data (eg ascidat)

# Call vectors within a data frame by using the $ to refer to a certain column. 
  
# calculate the mean CSCI value of the entire dataset
mean(cscidat$CSCI, na.rm = T)

# assign the vector containing sampling site information to a new variable
Sites <- ascidat$site_type


# Wrangling --------------------------------------------------

#Restart session to clear everything! (Ctrl + Shift + F10)
library(tidyverse)
cscidat <- read_csv('data/cscidat.csv')
ascidat <- read_csv('data/ascidat.csv')

###Selecting

# first, select some chosen columns
dplyr_sel1 <- select(cscidat, SampleID_old, New_Lat, New_Long, CSCI)

# examine those columns
head(dplyr_sel1)

# select everything but CSCI and COMID: the "-" sign indicates "not"
dplyr_sel2 <- select(cscidat, -CSCI, -COMID)
head(dplyr_sel2)

# select columns that contain the letter c
dplyr_sel3 <- select(cscidat, matches('c'))
head(dplyr_sel3)

# Note, these datasets should all be appearing in your Environment pane in the upper right hand corner of your screen as you continue.

###Filtering

# get CSCI scores greater than 0.79
dplyr_filt1 <- filter(cscidat, CSCI > 0.79)
head(dplyr_filt1)

# get CSCI scores above latitude 37N
dplyr_filt2 <- filter(cscidat, New_Lat > 37)
head(dplyr_filt2)

# use both filters
dplyr_filt3 <- filter(cscidat, CSCI > 0.79 & New_Lat > 37)
head(dplyr_filt3)

# You can use "&" to signify "and" and "|" to signify "or" in your wrangling statements.

###Mutating

# get observed taxa
dplyr_mut1 <- mutate(cscidat, observed = OE * E)
head(dplyr_mut1)

# add a column for lo/hi csci scores
dplyr_mut2 <- mutate(cscidat, CSCIcat = ifelse(CSCI <= 0.79, 'lo', 'hi'))
head(dplyr_mut2)

# Note: "ifelse" statements can be very helpful for conditional assignments. 
# Their basic structure is if *the criteria* is met, then print 'this', if not, then 
# print 'that'.

# So, the actual statement may look something like:
# ifelse(*the criteria*, 'this', 'that')

###More Functions

# arrange by CSCI scores
dplyr_arr <- arrange(cscidat, CSCI)
head(dplyr_arr)

# rename lat/lon (note the format of newName = oldName)
dplyr_rnm <- rename(cscidat, 
                    lat = New_Lat,
                    lon = New_Long
)
head(dplyr_rnm)

###Piping
csci_new <- cscidat %>% # Use the original dataset and then...
  select(CSCI, COMID, New_Lat) %>% # select only CSCI, COMID, and latitude columns and then...
  filter(New_Lat > 37) %>% # filter for Latitudes above 37 and then...
  mutate(CSCIcat = ifelse(CSCI <= 0.79, 'lo', 'hi')) # new column with categories according to CSCI.


# Plotting ----------------------------------------------------------------

#Restart session to clear everything! (Ctrl + Shift + F10)
library(tidyverse)
cscidat <- read_csv('data/cscidat.csv')
ascidat <- read_csv('data/ascidat.csv')

ggplot(data = ascidat) +
  geom_point(mapping = aes(x = site_type, y = ASCI))

# boxplot of ASCI scores

asci_box <- ggplot(data = ascidat) +
  geom_boxplot(mapping = aes(x = site_type, y = ASCI)) +
  labs(x = "Site Type",
       y = "ASCI Score",
       title = "CABW Workshop Figure",
       subtitle = "October 13, 2020",
       caption = "Data Source: CEDEN")

asci_box

# Save most recently run plot

ggsave(("asci_boxplot.png"),
       #path = "/Insert/File/Path/Here/Ending/In/A/Folder",
       width = 25,
       height = 15,
       units = "cm"
)