# EEOB590A Fall 2021
# Intro to R - Part 3

######## Topics #######
# 1. Practice reading in Excel files, with some added complexity
# 2. Learning about dates

# This script requires 2 data files:
# "leaf damage ag expt.xls"
# "Exploring_dates.xlsx"

# Start by loading the libraries you might need (hint, need one for reading in excel files)
library(readxl) #maybe readr would work instead?


# 1) Reading in Excel files

# We are going to work with the "leaf damage ag expt.xls" file. This is the
# exact file I found from an undergraduate project in 2007. It's not perfect.
# Let's see how we can fix it.

# 1.a - read in the "beans" worksheet and give it the same name as the worksheet
#tab it came from
beans <- read_excel("~/EEOB590A_DataScienceR/CourseMaterials/data/raw/leaf damage ag expt.xls", sheet = 1)

# Look at the structure of 'beans'. What class are each of the columns in?
str(beans) # all are characters except fruits and weight are numbers

# Read in the beans worksheet again, and tell R the appropriate class/column type
# for each column. Note that read_excel doesn't let you choose factor, so use text instead. Give this dataframe a new name so you can compare to earlier "beans" df.
beans2 <- read_excel("~/EEOB590A_DataScienceR/CourseMaterials/data/raw/leaf damage ag expt.xls", col_types = c("text", "text", "text","numeric", "numeric", "text", "numeric", "text", "numeric", "text"))


# Check to make sure the columns are now in the appropriate class/column type
str(beans2)
#what should be factor is "character" instead


# After you read it in, you realize that the "Number" column indicates the ID
# of each exclosure, and therefore should be a factor. Change that column to a
# factor and then check to make sure it is actually a factor
beans2$Number <- as.factor(beans2$Number)
class(beans2$Number)


# check the number of levels for this factor to make sure it converted correctly
levels(beans2$Number)
# factor with multiple levels

# 1.b - Herbivory Worksheet
# read in the "herbivory" worksheet and give it the same name as the worksheet it came from

herbivory <- read_excel("~/EEOB590A_DataScienceR/CourseMaterials/data/raw/leaf damage ag expt.xls", sheet = 2)
view(herbivory)

# Notice that there are some X's in the leaflet columns of the herbivory worksheet?
# Read it in again, but this time tell R that the X means NA, and give the dataframe a new  name so you can compare.

herbivory2 <- read_excel("~/EEOB590A_DataScienceR/CourseMaterials/data/raw/leaf damage ag expt.xls", sheet = 2, na = "X")
view(herbivory2)

# Read the herbivory datasheet in again, but pull in everything but columns L and M because they do not belong with rows 2-6. If you aren't sure how to do this, look at the help file for the function that reads in excel files

herbivory3 <- read_excel("~/EEOB590A_DataScienceR/CourseMaterials/data/raw/leaf damage ag expt.xls", sheet = 2, na = "X", range = "A1:K718")
view(herbivory3)

# 2) Dealing with dates

# Read in the "Exploring_dates.xlsx" file.

dates <- read_excel("~/EEOB590A_DataScienceR/CourseMaterials/data/raw/Exploring_dates.xlsx")

# Is this object a vector, matrix, dataframe, tibble, array or list?
str(dates)
# it's a tibble

# The name of this object is kinda unwieldy. Rename the object "dates"

date <- dates
#apparently i already named it this so i renamed it "date"

# What format does R recognize each of the dates as? Which ones did not read in
# as dates?
str(date)
# date4 and date6 read as numbers not dates. Others read in as YYY-MM-DD

view(date)

# Create a new column based on the difference in time between date 1 and date 2
# and name it "duration" and add it to the "dates" dataframe

date$duration <- date$date1 - date$date2

view(date)
