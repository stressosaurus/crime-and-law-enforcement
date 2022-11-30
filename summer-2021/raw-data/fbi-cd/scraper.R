### Install https://github.com/jacobkap/fbi package 
###   The R package was authored by Jacob Kaplan (Data Scientist at Princeton University ) 
###   https://jacobdkaplan.com/index.html
## Uncomment the following two lines of code.
## Run them once and comment it back.
# install.packages("devtools")
# devtools::install_github("jacobkap/fbi")

### Load packages
library(fbi) # Use ??fbi in the console for more information
library(tidyverse)

### set working directory
# To have the files save in the proper folders,
#   you need to set the working the directory the same as where the scraper.R exists.

### Set api key
API_KEY = "UQdRQU5cN0hitFUs5BNelClg2OS6Vywd5jqPEqaa"
set_api_key(API_KEY)
get_api_key() # Check: this should print back the API_KEY

### Set data folders
dir.create("csv-files")

### Agencies data
## get a list of agencies and their ORI codes
agencies <- fbi_api_agencies
write.csv(agencies,"csv-files/agencies.csv") # save data into a csv file

### NIBRS data
## use the nibrs_offenses data frame to get a list of offenses
##  Note that "all-offenses" offense level are summed counts
##  You need to specifically say what offense to get the specific offense data

## Get data by state
# Use ?get_nibrs_victim for more information on the parameters
robbery_victims_OR <- get_nibrs_victim(offense = "robbery", 
                                       state_abb = "OR")
# Use ?get_nibrs_offender for more information on the parameters
robbery_offender_OR <- get_nibrs_offender(offense = "robbery", 
                                          state_abb = "OR")

## Get data by agency
all_victims_LAPD <- get_nibrs_victim(offense = "all-offenses", 
                                     ori = "DE0010100")
all_offender_LAPD <- get_nibrs_offender(offense = "all-offenses", 
                                        ori = "DE0010100")
