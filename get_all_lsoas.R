# GET ALL LSOAS
#
# This file outputs a list of all LSOAs which occur in at least one month's data
# It saves the result as an R list in a folder of choice

setwd("/home/hoagy/crime_regression") # Set working directory
data.folder <- "data/month_files"     # Set folder to check for data

lsoas <- list()  # Initialize list of LSOAs to add to

for (file.name in list.files(data.folder)) {
  print(file.name)
  file <- readRDS(file.path(data.folder, file.name))            # Read the month file
  lsoas <- lsoas %>% append(unique(file$LSOA.code)) %>% unique  # Add all unique LSOAs in the month file and then remove duplicates
  lsoas <- lsoas[!is.na(lsoas)]                                 # Remove NAs
  lsoas <- lsoas[!lsoas == ""]                                  # Remove blank LSOA names
}

# Saves the file in a folder of choice
output.folder <- "useful_outputs"
saveRDS(lsoas, file.path(output.folder, "lsoa_list.rds"))