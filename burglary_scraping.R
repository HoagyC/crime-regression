# CREATING BURGLARY AND ROBBERY DATA TABLES
#
# This script takes as input the "...street.csv" data files from police.uk, pre-processed into a single R data frame for each month
# It counts the number of burglaries and robberies that were committed in each Lower layer single output area (LSOA) in each month
# It only counts those crimes where an outcome of some kind is given
# It also counts the number of burglaries and robberies which has an outcome which is part of an imported set of clear up outcomes
# It saves the outputs as separate dataframes which give the number of crimes and clearups by month and LSOA


# Importing the libraries used
library(plyr)
library(dplyr)
library(data.table)

setwd("/home/hoagy/crime_regression")             # Setting the code to run from this directory
dates <- readRDS("useful_outputs/dates_list.RDS") # Import list of dates (YYYY-MM)
lsoas <- readRDS("useful_outputs/lsoa_list.rds")  # Import list of all lsoas
data.folder <- ("data/month_files/")              # Folder containing the data, in monthly batches

# Initializing burglary dataframes
lsoa.monthly.burglary <- matrix(0, nrow=0, ncol=length(lsoas))  # Here we make the initial shape of the data frame
lsoa.monthly.burglary <- data.frame(lsoa.monthly.burglary)      # The matrix is converted into a data frame
colnames(lsoa.monthly.burglary) <- lsoas                        # Adds the names of the LSOAs as column names

lsoa.monthly.burglary.clearup <- data.frame(lsoa.monthly.burglary)  # Making copies to hold the rest of the data
lsoa.monthly.robbery <- data.frame(lsoa.monthly.burglary)
lsoa.monthly.robbery.clearup <- data.frame(lsoa.monthly.burglary)


print("Dataframes initialized")

clearup.cats <- readRDS("useful_outputs/clearup_categories.rds")     # Import the list of outcomes which we will assign as 'clearups'
first.date.with.outcomes <- "2012-04"                                # Before this month, most crimes do not have assigned outcomes
start.year <- first.date.with.outcomes %>% substr(1, 4) %>% strtoi            # Get starting year as number
start.month <- first.date.with.outcomes %>% substr(6, 7) %>% strtoi(base=10)  # Get starting month as number

print("Counting crimes and clearups in each LSOA..")

for (file.name in list.files(data.folder)){
  init.time <- proc.time()["elapsed"]        # Saving the starting time for each loop, to check how long each loop takes

  # Getting the year, month and full date of the month file
  year <- file.name %>% substr(1, 4) %>% strtoi
  month <- file.name %>% substr(6, 7) %>% strtoi(base=10) # Need to specify base or it interprets "08" as an empty octal!
  date <- file.name %>% substr(1, 7)
  
  if (year < start.year || (year == start.year && month < start.month)) {
    next  # Skip to the next month if data is from before outcome data is common
  }
  
  month.data <- readRDS(file.path(data.folder, file.name)) # Read the file containing the month's data
  
  # Getting only the relevant rows from the full month's dataset
  all.burglary <- month.data[month.data$Crime.type == "Burglary" & !(month.data$Last.outcome.category %in% c("", NA)), ]
  all.robbery <- month.data[month.data$Crime.type == "Robbery" & !(month.data$Last.outcome.category %in% c("", NA)), ]
  all.burglary.clearup <- all.burglary[all.burglary$Last.outcome.category %in% clearup.cats, ]
  all.robbery.clearup <- all.robbery[all.robbery$Last.outcome.category %in% clearup.cats, ]
  
  # Getting the count of burglaries in the given month in each LSOA
  burglary.locs <- all.burglary$LSOA.code %>% table %>% data.frame %>% transpose   # Counting (table command) the number of burglaries at each LSOA
  burglary.locs.df <- burglary.locs[2, ]                                           # Pulling the count of burglaries into a separate dataframe
  colnames(burglary.locs.df) <- burglary.locs[1, ]                                 # Adding the LSOAs as the column names
  if ("" %in% colnames(burglary.locs.df)) {
    burglary.locs.df <- select(burglary.locs.df, -"")                              # Removing blank LSOAs
  }
  lsoa.monthly.burglary <- rbind.fill(lsoa.monthly.burglary, burglary.locs.df)     # Adding the LSOA burglary counts to the bottom of the database
  
    # Same as above but for burglary clearups
  burglary.clearup.locs <- all.burglary.clearup$LSOA.code %>% table %>% data.frame %>% transpose
  burglary.clearup.locs.df <- burglary.clearup.locs[2, ]
  colnames(burglary.clearup.locs.df) <- burglary.clearup.locs[1, ]
  if ("" %in% colnames(burglary.clearup.locs.df)) {
    burglary.clearup.locs.df <- select(burglary.clearup.locs.df, -"")
  }
  lsoa.monthly.burglary.clearup <- rbind.fill(lsoa.monthly.burglary.clearup, burglary.clearup.locs.df)

  # Same as above with robberies
  robbery.locs <- all.robbery$LSOA.code %>% table %>% data.frame %>% transpose
  robbery.locs.df <- robbery.locs[2, ]
  colnames(robbery.locs.df) <- robbery.locs[1, ]
  if ("" %in% colnames(robbery.locs.df)) {
    robbery.locs.df <- select(robbery.locs.df, -"")
  }
  lsoa.monthly.robbery <- rbind.fill(lsoa.monthly.robbery, robbery.locs.df)
  
  # Same as above with robbery clearups
  robbery.clearup.locs <- all.robbery.clearup$LSOA.code %>% table %>% data.frame %>% transpose
  robbery.clearup.locs.df <- robbery.clearup.locs[2, ]
  colnames(robbery.clearup.locs.df) <- robbery.clearup.locs[1, ]
  if ("" %in% colnames(robbery.clearup.locs.df)) {
    robbery.clearup.locs.df <- select(robbery.clearup.locs.df, -"")
  }
  lsoa.monthly.robbery.clearup <- rbind.fill(lsoa.monthly.robbery.clearup, robbery.clearup.locs.df)
  
  time.taken <- (proc.time()["elapsed"] - init.time) %>% round(1) %>% paste("seconds")  # Calculating time taken to process data
  print(paste(file.name, time.taken))                                                   # Displaying the time taken
}

# Changing all NAs to zeros
lsoa.monthly.burglary[is.na(lsoa.monthly.burglary)] <- 0
lsoa.monthly.burglary.clearup[is.na(lsoa.monthly.burglary.clearup)] <- 0
lsoa.monthly.robbery[is.na(lsoa.monthly.robbery)] <- 0
lsoa.monthly.robbery.clearup[is.na(lsoa.monthly.robbery.clearup)] <- 0

# Adding a row names with months
n.dates <- nrow(lsoa.monthly.burglary)
row.names(lsoa.monthly.burglary) <- tail(dates, n.dates)           
row.names(lsoa.monthly.burglary.clearup) <- tail(dates, n.dates)           
row.names(lsoa.monthly.robbery) <- tail(dates, n.dates)           
row.names(lsoa.monthly.robbery.clearup) <- tail(dates, n.dates)           

# Saving the results in defined folder
save.folder <- "data/prepared_datasets/"
saveRDS(lsoa.monthly.burglary, file.path(save.folder, "burglary_by_month_lsoa"))
saveRDS(lsoa.monthly.burglary.clearup, file.path(save.folder, "burglary_clearup_by_month_lsoa"))
saveRDS(lsoa.monthly.robbery, file.path(save.folder, "robbery_by_month_lsoa"))
saveRDS(lsoa.monthly.robbery.clearup, file.path(save.folder, "robbery_clearup_by_month_lsoa"))
