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
police.forces <- readRDS("useful_outputs/police_force_list.RDS")
data.folder <- ("data/month_files/")              # Folder containing the data, in monthly batches

# Initializing burglary dataframes
pf.monthly.burglary <- matrix(0, nrow=0, ncol=length(police.forces))  # Here we make the initial shape of the data frame
pf.monthly.burglary <- data.frame(pf.monthly.burglary)                # The matrix is converted into a data frame
colnames(pf.monthly.burglary) <- police.forces                        # Adds the names of the LSOAs as column names

pf.monthly.burglary.clearup <- data.frame(pf.monthly.burglary)  # Making copies to hold the rest of the data
colnames(pf.monthly.burglary.clearup) <- police.forces          # Renaming otherwise spaces turn to periods
pf.monthly.robbery <- data.frame(pf.monthly.burglary)
colnames(pf.monthly.robbery) <- police.forces 
pf.monthly.robbery.clearup <- data.frame(pf.monthly.burglary)
colnames(pf.monthly.robbery.clearup) <- police.forces 

print(colnames(pf.monthly.burglary.clearup))

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
  burglary.locs <- all.burglary$Falls.within %>% table %>% data.frame %>% transpose   # Counting (table command) the number of burglaries at each LSOA
  burglary.locs.df <- burglary.locs[2, ]                                           # Pulling the count of burglaries into a separate dataframe
  colnames(burglary.locs.df) <- burglary.locs[1, ]                                 # Adding the LSOAs as the column names
  if ("" %in% colnames(burglary.locs.df)) {
    burglary.locs.df <- select(burglary.locs.df, -"")                              # Removing blank LSOAs
  }
  pf.monthly.burglary <- rbind.fill(pf.monthly.burglary, burglary.locs.df)     # Adding the LSOA burglary counts to the bottom of the database
  
    # Same as above but for burglary clearups
  burglary.clearup.locs <- all.burglary.clearup$Falls.within %>% table %>% data.frame %>% transpose
  burglary.clearup.locs.df <- burglary.clearup.locs[2, ]
  colnames(burglary.clearup.locs.df) <- burglary.clearup.locs[1, ]
  if ("" %in% colnames(burglary.clearup.locs.df)) {
    burglary.clearup.locs.df <- select(burglary.clearup.locs.df, -"")
  }
  pf.monthly.burglary.clearup <- rbind.fill(pf.monthly.burglary.clearup, burglary.clearup.locs.df)

  # Same as above with robberies
  robbery.locs <- all.robbery$Falls.within %>% table %>% data.frame %>% transpose
  robbery.locs.df <- robbery.locs[2, ]
  colnames(robbery.locs.df) <- robbery.locs[1, ]
  if ("" %in% colnames(robbery.locs.df)) {
    robbery.locs.df <- select(robbery.locs.df, -"")
  }
  pf.monthly.robbery <- rbind.fill(pf.monthly.robbery, robbery.locs.df)
  
  # Same as above with robbery clearups
  robbery.clearup.locs <- all.robbery.clearup$Falls.within %>% table %>% data.frame %>% transpose
  robbery.clearup.locs.df <- robbery.clearup.locs[2, ]
  colnames(robbery.clearup.locs.df) <- robbery.clearup.locs[1, ]
  if ("" %in% colnames(robbery.clearup.locs.df)) {
    robbery.clearup.locs.df <- select(robbery.clearup.locs.df, -"")
  }
  pf.monthly.robbery.clearup <- rbind.fill(pf.monthly.robbery.clearup, robbery.clearup.locs.df)
  
  time.taken <- (proc.time()["elapsed"] - init.time) %>% round(1) %>% paste("seconds")  # Calculating time taken to process data
  print(paste(file.name, time.taken))                                                   # Displaying the time taken
}


# Changing all NAs to zeros
pf.monthly.burglary[is.na(pf.monthly.burglary)] <- 0
pf.monthly.burglary.clearup[is.na(pf.monthly.burglary.clearup)] <- 0
pf.monthly.robbery[is.na(pf.monthly.robbery)] <- 0
pf.monthly.robbery.clearup[is.na(pf.monthly.robbery.clearup)] <- 0

# Adding a row names with months
n.dates <- nrow(pf.monthly.burglary)
row.names(pf.monthly.burglary) <- tail(dates, n.dates)           
row.names(pf.monthly.burglary.clearup) <- tail(dates, n.dates)           
row.names(pf.monthly.robbery) <- tail(dates, n.dates)           
row.names(pf.monthly.robbery.clearup) <- tail(dates, n.dates)           

# Saving the results in defined folder
save.folder <- "data/prepared_datasets/"
saveRDS(pf.monthly.burglary, file.path(save.folder, "burglary_by_month_pf.RDS"))
saveRDS(pf.monthly.burglary.clearup, file.path(save.folder, "burglary_clearup_by_month_pf.RDS"))
saveRDS(pf.monthly.robbery, file.path(save.folder, "robbery_by_month_pf.RDS"))
saveRDS(pf.monthly.robbery.clearup, file.path(save.folder, "robbery_clearup_by_month_pf.RDS"))
