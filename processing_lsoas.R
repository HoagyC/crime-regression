library(dplyr)
library(tcltk)

setwd("/home/hoagy/crime_regression") # set the working directory
data.folder <- "data/api_data"
data.sub.folders <- list.files(data.folder)

lsoas <- readRDS("lsoas.list")
dates <- readRDS("dates.list")
outcome.cats <- list()
print("Extracting LSOAs and dates from data")
for (folder in head(data.sub.folders, 1)){
  outcome.cats <- list()
  year <- folder %>% substr(1, 4) %>% strtoi
  month <- folder %>% substr(6, 7) %>% strtoi
  if (year <= 2018 || (year == 2018 && month < 6)) {
    init.t <- proc.time()
    folder.path <- file.path(data.folder, folder)
    for (file.name in list.files(folder.path)){
      # Going through each file (but not the 'outcome' files), to get the lsoas and dates
      if (!grepl("outcomes", file.name)) {
        file <- read.csv(file.path(folder.path, file.name))
        lsoas <- lsoas %>% append(unique(file$LSOA.code))
        dates <- dates %>% append(file.name %>% substr(1, 7))
        outcome.cats <- outcome.cats %>% append(unique(file$Last.outcome.category))
        outcome.cats <- unique(outcome.cats)
      }
    }
    print(folder)
    print(proc.time() - init.t)
    print(outcome.cats)
  }
  print(folder.path)
}
print("Extraction completed")

# Removing duplicate and blank lsoas
lsoas <- unique(lsoas)
lsoas <- lsoas[!lsoas %in% c("", NA)]
dates <- unique(dates)

# Initializing dataframes to hold the clearup and crime data which can be referenced by lsoa and date
# Initialized to zero so that we can add to it instead of overwriting
lsoa.clearup.data <- matrix(0, nrow=length(dates), ncol=length(lsoas))
lsoa.clearup.data <- data.frame(lsoa.clearup.data)
colnames(lsoa.clearup.data) <- lsoas
rownames(lsoa.clearup.data) <- dates

lsoa.crime.data <- matrix(0, nrow=length(dates), ncol=length(lsoas))
lsoa.crime.data <- data.frame(lsoa.crime.data)
colnames(lsoa.crime.data) <- lsoas
rownames(lsoa.crime.data) <- dates

print("Dataframes initialized")

crime.type <- "Violence and sexual offences"
crime.types <- list()
all.outcome.cats <- unique(outcome.cats)
clearup.cats <- c("Offender sent to prison", "Offender given suspended prison sentence", "Offender fined", "Offender ordered to pay compensation")

print("Counting crimes and clearups in each LSOA")
for (folder in head(data.sub.folders, 1)){
  folder.path <- file.path(data.folder, folder)
  year <- folder %>% substr(1, 4) %>% strtoi
  month <- folder %>% substr(6, 7) %>% strtoi
  if (year <= 2018 || (year == 2018 && month < 6)) {
    for (file.name in list.files(folder.path)){
      if (!grepl("outcomes", file.name)) {
        file <- read.csv(file.path(folder.path, file.name))
        present.lsoas <- unique(file$LSOA.code)
        date <- file.name %>% substr(1, 7)
        for (lsoa in present.lsoas) {
          if (!lsoa %in% c("", NA)) {
            
            # Summing the crimes in the correct lsoa that are of the chosen crime type
            crimes <- sum(file$Crime.type == crime.type & file$LSOA.code == lsoa)
            crime.types <- crime.types %>% append(unique(file$Crime.type)) %>% unique

            # Summing the crimes in the correct lsoa that are of the chosen crime type and cleared up
            clearups <- sum(file$Crime.type == crime.type & file$Last.outcome.category %in% clearup.cats & file$LSOA.code == lsoa)

            # Adding these figures to the relevant entry in the dataframes
            lsoa.crime.data[date, lsoa] <- lsoa.crime.data[date, lsoa] + crimes
            lsoa.clearup.data[date, lsoa] <- lsoa.clearup.data[date, lsoa] + clearups
          }
          
        }
      }
    }
  }
  print(folder.path)
}

View(lsoa.crime.data)

# 
# # Saving the completed datasets
# saveRDS(lsoa.crime.data, file="crime_data_early.rds")``
# saveRDS(lsoa.clearup.data, file="clearup_data_early.rds")



