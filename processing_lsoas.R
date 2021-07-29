library(dplyr)
library(tcltk)

data.folder <- "data/api_data"
data.sub.folders <- list.files(data.folder)

lsoas <- list()
dates <- list()
print("Extracting LSOAs and dates from data")
for (folder in data.sub.folders){
  folder.path <- file.path(data.folder, folder)
  for (file.name in list.files(folder.path)){
    # Going through each file (but not the 'outcome' files), to get the lsoas and dates
    if (!grepl("outcomes", file.name)) {
      file <- read.csv(file.path(folder.path, file.name))
      lsoas <- lsoas %>% append(unique(file$LSOA.code))
      dates <- dates %>% append(file.name %>% substr(1, 7))
    }
  }
  print(folder.path)
}
print("Extraction completed")

# Removing duplicate and blank lsoas
lsoas <- unique(lsoas)
lsoas <- lsoas[!lsoas %in% c("", NA)]
dates <- unique(dates)

# Initializing dataframes to hold the clearup and crime data which can be referenced by lsoa and date
# Must be initialized to zero (or at least, a number, or assigning by adding won't work)
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
clearup.cats <- c("Suspect charged")

print("Counting crimes and clearups in each LSOA")
for (folder in data.sub.folders){
  folder.path <- file.path(data.folder, folder)
    for (file.name in list.files(folder.path)){
      if (!grepl("outcomes", file.name)) {
        file <- read.csv(file.path(folder.path, file.name))
        present.lsoas <- unique(file$LSOA.code)
        date <- file.name %>% substr(1, 7)
        for (lsoa in present.lsoas) {
          if (!lsoa %in% c("", NA)) {
            
            # Summing the crimes in the correct lsoa that are of the chosen crime type
            crimes <- sum(file$Crime.type == crime.type & file$LSOA.code == lsoa)
            # Summing the crimes in the correct lsoa that are of the chosen crime type and cleared up
            clearups <- sum(file$Crime.type == crime.type & file$Last.outcome.category %in% clearup.cats & file$LSOA.code == lsoa)
            
            # Adding these figures to the relevant entry in the dataframes
            lsoa.crime.data[date, lsoa] <- lsoa.crime.data[date, lsoa] + crimes
            lsoa.clearup.data[date, lsoa] <- lsoa.clearup.data[date, lsoa] + clearups
          }
          
        }
      }
  }
  print(folder.path)
}


