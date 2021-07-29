## Cleaning procedures to process raw ods files to ready data

# Importing the libraries to be used
library(readODS)

# Setting the folder (relative to this this file) in which the raw data downloads are kept
# Both files originally downloaded from https://www.gov.uk/government/statistics/historical-crime-data
data.folder <- "Downloads"
source.1990 <- "rec-crime-pfa-1990-2002.ods"
source.2003 <- "rec-crime-pfa-2003-2015.ods"

# Getting data from 1990 to 2002
pfa.1990.raw <- read_ods(file.path(data.folder, source.1990))
pfa.1990 <- data.frame(pfa.1990.raw)
colnames(pfa.1990) <- pfa.1990[5, ]
pfa.1990.cropped <- tail(pfa.1990, -7)
pfa.1990.cleaned <- pfa.1990.cropped[!is.na(pfa.1990.cropped$Force), ]

# Getting data from 2003 to 2015
pfa.2003.raw <- read_ods(file.path(data.folder, source.2003))
pfa.2003 <- data.frame(pfa.2003.raw)
colnames(pfa.2003) <- pfa.2003[8,]
pfa.2003.cropped <- tail(pfa.2003, -8)
pfa.2003.cleaned <- pfa.2003.cropped[!is.na(pfa.2003.cropped$Force), ]

# Getting list of force names that appear in both data sets
forces <- unique(pfa.2003.cleaned$Force)
diff <- setdiff(forces, unique(pfa.1990.cleaned$Force)) # British Transport Police only appears in later years
forces <- forces[!(forces %in% c(diff, "Total"))]

# Initializing data frames to hold the cleaned data
years.2003 <- unique(pfa.2003.cleaned$Year)
violence.data.2003 <- data.frame(matrix(ncol=length(forces), nrow=length(years.2003)))
rownames(violence.data.2003) <- years.2003
colnames(violence.data.2003) <- forces

years.1990 <- unique(pfa.1990.cleaned$Year)
violence.data.1990 <- data.frame(matrix(ncol=length(forces), nrow=length(years.1990)))
rownames(violence.data.1990) <- years.1990
colnames(violence.data.1990) <- forces


# Extracting only the total violence against the person figures for each area
for (force in forces) {
  violence.data.2003[, force] <- pfa.2003.cleaned$`TOTAL VIOLENCE AGAINST THE PERSON`[pfa.2003.cleaned$Force == force]
  violence.data.1990[, force] <- pfa.1990.cleaned$`TOTAL VAP`[pfa.1990.cleaned$Force == force]
}

# Combining the two data sets
violence.data.full <- rbind(violence.data.1990, violence.data.2003)

View(violence.data.full)