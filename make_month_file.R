setwd("/home/hoagy/crime_regression")
data.folder <- "data/api_data_new/"

for (month.name in list.files(data.folder)) {
  first.one <- T
  for (file.name in list.files(file.path(data.folder, month.name))) {
    file <- read.csv(file.path(data.folder, month.name, file.name))
    file['Original file'] <- file.name
    if (!first.one) {
      print(file.name)
      month.file <- rbind(month.file, file)
      print(nrow(month.file))
    } else {
      month.file <- file
      first.one <- F
    }
  }
  saveRDS(month.file, file.path("data", "month_files", paste(month.name, "_new.RDS", sep="")))
  print(month.name)
}