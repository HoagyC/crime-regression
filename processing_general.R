library(dplyr)

act.all.files <- function(acting.func, data.folder, save = F, print.=F) {
  save.l <- list()
  file.list <- list.files(data.folder)
  for (file.name in file.list) {
    month.data <- readRDS(file.path(data.folder, file.name))
    output <- acting.func(month.data)
    print(file.name)
    if (print.) {
      print(output)
    }
    if (save) {
      save.l <- save.l %>% append(output)
    }
  }
  return(save.l)
}



data.folder <- "data/month_files"
prop.with.outcomes <- act.all.files(get.n.outcomes, data.folder, save=T)
