library(dplyr)
act.all.files.old <- function(acting.func, data.folder, file.func = NULL, final.func = NULL, timing = TRUE, quick = FALSE) {
  data.sub.folders <- list.files(data.folder)
  full.results <- list()
  for (folder in data.sub.folders){
    results <- list()
    if (timing) {
      init.time <- proc.time()
    }
    run.if <- if (!is.null(file.func)) file.func(folder) else TRUE
    if (run.if) {
      folder.path <- file.path(data.folder, folder)
      file.list <- if (quick) head(list.files(folder.path), quick) else list.files(folder.path)
      for (file.name in file.list){
        file <- read.csv(file.path(folder.path, file.name))
        result <- acting.func(file)
        results <- append(results, result)
      }
      if (is.null(final.func)) {
        full.results[length(full.results) + 1] <- list(results)
      } else {
        print(results)
        processed.res <- final.func(results)
        print(processed.res)
        full.results[length(full.results) + 1] <- processed.res
      }
    }
    if (timing) {
      print(proc.time() - init.time)
    }
  }
  return(full.results)
}

get.crime.cats <- function(file){
  crime.cats <- unique(file$Crime.type)
  return(crime.cats)
}

get.under.investigation <- function(file) {
  under.inv <- sum(file$Last.outcome.category == "Under investigation")
  return(under.inv)
}

get.n.outcomes <- function(file) {
  n.with.outcome <- nrow(file[!file$Last.outcome.category %in% c("", NA), ])
  return(c(n.with.outcome, n.with.outcome / nrow(file)))
}


get.outcome.cats <- function(file){
  outcome.cats <- unique(file$Last.outcome.category)
  return(outcome.cats)
}

collapse.to.unique <- function(x) {
  x <- x %>% unlist %>% unique %>% sort
  return(x)
}

sum.list <- function(x) {
  return(x %>% unlist %>% sum)
}

before.date <- function(year.in, month.in, reverse=F) {
  func <- function(folder) {
    year <- folder %>% substr(1, 4) %>% strtoi(base=10)
    month <- folder %>% substr(6, 7) %>% strtoi(base=10)
    print(paste(folder, year, month))
    if (!(is.numeric(year) && is.numeric(month))) {
      print(paste("getting year/month failed", year, month))
    }
    if (!reverse) {
      return((year < year.in | (year == year.in && month < month.in)))
    } else {
      return(!(year < year.in | (year == year.in && month < month.in))) 
    }
  }
  return(func)
}


