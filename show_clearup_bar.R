library(dplyr)

act.all.files <- function(acting.func, data.folder, save = T, print.=F) {
  save.l <- list()
  file.list <- tail(list.files(data.folder), -16)
  for (file.name in file.list) {
    month.data <- readRDS(file.path(data.folder, file.name))
    output <- acting.func(month.data)
    print(file.name)
    if (print.) {
      print(output)
    }
    if (save) {
      save.l[[length(save.l) + 1]] <- output
    }
  }
  return(save.l)
}

all.outcomes <- readRDS("useful_outputs/all_outcome_cats.rds")

get.n.outcomes <- function(month.data) {
  counts <- list()
  for (outc in all.outcomes) {
    counts[[outc]] <- length(which(month.data$Last.outcome.category == outc & 
                                  month.data$Crime.type == "Burglary"))
  }
  return(counts)
}


data.folder <- "data/month_files"
prop.with.outcomes <- act.all.files(get.n.outcomes, data.folder, save=T, print. = T)
bar.data <- lapply(prop.with.outcomes, unlist)
data.m <- do.call(cbind, bar.data)
data2 <- rbind(data.m[8:20, ], data.m[1:7, ], data.m[21:28, ])
col <- c(rep("red", 13), rep("grey", 15))
barplot(data2, col=col, names.arg = tail(list.files(data.folder), -16), cex.names = 0.5)
