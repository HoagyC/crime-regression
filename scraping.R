## Scraping data from the UK police API at https://data.police.uk/

library(dplyr)
library(request)

rate_limit = 15       # This is the maximum number of requests that can be made per second

force.names <- "https://data.police.uk/api/forces" %>% api() %>% http("GET")
force.ids <- lapply(force.names, function(x) x$id)
ids.by.area = list()

print(force.ids)

ids.by.area = list()
for (force in force.ids) {
  addr <- paste("https://data.police.uk/api/", force, "/neighbourhoods", sep = '')
  print(addr)
  data <- addr %>% api() %>% http("GET")
  area.ids <- list(lapply(data, function(x) x$id))
  ids.by.area[force] <- area.ids
  Sys.sleep(1 / rate_limit)
}

boundaries.by.area <- list()

for (force in force.ids) {
  county.boundaries <- list()
  for (local.id in unlist(ids.by.area[force])) {
    addr <- paste("https://data.police.uk/api/", force, "/", local.id, "/boundary", sep = '')
    print(addr)
    boundary <- addr %>% api() %>% http("GET") %>% list()
    county.boundaries[local.id] <- boundary
    Sys.sleep(1 / rate_limit)
    View(county.boundaries)
  }
  boundaries.by.area[force] <- county.boundaries
}

View(boundaries.by.area)
