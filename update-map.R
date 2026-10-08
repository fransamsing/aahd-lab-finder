# Run this after editing data/service_providers.csv or app/app.R.
# It rebuilds map.qmd so the website's map uses the latest data and code.
#
#   source("update-map.R")

app <- readLines("app/app.R", warn = FALSE)
csv <- readLines("data/service_providers.csv", warn = FALSE, encoding = "UTF-8")
qmd <- readLines("map.qmd", warn = FALSE)

start <- grep("^```\\{shinylive-r\\}", qmd)
stopifnot(length(start) == 1)
header <- qmd[seq_len(start - 1)]

block <- c(
  "```{shinylive-r}",
  "#| standalone: true",
  "#| viewerHeight: 1100",
  "",
  "## file: app.R",
  app,
  "",
  "## file: service_providers.csv",
  csv,
  "```"
)

writeLines(c(header, block), "map.qmd")
message("map.qmd updated with ", length(csv) - 1, " providers.")
