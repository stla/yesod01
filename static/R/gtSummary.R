library(gtExtras)
library(jsonlite)

dat <- fromJSON(jsonData)
gtSummary <- gt_plt_summary(dat, title = "Data summary")
html <- as_raw_html(gtSummary)
cat(html)
