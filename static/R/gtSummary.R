library(gt)
library(gtExtras)
library(jsonlite)

dat <- fromJSON(jsonFile)
gtSummary <- gt_plt_summary(dat, title = "Data summary")
html <- as_raw_html(gtSummary)
html <- gsub("—", "_", html)

outfile <- tempfile(fileext = ".html", tmpdir = dirname(jsonFile))
cat(html, file = outfile)
cat(outfile)
