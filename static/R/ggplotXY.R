library(ggplot2)
library(jsonlite)
library(base64enc)

maybeNumeric <- function(x) {
  xx <- as.numeric(x)
  if(anyNA(xx)) x else xx
}

#XY <- "{\"x\":[1,2],\"y\":[3,4]}"
dat <- as.data.frame(fromJSON(XY))

gg <- ggplot(dat, aes(x = maybeNumeric(x), y = y)) + 
  geom_point()

png <- tempfile(fileext = ".png")
ggsave(png, gg, width = w, height = h, units = "px", dpi = "print")

base64 <- dataURI(file = png, mime = "image/png")
cat(base64)
