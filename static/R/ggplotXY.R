library(ggplot2)
library(jsonlite)
library(base64enc)

#XY <- "{\"x\":[1,2],\"y\":[3,4]}"
dat <- as.data.frame(fromJSON(XY))

gg <- ggplot(dat, aes(x = x, y = y)) + 
  geom_point()

png <- tempfile(fileext = ".png")
ggsave(png, gg)

base64 <- dataURI(file = png, mime = "image/png")
cat(base64)
