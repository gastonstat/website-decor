# Title: Graphing random scatterplot
# Description: Script used to get background image for personal website
# Author: Gaston Sanchez
# Date: November 2021

# packages
library(MASS)

set.seed(20092006)
M = mvrnorm(300, c(0, 0), matrix(c(1, 0.8, 0.8, 1), 2))
x = M[,1]
y = M[,2]


# base colors for color-scheme
set.seed(11)

bv = runif(100, 0.53, 0.56)
blues = hsv(h = bv, s = 1, v = 1, alpha = runif(100, 0.6, 0.9))

gv = runif(100, 0.20, 0.25)
greens = hsv(h = gv, s = 1, v = 1, alpha = runif(100, 0.6, 0.9))

ov = runif(100, 0.10, 0.15)
oranges = hsv(h = ov, s = 1, v = 1, alpha = runif(100, 0.6, 0.9))

cols = sample(c(oranges, greens, blues), size = 100, replace = TRUE)

whites = hsv(h = 0, s = 0, v = 1, alpha = runif(100, 0.3, 0.7))
sizes = c(3.5 * runif(99), 7)


# scatterplot
png(file="full-scatterplot.png", width=1500, height=300)
op = par(bg = "white", mar = rep(0, 4))
plot(x, y, type = 'n', xlim = c(-2, 2), ylim = c(-3, 3), axes=FALSE)
points(x, y, pch = 19, cex = sizes, col = whites, bg = whites)
points(x, y, pch = 19, cex = sizes, col = cols, bg = cols)
par(op)
dev.off()
