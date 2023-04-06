# Title: Graphing random barchart
# Description: Script used to get background image for personal website
# Author: Gaston Sanchez
# Date: November 2021

# random normal distributions
set.seed(123)
dist1 = rnorm(100, mean = 3, sd = 2)
dist2 = rnorm(150, mean = 7, sd = 3)
dist3 = rnorm(200, mean = 11, sd = 4)
dist4 = rnorm(100, mean = 15, sd = 2)
dist5 = rnorm(100, mean = 18, sd = 1)

# assemble into a single distribution,
# and exploring its overall shape
dists = c(dist1, dist2, dist3, dist4, dist5)
min(dists)
max(dists)

#hist(dists, ylim=c(0, 20), col = "#8595E1",  
#     border = "white", main = "", xlab = "", ylab = "", 
#     axes = FALSE) 


# categorizing class intervals
breaks = seq(0, 20, by = 0.5)
n = length(breaks)

y = cuts = cut(
  dists, 
  breaks = breaks[], 
  labels = breaks[1:(n-1)])


# base colors for color-scheme
bv = runif(100, 0.53, 0.57)
blues = hsv(h = bv, s = 1, v = 0.8, alpha = runif(50, 0.7, 0.9))

gv = runif(100, 0.20, 0.25)
greens = hsv(h = gv, s = 1, v = 1, alpha = runif(50, 0.7, 0.9))

ov = runif(100, 0.08, 0.13)
oranges = hsv(h = ov, s = 1, v = 0.95, alpha = runif(50, 0.7, 0.9))

cols = sample(c(oranges, greens, blues), size = n, replace = TRUE)


# barchart
png(file="full-bars.png", width=1500, height=300)
op = par(mar = c(0.5, 0, 0, 0))
barplot(table(y), border = NA, axes = FALSE, names.arg = "", col = cols,
        xlim = c(1.5,46.5), ylim = c(0,30))
par(op)
dev.off()
