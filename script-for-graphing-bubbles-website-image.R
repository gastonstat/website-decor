# Title: Graphing random bubbles (trajectories)
# Description: Script used to get background image for personal website
# Details: uses data sets:
# - life_expectancy.csv
# - mortality_rate.csv
# Author: Gaston Sanchez
# Date: November 2021

# packages
library(colortools)

# read data files
life = read.csv("life_expectancy.csv", header=TRUE, row.names=1)
mort = read.csv("mortality_rate.csv", header=TRUE, row.names=1)

# get years
years = as.numeric(gsub("X", "", colnames(life)))

# colors (inspired from ggplot)
n = 15
col_names = hcl(h=seq(15, 375-360/n, length=n)%%360, c=100, l=65)

set.seed(12340)
lemons = runif(n, 0.2, 0.25)
marines = runif(n, 0.53, 0.55)
oranges = runif(n, 0.10, 0.15)

col_sample = sample(c(oranges, lemons, marines), size = n)
col_names = hsv(h = col_sample, s = 1, v = 1, 
                alpha = runif(100, 0.7, 0.9))


# plot
png(filename="full-bubbles.png", width=1500, height=200, pointsize = 16)
op = par(mar = rep(0, 4))
plot.new()
plot.window(xlim = c(53, 79), ylim = c(5, 128))
for (i in 1:15) {
  cols = sequential(col_names[i], 100/29, alpha=0.9, fun="log", plot=FALSE)
  size = as.numeric(log(life[i,] / 10))
  points(life[i,], mort[i,], pch=20, col=cols, cex=3)
}
# turn off par
par(op)
dev.off()
