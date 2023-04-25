# Title: Graphing random boxplots
# Description: Script used to get background image for personal website
# Author: Gaston Sanchez
# Date: April 2023


# setting seed for random numbers 
set.seed(123) 

# generate random data matrix
# (nrow and ncol values chosen somewhat arbitrarily)
n = 50
dat = matrix(NA, nrow = 15, ncol = n) 
for (i in 1:n) { 
  dat[,i] = (3/4) * runif(13, 0.2, 0.5) + runif(15)/8   
} 

# base colors for color-scheme
bv = runif(100, 0.53, 0.57)
blues = hsv(h = bv, s = 1, v = 0.8, alpha = runif(50, 0.7, 0.9))

gv = runif(100, 0.20, 0.25)
greens = hsv(h = gv, s = 1, v = 1, alpha = runif(50, 0.7, 0.9))

ov = runif(100, 0.10, 0.10)
oranges = hsv(h = ov, s = 1, v = 0.95, alpha = runif(50, 0.7, 0.9))

cols = sample(c(oranges, greens, blues), size = n, replace = TRUE)



# boxplots
png(file="full-boxplots.png", width=1500, height=200)
op = par(mar = c(0, 0, 0, 0))
plot.new() 
plot.window(xlim = c(0, n+1), ylim = c(0.15, 0.5), xaxs = "i") 
boxplot(dat, col = cols, axes = FALSE, 
        xlim = c(0,n), range = 0,
        notch = FALSE, add = TRUE) 
par(op)
dev.off()
