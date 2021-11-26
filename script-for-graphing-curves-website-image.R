# Title: Graphing random curves (polygons)
# Description: Script used to get background image for personal website
# Author: Gaston Sanchez
# Date: November 2021

# new runs with various sets of 10 points
set.seed(4326)
N <- 100
x <- seq(from = -0.1, to = 1.1, length.out = N)
target <- sin(1 + x^2)
noise <- rnorm(N, mean = 0, sd = 0.03)
y <- target + noise
n <- 20

samples <- as.list(1:3)
x_ins <- as.list(1:3)
y_ins <- as.list(1:3)

for (h in 1:3) {
  samples[[h]] <- sample.int(N, size = n)
  x_ins[[h]] <- x[samples[[h]]]
  y_ins[[h]] <- y[samples[[h]]]
}


# color scheme
cols <- c("#e5fb2a", "#2588b3aa", "#ff8700aa")


# plot
set.seed(125)
png(file="full-curves.png", width=1500, height=300)
op = par(bg = "white", mar = rep(0, 4))
plot(x, target, type = "l", lwd = 3, 
     xlab = "", ylab = "", axes = FALSE,
     xlim = c(0, 1), ylim = c(0.75, 1.10),
     col = "#white", las = 1)
cols <- sample(c("#e5fb2a", "#2588b3aa", "#ff8700aa"), 3)
for (sam in 1:3) {
  newdat <- data.frame(x = x_ins[[sam]], y = y_ins[[sam]])
  reg <- lm(y ~ poly(x, degree = 8), data = newdat)
  lines(x, predict(reg, data.frame(x = x)), 
        col = cols[sam], lwd = 4)
}
cols <- sample(c("#e5fb2a", "#2588b3aa", "#ff8700aa"), 3)
for (sam in 1:3) {
  newdat <- data.frame(x = x_ins[[sam]], y = y_ins[[sam]])
  reg <- lm(y ~ poly(x, degree = 6), data = newdat)
  lines(x, predict(reg, data.frame(x = x)), 
        col = cols[sam], lwd = 4)
}
cols <- sample(c("#e5fb2a", "#2588b3aa", "#ff8700aa"), 3)
for (sam in 1:3) {
  newdat <- data.frame(x = x_ins[[sam]], y = y_ins[[sam]])
  reg <- lm(y ~ poly(x, degree = 3), data = newdat)
  lines(x, predict(reg, data.frame(x = x)), 
        col = cols[sam], lwd = 4)
}
par(op)
dev.off()
