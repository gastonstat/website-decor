# Title: Graphing random tiles
# Description: Script used to get background image for personal website
# Author: Gaston Sanchez
# Date: April 2023


# base colors for color-scheme
set.seed(15)

bv = runif(100, 0.53, 0.56)
blues = hsv(h = bv, s = 1, v = 1, alpha = runif(100, 0.8, 1))

gv = runif(100, 0.20, 0.25)
greens = hsv(h = gv, s = 1, v = 1, alpha = runif(100, 0.8, 1))

ov = runif(50, 0.10, 0.15)
oranges = hsv(h = ov, s = 1, v = 1, alpha = runif(50, 0.8, 1))

whites = hsv(h = 0, s = 0, v = 1, alpha = runif(100, 0.3, 0.7))


# grid-structure for tiles (based on dims width=1500, height=300)
# (coordinates for rectangles: 15 for width, 3 for height)
x_step_size = 0.25
xlefts = seq(from = 0, to = 14.75, by = x_step_size)
xrights = seq(from = 0.25, to = 15, by = x_step_size)

y_step_size = 0.25
ybottoms = rep(0, length(xlefts))
ytops = rep(y_step_size, length(xrights))

y_steps = seq(from = 0, to = 1.75, by = y_step_size)

# colors (better in a matrix for plotting convenience)
num_row = length(y_steps)
num_col = length(xlefts)

cols_vec = sample(c(oranges, greens, blues, whites), 
                  size = num_row * num_col, replace = TRUE)

cols = matrix(cols_vec, num_row, num_col)


png(file="full-tiles.png", width=1500, height=200)
op = par(bg = "white", mar = rep(0,4))
plot.new()
plot.window(xlim = c(0.5, 14.5), ylim = c(0, 2))
for (i in seq_along(y_steps)) {
  rect(xlefts, ybottoms + y_steps[i], 
       xrights, ytops + y_steps[i], 
       col = cols[i,],
       border = "white")
}
abline(v = xlefts, col = "white", lwd = 4)
abline(h = y_steps, col = "white", lwd = 4)
par(op)
dev.off()
