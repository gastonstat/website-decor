# Title: Graphing random character strings
# Description: Script used to get background image for personal website
# Author: Gaston Sanchez
# Date: November 2021

# auxiliary function
validate_steps <- function(x) {
  if (length(x) != 1 || !is.numeric(x) || x %% 1 != 0 || x <= 0) {
    stop("\n'steps' must be a positive integer")
  } else {
    TRUE
  }
}

# random walk in 2-dimensions
random_walk2 <- function(steps = 10, start = c(0, 0)) {
  validate_steps(steps)
  directions <- c('right', 'left', 'up', 'down')
  # higher probabilities for left-right steps
  walk <- sample(directions, steps, replace = TRUE, 
                 prob = c(0.4, 0.4, 0.10, 0.10))
  # auxiliary "lookup" vectors 
  x_lookup <- c('right' = 1, 'left' = -1, 'up' = 0, 'down' = 0)
  y_lookup <- c('right' = 0, 'left' = 0, 'up' = 1, 'down' = -1)
  # x-y coordinates
  x_coords = cumsum(c(start[1], x_lookup[walk]))
  names(x_coords) <- NULL
  y_coords = cumsum(c(start[2], y_lookup[walk]))
  names(y_coords) <- NULL
  
  # output
  final_position <- c(x_coords[steps + 1], y_coords[steps + 1])
  result <- list(
    start = c(0, 0),
    final = final_position,
    #summary = cbind(x = summary(x_coords), y = summary(y_coords)),
    path = cbind(x_coords, y_coords)
  )
  result
}


# set of initial walks
set.seed(456)
n = 500
walk_orange = random_walk2(start = c(0, 0), steps = n)
walk_green = random_walk2(start = c(0, 0), steps = n)
walk_blue = random_walk2(start = c(0, 0), steps = n)

xmin = min(c(walk_orange$path[,1], walk_green$path[,1], walk_blue$path[,1]))
xmax = max(c(walk_orange$path[,1], walk_green$path[,1], walk_blue$path[,1]))

ymin = min(c(walk_orange$path[,2], walk_green$path[,2], walk_blue$path[,2]))
ymax = max(c(walk_orange$path[,2], walk_green$path[,2], walk_blue$path[,2]))


# base colors for color-scheme
bv = runif(100, 0.53, 0.56)
blues = hsv(h = bv, s = 1, v = 1, alpha = 0.9)

gv = runif(100, 0.20, 0.25)
greens = hsv(h = gv, s = 1, v = 1, alpha = 0.9)

ov = runif(100, 0.10, 0.13)
oranges = hsv(h = ov, s = 1, v = 1, alpha = 0.9)


# plot
png(file="full-walks.png", width=1500, height=300)
op = par(bg = "white", mar = rep(0, 4))
plot(0, 0, xlim = c(xmin+2, xmax-2), ylim = c(ymin, ymax),
     type = "n", xlab = "", ylab = "", axes = FALSE)
lines(walk_green$path[ ,1], walk_green$path[ ,2], 
      col = greens[1], lwd = 1.5)
lines(walk_orange$path[ ,1], walk_orange$path[ ,2], 
      col = oranges[1], lwd = 1.3)
lines(walk_blue$path[ ,1], walk_blue$path[ ,2], 
      col = blues[1], lwd = 1.2)

for (k in 1:5) {
  walk_orange = random_walk2(
    start = c(0, 0),
    steps = sample(seq(30, 100, 10), size = 1))
  walk_green = random_walk2(
    start = c(0, 0), 
    steps = sample(seq(30, 100, 10), size = 1))
  walk_blue = random_walk2(
    start = c(0, 0), 
    steps = sample(seq(30, 100, 10), size = 1))

  lines(walk_green$path[ ,1], walk_green$path[ ,2], 
        col = greens[k+1], lwd = 1.5)
  lines(walk_orange$path[ ,1], walk_orange$path[ ,2], 
        col = oranges[k+1], lwd = 1.3)
  lines(walk_blue$path[ ,1], walk_blue$path[ ,2], 
        col = blues[k+1], lwd = 1.2)
  
}
par(op)
dev.off()
