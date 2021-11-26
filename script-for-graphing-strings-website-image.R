# Title: Graphing random character strings
# Description: Script used to get background image for personal website
# Author: Gaston Sanchez
# Date: November 2021

# population size, number of yarns, and generations
size <- 100
n <- 15
gens <- 50

# letters and symbols
punc <- c(".", ",", ":", ";", "/", "|", "(", "\\)", "[", "]", "{", 
          "\\}", "\\#", "$", "%", "^", "&", "+", "-", "?", "<", ">", "*")
strs <- c(letters, LETTERS, 0:9, punc)


# base colors for color-scheme
set.seed(333)

bv = runif(100, 0.53, 0.56)
blues = hsv(h = bv, s = 1, v = 0.8, alpha = runif(100, 0.6, 0.9))

gv = runif(100, 0.20, 0.25)
greens = hsv(h = gv, s = 1, v = 1, alpha = runif(100, 0.6, 0.9))

ov = runif(100, 0.08, 0.13)
oranges = hsv(h = ov, s = 1, v = 0.95, alpha = runif(100, 0.6, 0.9))


# graphical parameters
png("full-strings.png", width = 1500, height = 300, pointsize = 20)
set.seed(12345)
op <- par(bty = "n", mar = c(0, 0, 0, 0), bg = 'white')
# plot
plot(1:gens, rep(0, gens), type = 'n', 
     ylim = c(0.10, 1), 
     xlim = c(2, gens-1), axes = FALSE)
for (j in 1:n) {
  p <- 0.5 # probability
  freq <- c(rep(p, 10), rep(NA, gens - 10))
  for (i in 11:gens) {
    aux <- rbinom(1, size, p) # binomial number
    p <- aux / size
    freq[i] <- p
  }
  cexs <- sample(seq(0.05, 0.5, by = 0.05), 1) * 5
  temp <- lowess(freq ~ 1:gens)
  cols = sample(c(oranges, greens, blues), size = length(temp$x), replace=TRUE)
  aux_strs <- sample(strs, size = length(temp$x), replace = TRUE)
  text(temp$x, temp$y, labels = aux_strs, 
       family = "mono",
       col = cols,
       cex = cexs)
}
# reset par
par(op)
dev.off()
