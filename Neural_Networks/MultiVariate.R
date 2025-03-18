##################################################
###-----1. Applications and visualization------###
##################################################

library(lattice)
library(MASS)
if(!require(heplots)) install.packages("heplots"); library(heplots)
if(!require(MVA)) install.packages("MVA"); library(MVA)
if(!require(KernSmooth)) install.packages("KernSmooth"); library(KernSmooth)
if(!require(scatterplot3d)) install.packages("scatterplot3d"); library(scatterplot3d)
if(!require(aplpack)) install.packages("aplpack"); library(aplpack)
if(!require(freqparcoord)) install.packages("freqparcoord"); library(freqparcoord)

#---1.1 Data set "measure" from Everitt/Hothorn---
measure <-
  structure(list(V1 = 1:20,
                 V2 = c(34L, 37L, 38L, 36L, 38L, 43L, 40L, 38L, 40L, 41L, 
                        36L, 36L, 34L, 33L, 36L, 37L, 34L, 36L, 38L, 35L), 
                 V3 = c(30L, 32L, 30L, 33L, 29L, 32L, 33L, 30L, 30L, 32L,
                        24L, 25L, 24L, 22L, 26L, 26L, 25L, 26L, 28L, 23L), 
                 V4 = c(32L, 37L, 36L, 39L, 33L, 38L, 42L, 40L, 37L, 39L, 
                        35L, 37L, 37L, 34L,38L, 37L, 38L, 37L, 40L, 35L)), 
            .Names = c("V1", "V2", "V3", "V4"), 
            class = "data.frame", 
            row.names = c(NA, -20L))
measure <- measure[, -1]
names(measure) <- c("chest", "waist", "hips")
measure$gender <- gl(2, 10)
levels(measure$gender) <- c("male", "female")
measure

# covariance matrix (= variance-covariance matrix):
cov(measure[, c("chest", "waist", "hips")]) # only makes sense for metric variables
cov(subset(measure, gender == "female")[ , c("chest", "waist", "hips")]) # subset for females
cov(subset(measure, gender == "male")[, c("chest", "waist", "hips")]) # subset for males

# correlation matrix:
cor(measure[, c("chest", "waist", "hips")])

# distance matrix:
dist(scale(measure[, c("chest", "waist", "hips")], center = FALSE))

x
x <- scale(measure[, c("chest", "waist", "hips")], center = FALSE)[1:2, ]
x[1, ] - x[2, ]
sum((x[1, ] - x[2, ])^2)
sqrt(sum((x[1, ] - x[2, ])^2))

# generalized distance (deviation of each observation from the mean vector of 
# the data set)
x <- measure[, c("chest", "waist", "hips")]
cm <- colMeans(x)
S <- cov(x)

d <- apply(x, MARGIN = 1, function(x) 
  t(x - cm) %*% solve(S) %*% (x - cm)) # solve(S) calculates the inverse of S
d

# QQ plots:
qqnorm(measure[, "chest"], main = "chest"); qqline(measure[, "chest"])
qqnorm(measure[, "waist"], main = "waist"); qqline(measure[, "waist"])
qqnorm(measure[, "hips"], main = "hips"); qqline(measure[, "hips"])

# Chi-squared plot the generalized distances:
plot(qchisq((1:nrow(x) - 1/2) / nrow(x), df = 3), sort(d),
     xlab = expression(paste(chi[3]^2, " Quantile")), 
     ylab = "Ordered distances")
abline(a = 0, b = 1)



#---1.2 Data set "USAirpollution" from Everitt/Hothorn---

# NV plots:
data("USairpollution", package = "HSAUR2")
layout(matrix(1:8, nc = 2))
sapply(colnames(USairpollution), function(x) {
  qqnorm(USairpollution[[x]], main = x)
  qqline(USairpollution[[x]])
})

x <- USairpollution
cm <- colMeans(x)
S <- cov(x)
d <- apply(x, 1, function(x) t(x - cm) %*% solve(S) %*% (x - cm))
plot(qc <- qchisq((1:nrow(x) - 1/2) / nrow(x), df = 7), # Caution: df = 7, not df = 6 as in the book!
     sd <- sort(d),
     xlab = expression(paste(chi[7]^2, " Quantile")), 
     ylab = "Ordered distances", xlim = range(qc) * c(1, 1.1))
oups <- which(rank(abs(qc - sd), ties = "random") > nrow(x) - 3)
text(qc[oups], sd[oups] - 1.5, names(oups))
abline(a = 0, b = 1)
par(mfrow = c(1, 1))

# Scatterplots:
mlab <- "Manufacturing enterprises with 20 or more workers"
plab <- "Population size (1970 census) in thousands"

plot(popul ~ manu, data = USairpollution, 
     xlab = mlab, ylab = plab)
rug(USairpollution$manu, side = 1)
rug(USairpollution$popul, side = 2)

# Scatterplot incl. histogramm and Boxplot for the univariate distributions:
layout(matrix(c(2, 0, 1, 3), nrow = 2, byrow = TRUE),
       widths = c(2, 1), heights = c(1, 2), respect = TRUE)
xlim <- with(USairpollution, range(manu)) * 1.1
plot(popul ~ manu, data = USairpollution, cex.lab = 0.9,
     xlab = mlab, ylab = plab, type = "n", xlim = xlim)
with(USairpollution, text(manu, popul, cex = 0.6,
                          labels = abbreviate(row.names(USairpollution))))
with(USairpollution, hist(manu, main = "", xlim = xlim))
with(USairpollution, boxplot(popul))
par(mfrow = c(1, 1))

# scatterplot including bivariate boxplot to identify outliers:
outcity <- match(lab <- c("Chicago", "Detroit", 
                          "Cleveland", "Philadelphia"), rownames(USairpollution))
x <- USairpollution[, c("manu", "popul")]
bvbox(x, mtitle = "", xlab = mlab, ylab = plab)
text(x$manu[outcity], x$popul[outcity], labels = lab,
     cex = 0.7, pos = c(2, 2, 4, 2, 2))

with(USairpollution, cor(manu, popul))
outcity <- match(c("Chicago", "Detroit", 
                   "Cleveland", "Philadelphia"),
                 rownames(USairpollution))
with(USairpollution, cor(manu[-outcity], popul[-outcity]))

# Alternative to the bivariate boxplot: convex hull (for bivariate data):
par(mfrow = c(1, 1))
(hull <- with(USairpollution, chull(manu, popul)))
with(USairpollution, 
     plot(manu, popul, pch = 1, xlab = mlab, ylab = plab))
with(USairpollution, 
     polygon(manu[hull], popul[hull], density = 15, angle = 30))

# Effect of removing all points lying on the convex hull:
with(USairpollution, cor(manu[-hull], popul[-hull]))


# Chi-Plot: Check dependence/independence of variables; Independence is given
# if the points in the chi-plot aren't scattered around 0 in a random manner and
# are mainly not located in the area bounded by the horizonal dashed lines.
with(USairpollution, plot(manu, popul, 
                          xlab = mlab, ylab = plab, 
                          cex.lab = 0.9))
with(USairpollution, chiplot(manu, popul))


# Bubble und Glyph-Plots: Additional variables can be taken into account in the 
# two-dimensional representation by additional graphical element:
ylim <- with(USairpollution, range(wind)) * c(0.95, 1)
plot(wind ~ temp, data = USairpollution, 
     xlab = "Average annual temperature (Fahrenheit)",
     ylab = "Average annual wind speed (m.p.h.)", pch = 10,
     ylim = ylim)
with(USairpollution, symbols(temp, wind, circles = SO2, 
                             inches = 0.5, add = TRUE))


plot(wind ~ temp, data = USairpollution,
     xlab = "Average annual temperature (Fahrenheit)",
     ylab = "Average annual wind speed (m.p.h.)", pch = 10,
     ylim = ylim)
with(USairpollution,
     stars(USairpollution[,-c(2, 5)], locations = cbind(temp, wind),
           labels = NULL, add = TRUE, cex = 0.5))

stars(USairpollution, cex = 0.55)
faces(USairpollution, cex = 0.55)

# Bivariate kernel density estimates und 3D-Plots:
CYGOB1d <- bkde2D(CYGOB1, bandwidth = sapply(CYGOB1, dpik))
plot(CYGOB1, xlab = "log surface temperature",
     ylab = "log light intensity")
contour(x = CYGOB1d$x1, y = CYGOB1d$x2, 
        z = CYGOB1d$fhat, add = TRUE)

persp(x = CYGOB1d$x1, y = CYGOB1d$x2, z = CYGOB1d$fhat,
      xlab = "log surface temperature",
      ylab = "log light intensity",
      zlab = "density")

panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "grey", ...)
}

pairs(measure[, c("chest", "waist", "hips")],
      diag.panel = panel.hist,
      panel = function (x,y) {
        data <- data.frame(cbind(x,y))
        par(new = TRUE)
        den <- bkde2D(data, bandwidth = sapply(data, dpik))
        contour(x = den$x1, y = den$x2, 
                z = den$fhat, axes = FALSE)
      })

# 3D scatterplots:
with(measure, scatterplot3d(chest, waist, hips,
                            pch = (1:2)[gender], type = "h", angle = 55))

with(USairpollution, 
     scatterplot3d(temp, wind, SO2, type = "h",
                   angle = 55))

# Trellis-Plots/Multiple conditioning: Relationship between two variables,
# conditioned on a third

# Dichotomization of "wind":
plot(xyplot(SO2 ~ temp | cut(wind, 2), data = USairpollution))

# four overlapping ranges for "SO2" (shingles):
pollution <- with(USairpollution, equal.count(SO2, 4))
plot(cloud(precip ~ temp * wind | pollution, panel.aspect = 0.9,
           data = USairpollution))

plot(xyplot(lat ~ long| cut(depth, 3), data = quakes, 
            layout = c(3, 1), xlab = "Longitude", 
            ylab = "Latitude"))

# Four variables: overlapping intervals for "Magnitude",
# shadings for depth:
Magnitude <- with(quakes, equal.count(mag, 4))
depth.ord <- with(quakes, rev(order(depth)))
quakes.ordered <- quakes[depth.ord,]
depth.breaks <- with(quakes.ordered, do.breaks(range(depth),50))
quakes.ordered$color<-level.colors(quakes.ordered$depth,at = depth.breaks,
                                   col.regions = grey.colors)
plot(xyplot(lat ~ long | Magnitude, data = quakes.ordered,
            aspect = "iso", groups = color, cex = 2, col = "black",
            panel = function(x, y, groups, ..., subscripts) {
              fill <- groups[subscripts]
              panel.grid(h = -1, v = -1)
              panel.xyplot(x, y, pch = 21, fill = fill, ...)
            },
            legend =
              list(right =
                     list(fun = draw.colorkey,
                          args = list(key = list(col = gray.colors,
                                                 at = depth.breaks),
                                      draw = FALSE))),
            xlab = "Longitude", ylab = "Latitude"))

# 3D scatterplot conditioned on the magnitude of the quake:
plot(cloud(depth ~ lat * long | Magnitude, data = quakes,
           zlim = rev(range(quakes$depth)),
           screen = list(z = 105, x = -70), panel.aspect = 0.9,
           xlab = "Longitude", ylab = "Latitude", zlab = "Depth"))

# Stalaktite plot: Based on generalized distances; useful in identifying
# multivariate outliers zu identifizieren:
# On the y-axis, we show the number of observations that are used to estimate
# the means and covariances, and for each observation a "*" indicates that it
# is an outlier. In the example given below, in the beginning (for small n)
# almost every observation is considered and outlier, while when using all 
# observations, only Chicago, Providence, and Phoenix remain.
stalac(USairpollution)



#########################################
###-----2. Parallel coordinates ------###
#########################################

parcoord(mtcars[, 1:4], var.label = TRUE) # MASS package
parallelplot(mtcars[, 1:4]) # lattice package

# high positive correlation - "parallel" lines (high-high, medium-medium, low-low):
cor(mtcars[c("disp", "cyl")])
parcoord(mtcars[c("disp", "cyl")], var.label = TRUE)
# high negative correlation - crossing lines:
cor(mtcars[c("mpg", "cyl")])
parcoord(mtcars[c("mpg", "cyl")], var.label = TRUE)
# no correlation - no visible pattern (in parts parallel, in parts crossing):
cor(mtcars[c("qsec", "drat")])
parcoord(mtcars[c("qsec", "drat")], var.label = TRUE)

# five lines with the highest multivariate density:
freqparcoord(mtcars, m = 5, dispcols = 1:4, k = 7)
# five outliers:
freqparcoord(mtcars[, 1:4], -5, k = 7, keepidxs = 1)
p <- freqparcoord(mtcars[, 1:4], -1 , k = 7, keepidxs = 1)
p$idxs
mtcars[p$idxs, ]