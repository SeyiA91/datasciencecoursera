rm(list = ls())
setwd("~/dev/datasciencecoursera/datasciencecoursera/Course_4/")

##### Base Plotting #####
# examples can be found in 
# base plotting can be done in multiple phases (i.e. plotting then adding 
# additional lines as well as annotation can all be diffrent commands run separately)
# margins and spacing are automatically handled
# panel fxns can be specified/customized to modify whats plotted in each panel

##### Lattice Plotting #####
# lattice is different, it must all be run at once
# xyplot() main fxn for creating scatterplots
# bwplot() box-and-whisker plots
# histogram()
# stripplot() boxplot but with actual points
# dotplot() plot dots on 'violiin strings'
# splom() scatterplot matrix; like pairs() in base plotting
# levelplot(), countorplot() for plotting 'image' data

require(lattice)
require(datasets)

# simple scatterplot
par(mfrow = c(1,1))
# vars before '~' symbolize the y axis
xyplot(Ozone ~ Wind, data = airquality)
# convert 'Month' to factor variable
airquality <- transform(airquality, Month = factor(Month))
# the chart is split on Month which is a factor variable
xyplot(Ozone ~ Wind | Month, data = airquality, layout = c(5,1))
# layout specifies first the number of columns then the number of rows
# in this case: 5 columns and 1 row

# base graphics plot data directly to the screen
# lattice returns and object of class trellis

set.seed(10)
x <- rnorm(100)
f <- rep(0:1, each = 50)
y <- x + f - f * x + rnorm(100, sd = 0.5)
f <- factor(f, labels = c('Group 1', 'Group 2'))
xyplot(y ~ x | f, layout = c(2,1)) # plot with 2 panels

# Custom panel functions (abline)
xyplot(y ~ x| f, panel = function(x,y, ...) {
    panel.xyplot(x, y, ...) # first call the default panel fucntion for xyplot
    panel.abline(h = median(y), lty = 2) # add a horizontal line at the median
})

# Custom panel functions (regression line)
xyplot(y ~ x | f, panel = function(x, y, ...){
    panel.xyplot(x, y, ...) # first call default panel function
    panel.lmline(x, y, col = 2) # overlay a simple linear regression line
})

#### GGPLOT Plotting ####
# qplot() works similarly to base plot()
require(ggplot2)
str(mpg)
qplot(displ, hwy, data = mpg) # plots a scatter plot
qplot(displ, hwy, data = mpg, color = drv) # plots a scatter plot points distingueshed by factor var, drv
qplot(displ, hwy, data = mpg, geom = c('point', 'smooth')) # fit a regression to the plot
qplot(hwy, data = mpg, fill = drv) # plots a histogram (only passed one variable from mpg df) and 
# distinguishes segments of the bars by factor
qplot(displ, hwy, data = mpg, facets = . ~ drv) # separates plots by factor similar to lattice xyplot 
# (i.e expression after '|' symbol )
qplot(hwy, data = mpg, facets = drv ~ ., binwidth = 2) # does same as above but with histograms
# facets are essentially the division of graphs by factor variable

###### necessities of ggplot2 ######
# data frame
# aesthtetic mappings (aes) how the data are mappe to color, size
# geoms: geometric objects like points, lines, shapes
# facets: for conditional plots
# stats: statitstical transformations like binning, quantiles, smoothing
# scales: what scale an aesthetic map uses (e.g. male = red, female = blue)
# coordinate sytem

# using ggplot2 fxn is simiilar to building amodel
# plot the data
# overlay a summary
# metadata and annotation

######## hiearchical clustering ########
# steps:
# find closest two things
# put them together
# find next closest, etc.
# requires:
# defined distance
# merging approach


# creating a random coordinate plot
set.seed(1234)
par(mar=c(0,0,0,0))
x <- rnorm(12, mean = rep(1:3, each = 4), sd = 0.2)
y <- rnorm(12, mean = rep(c(1,2,1), each = 4), sd = 0.2)
plot(x,y,col = 'blue', pch = 19, cex = 2)
text(x+0.05,y+0.05, labels = as.character(1:12))

dataFrame <- data.frame(x = x, y = y)
dist(dataFrame) # defaults to euclidean distance metric

# hierarchical clustering 1
suppressMessages(library(fields))
dataFrame <- data.frame(x=x, y=y)
rdistxy <- rdist(dataFrame)
diag(rdistxy) <- diag(rdistxy) + 1e5

# find index of points with min distance
ind <- which(rdistxy == min(rdistxy), arr.ind = T)
par(mfrow = c(1,2), mar = rep(0.2, 4))
# plot points with minimum overlayed
plot(x,y, col='blue', pch=19,cex=2)
text(x+0.05,y+0.05,labels = as.character(1:12))
points(x[ind[1,]],y[ind[1,]],col='orange',pch=19,cex=2)

# make a cluster and cut it at the right height
distxy <- dist(dataFrame)
hcluster <- hclust(distxy)
dendro <- as.dendrogram(hcluster)
cutDendro <- cut(dendro, h=(hcluster$height[1] + 0.00001))
plot(cutDendro$lower[[11]], yaxt='n')

# hiearchical clustering 2
library(fields)
dataFrame <- data.frame(x=x,y=y)
rdistxy <- rdist(dataFrame)
diag(rdistxy) <- diag(rdistxy) + 1e5

# Find the index of the points with minimum distance
ind <- which(rdistxy == min(rdistxy),arr.ind=TRUE)
par(mar=rep(0.2,4))
# Plot the points with the minimum overlayed
plot(x,y,col="blue",pch=19,cex=2)
text(x+0.05,y+0.05,labels=as.character(1:12))
points(x[ind[1,]],y[ind[1,]],col="orange",pch=19,cex=2)
points(mean(x[ind[1,]]),mean(y[ind[1,]]),col="black",cex=3,lwd=3,pch=3)
points(mean(x[ind[1,]]),mean(y[ind[1,]]),col="orange",cex=5,lwd=3,pch=1)

# hierarchical clustering 3
library(fields)
dataFrame <- data.frame(x=x,y=y)
rdistxy <- rdist(dataFrame)
diag(rdistxy) <- diag(rdistxy) + 1e5

# Find the index of the points with minimum distance
ind <- which(rdistxy == rdistxy[order(rdistxy)][3],arr.ind=TRUE)
par(mfrow=c(1,3),mar=rep(0.2,4))
# Plot the points with the minimum overlayed
plot(x,y,col="blue",pch=19,cex=2)
text(x+0.05,y+0.05,labels=as.character(1:12))
points(x[c(5,6)],y[c(5,6)],col="orange",pch=19,cex=2)
points(x[ind[1,]],y[ind[1,]],col="red",pch=19,cex=2)

# Make dendogram plots
distxy <- dist(dataFrame)
hcluster <- hclust(distxy)
dendro <- as.dendrogram(hcluster)
cutDendro <- cut(dendro,h=(hcluster$height[2]) )
plot(cutDendro$lower[[10]],yaxt="n")
plot(cutDendro$lower[[5]],yaxt="n")

dataFrame <- data.frame(x=x,y=y)
distxy <- dist(dataFrame)
hClustering <- hclust(distxy)
par(mfrow=c(1,1))
plot(hClustering)

# euclidean is straight line distance btwn two points
# manhattan distance is similar to the idea of the grid in which the distance btwn two 
# points is not possible via a straight line like euclidean

myplclust <- function(hclust, lab = hclust$labels, lab.col = rep(1, length(hclust$labels)),
                      hang = 0.1, ...) {
    y <- rep(hclust$height, 2)
    x <- as.numeric(hclust$merge)
    y <- y[which(x < 0)]
    x <- x[which(x < 0)]
    x <- abs(x)
    y <- y[order(x)]
    x <- x[order(x)]
    plot(hclust, labels = F, hang = hang, ...)
    text(x = x, y = y[hclust$order] - (max(hclust$height) * hang), labels = lab[hclust$order],
         col = lab.col[hclust$order], srt = 90, adj = c(1, 0.5), xpd = NA, ...)
}

dataFrame <- data.frame(x = x, y = y)
distxy <- dist(dataFrame)
hClustering <- hclust(distxy)
myplclust(hClustering, lab = rep(1:3, each = 4), lab.col = rep(1:3, each = 4))

# hierchical clustering is used for mostly exploratory efforts


