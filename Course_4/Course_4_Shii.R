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