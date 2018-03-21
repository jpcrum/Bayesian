# IntegralOfDensityNew.R
# Author: Yuxiao Huang
# Reference:
# The code is from the book by Professor John K. Kruschke,
# with some trivial changes by Yuxiao Huang
# Please find the reference to and website of the book below:
# Kruschke, J. K. (2014). Doing Bayesian Data Analysis: A Tutorial with R, JAGS, and Stan. 2nd Edition. Academic Press / Elsevier
# https://sites.google.com/site/doingbayesiandataanalysis/

setwd("C:/Users/sjcrum/Documents/Bayesian Statistics/Exercise 1")

source("DBDA2E-utilities.R")
# Graph of probability density function, with comb of intervals.
xlow  =  0# Specify low end of x-axis.
xhigh =  1# Specify high end of x-axis.
dx =  0.1 # Specify interval width on x-axis
# Specify comb of points along the x axis:
x = seq(from = xlow, to = xhigh, by = dx)
# Compute y values, i.e., probability density at each value of x:
y = 6*x*(1-x)
  # Plot the function. "plot" draws the intervals. "lines" draws the bell curve.
  openGraph(width=7,height=5)
plot( x , y , type="o" ,lwd=1 , cex.axis=1.5
      , xlab="x" , ylab="p(x)" , cex.lab=1.5
      , main="Probability Density of 6x(1 - x)" , cex.main=1.5 )

lines(x, y, lwd=3 ,  col="skyblue")
# Approximate the integral as the sum of width * height for each interval.
area = apply(x, sum)
  # Display info in the graph.
  text( 0.2 , .9*max(y) , bquote( paste(Delta , "x = " ,.(dx)) )
        , adj=c(1,.5) , cex=1.5 )
text( 0.2 , .75*max(y) ,
      bquote(
        paste( sum(,x,) , " " , Delta , "x p(x) = " , .(signif(area,3)) )
      ) , adj=c(1,.5) , cex=1.5 )
# Save the plot to an PDF file.
saveGraph( file = "IntegralOfDensity" , type="pdf" )