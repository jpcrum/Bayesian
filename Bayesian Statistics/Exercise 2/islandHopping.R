# islandHopping.R
# Author: Yuxiao Huang
# Reference:
# The code from the book by Professor John K. Kruschke,
# with some trivial changes by Yuxiao Huang
# Please find the reference to and website of the book below:
# Kruschke, J. K. (2014). Doing Bayesian Data Analysis: A Tutorial with R, JAGS, and Stan. 2nd Edition. Academic Press / Elsevier
# https://sites.google.com/site/doingbayesiandataanalysis/

setwd("C:/Users/sjcrum/Documents/Bayesian Statistics/Exercise 2")

graphics.off()
rm(list=ls(all=TRUE))
fileNameRoot="BernMetrop" # for output filenames
source("DBDA2E-utilities.R")

# Specify the prior
pTheta = 1 : 7 / 28

# Define the relative probability of the target distribution, 
# as a function of vector theta. For our application, this
# target distribution is the prior distribution.

targetRelProb = function(theta) {
  if (theta < 1 | theta > 7) {
    return(0)
  }
  return(pTheta[theta])
}


# Specify the length of the trajectory, i.e., the number of jumps to try:
trajLength = 10000 # arbitrary large number, a small number (i.e., 1000) will triger error in plotPost function

# Initialize the vector that will store the results:
# Creates a vector with all zeros and length 10000
trajectory = rep( 0 , trajLength )

# Specify where to start the trajectory:
trajectory[1] = 4 # the value in the textbook

# Specify the burn-in period:
burnIn = ceiling( 0.01 * trajLength ) # arbitrary number, less than trajLength

# Initialize accepted, rejected counters, just to monitor performance:
nAccepted = 0
nRejected = 0

# Now generate the random walk. The 't' index is time or trial in the walk.
# Specify seed to reproduce same random walk:
set.seed(0)

# Metropolis algorithm
for ( t in 1:(trajLength-1) ) {
  currentPosition = trajectory[t]
  
  # Use the proposal distribution to generate a proposed jump.
  if (runif(1) < 0.5) {  # coin flip
    proposedJump = - 1
  } else {
    proposedJump = 1
  }
  
  # Compute the probability of accepting the proposed jump.
  probAccept = min(1, targetRelProb(currentPosition + proposedJump) 
                   / targetRelProb(currentPosition))
  
  # Generate a random uniform value from the interval [0,1] to
  # decide whether or not to accept the proposed jump.
  if ( runif(1) < probAccept ) {
    # accept the proposed jump
    trajectory[ t+1 ] = currentPosition + proposedJump
    # increment the accepted counter, just to monitor performance
    if ( t > burnIn ) { nAccepted = nAccepted + 1 }
  } else {
    # reject the proposed jump, stay at current position
    trajectory[ t+1 ] = currentPosition
    # increment the rejected counter, just to monitor performance
    if ( t > burnIn ) { nRejected = nRejected + 1 }
  }
}


# Extract the post-burnIn portion of the trajectory.
acceptedTraj = trajectory[ (burnIn+1) : length(trajectory) ]

# Display the chain.
openGraph(width=4,height=8)
layout( matrix(1:4,nrow=4) )
par(mar=c(3,4,2,3),mgp=c(2,0.7,0))

# Prior histogram
plot( 1 : 7 , pTheta , main="Prior distribution" , ylab=expression("P(" * theta * ")"),
      xlab=bquote(theta) , xlim=c(1, 7) , ylim = c(0, 0.3) ,
      type="h" , col="skyblue" , cex.lab=1.5 , lwd = 10)

# Posterior histogram:
paramInfo = plotPost( acceptedTraj , xlim=c(1, 7) , xlab=bquote(theta) ,
                      cex.main=1.5 ,
                      main=bquote(
                        "Eff.Sz." == .(round(effectiveSize(acceptedTraj),1)) ) )

# Trajectory, a.k.a. trace plot, end of chain:
idxToPlot = (trajLength-200):trajLength
plot( trajectory[idxToPlot] , idxToPlot , main="End of Chain" ,
      xlab=bquote(theta) , xlim=c(1, 7) , ylab="Step in Chain" ,
      type="o" , pch=20 , col="skyblue" , cex.lab=1.5 )
# Display proposal SD and acceptance ratio in the plot.
text( 0.8 , trajLength , adj=c(0.0,1.1) , cex=1 ,
      labels = bquote( frac(N[acc],N[pro]) == 
                         .(signif( nAccepted/length(acceptedTraj) , 3 ))))

# Trajectory, a.k.a. trace plot, beginning of chain:
idxToPlot = 1:200
plot( trajectory[idxToPlot] , idxToPlot , main="Beginning of Chain" ,
      xlab=bquote(theta) , xlim=c(1, 7) , ylab="Step in Chain" ,
      type="o" , pch=20 , col="skyblue" , cex.lab=1.5 )
# Indicate burn in limit (might not be visible if not in range):
if ( burnIn > 0 ) {
  abline(h=burnIn,lty="dotted")
  text( 1.0 , burnIn+1 , "Burn In" , adj=c(0.1,1.1), cex=1 )
}

saveGraph(file = "mcmcFig", type = "pdf")