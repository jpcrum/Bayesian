# Author: Jack Crum
# Date: 2/28/2018
# Assignment: DATS 6450 - 11, Spring 2018, Homework 2 


# # Setting the working directory to homework file

setwd("C:/Users/sjcrum/Documents/Bayesian Statistics/Homework3")

source('DBDA2E-utilities.R')

# # Load Data Function

# takes as input the test results file and outputs the results as a dataframe

loadData <- function(file){  # define function and parameters
  df <- read.csv(file)  # read csv file to dataframe variable
  df <- as.data.frame(df)
  return(df)  # return the dataframe
}


# # Formal Analysis Function

# takes as input the data, the parameters of the prior, the bins, and the name 
# of the figure, and derives the posterior using formal analysis, and plots the 
# density of the posterior distribution

formalAnalysis <- function(z, N, a, b, thetas, figName){

  # compute the posterior at each value of theta
  # because of conjugate prior of beta and bernoulli, posterior is beta distribution
  pThetaGivenData = dbeta(thetas, a + z, b + N - z)
  
  openGraph(width=10,height=7) # open a graph
  
  # plot the beta distribution
  plot(thetas, pThetaGivenData, type = "l", 
                                lwd = 3, 
                                xlim = c(0, 1), 
                                cex.axis = 1.2, 
                                xlab = expression(theta), 
                                ylab = expression(paste("p(", theta, "|D)")), 
                                cex.lab = 1.5, 
                                main = "Formal Analysis", 
                                cex.main = 1.5, 
                                col = "red")

  
  saveGraph(file = sprintf("%s", figName) , type="pdf") # save the graph
  
}


# # Grid Approximation Function

# takes as input the data, the parameters of the prior, the bins, and the name 
# of the figure, and estimates the posterior using grid approximation, and plots 
# the histogram of the posterior distribution

gridApproximation <- function(z, N, a, b, thetas, figName){
  posteriors = c()  # create empty posteriors vector
  for (i in 1:length(thetas)) {   # set up for loop to test each theta in grid
    
    # return 0 if theta is outside [0,1]
    if (thetas[i] < 0 | thetas[i] > 1) {
      return(0)
    } else { 
      
      prior = thetas[i] ^ (a-1) * (1-thetas[i]) ^ (b-1) # calculate beta prior
      likelihood = thetas[i]^z * (1-thetas[i])^(N-z) # calculate bernoulli likelihood
      pD = beta(a, b) # calculate beta constant of prior
      pZN = beta(z + a, N - z + b) / beta(a, b)  # calculate beta constant of posterior
      
      posterior = (likelihood * (prior / pD)) / pZN  # calculate posterior
      posteriors[i] = posterior # amend each posterior to posteriors vector
    }
  }
  
  openGraph(width=10,height=7) # open graph
  
  # plot graph
  plot(thetas, posteriors, 
                type="h",
                col = "red",
                lwd=3, 
                cex.axis=1.5, 
                xlab = expression(theta), 
                ylab = expression(paste("p(", theta, "|D)")),
                cex.lab=1.5, 
                main="Grid Approximation", 
                cex.main=1.5 )
  
  saveGraph(file = sprintf("%s", figName) , type="pdf") # save graph
}




# # MCMC Function

mcmc = function(z, N, a, b, trajLength, proposalSD, figName){
  
  myData = testResults[,2] # get the test results values
  
  # Define the prior density function
  prior = function( theta , N , z ) {
    pDataGivenTheta = theta^z * (1-theta)^(N-z)
    # The theta values passed into this function are generated at random,
    # and therefore might be inadvertently greater than 1 or less than 0.
    # The likelihood for theta > 1 or for theta < 0 is zero:
    pDataGivenTheta[ theta > 1 | theta < 0 ] = 0
    return( pDataGivenTheta )
  }
  
  # Define the likehood density function. 
  likelihood = function( theta , a , b ) {
    pTheta = theta ^ (a-1) * (1-theta) ^ (b-1)
    return( pTheta )
  }
  
  # Define the posterior density function
  targetRelProb = function(theta) {
    targetRelProb =  prior(theta , N, z) * likelihood(theta, a, b)
    return( targetRelProb )
  }
  
  # Initialize the vector that will store the results:
  trajectory = rep( 0 , trajLength )  
  
  # Specify where to start the trajectory:
  trajectory[1] = 0.01 # arbitrary value
  # Specify the burn-in period:
  burnIn = ceiling( 0.0 * trajLength ) # arbitrary number, less than trajLength
  # Initialize accepted, rejected counters, just to monitor performance:
  nAccepted = 0
  nRejected = 0
  
  
  # Now generate the random walk. The 't' index is time or trial in the walk.
  # Specify seed to reproduce same random walk:
  set.seed(47405)
  # Specify standard deviation of proposal distribution:
  for ( t in 1:(trajLength-1) ) {
    currentPosition = trajectory[t]
    # Use the proposal distribution to generate a proposed jump.
    proposedJump = rnorm( 1 , mean=0 , sd=proposalSD )
    # Compute the probability of accepting the proposed jump.
    probAccept = min( 1,
                      targetRelProb( currentPosition + proposedJump )
                      / targetRelProb( currentPosition ) )
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
  #Generates letter part of the trajectory
  
  # End of Metropolis algorithm.
  # Display the chain.
  
  # Open graph and set parameters
  openGraph(width=6,height=10)
  layout( matrix(1:3,nrow=3) )
  par(mar=c(3,4,2,1),mgp=c(2,0.7,0))
  
  # Posterior histogram:
  paramInfo = plotPost( acceptedTraj , xlim=c(0,1) , xlab=bquote(theta) , 
                        cex.main=2.0 ,
                        main=bquote( list( "Prpsl.SD" == .(proposalSD) ,
                                           "Eff.Sz." == .(round(effectiveSize(acceptedTraj),1)) ) ) )
  
  # Trajectory, a.k.a. trace plot, end of chain:
  idxToPlot = 1:trajLength
  plot( trajectory[idxToPlot] , idxToPlot , main="End of Chain" ,
        xlab=bquote(theta) , xlim=c(0,1) , ylab="Step in Chain" ,
        type="o" , pch=20 , col="skyblue" , cex.lab=1.5 )
  # Display proposal SD and acceptance ratio in the plot.
  text( 0.0 , trajLength , adj=c(0.0,1.1) , cex=1.75 ,
        labels = bquote( frac(N[acc],N[pro]) == 
                           .(signif( nAccepted/length(acceptedTraj) , 3 ))))
  
  saveGraph(file = sprintf("%s", figName) , type="pdf") # save the graph

}


