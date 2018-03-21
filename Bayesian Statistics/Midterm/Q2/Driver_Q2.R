# Driver_Q2.R
# This is the Driver file for Midterm of course:
# Bayesian Methods for Data Science (DATS 6450 - 11, Spring 2018)
# Data Science @ George Washington University
# Author: Yuxiao Huang

# Students should implement he following three functions in Solution_Q2.R:
# loadData: takes as input the stock returns of B and outputs the stock returns as a dataframe
# bayesianInference: takes as input the dataframe and outputs the vector of posterior probabilities 
# plotPosteriors: takes as input the vector of posterior probabilities and name of the figure (where $x$ axis is the number of data and $y$ axis the corresponding posterior probabilities), and outputs the figure 

source("Solution_Q2.R")

# Stock returns file
stockReturnsFile = "stockReturns.csv"
# Stock returns number
stockReturnsNum = 100
# The probability
stockReturnsUpProb = 0.6
# Posterior probabilitites figure
posteriorsFig = "posteriorsFig"

# Takes as input the Stock returns number, probability of the returns going up, and name of the returns file, and outputs the returns file
# generateData(stockReturnsNum, stockReturnsUpProb, stockReturnsFile)

# Takes as input the Stock returns file and outputs the results as a dataframe
stockReturns = loadData(stockReturnsFile)

# Takes as input the dataframe and outputs the vector of posterior probabilities 
posteriors = bayesianInference(stockReturns)

# Takes as input the vector of posterior probabilities and name of the figure (where x axis is the number of data and y axis the corresponding posterior probabilities), and outputs the figure
plotPosteriors(posteriors, posteriorsFig)