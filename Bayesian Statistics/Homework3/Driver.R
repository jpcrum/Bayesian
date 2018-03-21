# Driver.R
# This is the Driver file for Homework 3 (Question 1) of course:
# Bayesian Methods for Data Science (DATS 6450 - 11, Spring 2018)
# Data Science @ George Washington University
# Author: Yuxiao Huang

# Students should implement the following four functions in Solution.R:
# loadData: takes as input the test results file and outputs the results as a dataframe. You may use your implementation for this function in previous homework, with appropriate citation
# formalAnalysis: takes as input the data, the parameters of the prior, the bins, and the name of the figure, and derives the posterior using formal analysis, and plots the density of the posterior distribution
# gridApproximation: takes as input the data, the parameters of the prior, the bins, and the name of the figure, and estimates the posterior using grid approximation, and plots the histogram of the posterior distribution
# mcmc: takes as input the data, the parameters of the prior, the bins, and the name of the figure, and estimates the posterior using mcmc, and plots the histogram of the posterior distribution and trajectory of the chain

source("Solution.R")

# Test results file
testResultsFile = "testResults.csv"
# Histogram figure of formal analysis
formalAnalysisFig = "formalAnalysisFig"
# Histogram figure of grid approximation
gridApproximationFig = "gridApproximationFig"
# Histogram and trajectory figure of mcmc
mcmcFig = "mcmcFig"

# Takes as input the test results file and outputs the results as a dataframe
testResults = loadData(testResultsFile)

# Get N
N = nrow(testResults) 
# Get z
z = sum(testResults[, 2]) 

# Specify the parameters of beta
a = 1
b = 1

# Specify the discretized theta
thetas = seq(from = 0, to = 1, by = 0.01)

formalAnalysis(z, N, a, b, thetas, formalAnalysisFig)

gridApproximation(z, N, a, b, thetas, gridApproximationFig)

# Specify the length of the trajectory, i.e., the number of jumps to try
trajLength = 50000
# Specify standard deviation of proposal distribution
proposalSD = 0.2

mcmc(z, N, a, b, trajLength, proposalSD, mcmcFig)