# Driver.R
# This is the Driver file for Homework 2 of course:
# Bayesian Methods for Data Science (DATS 6450 - 11, Spring 2018)
# Data Science @ George Washington University
# Author: Yuxiao Huang

# Students should implement the following four functions in Solution.R:
# generateData: takes as input the test results number, probability of positive results, and name of the test results file, and outputs the results file
# loadData: takes as input the test results file and outputs the results as a dataframe
# bayesianInference: takes as input the dataframe, prior, false positive rate, and hit rate, and outputs the vector of posterior probabilities 
# plotProsterior: takes as input the vector of posterior probabilities and name of the figure (where x axis is the number of data and y axis the corresponding posterior probabilities), and outputs the figure

source("Solution.R")

# Test results file
testResultsFile = "testResults.csv"
# Test results number
testRusultsNum = 100
# Positive test results probability
PosTestResultsProb = 0.9
# Posterior probabilitites figure
posteriorsFig = "posteriorsFig"

# Probabilities
prior = 0.001 # Prior probabilities
fpr = 0.05 # False positive rate
hr = 0.99 # Hit rate

# Takes as input the test results number, probability of positive results, and name of the test results file, and outputs the results file
generateData(testRusultsNum, PosTestResultsProb, testResultsFile)

# Takes as input the test results file and outputs the results as a dataframe
testResults = loadData(testResultsFile)

# Takes as input the dataframe, prior, false positive rate, and hit rate, and outputs the vector of posterior probabilities 
posteriors = bayesianInference(testResults, prior, fpr, hr)

# Takes as input the vector of posterior probabilities and name of the figure (where x axis is the number of data and y axis the corresponding posterior probabilities), and outputs the figure
plotPosteriors(posteriors, posteriorsFig)