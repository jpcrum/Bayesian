# Driver.R
# This is the Driver file for Homework 1 of course:
# Bayesian Methods for Data Science (DATS 6450 - 11, Spring 2018)
# Data Science @ George Washington University
# Author: Yuxiao Huang

# Students should implement the following four functions in Solution.R:
# loadData: load dataset and return the data of cnt and temp
# train: train the model on the training set hour.csv and return the coefficients, beta_1 and beta_0
# test: test the model on the testing set day.csv and return the Residual Sum of Squares (RSS, defined on page 62 of ISL)
# plotDataModel: plot the data of cnt and temp, the linear model, and save the figures to trainingResultFig.pdf and testingResultFig.pdf
source("Solution.R")

# Training set
trainingSet = "hour.csv"
# Testing set
testingSet = "day.csv"
# Training result figure
trainingResultFig = "trainingResultFig"
# Testing result figure
testingResultFig = "testingResultFig"
# RSS file
rssFile = "rss.csv"

# Load data from trainning set
dataOfCntTemp = loadData(trainingSet, "cnt", "temp")
cnt = dataOfCntTemp[[1]]
temp = dataOfCntTemp[[2]]

# Train the linear model on training data
beta = train(cnt, temp)
beta0 = beta[[1]]
beta1 = beta[[2]]

# Plot the data and linear model
plotDataModel(cnt, temp, beta1, beta0, trainingResultFig)

# Load data from testing set
dataOfCntTemp = loadData(testingSet, "cnt", "temp")
cnt = dataOfCntTemp[[1]]
temp = dataOfCntTemp[[2]]

# Plot the data and linear model
plotDataModel(cnt, temp, beta1, beta0, testingResultFig)

# Test the linear model on testing data
rss = test(cnt, temp, beta1, beta0)

# Write rss file
write.csv(rss, file = rssFile, row.names = FALSE)