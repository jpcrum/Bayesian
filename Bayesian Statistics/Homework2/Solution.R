# Author: Jack Crum
# Date: 2/14/2018
# Assignment: DATS 6450 - 11, Spring 2018, Homework 2 


# # Setting the working directory to homework file

setwd("C:/Users/sjcrum/Documents/Bayesian Statistics/Homework2")


# # Generate Data Function

#takes as input the test results number, probability of positive results, and name of the test results file, and outputs the results file

generateData <- function(testNum, posProb, file){ #define function name and parameters
  set.seed(29)  # set the seed to ensure reproducibility
  binom = c(0,1)  # create vector of possible outcomes (0 = healthy, 1 = sick)
  # generate data vector with length = testNum using random selection from outcomes
  # set probability of 1 to the positive results probability (0.9) 
  # and of 0 to (1 - positive probability), so total probability = 1
  data <- sample(binom, testNum, prob = c((1 - posProb), posProb), replace = TRUE)
  write.csv(data, sprintf("%s", file))  # export the data as a csv file
}


# # Load Data Function

# takes as input the test results file and outputs the results as a dataframe

loadData <- function(file){  # define function and parameters
  df <- read.csv(file)  # read csv file to dataframe variable
  return(df)  # return the dataframe
}


# # Bayesian Inference Function

# takes as input the dataframe, prior, false positive rate, and hit rate, and outputs 
#the vector of posterior probabilities 

bayesianInference <- function(df, prior, fpr, hr){ # define function and parameters 
  posteriorProb = c()  # initiate an empty vector
  for (i in 1:nrow(df)){  # run a for loop over "testNum" loops
    # Bayes Rule for determining posterior probability
    # prior = prior, hr = likelihood, 
    posterior = (prior * hr) / ((prior * hr) + (fpr * (1 - prior))) 
    posteriorProb[i] = posterior  # append the value to the posteriorProb vector
    prior = posterior  # update the next prior as the current posterior for the next loop
  }
  return(posteriorProb)  # return the posterior probability vector
}


# # Plot Posterior Function

# takes as input the vector of posterior probabilities and name of the figure (where x axis
# is the number of data and y axis the corresponding posterior probabilities), and outputs the figure


plotPosteriors <- function(y, fileName){  # define functions and parameters
  plot(y, type = "l", col = "red",  # plot test number vs posterior probability as a line graph
                      lwd = 2,  # line width
                      log = "x",  # make the x-axis logarithmic 
                      xlab = "Test Number",  # x axis label
                      ylab = "Posterior Probability",  # y axis label
                      main = paste(fileName))  # main title 

  dev.copy(pdf, sprintf("%s.pdf", fileName), width=8, height=6)  # export graph as a pdf to working directory
  dev.off()  # tell R that plotting is finised
}