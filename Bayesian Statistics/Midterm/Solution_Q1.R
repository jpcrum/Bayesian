#Author: Jack Crum
#Date: 2/21/2018


setwd("C:/Users/sjcrum/Documents/Bayesian Statistics/Midterm")



# # Load Data Function

# takes as input the test results file and outputs the results as a dataframe

loadData <- function(file){  # define function and parameters
  df <- read.csv(file)  # read csv file to dataframe variable
  df <- df[,2]
  df <- as.data.frame(df)
  return(df)  # return the dataframe
}

# Takes as input the dataframe and prior and outputs the vector of posterior probabilities 
bayesianInference = function(stocks) {
  
  posteriorProb = c() # Create an empty list

  prior = 0.5
  
  for (i in 1:nrow(stocks)) {
    # Get the stock
    if (stocks == "1") {
      
    # Get the numerator and denominator in Bayes' rule
      numerator = stockReturnsUpProb * prior
      denominator = stockReturnsUpProb * prior + (1 - stockReturnsUpProb) * (1 - prior)

    } else {
      # Get the numerator and denominator in Bayes' rule
      numerator = (1 - stockReturnsUpProb) * prior
      denominator = (1 - stockReturnsUpProb) * prior + (stockReturnsUpProb) * (1 - prior)
    }
    
    # Apply Bayes' rule
    posterior = numerator / denominator
    posteriorProb[i] = posterior
    # The posterior for test result yi is the prior for result yi+1
    prior = posterior 
  }
  
  return(posteriorProb)
}

# # Plot Posterior Function

# takes as input the vector of posterior probabilities and name of the figure (where x axis
# is the number of data and y axis the corresponding posterior probabilities), and outputs the figure


plotPosteriors <- function(y, fileName){  # define functions and parameters
  plot(y, type = "l", col = "red",  # plot test number vs posterior probability as a line graph
       lwd = 2,  # line width
       log = "x",  # make the x-axis logarithmic 
       xlab = "Stock Return Number",  # x axis label
       ylab = "Posterior Probability",  # y axis label
       main = paste(fileName))  # main title 
  
  dev.copy(pdf, sprintf("%s.pdf", fileName), width=8, height=6)  # export graph as a pdf to working directory
  dev.off()  # tell R that plotting is finised
}
