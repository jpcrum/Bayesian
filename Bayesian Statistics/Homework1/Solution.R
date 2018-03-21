# # Setting the working directory to homework fil

setwd("C:/Users/sjcrum/Documents/Bayesian Statistics/Homework1")


 
# # Function to load the data
loadData <- function(file_name, col1, col2){ #defining the parameters
  df <- read.csv(file_name)   #importing the csv file
  df <- df[,c(col1, col2)]    #subsetting the dataframe to included desired columns
  return(df)                  #returning the dataframe
}




# # Function to train the model on the training set

train <- function(y, x){     #defining the parameters
  model <- lm(y ~ x)         #creating a linear model
  coeffs <- model$coefficients    #getting the model coefficients
  return(coeffs)                  #returning the coefficients
}




# # Function to test the model on the training data

test <- function(y, x, b2, b1){   #setting the parameters
  df <- cbind(y,x)                #binding the two columns x and y to create a dataframe
  rss_vector = c()                #setting rss_vector to an empty vector
  for (i in 1:nrow(df)){          #for loop to iterate over all rows of dataframe
    resid = (df[i,1] - b1 - (b2 * df[i,2])) ^ 2  #calucalting squared residual of each data point
    rss_vector[i] = resid         #adding the residual to the end of rss_vector
  }
  rss = sum(rss_vector)           #calculating the sum of the rss_vector to calculate RSS
  print(cat("Residual Sum of Squares: ", rss, "\n"))   #Printing RSS value
  return(rss) #returning rss value
}




# # Function to plot the data and linear model

plotDataModel <- function(y, x, b1, b2, figure){    #set the parameters
  
  plot(x, y, xlab = "temp",         #create a scatter plot for x and y 
             ylab = "cnt",          #and label axes and title
             main = paste(figure)) 
  abline(b2, b1, col = "red")   #add the model line to the plot by the coefficients
  dev.copy(pdf, sprintf("%s.pdf", figure), width=10, height=6) #export graph as a pdf to working directory
  dev.off() #tell R that plotting is finised
}

