# Driver.R
# This is the Driver file for Exercise 4 of course:
# Bayesian Methods for Data Science (DATS 6450 - 11, Spring 2018)
# Data Science @ George Washington University
# Author: Yuxiao Huang

# Reference:
# Some of the code is from the book by Professor John K. Kruschke
# Please find the reference to and website of the book below:
# Kruschke, J. K. (2014). Doing Bayesian Data Analysis: A Tutorial with R, JAGS, and Stan. 2nd Edition. Academic Press / Elsevier
# https://sites.google.com/site/doingbayesiandataanalysis/

# Students should implement the following function in Solution.R:
# genMCMC: generates the MCMC chain

#------------------------------------------------------------------------------- 
# Optional generic preliminaries:
graphics.off() # This closes all of R's graphics windows.
rm(list=ls())  # Careful! This clears all of R's memory!

#------------------------------------------------------------------------------- 
# Read the data 
myData = read.csv("glucoseData(1).csv")

#------------------------------------------------------------------------------- 
# Load the relevant model into R's working memory:
source("Solution.R")

#------------------------------------------------------------------------------- 
# Optional: Specify filename root and graphical format for saving output.
# Otherwise specify as NULL or leave saveName and saveType arguments 
# out of function calls.
fileNameRoot = "glucose_" 
graphFileType = "pdf"

#------------------------------------------------------------------------------- 
# Generate the MCMC chain:
mcmcCoda = genMCMC( data=myData , numSavedSteps=11000 , saveName=fileNameRoot ,
                    thinSteps=20 )

#------------------------------------------------------------------------------- 
# Display diagnostics of chain, for specified parameters:
for ( parName in c( "thetaA","thetaB","thetaC","omega") ) {
  diagMCMC( codaObject=mcmcCoda , parName=parName ,
            saveName=fileNameRoot , saveType=graphFileType )
}

#------------------------------------------------------------------------------- 
# Display posterior information:
plotMCMC( mcmcCoda , data=myData , 
          compVal=NULL ,
          diffSList=list( c("thetaA","thetaB") ,
                          c("thetaA","thetaC") , 
                          c("thetaB","thetaC") ) ,
          compValDiff=0.0,
          saveName=fileNameRoot , saveType=graphFileType )
#------------------------------------------------------------------------------- 