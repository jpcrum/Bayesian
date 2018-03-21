# Probability.R
# Author: Yuxiao Huang
# Reference:
# The code is from the book by Professor John K. Kruschke,
# with some trivial changes by Yuxiao Huang
# Please find the reference to and website of the book below:
# Kruschke, J. K. (2014). Doing Bayesian Data Analysis: A Tutorial with R, JAGS, and Stan. 2nd Edition. Academic Press / Elsevier
# https://sites.google.com/site/doingbayesiandataanalysis/

####################################################################
# Code example:
show( HairEyeColor ) # Show data 

EyeHairFreq = apply( HairEyeColor, c("Eye" , "Hair"), sum ) # Sum across sex 
show( EyeHairFreq )

EyeHairProp = EyeHairFreq / sum( EyeHairFreq ) # Calculate p(Eye, Hair), Table 4.1 (page 27 of the slides)
show( EyeHairProp )
show( round( EyeHairProp , 2 ) ) # Round to the second decimal place

####################################################################
# Complete the following code:
HairFreq =  apply(HairEyeColor, c("Hair"), sum)
  show( HairFreq )

HairProp =  HairFreq / sum(HairFreq) 
  show( round( HairProp , 2 ) )

EyeFreq =  apply(HairEyeColor, c("Eye"), sum)
  show( EyeFreq )  

EyeProp =  EyeFreq / sum(EyeFreq)
  show( round( EyeProp , 2 ) )
  
HairCondEyeBlueProp = EyeHairProp[2,] / sum(EyeHairProp[2,])
  show( round( HairCondEyeBlueProp , 2 ) )

HairCondEyeProp =  # Calculate p(Hair | Eye)
  show( round( HairCondEyeProp , 2 ) )