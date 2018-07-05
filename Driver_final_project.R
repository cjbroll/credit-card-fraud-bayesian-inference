library(randomForest)
results = data.frame(read.csv(file="creditcardcsvpresent.csv"))
model = randomForest(isFradulent~.,data=results)
importance(model)
varImpPlot(model)

# Drop zeroes
nonzero <- results[results$Transaction_amount > 0, ]
# Sort ascending
sorted_nonzero <- nonzero[order(nonzero$Transaction_amount),]
dim(sorted_nonzero)
# Split into three equal-sized groups by Transaction_amount
# Transactions where $0 < amount <= $3,974.215
theta_1 <- sorted_nonzero[1:994,]$isFradulent #Last Transaction_amount= $3,974.215
# Transactions where $3,974.215 < amount <= $11,700.54
theta_2 <- sorted_nonzero[995:1988,]$isFradulent #Last Transaction_amount= $11,700.54
# Transactions where $11,700.54 < amount <= $80,000
theta_3 <- sorted_nonzero[1989:2982,]$isFradulent #Last Transaction_amount= $80,000.00

# Convert factors to binary values (1 = Fraudulent, 0 = not fraudulent)
theta_1_numeric <- as.numeric(theta_1) - 1
theta_2_numeric <- as.numeric(theta_2) - 1
theta_3_numeric <- as.numeric(theta_3) - 1

myData <- cbind(theta_1_numeric, theta_2_numeric, theta_3_numeric)
myData <- as.data.frame(myData)

# START MCMC
# glucose_Driver.R
# Author: Yuxiao Huang
# Reference:
# The code is from the book by Professor John K. Kruschke,
# with some trivial changes by Yuxiao Huang
# Please find the reference to and website of the book below:
# Kruschke, J. K. (2014). Doing Bayesian Data Analysis: A Tutorial with R, JAGS, and Stan. 2nd Edition. Academic Press / Elsevier
# https://sites.google.com/site/doingbayesiandataanalysis/


graphics.off() # This closes all of R's graphics windows.
source("Solution_final_project.R")

fileNameRoot = "project_" 
graphFileType = "pdf" 
#------------------------------------------------------------------------------- 
# Generate the MCMC chain:
mcmcCoda = genMCMC( data=myData , numSavedSteps=50000 , saveName=fileNameRoot )
#------------------------------------------------------------------------------- 
# Display diagnostics of chain, for specified parameters:
parameterNames = varnames(mcmcCoda) # get all parameter names
for ( parName in parameterNames ) {
  diagMCMC( codaObject=mcmcCoda , parName=parName , 
            saveName=fileNameRoot , saveType=graphFileType )
}
#------------------------------------------------------------------------------- 
# Get summary statistics of chain:
summaryInfo = smryMCMC( mcmcCoda , compVal=NULL , #rope=c(0.45,0.55) ,
                        compValDiff=0.0 , #ropeDiff = c(-0.05,0.05) ,
                        saveName=fileNameRoot )
# Display posterior information:
plotMCMC( mcmcCoda , data=myData , compVal=NULL , #rope=c(0.45,0.55) ,
          compValDiff=0.0 , #ropeDiff = c(-0.05,0.05) ,
          saveName=fileNameRoot , saveType=graphFileType )
#------------------------------------------------------------------------------- 



