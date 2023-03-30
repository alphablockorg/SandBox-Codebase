
library(dplyr)
source("Utils.R")

#choose between S&P100, S&P500 or Crypto10
#!! Only change Product name
workingWith = "Crypto30"


if(workingWith == "S&P100" || workingWith == "S&P500" || workingWith == "TSX60" || workingWith == "BSE100" || workingWith == "Indonesia30")
{
  dataPointsFor1Quarter = 262/4 #minus weekends
  quarters <-list(1,2,3,4,5,6,7,8,12,20) 
  
}else{
  # dataPointsFor1Quarter should be 92 for Crypto Dataset because they trade every day of the year, even weekends
  dataPointsFor1Quarter = 92
  quarters <- list(1,2,3,4) # only making 4 quarters because there isn't enough trading data 
  
}




dataSet <- read.csv(paste0(workingWith, "TimeIndex.csv") , sep = ",", header = TRUE)
if(colnames(dataSet)[1] =="X"){dataSet = dataSet[,-1]}

dataSet <- as.data.frame(dataSet)

for(quarter in quarters)
{
   generateRanking(dataSet, quarter, dataPointsFor1Quarter, fileNameRanking, fileNameReturns, workingWith)
}
#period measured in quarters


