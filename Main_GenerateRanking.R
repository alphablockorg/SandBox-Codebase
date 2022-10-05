
library(dplyr)
source("Utils.R")

#choose between S&P100, S&P500 or Crypto10
#!! Only change Product name
workingWith = "Crypto10"


if(workingWith == "S&P100" || workingWith == "S&P500")
{
  dataPointsFor1Quarter = 262/4 #minus weekends
  quarters <- list(1,2,3,4,5,6,8,12,20) 
  if(workingWith == "S&P100")
  {
    nameOfTheFileWithStockData = "S&P100TimeIndex.csv"
  }else{
    nameOfTheFileWithStockData = "S&P500TimeIndex.csv"
  }
}else{
  # dataPointsFor1Quarter should be 92 for Crypto Dataset because they trade every day of the year, even weekends
  dataPointsFor1Quarter = 92
  quarters <- list(1,2,3,4) # only making 4 quarters because there isn't enough trading data 
  nameOfTheFileWithStockData = "Crypto10TimeIndex.csv"
}




dataSet <- read.csv(nameOfTheFileWithStockData, sep = ",", header = TRUE)
if(colnames(dataSet)[1] =="X"){dataSet = dataSet[,-1]}

dataSet <- as.data.frame(dataSet)

for(quarter in quarters)
{
   generateRanking(dataSet, quarter, dataPointsFor1Quarter, fileNameRanking, fileNameReturns, workingWith)
}
#period measured in quarters


