
library(dplyr)
source("Utils.R")
dataPointsFor1Quarter = 262/4 #minus weekends
quarters <- list(1,2,3,4,6,8,12,20) 
productName="S&P100" # or S&P500
nameOfTheFileWithStockData = "S&P100TimeIndex.csv"


dataSet <- read.csv(nameOfTheFileWithStockData, sep = ",", header = TRUE)
if(colnames(dataSet)[1] =="X"){dataSet = dataSet[,-1]}

dataSet <- as.data.frame(dataSet)

for(quarter in quarters)
{
   generateRanking(dataSet, quarter, dataPointsFor1Quarter, fileNameRanking, fileNameReturns, productName)
}
#period measured in quarters


