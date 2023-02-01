###
#  Generating Portfolios with symbols selected from the Ranking files and run MPT stats code on them.
#
# Input : Stocks Time Index data csv  
#         Benchmark Time Index data csv   
#         Ranking file
#         Type of the Portfolio to generate : Value, Core, Growth, VG, All
#
# Output: multiple csv files containing
#                    Index Components data
#                    Index Daily Return
#                    Summary files
###

source("Functions.R")
source("Utils.R")
source("Summary Code For Continuous Portfolios.R")
source("Drawdown Function.R")

#!!! choose between S&P100, S&P500, Crypto10 , TSX60" , "BSE100", "Indonesia30
#!! Only change Product name in workingWith and startDate
workingWith = "S&P100"
startDate = "2012-01-02"


#---------------------------------- PARAMETERS SETUP -------------------------------------------------------
type="Index" 

# measures are in quarters
rankingToUse = 7

valueWeight = 40
growthWeight = 40
startPortfoliosEvery = "Month"

#the name of the csv file containing the Stocks Time Index data
symbolsTimeIndexfileName = paste0(workingWith,"TimeIndex.csv") 

#the name of the csv file containing the Benchmark Time Index data
benchmarkTimeIndexFileName = paste0(workingWith,"_Benchmark_Index.csv")

#for naming purpose only , keep it suggestive to benchmark name
benchmark = paste0(workingWith,"_Index")
if(workingWith == "Crypto10"){

  minNumberOfSymbols = 10
  # measures are in quarters
  rankingToUse = 3
  
  startPortfoliosEvery = "Day"
}
#---------------------------------- Generate Rebalancing Portfolios and Stats -------------------------------------------------------


  
  rankingFilesName = getRankingFileName(paste0("Q",rankingToUse), workingWith)
  rankNameForOutputFileNotations = getRankingOutputFile(paste0("Q",rankingToUse))
  
  #folders must exist
  folder = paste0("Q",rankingToUse)
  folderMain = paste0(workingWith,"_",type)
  
  
  GenerateRebalancingPortfolioAndMPT(symbolsTimeIndexfileName, benchmarkTimeIndexFileName, benchmark,minNumberOfSymbols,
                            rankingFilesName, rankNameForOutputFileNotations,
                            folder, folderMain, workingWith, startPortfoliosEvery,  valueWeight, growthWeight, startDate) 

