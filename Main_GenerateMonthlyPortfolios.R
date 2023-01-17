# use this code to generate Portfolios that run for 1,3,6,9,12,18,24,36,48,60 months
#  this code will not generate MPT stats

###
#
# Input : Stocks Time Index data csv  
#         Benchmark Time Index data csv   
#         Ranking file
#         Type of the Portfolio to generate : Value, Core, Growth, VG, All
#
# Output: multiple csv files containing
#                    Portfolio Components data
#                    Portfolio Daily Return
#                   
###

source("Functions.R")
source("Utils.R")
source("Summary Code MPT Stats Functions.R")
source("Drawdown Function.R")


months <- c(1,3,6,9,12,18,24,36,48,60)

#!!! choose between S&P100, S&P500 or Crypto10
#!! Only change Product name in workingWith
workingWith = "Crypto10"

#!!! choose between Value, Core, Growth, VG, ALL
# VG means Value and Growth stocks , ALL means all stocks
type="ALL"  #!!!! In Case of Crypto10 - run only ALL

# measures are in quarters
rankingToUse = c(1,2,3,4,5,6,8,12,20)

#---------------------------------- PARAMETERS SETUP -------------------------------------------------------
#the name of the csv file containing the Stocks Time Index data
symbolsTimeIndexfileName = paste0(workingWith,"TimeIndex.csv") 

#the name of the csv file containing the Benchmark Time Index data
benchmarkTimeIndexFileName = paste0(workingWith,"_Benchmark_Index.csv")

#for naming purpose only , keep it suggestive to benchmark name
benchmark = paste0(workingWith,"_Index")

if(workingWith == "S&P100")
{
  
  #must be 100 for S&P 100 and 500 for S&P 500
  #this will influence the starting dates of the Portfolios, the P will only start when the input dataset with EOD data has values for all 100 symbols
  minNumberOfSymbols = 100
  
  
}else if(workingWith == "S&P500"){
  
  minNumberOfSymbols = 500
  
}else{
  
  type = "ALL" 
  minNumberOfSymbols = 10
  # measures are in quarters
  rankingToUse = c(1,2,3,4)
}

#---------------------------------- Generate Portfolios and Stats -------------------------------------------------------

for(i in rankingToUse)
{
  
  rankingFilesName = getRankingFileName(paste0("Q",i), workingWith)
  rankNameForOutputFileNotations = getRankingOutputFile(paste0("Q",i))
  
  #folders must exist
  folder = paste0("Q",i)
  folderMain = paste0(workingWith,"_",type)
  
  if(workingWith != "Crypto10"){
    GenerateMonthlyPortfolios(symbolsTimeIndexfileName, benchmarkTimeIndexFileName, benchmark,minNumberOfSymbols,
                            rankingFilesName, rankNameForOutputFileNotations,
                            folder, folderMain, type ,  workingWith,"Month") 
  }else{
    GenerateMonthlyPortfolios(symbolsTimeIndexfileName, benchmarkTimeIndexFileName, benchmark,minNumberOfSymbols,
                            rankingFilesName, rankNameForOutputFileNotations,
                            folder, folderMain, type, workingWith, "Day") 
  }
}
