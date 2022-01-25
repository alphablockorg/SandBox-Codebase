###
#  Generating Portfolios with symbols selected from the Ranking files and run MPT stats code on them.
#
# Input : Stocks Time Index data csv  
#         Benchmark Time Index data csv   
#         Ranking file
#         Type of the Portfolio to generate : Value, Core, Growth, VG, All
#
# Output: multiple csv files containing
#                    Portfolio Components data
#                    Portfolio Daily Return
#                    Summary files
###

source("Functions.R")
source("Utils.R")
source("Summary Code MPT Stats Functions.R")
source("Drawdown Function.R")

workingWith = "S&P100" # or S&P500

#the name of the csv file containing the Stocks Time Index data
#"S&P100TimeIndex.csv" or "S&P500TimeIndex.csv"
symbolsTimeIndexfileName = "S&P100TimeIndex.csv"

#the name of the csv file containing the Benchmark Time Index data
# "S&P100_Benchmark_Index.csv" or "S&P500Benchmark.csv"
benchmarkTimeIndexFileName = "S&P100_Benchmark_Index.csv"

#for naming purpose only , keep it suggestive to benchmark name
benchmark = "S&P100_Index"

#must be 100 for S&P 100 and 500 for S&P 500
#this will influence the starting dates of the Portfolios, the P will only start when the input dataset with EOD data has values for all 100 symbols
minNumberOfSymbols = 100

#choose between 1,2,3,4,5,6,8,12,20
# where 1 means 1 quarter ranking
rankingToUse = c(1,2,3,4,5,6,8,12,20)

#choose between Value, Core, Growth, VG, ALL
# VG means Value and Growth stocks , ALL means all stocks
type="Growth"
for(i in rankingToUse)
{
  
  rankingFilesName = getRankingFileName(paste0("Q",i), workingWith)
  rankNameForOutputFileNotations = getRankingOutputFile(paste0("Q",i))
  
  #folders must exist
  folder = paste0("Q",i)
  folderMain = paste0(workingWith,"_",type)
  
  
  GeneratePortfolioAndMPT(symbolsTimeIndexfileName, benchmarkTimeIndexFileName, benchmark,minNumberOfSymbols,
                          rankingFilesName, rankNameForOutputFileNotations,
                          folder, folderMain, type) 
}
