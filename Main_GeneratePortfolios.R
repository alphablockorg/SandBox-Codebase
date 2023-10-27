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

source("Functions2.R")
source("Functions.R")
source("Utils.R")
source("Summary Code MPT Stats Functions.R")
source("Drawdown Function.R")
source("Summary Code For Continuous Portfolios.R")


#!!! choose between S&P100, S&P500 or Crypto10
#!! Only change Product name in workingWith
workingWith = "S&P500"

#!!! choose between Value, Core, Growth, VG, ALL
# VG means Value and Growth stocks , ALL means all stocks
type="OnePortfolio"  #!!!! In Case of Crypto10 - run only ALL

# measures are in quarters
rankingToUse = c(1,2,3,4,5,6,7,8,12,20)
generatePortfoliosWithRebalance = TRUE

#---------------------------------- PARAMETERS SETUP -------------------------------------------------------
if(workingWith == "S&P100")
{
    #the name of the csv file containing the Stocks Time Index data
    symbolsTimeIndexfileName = "S&P100TimeIndex.csv"
    
    #the name of the csv file containing the Benchmark Time Index data
    benchmarkTimeIndexFileName = "S&P100_Benchmark_Index.csv"
    
    #for naming purpose only , keep it suggestive to benchmark name
    benchmark = "S&P100_Index"
    
    #must be 100 for S&P 100 and 500 for S&P 500
    #this will influence the starting dates of the Portfolios, the P will only start when the input dataset with EOD data has values for all 100 symbols
    minNumberOfSymbols = 100


}else if(workingWith == "S&P500"){
  #the name of the csv file containing the Stocks Time Index data
  symbolsTimeIndexfileName = "S&P500TimeIndex.csv"
  
  #the name of the csv file containing the Benchmark Time Index data
  benchmarkTimeIndexFileName = "S&P500_Benchmark_Index.csv"
  
  #for naming purpose only , keep it suggestive to benchmark name
  benchmark = "S&P500_Index"
  
  minNumberOfSymbols = 500

  }else if(workingWith == "Crypto10"){
    #the name of the csv file containing the Stocks Time Index data
    symbolsTimeIndexfileName = "Crypto10TimeIndex.csv"
    
    #the name of the csv file containing the Benchmark Time Index data
    benchmarkTimeIndexFileName = "Crypto10_Benchmark_Index.csv"
    
    #for naming purpose only , keep it suggestive to benchmark name
    benchmark = "Crypto10_Index"
    
    #if(type == "ALL"){minNumberOfSymbols = 10}else{ minNumberOfSymbols = 9}
    
    type = "ALL" 
    minNumberOfSymbols = 10
    # measures are in quarters
    rankingToUse = c(1,2,3,4)
  }else if(workingWith == "India29") {
  #the name of the csv file containing the Stocks Time Index data
  symbolsTimeIndexfileName = "India29TimeIndex.csv"
  #the name of the csv file containing the Benchmark Time Index data
  benchmarkTimeIndexFileName = "India29_Benchmark_Index.csv"
  #for naming purpose only , keep it suggestive to benchmark name
  benchmark = "SOLMID.BO"
  #this will influence the starting dates of the Portfolios, the P will only start when the input dataset with EOD data has values for all 100 symbols
  minNumberOfSymbols = 28
  
  }else if(workingWith == "Crypto30"){
    #the name of the csv file containing the Stocks Time Index data
    symbolsTimeIndexfileName = "Crypto30TimeIndex.csv"
    
    #the name of the csv file containing the Benchmark Time Index data
    benchmarkTimeIndexFileName = "Crypto30_Benchmark_Index.csv"
    
    #for naming purpose only , keep it suggestive to benchmark name
    benchmark = "Crypto30_Index"
    
    #if(type == "ALL"){minNumberOfSymbols = 10}else{ minNumberOfSymbols = 9}
    
    if(type =="ALL"){
      minNumberOfSymbols = 25
    } else
    {
      minNumberOfSymbols = 10
    }
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
  
  if(type=="OnePortfolio")
  {
    fromDate <- c(as.character(as.Date( "2020-01-06")),as.character(as.Date( "2021-01-04")),as.character(as.Date( "2022-01-03")),as.character(as.Date( "2023-01-03")))
    toDate <- as.character(as.Date( "2023-10-24"))
    core_bottom = c(0,0,0)
    core_top = c(99,98,95)
    value_weight = c(0,0,0,0,0,0,0)
    growth_weight = c(50,90,80,70,30,20,10)
    for(d in fromDate)
    {
        GeneratePortfolioAndMPT_V2(symbolsTimeIndexfileName, benchmarkTimeIndexFileName, benchmark,
                                               rankingFilesName, rankNameForOutputFileNotations,
                                               folder, folderMain, d, toDate, core_bottom, core_top, value_weight,growth_weight, generatePortfoliosWithRebalance) 
    }
  }
  else if(workingWith != "Crypto10" && workingWith != "Crypto30"){
    GeneratePortfolioAndMPT(symbolsTimeIndexfileName, benchmarkTimeIndexFileName, benchmark,minNumberOfSymbols,
                            rankingFilesName, rankNameForOutputFileNotations,
                            folder, folderMain, type ,workingWith, "Month") 
  }else{
    GeneratePortfolioAndMPT(symbolsTimeIndexfileName, benchmarkTimeIndexFileName, benchmark,minNumberOfSymbols,
                          rankingFilesName, rankNameForOutputFileNotations,
                          folder, folderMain, type, workingWith, "Day") 
  }
}
