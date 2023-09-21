source("Utils.R")
library("dplyr")
#---------------------- Generate Portfolios and Summary Tables --------------
GeneratePortfolioAndMPT <- function(symbolsTimeIndexfileName, benchmarkTimeIndexFileName, benchmark,minNumberOfSymbols,
                                    rankingFilesName, rankNameForOutputFileNotations,
                                    folder, folderMain, type, workingWith, startPortfoliosEvery) 
{
  
  folder <- paste0(folderMain,folder,"/")
  #reading benchmark data form file
  benchmarkDataset <-  data.frame(read.csv(file = benchmarkTimeIndexFileName, sep=",",header=TRUE,stringsAsFactors=F, fileEncoding="utf-8", check.names=FALSE))
  if(dim(benchmarkDataset)[2] == 3) {benchmarkDataset = benchmarkDataset[,c(2,3)]}
  benchmarkDataset$Index <- as.Date(benchmarkDataset$Index)
  
  # for each Ranking file given as input the code will create lenght(formDate)*3 Portfolios (value/core/growth)
  # and will generate MPT stats for each
  
  
    createIfFolderDontExists(folder)
    #reading ranking data from ranking file
    rankingDataset <- data.frame(read.csv(file =  rankingFilesName, sep=",",header=TRUE,stringsAsFactors=F, fileEncoding="utf-8", check.names=FALSE))
    
    #finding the first row in the ranking file where {minNumberOfSymbols} have values
    date = firstRowWithEnoughSymbols( rankingDataset, minNumberOfSymbols)
    
    # each Portfolio will be run for 1,2 and 3 years
    for(run_for_years in seq(1,3,1))
    {
      final_summary <- as.data.frame(NA)
      average_Summary <- as.data.frame(NA)
      secventa <- seq(1, run_for_years,1) #used at Summary table
      print(paste(as.Date(date), "   " , as.Date(rankingDataset[dim(rankingDataset)[1] - 250*run_for_years,1])))
      #generates a vector of starting dates and End dates for Portfolio generation
     if( add.month(as.Date(date),1) < as.Date(rankingDataset[dim(rankingDataset)[1] -250*run_for_years,1]))
       {
         if(startPortfoliosEvery == "Month")
         { 
            #this will generate Portfolios starting every month
            fromDate <-  seq(as.Date(date),as.Date(rankingDataset[dim(rankingDataset)[1] -250*run_for_years,1]), by = "month")
            toDate <- add.year(add.day(fromDate,1), run_for_years)
         }else{
           #this will generate Portfolios starting every day
           fromDate <-  seq(as.Date(date),as.Date(rankingDataset[dim(rankingDataset)[1] -250*run_for_years,1]), by = "day")
           toDate <- add.year(add.day(fromDate,1), run_for_years)
         }  
       
          fromDate <- as.character(fromDate)
          toDate <- as.character(toDate)
          
          #is there enough data to biuld a Portfolio
          enoughData <- which(toDate <= as.Date(rankingDataset[dim(rankingDataset)[1],1]))
          if(length(enoughData) >0 ) {
            fromDate <- fromDate[enoughData]
            toDate <- toDate[enoughData]
            
            #for eaach Starting date in the vector willgenerate a Portfolio and Summary table
            for(i  in seq(1,length(fromDate),1) )
            {
              
              #portfolio generation
              print(paste0("Generating Portfolio with strarting date: ",fromDate[i]))
              
              #reading the ranking data on the day of the begining of the Portfolio
              rankingData <- as.data.frame(rankingDataset[rankingDataset$Date == fromDate[i],])
              #in case that on the exact day we selected as the begining of the Portoflio, there is no ranking data, we add days until we find a valid date with enough data in the ranking file
              while(dim(rankingData)[1]<1)
              {
                print(paste0("Trying to find a valid ranking for ",fromDate[i] ))
                #maybe weekend or holiday
                fromDate[i] <- as.character( as.Date(add.day(fromDate[i], 1),format='%Y-%m-%d'))
                rankingData <- as.data.frame(rankingDataset[rankingDataset$Date == fromDate[i],])
                # toDate[i] <- as.character( as.Date(add.day(toDate[i], 1),format='%Y-%m-%d'))
              }
              
              benchmarkName = paste0(fromDate[i],"_to_", toDate[i],"_",benchmark)
              benchmarkDataset_subset <-  benchmarkDataset[benchmarkDataset$Index >= fromDate[i] & benchmarkDataset$Index <= toDate[i],]
              benchmarkDataset_subset$Index <- as.Date(benchmarkDataset_subset$Index)
              #write.csv(benchmarkDataset_subset, paste0(benchmarkName,".csv"))
              #View(benchmarkDataset_subset)
              
              #divide into Value/Core/Growth
              
              #divide ranking file symbols into bins
              {
                rankingData<-rankingData[-1]
                graterThan80 <- which(rankingData>= 80)
                growth_symbols <- colnames(rankingData)[graterThan80]
                
                between_20_80 <- which(rankingData< 80 & rankingData>20)
                core_symbols <- colnames(rankingData)[between_20_80]
                
                lessThan_20 <- which(rankingData<=20)
                value_symbols <- colnames(rankingData)[lessThan_20]
                
                if( toupper(type) == "GROWTH"){symbols = growth_symbols}
                if( toupper(type) == "VALUE"){symbols = value_symbols}
                if( toupper(type) == "CORE"){symbols = core_symbols}
                #symbols <- symbols[ -which(symbols == "FB")]
                if( toupper(type) == "VG"){symbols = c(value_symbols, growth_symbols )}
                
                
              }
              
              portfolioName = paste0(fromDate[i],"_to_", toDate[i],"_",type,"Portfolio_", rankNameForOutputFileNotations)
              print(paste0("Generating ", type, "Portfolio for ", portfolioName))
             
              
              #------------------ generate Portfolios -------------------------------------------------
              
             if(toupper(type) == "ALL")
               {
                  value_weight = 40
                  growth_weight = 40
                  if(workingWith == "Crypto10")
                  {
                    value_weight = 20
                    growth_weight = 20
                  }
             
                   portfolioValues <- GeneratePortfolioUnEqualWeighted_LoadFromFile_WithInputSymbols(fromDate[i], toDate[i], 
                                                                                                  value_symbols, 
                                                                                                  value_weight,
                                                                                                  core_symbols, 
                                                                                                  growth_symbols,
                                                                                                  growth_weight,
                                                                                                  symbolsTimeIndexfileName, 
                                                                                                  paste0(folder,portfolioName))
             }else if(toupper(type) == "ALL1")
             {
               value_weight = 10
               growth_weight = 80
               portfolioValues <- GeneratePortfolioUnEqualWeighted_LoadFromFile_WithInputSymbols(fromDate[i], toDate[i], 
                                                                                                 value_symbols, 
                                                                                                 value_weight,
                                                                                                 core_symbols, 
                                                                                                 growth_symbols,
                                                                                                 growth_weight,
                                                                                                 symbolsTimeIndexfileName, 
                                                                                                 paste0(folder,portfolioName))
             }else if(toupper(type) == "ALL2")
             {
               value_weight = 20
               growth_weight = 70
               portfolioValues <- GeneratePortfolioUnEqualWeighted_LoadFromFile_WithInputSymbols(fromDate[i], toDate[i], 
                                                                                                 value_symbols, 
                                                                                                 value_weight,
                                                                                                 core_symbols, 
                                                                                                 growth_symbols,
                                                                                                 growth_weight,
                                                                                                 symbolsTimeIndexfileName, 
                                                                                                 paste0(folder,portfolioName))
             }else if(toupper(type) == "ALL3")
             {
               value_weight = 30
               growth_weight = 60
               portfolioValues <- GeneratePortfolioUnEqualWeighted_LoadFromFile_WithInputSymbols(fromDate[i], toDate[i], 
                                                                                                 value_symbols, 
                                                                                                 value_weight,
                                                                                                 core_symbols, 
                                                                                                 growth_symbols,
                                                                                                 growth_weight,
                                                                                                 symbolsTimeIndexfileName,
                                                                                                 paste0(folder,portfolioName))
               
             }else if(toupper(type) == "ALL4")
             {
               value_weight = 40
               growth_weight = 50
               portfolioValues <- GeneratePortfolioUnEqualWeighted_LoadFromFile_WithInputSymbols(fromDate[i], toDate[i], 
                                                                                                 value_symbols, 
                                                                                                 value_weight,
                                                                                                 core_symbols, 
                                                                                                 growth_symbols,
                                                                                                 growth_weight,
                                                                                                 symbolsTimeIndexfileName, 
                                                                                                 paste0(folder,portfolioName))
               
             }else if(toupper(type) == "ALL5")
             {
               value_weight = 50
               growth_weight = 40
               portfolioValues <- GeneratePortfolioUnEqualWeighted_LoadFromFile_WithInputSymbols(fromDate[i], toDate[i], 
                                                                                                 value_symbols, 
                                                                                                 value_weight,
                                                                                                 core_symbols, 
                                                                                                 growth_symbols,
                                                                                                 growth_weight,
                                                                                                 symbolsTimeIndexfileName, 
                                                                                                 paste0(folder,portfolioName))
               
             }else if(toupper(type) == "ALL6")
             {
               value_weight = 60
               growth_weight = 30
               portfolioValues <- GeneratePortfolioUnEqualWeighted_LoadFromFile_WithInputSymbols(fromDate[i], toDate[i], 
                                                                                                 value_symbols, 
                                                                                                 value_weight,
                                                                                                 core_symbols, 
                                                                                                 growth_symbols,
                                                                                                 growth_weight,
                                                                                                 symbolsTimeIndexfileName, 
                                                                                                 paste0(folder,portfolioName))
             }else if(toupper(type) == "ALL7")
             {
               value_weight = 70
               growth_weight = 20
               portfolioValues <- GeneratePortfolioUnEqualWeighted_LoadFromFile_WithInputSymbols(fromDate[i], toDate[i], 
                                                                                                 value_symbols, 
                                                                                                 value_weight,
                                                                                                 core_symbols, 
                                                                                                 growth_symbols,
                                                                                                 growth_weight,
                                                                                                 symbolsTimeIndexfileName, 
                                                                                                 paste0(folder,portfolioName))
             }else if(toupper(type) == "ALL8")
             {
               value_weight = 80
               growth_weight = 10
               portfolioValues <- GeneratePortfolioUnEqualWeighted_LoadFromFile_WithInputSymbols(fromDate[i], toDate[i], 
                                                                                                 value_symbols, 
                                                                                                 value_weight,
                                                                                                 core_symbols, 
                                                                                                 growth_symbols,
                                                                                                 growth_weight,
                                                                                                 symbolsTimeIndexfileName, 
                                                                                                 paste0(folder,portfolioName))
               
             }else if(toupper(type) == "ALL9")
             {
               value_weight = 10
               growth_weight = 70
               portfolioValues <- GeneratePortfolioUnEqualWeighted_LoadFromFile_WithInputSymbols(fromDate[i], toDate[i], 
                                                                                                 value_symbols, 
                                                                                                 value_weight,
                                                                                                 core_symbols, 
                                                                                                 growth_symbols,
                                                                                                 growth_weight,
                                                                                                 symbolsTimeIndexfileName, 
                                                                                                 paste0(folder,portfolioName))
               
             }else if(toupper(type) == "ALL10")
             {
               value_weight = 20
               growth_weight = 60
               portfolioValues <- GeneratePortfolioUnEqualWeighted_LoadFromFile_WithInputSymbols(fromDate[i], toDate[i], 
                                                                                                 value_symbols, 
                                                                                                 value_weight,
                                                                                                 core_symbols, 
                                                                                                 growth_symbols,
                                                                                                 growth_weight,
                                                                                                 symbolsTimeIndexfileName, 
                                                                                                 paste0(folder,portfolioName))
             }else if(toupper(type) == "ALL11")
             {
               value_weight = 30
               growth_weight = 50
               portfolioValues <- GeneratePortfolioUnEqualWeighted_LoadFromFile_WithInputSymbols(fromDate[i], toDate[i], 
                                                                                                 value_symbols, 
                                                                                                 value_weight,
                                                                                                 core_symbols, 
                                                                                                 growth_symbols,
                                                                                                 growth_weight,
                                                                                                 symbolsTimeIndexfileName, 
                                                                                                 paste0(folder,portfolioName))
               
             }else if(toupper(type) == "ALL12")
             {
               value_weight = 40
               growth_weight = 40
               portfolioValues <- GeneratePortfolioUnEqualWeighted_LoadFromFile_WithInputSymbols(fromDate[i], toDate[i], 
                                                                                                 value_symbols, 
                                                                                                 value_weight,
                                                                                                 core_symbols, 
                                                                                                 growth_symbols,
                                                                                                 growth_weight,
                                                                                                 symbolsTimeIndexfileName, 
                                                                                                 paste0(folder,portfolioName))
               
             }else if(toupper(type) == "ALL13")
             {
               value_weight = 50
               growth_weight = 30
               portfolioValues <- GeneratePortfolioUnEqualWeighted_LoadFromFile_WithInputSymbols(fromDate[i], toDate[i], 
                                                                                                 value_symbols, 
                                                                                                 value_weight,
                                                                                                 core_symbols, 
                                                                                                 growth_symbols,
                                                                                                 growth_weight,
                                                                                                 symbolsTimeIndexfileName, 
                                                                                                 paste0(folder,portfolioName))
             }else if(toupper(type) == "ALL14")
             {
               value_weight = 60
               growth_weight = 20
               portfolioValues <- GeneratePortfolioUnEqualWeighted_LoadFromFile_WithInputSymbols(fromDate[i], toDate[i], 
                                                                                                 value_symbols, 
                                                                                                 value_weight,
                                                                                                 core_symbols, 
                                                                                                 growth_symbols,
                                                                                                 growth_weight,
                                                                                                 symbolsTimeIndexfileName, 
                                                                                                 paste0(folder,portfolioName))
             }else if(toupper(type) == "ALL15")
             {
               value_weight = 70
               growth_weight = 10
               portfolioValues <- GeneratePortfolioUnEqualWeighted_LoadFromFile_WithInputSymbols(fromDate[i], toDate[i], 
                                                                                                 value_symbols, 
                                                                                                 value_weight,
                                                                                                 core_symbols, 
                                                                                                 growth_symbols,
                                                                                                 growth_weight,
                                                                                                 symbolsTimeIndexfileName,
                                                                                                 paste0(folder,portfolioName))
               
             }else if(toupper(type) == "ALL16")
             {
               value_weight = 10
               growth_weight = 60
               portfolioValues <- GeneratePortfolioUnEqualWeighted_LoadFromFile_WithInputSymbols(fromDate[i], toDate[i], 
                                                                                                 value_symbols, 
                                                                                                 value_weight,
                                                                                                 core_symbols, 
                                                                                                 growth_symbols,
                                                                                                 growth_weight,
                                                                                                 symbolsTimeIndexfileName, 
                                                                                                 paste0(folder,portfolioName))
               
             }else if(toupper(type) == "ALL17")
             {
               value_weight = 20
               growth_weight = 50
               portfolioValues <- GeneratePortfolioUnEqualWeighted_LoadFromFile_WithInputSymbols(fromDate[i], toDate[i], 
                                                                                                 value_symbols, 
                                                                                                 value_weight,
                                                                                                 core_symbols, 
                                                                                                 growth_symbols,
                                                                                                 growth_weight,
                                                                                                 symbolsTimeIndexfileName, 
                                                                                                 paste0(folder,portfolioName))
               
             }else if(toupper(type) == "ALL18")
             {
               value_weight = 30
               growth_weight = 40
               portfolioValues <- GeneratePortfolioUnEqualWeighted_LoadFromFile_WithInputSymbols(fromDate[i], toDate[i], 
                                                                                                 value_symbols, 
                                                                                                 value_weight,
                                                                                                 core_symbols, 
                                                                                                 growth_symbols,
                                                                                                 growth_weight,
                                                                                                 symbolsTimeIndexfileName, 
                                                                                                 paste0(folder,portfolioName))
             }else if(toupper(type) == "ALL19")
             {
               value_weight = 40
               growth_weight = 30
               portfolioValues <- GeneratePortfolioUnEqualWeighted_LoadFromFile_WithInputSymbols(fromDate[i], toDate[i], 
                                                                                                 value_symbols, 
                                                                                                 value_weight,
                                                                                                 core_symbols, 
                                                                                                 growth_symbols,
                                                                                                 growth_weight,
                                                                                                 symbolsTimeIndexfileName, 
                                                                                                 paste0(folder,portfolioName))
             }else if(toupper(type) == "ALL20")
             {
               value_weight = 50
               growth_weight = 20
               portfolioValues <- GeneratePortfolioUnEqualWeighted_LoadFromFile_WithInputSymbols(fromDate[i], toDate[i], 
                                                                                                 value_symbols, 
                                                                                                 value_weight,
                                                                                                 core_symbols, 
                                                                                                 growth_symbols,
                                                                                                 growth_weight,
                                                                                                 symbolsTimeIndexfileName, 
                                                                                                 paste0(folder,portfolioName))
               
             }else if(toupper(type) == "ALL21")
             {
               value_weight = 60
               growth_weight = 10
               portfolioValues <- GeneratePortfolioUnEqualWeighted_LoadFromFile_WithInputSymbols(fromDate[i], toDate[i], 
                                                                                                 value_symbols, 
                                                                                                 value_weight,
                                                                                                 core_symbols, 
                                                                                                 growth_symbols,
                                                                                                 growth_weight,
                                                                                                 symbolsTimeIndexfileName, 
                                                                                                 paste0(folder,portfolioName))
               
             }else if(toupper(type) == "ALL22")
             {
               value_weight = 10
               growth_weight = 50
               portfolioValues <- GeneratePortfolioUnEqualWeighted_LoadFromFile_WithInputSymbols(fromDate[i], toDate[i], 
                                                                                                 value_symbols, 
                                                                                                 value_weight,
                                                                                                 core_symbols, 
                                                                                                 growth_symbols,
                                                                                                 growth_weight,
                                                                                                 symbolsTimeIndexfileName, 
                                                                                                 paste0(folder,portfolioName))
             }else if(toupper(type) == "ALL23")
             {
               value_weight = 20
               growth_weight = 40
               portfolioValues <- GeneratePortfolioUnEqualWeighted_LoadFromFile_WithInputSymbols(fromDate[i], toDate[i], 
                                                                                                 value_symbols, 
                                                                                                 value_weight,
                                                                                                 core_symbols, 
                                                                                                 growth_symbols,
                                                                                                 growth_weight,
                                                                                                 symbolsTimeIndexfileName, 
                                                                                                 paste0(folder,portfolioName))
               
             }else if(toupper(type) == "ALL24")
             {
               value_weight = 30
               growth_weight = 30
               portfolioValues <- GeneratePortfolioUnEqualWeighted_LoadFromFile_WithInputSymbols(fromDate[i], toDate[i], 
                                                                                                 value_symbols, 
                                                                                                 value_weight,
                                                                                                 core_symbols, 
                                                                                                 growth_symbols,
                                                                                                 growth_weight,
                                                                                                 symbolsTimeIndexfileName, 
                                                                                                 paste0(folder,portfolioName))
             }else if(toupper(type) == "ALL25")
             {
               value_weight = 40
               growth_weight = 20
               portfolioValues <- GeneratePortfolioUnEqualWeighted_LoadFromFile_WithInputSymbols(fromDate[i], toDate[i], 
                                                                                                 value_symbols, 
                                                                                                 value_weight,
                                                                                                 core_symbols, 
                                                                                                 growth_symbols,
                                                                                                 growth_weight,
                                                                                                 symbolsTimeIndexfileName, 
                                                                                                 paste0(folder,portfolioName))
             }else if(toupper(type) == "ALL26")
             {
               value_weight = 50
               growth_weight = 10
               portfolioValues <- GeneratePortfolioUnEqualWeighted_LoadFromFile_WithInputSymbols(fromDate[i], toDate[i], 
                                                                                                 value_symbols, 
                                                                                                 value_weight,
                                                                                                 core_symbols, 
                                                                                                 growth_symbols,
                                                                                                 growth_weight,
                                                                                                 symbolsTimeIndexfileName, 
                                                                                                 paste0(folder,portfolioName))
             }else if(toupper(type) == "ALL27")
             {
               value_weight = 10
               growth_weight = 40
               portfolioValues <- GeneratePortfolioUnEqualWeighted_LoadFromFile_WithInputSymbols(fromDate[i], toDate[i], 
                                                                                                 value_symbols, 
                                                                                                 value_weight,
                                                                                                 core_symbols, 
                                                                                                 growth_symbols,
                                                                                                 growth_weight,
                                                                                                 symbolsTimeIndexfileName,
                                                                                                 paste0(folder,portfolioName))
               
             }else if(toupper(type) == "ALL28")
             {
               value_weight = 20
               growth_weight = 30
               portfolioValues <- GeneratePortfolioUnEqualWeighted_LoadFromFile_WithInputSymbols(fromDate[i], toDate[i], 
                                                                                                 value_symbols, 
                                                                                                 value_weight,
                                                                                                 core_symbols, 
                                                                                                 growth_symbols,
                                                                                                 growth_weight,
                                                                                                 symbolsTimeIndexfileName, 
                                                                                                 paste0(folder,portfolioName))
               
             }else if(toupper(type) == "ALL29")
             {
               value_weight = 30
               growth_weight = 20
               portfolioValues <- GeneratePortfolioUnEqualWeighted_LoadFromFile_WithInputSymbols(fromDate[i], toDate[i], 
                                                                                                 value_symbols, 
                                                                                                 value_weight,
                                                                                                 core_symbols, 
                                                                                                 growth_symbols,
                                                                                                 growth_weight,
                                                                                                 symbolsTimeIndexfileName, 
                                                                                                 paste0(folder,portfolioName))
               
             }else if(toupper(type) == "ALL30")
             {
               value_weight = 40
               growth_weight = 10
               portfolioValues <- GeneratePortfolioUnEqualWeighted_LoadFromFile_WithInputSymbols(fromDate[i], toDate[i], 
                                                                                                 value_symbols, 
                                                                                                 value_weight,
                                                                                                 core_symbols, 
                                                                                                 growth_symbols,
                                                                                                 growth_weight,
                                                                                                 symbolsTimeIndexfileName, 
                                                                                                 paste0(folder,portfolioName))
             }else if(toupper(type) == "ALL31")
             {
               value_weight = 10
               growth_weight = 30
               portfolioValues <- GeneratePortfolioUnEqualWeighted_LoadFromFile_WithInputSymbols(fromDate[i], toDate[i], 
                                                                                                 value_symbols, 
                                                                                                 value_weight,
                                                                                                 core_symbols, 
                                                                                                 growth_symbols,
                                                                                                 growth_weight,
                                                                                                 symbolsTimeIndexfileName, 
                                                                                                 paste0(folder,portfolioName))
             }else if(toupper(type) == "ALL32")
             {
               value_weight = 20
               growth_weight = 20
               portfolioValues <- GeneratePortfolioUnEqualWeighted_LoadFromFile_WithInputSymbols(fromDate[i], toDate[i], 
                                                                                                 value_symbols, 
                                                                                                 value_weight,
                                                                                                 core_symbols, 
                                                                                                 growth_symbols,
                                                                                                 growth_weight,
                                                                                                 symbolsTimeIndexfileName, 
                                                                                                 paste0(folder,portfolioName))
               
             }else if(toupper(type) == "ALL33")
             {
               value_weight = 30
               growth_weight = 10
               portfolioValues <- GeneratePortfolioUnEqualWeighted_LoadFromFile_WithInputSymbols(fromDate[i], toDate[i], 
                                                                                                 value_symbols, 
                                                                                                 value_weight,
                                                                                                 core_symbols, 
                                                                                                 growth_symbols,
                                                                                                 growth_weight,
                                                                                                 symbolsTimeIndexfileName, 
                                                                                                 paste0(folder,portfolioName))
               
             }else if(toupper(type) == "ALL34")
             {
               value_weight = 10
               growth_weight = 20
               portfolioValues <- GeneratePortfolioUnEqualWeighted_LoadFromFile_WithInputSymbols(fromDate[i], toDate[i], 
                                                                                                 value_symbols, 
                                                                                                 value_weight,
                                                                                                 core_symbols, 
                                                                                                 growth_symbols,
                                                                                                 growth_weight,
                                                                                                 symbolsTimeIndexfileName, 
                                                                                                 paste0(folder,portfolioName))
             }else if(toupper(type) == "ALL35")
             {
               value_weight = 20
               growth_weight = 10
               portfolioValues <- GeneratePortfolioUnEqualWeighted_LoadFromFile_WithInputSymbols(fromDate[i], toDate[i], 
                                                                                                 value_symbols, 
                                                                                                 value_weight,
                                                                                                 core_symbols, 
                                                                                                 growth_symbols,
                                                                                                 growth_weight,
                                                                                                 symbolsTimeIndexfileName, 
                                                                                                 paste0(folder,portfolioName))
               
             }else if(toupper(type) == "ALL36")
             {
               value_weight = 10
               growth_weight = 10
               portfolioValues <- GeneratePortfolioUnEqualWeighted_LoadFromFile_WithInputSymbols(fromDate[i], toDate[i], 
                                                                                                 value_symbols, 
                                                                                                 value_weight,
                                                                                                 core_symbols, 
                                                                                                 growth_symbols,
                                                                                                 growth_weight,
                                                                                                 symbolsTimeIndexfileName, 
                                                                                                 paste0(folder,portfolioName))
               
               
             }else if(toupper(type) == "VG"){
               value_weight = 50
               growth_weight = 50
               portfolioValues <- GeneratePortfolioUnEqualWeighted_LoadFromFile_WithInputSymbols(fromDate[i], toDate[i], 
                                                                                                 value_symbols, 
                                                                                                 value_weight,
                                                                                                 NULL, 
                                                                                                 growth_symbols,
                                                                                                 growth_weight,
                                                                                                 symbolsTimeIndexfileName, 
                                                                                                 paste0(folder,portfolioName))
             }else{
               #### Generates the Portfolio EqualWeighted#############
               portfolioValues <- GeneratePortfolioEqualWeighted_LoadFromFile_WithInputSymbols(fromDate[i], toDate[i], symbols, symbolsTimeIndexfileName, paste0(folder,portfolioName))
              }
              #------------------ generate portfolio summary file -------------------------------------------------
              
              portfolioValues <- as.data.frame(portfolioValues)
              #rearange the columns
              portfolioValues <- portfolioValues[c("Date","TotalValue", "DailyReturn", "Index")  ]
              #just for  summary code purpose...
              colnames(portfolioValues) <- c("Index", "TotalValue", "RemainingValue", "SymbolsValue")
              
              if(workingWith == "Crypto10" || workingWith == "Crypto30")
              {
                #crypto offers 24/7 tranding. The Portfolios run in weekend but the Crypo10Index doesn't, so we will fill in the gaps with Friday data. 
                benchmarkDataset_subset <- FillGapsInTimeIndex(portfolioValues, benchmarkDataset_subset)
              }
              #MPT stats code generator
              summary =  SummaryCode(portfolioValues, benchmarkDataset_subset,portfolioName , benchmarkName, folder)
              
              final_summary <- cbind(final_summary,summary)
              #arrange a temp dataset with the Portfolio results and diff
              {
                temp <- as.matrix( summary[, c(1+ secventa)])
                temp <- rbind(temp,c(1+ secventa) )
                colnames(temp) <- paste0(secventa, " Y ", fromDate[i], " to ", toDate[i])
                
                
                for( k in seq(1,dim(temp)[2],1))
                {
                  # anlualized diff
                  temp[4,k] <- as.numeric( as.numeric(temp[2,k]) - as.numeric(temp[3,k]))
                  #volatility diff
                  temp[7,k] <- as.numeric(temp[5,k]) - as.numeric(temp[6,k])
                  
                  temp[25,k] <- as.numeric(temp[23,k]) - as.numeric(temp[24,k])
                }
                
                {
                  temp <- as.data.frame(temp)
                  rownames(temp)[2] <- "P Annualized Returns"
                  rownames(temp)[3] <- "B Annualized Returns"
                  rownames(temp)[4] <- "AR DIFF(P-B)"
                  rownames(temp)[5] <- "P Volatility"
                  rownames(temp)[6] <- "B Volatility"
                  rownames(temp)[7] <- "V DIFF(P-B)"
                  rownames(temp)[8] <- "P Tracking Error"
                  rownames(temp)[9] <- "B Tracking Error"
                  rownames(temp)[10] <- "IR"
                  rownames(temp)[11] <- "P Information Ratio"
                  rownames(temp)[12] <- "B Information Ratio"
                  rownames(temp)[13] <- "AS"
                  rownames(temp)[14] <- "P Alpha Stat"
                  rownames(temp)[15] <- "B Alpha Stat"
                  rownames(temp)[16] <- "BS"
                  rownames(temp)[17] <- "P Beta Stat"
                  rownames(temp)[18] <- "B Beta Stat"
                  rownames(temp)[19] <- "RS"
                  rownames(temp)[20] <- "P R-Squared"	
                  rownames(temp)[21] <- "B R-Squared"	
                  rownames(temp)[22] <- "MD"
                  rownames(temp)[23] <- "P Max Drawdown"
                  rownames(temp)[24] <- "B Max Drawdown"
                  rownames(temp)[25] <- "MD DIFF(P-B)"
                }
              }
              #create a dataset for output
              
              for(col in seq(1, dim(temp)[2],1))
              {
                temp[, col] <- as.numeric(as.character(temp[,col]))
              }
              
            
            
            last_column <- dim(temp)[2]
              if(i == 1)
              {
                average_Summary <- temp[,last_column]  #rowMeans(temp, na.rm=TRUE)
                average_Summary <- as.matrix(average_Summary)
                rownames(average_Summary) <- rownames(temp)
                colnames(average_Summary) <- colnames(temp[last_column])
              }else{
                  col_names <- colnames(average_Summary)
                  average_Summary <- cbind(average_Summary, temp[,last_column])  #cbind(average_Summary, rowMeans(temp, na.rm=TRUE))
                  colnames(average_Summary) <- c(col_names, colnames(temp[last_column]))
                }
                
              
              
              
            }  
            
            
            write.csv(average_Summary, file = paste0(folder,"Average_Summary_",type,"_Run_for_",run_for_years, "_years_",rankNameForOutputFileNotations, ".csv"))
            write.csv(final_summary, file = paste0(folder, "Final_Summary_",type,"_Run_for_",run_for_years, "_years_",rankNameForOutputFileNotations, ".csv"))
          }
     }
    }
    
  
}




#----------------------Generate Portfolio Equal Weighted ----------
GeneratePortfolioEqualWeighted_LoadFromFile_WithInputSymbols = function(fromDate, toDate, 
                                                                        symbolGuidList, 
                                                                        timeIndexFileName, 
                                                                        fileAppendix)
{
  # ---------------Extract price ------------------------------------------------------------
  
  symbolGuidList_inFile <- ""
  fileTimeIndexValue <- data.frame(read.csv(file = timeIndexFileName, sep=",",header=TRUE,stringsAsFactors=F, fileEncoding="utf-8", check.names=FALSE))
  #get stocks EOD data for the interval needed to run the Portfolio
  symbolsTimeIndexValues <- fileTimeIndexValue[fileTimeIndexValue$Date >= fromDate & fileTimeIndexValue$Date <= toDate ,]
  #column rearange
  symbolGuidList_inFile <- colnames(symbolsTimeIndexValues)[2:dim(symbolsTimeIndexValues)[2]]
  symbolGuidList_inFile<- na.omit(symbolGuidList_inFile)
  
  
  for( i in symbolGuidList){
    if(any( is.na( symbolsTimeIndexValues[i]))){
      #print a warning message, in case one of the Symbols selected to be part of the Portfolio has no data
      
      print(paste0("These simbols don't have data in timeIndex file ",i))
      print(paste0("For Portfolio StartDate ", fromDate ," and ending in " , toDate))
      position = which(symbolGuidList == i)
      symbolGuidList <- symbolGuidList[- position]
    }
  }
  
  
  #----------------Create the table of Symbols, their weight, portfolioSymbol.Value and number of units-------------------------------------------------------------------------------------------
  
  
  # vectorize assign, get and exists functions
  assign_hash <- Vectorize(assign, vectorize.args = c("x", "value"))
  get_hash <- Vectorize(get, vectorize.args = "x")
  exists_hash <- Vectorize(exists, vectorize.args = "x")
  
  #----------------Determine each symbol Weight and UnitsFraction ------------------
  #determine one symbol weight relative to the number of symbols that are going to be part of the P
  numberOfStocks = length(symbolGuidList)
  oneSymbolWeight =  100/ numberOfStocks
  
  symbolUnitFractionList <- list()
  symbolList <- list()
  
  #create a matrix with each symbol's UnitFraction
  #the UnitFraction is calculated at the begining of the P so that is equal weighted and will not change
  symbolPortfolioData <- matrix(ncol = 3 )
  colnames(symbolPortfolioData) <- c("SymbolGuid","SymbolTimeIndexValue","UnitsFraction")
  
  #get the data only for symbols given as input in function
  symbolsTimeIndexValues <- symbolsTimeIndexValues[,c("Date",symbolGuidList) ]
  
  if(length(symbolsTimeIndexValues[,1]) > 0)
  {
    for(i in seq(1,length(symbolsTimeIndexValues[1,]),1))
    {
      if(i > 1)
      {
        
        # set up INITIAL Portfolio Symbols UnitsFraction and PortfolioSymbolsValue
        if(!is.na(symbolsTimeIndexValues[1,i]))
        {
          symbolTimeIndexValue = as.double(symbolsTimeIndexValues[1,i])
          unitsFraction = oneSymbolWeight/ symbolTimeIndexValue
          portfolioSymbolValue = unitsFraction * symbolTimeIndexValue
          
          symbolPortfolioData <- rbind(symbolPortfolioData, c( colnames(symbolsTimeIndexValues[i]), formatC(as.numeric(symbolTimeIndexValue), digits = 12, format = "f") , formatC(as.numeric(unitsFraction), digits = 12, format = "f")))
          
          #create a list o symbols and a list of UnitFractions
          symbolUnitFractionList <- c( symbolUnitFractionList, unitsFraction)
          symbolList <- c(symbolList, colnames(symbolsTimeIndexValues[i]))
        }
        
      }
    }
    
  }
  
  symbolPortfolioData <- na.omit(symbolPortfolioData)
  
  # initialize hash
  hash <- new.env(hash = TRUE, parent = emptyenv(), size = 100L)
  
  # map the list of SymbolsGuid with the UnitFraction List/ assign values to keys
  assign_hash(symbolList, symbolUnitFractionList, hash)
  
  
  
  #----------------Create Portfolio Values-------------------------------------------------------------------------------------------
  
  portfolioDailyReturn <- matrix(ncol = 4)
  previousPortfolioValue = 0.0
  symbolUnitsFraction = 0.0
  
  colnames(portfolioDailyReturn)=c( "Date","DailyReturn", "TotalValue", "Index")
  
  
  if(length(symbolsTimeIndexValues[,1]) > 0)
  {
    
    for(i in seq(1,length(symbolsTimeIndexValues[,1]),1))  
    {
      suma = 0.0  # here will keep the sum of Symbols for each row(day).
      
      for(j in seq(1,length(symbolsTimeIndexValues[1,]),1))
      {
        if(j>1 && colnames(symbolsTimeIndexValues[j]) %in% symbolGuidList) # needed for dataservice -> && (typeof(symbolsTimeIndexValues[i,j]) == "character") # take only erery second element to be added
        {
          symbolUnitsFraction = get_hash(c(colnames(symbolsTimeIndexValues[j])), hash)
          if(!is.na(symbolsTimeIndexValues[i,j]))
          {
            suma = suma + as.double(symbolsTimeIndexValues[i,j])* symbolUnitsFraction
          }
        }
      }
      #get Portfolio initial value
      if(i == 1)
      {
        previousPortfolioValue = suma
      }
      
      
      dailyReturn <- (suma-previousPortfolioValue)/previousPortfolioValue*100
      previousPortfolioValue <- suma
      if(!is.na(dailyReturn))
      {
        portfolioDailyReturn<-rbind(portfolioDailyReturn, c( symbolsTimeIndexValues[i,1], formatC(as.numeric(dailyReturn), digits = 12, format = "f") , formatC(as.numeric(suma), digits = 12, format = "f"), convertDateToIndex(symbolsTimeIndexValues[i,1] )))
      }
    }
  }
  
  
  portfolioDailyReturn<-na.omit(portfolioDailyReturn)
  
  #----------------Save to File Daily Return ---------------------------------------------------------------
  sql_data_format <- as.data.frame(portfolioDailyReturn)
  row.names(sql_data_format)<-NULL
  filename = paste0(fileAppendix,"_DailyReturn",  ".csv")
  write.csv2(sql_data_format , file = filename)
  
  sql_symbolPortfolioData <-as.data.frame(symbolPortfolioData)
  row.names(sql_symbolPortfolioData)<-NULL
  filename = paste0(fileAppendix, "_Components",  ".csv")
  write.csv2(sql_symbolPortfolioData , file = filename ,row.names = FALSE)
  
  return(portfolioDailyReturn)
}

#----------------------Generate Portfolio UnEqual Weighted ----------
# fromDate <- fromDate[i]
# toDate <- toDate[i]
# value_symbolGuidList <- value_symbols
# value_percentage <- value_weight
# core_symbolGuidList<- core_symbols
# growth_symbolGuidList<- growth_symbols
# growth_percentage <- growth_weight
# timeIndexFileName <- symbolsTimeIndexfileName
# fileAppendix <- paste0(folder,portfolioName)

GeneratePortfolioUnEqualWeighted_LoadFromFile_WithInputSymbols = function(fromDate, toDate, 
                                                                          value_symbolGuidList, 
                                                                          value_percentage,
                                                                          core_symbolGuidList, 
                                                                          growth_symbolGuidList,
                                                                          growth_percentage,
                                                                          timeIndexFileName, 
                                                                          fileAppendix)
{
  # --------------------------------------------------------- Extracting prices ------------------------------------------------------------
  
  core_percentage = 100 - value_percentage - growth_percentage
  
  symbolGuidList_inFile <- ""
  fileTimeIndexValue <- data.frame(read.csv(file = timeIndexFileName, sep=",",header=TRUE,stringsAsFactors=F, fileEncoding="utf-8", check.names=FALSE))
  #get stocks EOD data for the interval needed to run the Portfolio
  symbolsTimeIndexValues <- fileTimeIndexValue[fileTimeIndexValue$Date >= fromDate & fileTimeIndexValue$Date <= toDate ,]
  
  if(is.na(symbolsTimeIndexValues[dim(symbolsTimeIndexValues)[1],1])) { symbolsTimeIndexValues <-symbolsTimeIndexValues[-dim(symbolsTimeIndexValues)[1],]}
  #column rearange
  symbolGuidList_inFile <- colnames(symbolsTimeIndexValues)[2:dim(symbolsTimeIndexValues)[2]]
  symbolGuidList_inFile<- na.omit(symbolGuidList_inFile)
  if(!is.null(core_symbolGuidList))
  {
     symbolGuidList<- c( value_symbolGuidList, core_symbolGuidList, growth_symbolGuidList)
  }else { symbolGuidList<- c( value_symbolGuidList, growth_symbolGuidList)}
  
  
  
  for( i in symbolGuidList){
    if(any( is.na( symbolsTimeIndexValues[i]))){
      #print a warning message, in case one of the Symbols selected to be part of the Portfolio has no data
      
      print(paste0("These simbols don't have data in timeIndex file ",i))
      print(paste0("For Portfolio StartDate ", fromDate ," and ending in " , toDate))
      position = which(symbolGuidList == i)
      symbolGuidList <- symbolGuidList[- position]
      if(!is.null(core_symbolGuidList) && i %in% core_symbolGuidList){core_symbolGuidList = core_symbolGuidList[- which(core_symbolGuidList == i) ]}
      if(i %in% value_symbolGuidList){value_symbolGuidList = value_symbolGuidList[- which(value_symbolGuidList == i) ]}
      if(i %in% growth_symbolGuidList){growth_symbolGuidList = growth_symbolGuidList[- which(growth_symbolGuidList == i) ]}
    }
  }
  
  
  #----------------Create the table of Symbols, their weight, portfolioSymbol.Value and number of units-------------------------------------------------------------------------------------------
  
  
  # vectorize assign, get and exists functions
  assign_hash <- Vectorize(assign, vectorize.args = c("x", "value"))
  get_hash <- Vectorize(get, vectorize.args = "x")
  exists_hash <- Vectorize(exists, vectorize.args = "x")
  
  if(!is.null(core_symbolGuidList)){
    core_oneSymbolWeight =  core_percentage/ length(core_symbolGuidList)  
  }else{core_oneSymbolWeight = 0}
  
  value_oneSymbolWeight =  value_percentage/ length(value_symbolGuidList)
  growth_oneSymbolWeight =  growth_percentage/ length(growth_symbolGuidList)
  #print(paste0("Core w: ",core_oneSymbolWeight, " Value: ",value_oneSymbolWeight, "Growth w :", growth_oneSymbolWeight))
  
  
  
  symbolUnitFractionList <- list()
  symbolList <- list()
  
  symbolPortfolioData <- matrix(ncol = 4)
  colnames(symbolPortfolioData) <- c("SymbolGuid","SymbolTimeIndexValue","UnitsFraction", "Bin")
  
  #get the data only for symbols given as input in function
  symbolsTimeIndexValues <- symbolsTimeIndexValues[,c("Date",symbolGuidList) ]
  bin=""
  write.csv(symbolsTimeIndexValues, "Timeindex1.csv")
  
  if(length(symbolsTimeIndexValues[,1]) > 0)
  {
    for(i in seq(1,length(symbolsTimeIndexValues[1,]),1))
    {
      if(i > 1)
      {
        
        # set up INITIAL Portfolio Symbols UnitsFraction and PortfolioSymbolsValue
        if(!is.na(symbolsTimeIndexValues[1,i]))
        {
          symbolTimeIndexValue = as.double(symbolsTimeIndexValues[1,i])
          if(length(value_symbolGuidList[value_symbolGuidList %in% colnames(symbolsTimeIndexValues)[i]]) >0)
          {
            unitsFraction = value_oneSymbolWeight/ symbolTimeIndexValue  
            bin = "Value"
            
          }
          if(!is.null(core_symbolGuidList) && length(core_symbolGuidList[core_symbolGuidList %in% colnames(symbolsTimeIndexValues)[i]])>0)
          {
            unitsFraction = core_oneSymbolWeight/ symbolTimeIndexValue  
            bin = "Core"
          }
          if(length(growth_symbolGuidList[growth_symbolGuidList %in% colnames(symbolsTimeIndexValues)[i]])>0)
          {
            unitsFraction = growth_oneSymbolWeight/ symbolTimeIndexValue  
            bin = "Growth"
          }
          
          symbolPortfolioData <- rbind(symbolPortfolioData, c( colnames(symbolsTimeIndexValues[i]), formatC(as.numeric(symbolTimeIndexValue), digits = 12, format = "f") , formatC(as.numeric(unitsFraction), digits = 12, format = "f"), bin))
          
          #create a list o symbols and a list of UnitFractions
          symbolUnitFractionList <- c( symbolUnitFractionList, unitsFraction)
          symbolList <- c(symbolList, colnames(symbolsTimeIndexValues[i]))
        }
        
        
      }
    }
    
  }
  
  symbolPortfolioData <- na.omit(symbolPortfolioData)
  
  # initialize hash
  hash <- new.env(hash = TRUE, parent = emptyenv(), size = 100L)
  
  # map the list of SymbolsGuid with the UnitFraction List/ assign values to keys
  assign_hash(symbolList, symbolUnitFractionList, hash)
  
  
  
  #----------------Create Portfolio Values-------------------------------------------------------------------------------------------
  
  portfolioDailyReturn <- matrix(ncol = 4)
  previousPortfolioValue = 0.0
  symbolUnitsFraction = 0.0
  
  colnames(portfolioDailyReturn)=c( "Date","DailyReturn", "TotalValue", "Index")
  
  if(length(symbolsTimeIndexValues[,1]) > 0)
  {
    
    for(i in seq(1,length(symbolsTimeIndexValues[,1]),1))  
    {
      suma = 0.0  # here will keep the sum of Symbols for each row.
      
      for(j in seq(1,length(symbolsTimeIndexValues[1,]),1))
      {
        if(j>1 && colnames(symbolsTimeIndexValues[j]) %in% symbolGuidList) # needed for dataservice -> && (typeof(symbolsTimeIndexValues[i,j]) == "character") # take only erery second element to be added
        {
          symbolUnitsFraction = get_hash(c(colnames(symbolsTimeIndexValues[j])), hash)
          if(!is.na(symbolsTimeIndexValues[i,j]))
          {
            suma = suma + as.double(symbolsTimeIndexValues[i,j])* symbolUnitsFraction
          }
        }
      }
      #get Portfolio initial value
      if(i == 1)
      {
        previousPortfolioValue = suma
      }
      
      
      dailyReturn <- (suma-previousPortfolioValue)/previousPortfolioValue*100
      previousPortfolioValue <- suma
      if(!is.na(dailyReturn))
      {
        portfolioDailyReturn<-rbind(portfolioDailyReturn, c( symbolsTimeIndexValues[i,1], formatC(as.numeric(dailyReturn), digits = 12, format = "f") , formatC(as.numeric(suma), digits = 12, format = "f"), convertDateToIndex(symbolsTimeIndexValues[i,1] )))
      }
    }
  }
  
  
  portfolioDailyReturn<-na.omit(portfolioDailyReturn)
  
  #-------------------------------------------------------Save to File Daily Return ---------------------------------------------------------------
  sql_data_format <- as.data.frame(portfolioDailyReturn)
  row.names(sql_data_format)<-NULL
  filename = paste0(fileAppendix,"_DailyReturn",  ".csv")
  write.csv2(sql_data_format , file = filename)
  
  sql_symbolPortfolioData <-as.data.frame(symbolPortfolioData)
  row.names(sql_symbolPortfolioData)<-NULL
  filename = paste0(fileAppendix, "_Components",  ".csv")
  write.csv2(sql_symbolPortfolioData , file = filename ,row.names = FALSE)
  
  return(portfolioDailyReturn)
}



FillGapsInTimeIndex = function(portfolioValues, benchmarkDataset_subset)
{
  # format the Date column
  portfolioValues$Index <- as.Date(portfolioValues$Index)
  
  joinDS <- portfolioValues  %>% left_join(benchmarkDataset_subset)
  
  for(i in seq(1, dim(joinDS)[1],1))
  {
    #case 1 : first row is NA
    if(is.na(joinDS[i,5]) && i == 1)
    {
      j=1
      while(is.na(joinDS[j,5]))
      {
        j = j+1
      }
      joinDS[i,5] <- joinDS[j,5]
    }
    
    #case 2 : finds NA in timeseries
    
    if(is.na(joinDS[i,5])){joinDS[i,5] <- joinDS[i-1,5]}
  }
  
  benchmarkValues <- joinDS[,c(1,5)]
  return(benchmarkValues)
}





#---------------------- Generate Rebalancing Portfolios and Summary Tables --------------
GenerateRebalancingPortfolioAndMPT <- function(symbolsTimeIndexfileName, benchmarkTimeIndexFileName, benchmark,minNumberOfSymbols,
                                               rankingFilesName, rankNameForOutputFileNotations,
                                               folder, folderMain, workingWith, startPortfoliosEvery, valueWeight, growthWeight, startDate) 
{
  
  folder <- paste0(folderMain,folder,"/")
  #reading benchmark data form file
  benchmarkDataset <-  data.frame(read.csv(file = benchmarkTimeIndexFileName, sep=",",header=TRUE,stringsAsFactors=F, fileEncoding="utf-8", check.names=FALSE))
  if(dim(benchmarkDataset)[2] == 3) {benchmarkDataset = benchmarkDataset[,c(2,3)]}
  benchmarkDataset$Index <- as.Date(benchmarkDataset$Index)
  
  # for each Ranking file given as input the code will create lenght(formDate)*3 Portfolios (value/core/growth)
  # and will generate MPT stats for each
  
  
  createIfFolderDontExists(folder)
  #reading ranking data from ranking file
  rankingDataset <- data.frame(read.csv(file =  rankingFilesName, sep=",",header=TRUE,stringsAsFactors=F, fileEncoding="utf-8", check.names=FALSE))
  
  #finding the first row in the ranking file where {minNumberOfSymbols} have values
  if(workingWith == "Crypto10"|| workingWith == "Crypto30"){
    date =  firstRowWithEnoughSymbols( rankingDataset, minNumberOfSymbols)
  }else{
    date = startDate
  }
  
  
  final_summary <- as.data.frame(NA)
  average_Summary <- as.data.frame(NA)
 
  #run for at least 1 year
  if( add.month(as.Date(date),1) < as.Date(rankingDataset[dim(rankingDataset)[1] -250,1]))
  {
    if(startPortfoliosEvery == "Month")
    { 
      #this will generate Portfolios starting every month
      fromDate <-  seq(as.Date(date),as.Date(rankingDataset[dim(rankingDataset)[1] -250,1]), by = "month")
      toDate <-  benchmarkDataset[dim(benchmarkDataset)[1],1]
    }else{
      #this will generate Portfolios starting every day
      fromDate <-  seq(as.Date(date),as.Date(rankingDataset[dim(rankingDataset)[1] -250,1]), by = "day")
      toDate <-  benchmarkDataset[dim(benchmarkDataset)[1],1]
    }  
    
    fromDate <- as.character(fromDate)
    toDate <- as.character(toDate)
    
    
      
      #for eaach Starting date in the vector willgenerate a Portfolio and Summary table
      for(i  in seq(1,length(fromDate),1) )
      {
        
        #portfolio generation
        print(paste0("Generating Portfolio with strarting date: ",fromDate[i]))
        
        
        benchmarkName = paste0(fromDate[i],"_to_", toDate,"_",benchmark)
        benchmarkDataset_subset <-  benchmarkDataset[benchmarkDataset$Index >= fromDate[i] & benchmarkDataset$Index <= toDate,]
        benchmarkDataset_subset$Index <- as.Date(benchmarkDataset_subset$Index)
        
        portfolioName = paste0(fromDate[i],"_to_", toDate,"_","Index_", rankNameForOutputFileNotations)
        print(paste0("Generating Index for ", portfolioName))
        
        
        portfolioValues <- GeneratePortfolioUnEqualWeighted_WithRebalance(fromDate[i], toDate, 
                                                                          rankingDataset,
                                                                          valueWeight,growthWeight,
                                                                          symbolsTimeIndexfileName, 
                                                                          paste0(folder,portfolioName))
        
        
        portfolioValues <- as.data.frame(portfolioValues)
        #rearange the columns
        portfolioValues <- portfolioValues[c("Date","TotalValue", "DailyReturn", "Index")  ]
        #just for  summary code purpose...
        colnames(portfolioValues) <- c("Index", "TotalValue", "RemainingValue", "SymbolsValue")
        
        
        #MPT stats code generator
        #summary1 =  SummaryCode(portfolioValues, benchmarkDataset_subset,portfolioName , benchmarkName, folder)
        summary =  SummaryCodeForContinuousRunningPortfolios(portfolioValues, benchmarkDataset_subset,portfolioName , benchmarkName, folder)
        columns = dim(summary)[2]-1
       
        if(columns-1>0){
          col_seq <- colnames(summary)[c(2:columns)]
          secventa <- seq(1,columns-1,1 )
        }else {
          col_seq <- colnames(summary)[2]
          secventa <- 1
        } 
        
        final_summary <- cbind(final_summary,summary)
        #arrange a temp dataset with the Portfolio results and diff
        {
          temp <- as.matrix( summary[, c(1+ secventa)])
          temp <- rbind(temp,c(1+ secventa) )
          colnames(temp) <- paste0( col_seq, " ", fromDate[i], " to ", toDate)
          
          
          for( k in seq(1,dim(temp)[2],1))
          {
            # anlualized diff
            temp[4,k] <- as.numeric( as.numeric(temp[2,k]) - as.numeric(temp[3,k]))
            #volatility diff
            temp[7,k] <- as.numeric(temp[5,k]) - as.numeric(temp[6,k])
            
            temp[25,k] <- as.numeric(temp[23,k]) - as.numeric(temp[24,k])
          }
          
          {
            temp <- as.data.frame(temp)
            rownames(temp)[2] <- "P Annualized Returns"
            rownames(temp)[3] <- "B Annualized Returns"
            rownames(temp)[4] <- "AR DIFF(P-B)"
            rownames(temp)[5] <- "P Volatility"
            rownames(temp)[6] <- "B Volatility"
            rownames(temp)[7] <- "V DIFF(P-B)"
            rownames(temp)[8] <- "P Tracking Error"
            rownames(temp)[9] <- "B Tracking Error"
            rownames(temp)[10] <- "IR"
            rownames(temp)[11] <- "P Information Ratio"
            rownames(temp)[12] <- "B Information Ratio"
            rownames(temp)[13] <- "AS"
            rownames(temp)[14] <- "P Alpha Stat"
            rownames(temp)[15] <- "B Alpha Stat"
            rownames(temp)[16] <- "BS"
            rownames(temp)[17] <- "P Beta Stat"
            rownames(temp)[18] <- "B Beta Stat"
            rownames(temp)[19] <- "RS"
            rownames(temp)[20] <- "P R-Squared"	
            rownames(temp)[21] <- "B R-Squared"	
            rownames(temp)[22] <- "MD"
            rownames(temp)[23] <- "P Max Drawdown"
            rownames(temp)[24] <- "B Max Drawdown"
            rownames(temp)[25] <- "MD DIFF(P-B)"
          }
        }
        #create a dataset for output
        
        for(col in seq(1, dim(temp)[2],1))
        {
          temp[, col] <- as.numeric(as.character(temp[,col]))
        }
        
        
        
        last_column <- dim(temp)[2]
        if(i == 1)
        {
          average_Summary <- temp[,last_column]  #rowMeans(temp, na.rm=TRUE)
          average_Summary <- as.matrix(average_Summary)
          rownames(average_Summary) <- rownames(temp)
          colnames(average_Summary) <- colnames(temp[last_column])
        }else{
          col_names <- colnames(average_Summary)
          average_Summary <- cbind(average_Summary, temp[,last_column])  #cbind(average_Summary, rowMeans(temp, na.rm=TRUE))
          colnames(average_Summary) <- c(col_names, colnames(temp[last_column]))
        }
        
        
        
        
      }  
      
      
      write.csv(average_Summary, file = paste0(folder,"Average_Summary_",rankNameForOutputFileNotations, ".csv"))
      write.csv(final_summary, file = paste0(folder, "Final_Summary_",rankNameForOutputFileNotations, ".csv"))
    
  }
  
  
  
}

#------------------------ Rebalancing Portfolios Func ------------
GeneratePortfolioUnEqualWeighted_WithRebalance = function(fromDate, toDate, rankingDataset,
                                                          value_percentage,
                                                          growth_percentage,
                                                          symbolsTimeIndexfileName, 
                                                          fileAppendix)
{
  
  fileTimeIndexValue <- data.frame(read.csv(file = symbolsTimeIndexfileName, sep=",",header=TRUE,stringsAsFactors=F, fileEncoding="utf-8", check.names=FALSE))
  symbolsTimeIndexValues <- fileTimeIndexValue[fileTimeIndexValue$Date >= fromDate & fileTimeIndexValue$Date <= toDate ,]
  
  #column rearange
  symbolGuidList <- colnames(symbolsTimeIndexValues)[2:dim(symbolsTimeIndexValues)[2]]
  
  for( i in symbolGuidList){
    if(any( is.na( symbolsTimeIndexValues[i]))){

     # print(paste0("These symbols don't have data in time Index file ",i))
     # print(paste0("For Portfolio StartDate ", fromDate ," and ending in " , toDate))
      position = which(symbolGuidList == i)
      symbolGuidList <- symbolGuidList[- position]

    }
  }


  
  
  #----------------Create Portfolio Values-------------------------------------------------------------------------------------------
  symbolPortfolioUnitsData <- GenerateUnitsDataSet( symbolsTimeIndexValues, rankingDataset, symbolGuidList,value_percentage,
                                                   growth_percentage, fromDate, FALSE)
  symbolPortfolioUnitsData<- as.data.frame(symbolPortfolioUnitsData)
  symbolPortfolioData<- cbind(fromDate, symbolPortfolioUnitsData)
  colnames(symbolPortfolioData) <- c("Inception", colnames(symbolPortfolioUnitsData))
  
  portfolioDailyReturn <- matrix(ncol = 4)
  previousPortfolioValue = 0.0
  symbolUnitsFraction = 0.0
  
  colnames(portfolioDailyReturn)=c( "Date","DailyReturn", "TotalValue", "Index")
  
  ValueRebalanceIndicator <-seq(3, 20,3)
  CGRebalanceIndicators <- seq(1,20,1)
  CGRebalanceIndicators <- CGRebalanceIndicators[-ValueRebalanceIndicator]

  
  if(length(symbolsTimeIndexValues[,1]) > 0)
  {
    noOfRebalances = NULL
    
    for(i in seq(1,length(symbolsTimeIndexValues[,1]),1))  
    {
      suma = 0.0  # here will keep the sum of Symbols for each row.
    
      monthsSinceStart = elapsed.months(symbolsTimeIndexValues[i,1], symbolsTimeIndexValues[1,1])
      
      if( monthsSinceStart %in% (12*CGRebalanceIndicators)   &&  ( is.null(noOfRebalances) || !is.element(monthsSinceStart, noOfRebalances ))) 
      {
        #reevaluate the symbols bin after every year
        noOfRebalances <- c(noOfRebalances, monthsSinceStart)
        timeIndexDataSet <- fileTimeIndexValue[fileTimeIndexValue$Date >= symbolsTimeIndexValues[i,1] & fileTimeIndexValue$Date <= toDate ,]
   
        symbolPortfolioUnitsData <- GenerateUnitsDataSet( timeIndexDataSet, rankingDataset, symbolGuidList,value_percentage,
                                                         growth_percentage, symbolsTimeIndexValues[i,1], TRUE, symbolPortfolioUnitsData)
        
        symbolPortfolioUnitsData <- as.data.frame(symbolPortfolioUnitsData)
        columnNames <- colnames(symbolPortfolioData)
        symbolPortfolioData<- cbind(symbolPortfolioData, symbolsTimeIndexValues[i,1], symbolPortfolioUnitsData)
        colnames(symbolPortfolioData) <- c(columnNames, paste0("Year",monthsSinceStart/12), colnames(symbolPortfolioUnitsData))
       
        
      }
      if( monthsSinceStart %in% (12*ValueRebalanceIndicator) &&  !is.element(monthsSinceStart, noOfRebalances )) 
      {
        #reevaluate the symbols bin after every year
        noOfRebalances = c(noOfRebalances, monthsSinceStart)
        newTimeIndexForSelection <- fileTimeIndexValue[fileTimeIndexValue$Date >= symbolsTimeIndexValues[i,1] & fileTimeIndexValue$Date <= toDate ,]
        symbolPortfolioUnitsData <- GenerateUnitsDataSet(newTimeIndexForSelection, rankingDataset, symbolGuidList,value_percentage,
                                                         growth_percentage, symbolsTimeIndexValues[i,1], FALSE, symbolPortfolioUnitsData)
        symbolPortfolioUnitsData <- as.data.frame(symbolPortfolioUnitsData)
        columnNames <- colnames(symbolPortfolioData)
        symbolPortfolioData<- cbind(symbolPortfolioData, symbolsTimeIndexValues[i,1], symbolPortfolioUnitsData)
        colnames(symbolPortfolioData) <- c(columnNames, paste0("Year",monthsSinceStart/12), colnames(symbolPortfolioUnitsData))
       
        
      }
      
      
      for(j in seq(1,length(symbolsTimeIndexValues[1,]),1))
      {
        if(j>1 && colnames(symbolsTimeIndexValues[j]) %in% symbolGuidList) # needed for dataservice -> && (typeof(symbolsTimeIndexValues[i,j]) == "character") # take only erery second element to be added
        {
          
          symbolUnitsFraction = as.numeric(as.character(symbolPortfolioUnitsData[symbolPortfolioUnitsData$SymbolGuid == colnames(symbolsTimeIndexValues[j]),"UnitsFraction"]))
          if(!is.na(symbolsTimeIndexValues[i,j]))
          {
            suma = suma + as.double(symbolsTimeIndexValues[i,j])* symbolUnitsFraction
          }
        }
      }
      #get Portfolio initial value
      if(i == 1)
      {
        previousPortfolioValue = suma
      }
      
      
      dailyReturn <- (suma-previousPortfolioValue)/previousPortfolioValue*100
      previousPortfolioValue <- suma
      #print(paste0(symbolsTimeIndexValues[i,1], " - ", suma) )
     
      if(!is.na(dailyReturn))
      {
           portfolioDailyReturn<-rbind(portfolioDailyReturn, c( symbolsTimeIndexValues[i,1], formatC(as.numeric(dailyReturn), digits = 12, format = "f") , formatC(as.numeric(suma), digits = 12, format = "f"), convertDateToIndex(symbolsTimeIndexValues[i,1] )))
      }
    }
  }
  
  
  portfolioDailyReturn<-na.omit(portfolioDailyReturn)
  
  #-------------------------------------------------------Save to File Daily Return ---------------------------------------------------------------
  print_portfolioDailyReturn <- as.data.frame(portfolioDailyReturn)
  row.names(print_portfolioDailyReturn)<-NULL
  filename = paste0(fileAppendix,"_DailyReturn",  ".csv")
  write.csv2(print_portfolioDailyReturn , file = filename)
  
  print_symbolPortfolioData <-as.data.frame(symbolPortfolioData)
  row.names(print_symbolPortfolioData)<-NULL
  filename = paste0(fileAppendix, "_Components",  ".csv")
  write.csv2(print_symbolPortfolioData , file = filename ,row.names = FALSE)
  
  return(portfolioDailyReturn)
}

#evalDate = symbolsTimeIndexValues[i,1]
GenerateUnitsDataSet = function( timeIndexDataSet, rankingDataset, symbolGuidList, value_percentage,
                                growth_percentage,evalDate, keepValue, oldSymbolPortfolioUnitsData= NULL)
{
  
  core_percentage = 100 - value_percentage - growth_percentage
  
  rankingData <- as.data.frame(rankingDataset[rankingDataset$Date == evalDate,])
  
  #in case that on the exact day we selected as the beginning of the Portfolio, there is no ranking data, we add days until we find a valid date with enough data in the ranking file
 
  while(dim(rankingData)[1]<1 && evalDate <= Sys.Date())
  {
    print(paste0("Trying to find a valid ranking for ",evalDate ))
    
    #maybe weekend or holiday
    evalDate <- as.character( as.Date(add.day(evalDate, 1),format='%Y-%m-%d'))
    rankingData <- as.data.frame(rankingDataset[rankingDataset$Date == evalDate,])
    
  }
  #divide into Value/Core/Growth
  
  #divide ranking file symbols into bins
 
  rankingData<-rankingData[-1]
  na_values_positions <-  which(is.na(rankingData))
  if(length(na_values_positions)>0){ rankingData<-rankingData[-na_values_positions]}
  
  graterThan80 <- which(rankingData>= 80 )
  growth_symbols <- colnames(rankingData)[graterThan80]
  
   if(!keepValue)
  {
    between_20_80 <- which(rankingData< 80 & rankingData>20)
    core_symbols <- colnames(rankingData)[between_20_80]
    
    lessThan_20 <- which(rankingData<=20 )
    value_symbols <- colnames(rankingData)[lessThan_20]
   
  
  }else{
    value_symbols <- as.character(oldSymbolPortfolioUnitsData[oldSymbolPortfolioUnitsData$Bin =="Value",1])
   
    position = which(growth_symbols %in%  value_symbols)
    if(length(position)>0) growth_symbols = growth_symbols[- position]
    
    position = which(symbolGuidList %in%  c(value_symbols,  growth_symbols))
    if(length(position)>0)  core_symbols <-  symbolGuidList[- position]
    
  }
    
  position = which(!value_symbols %in% symbolGuidList)
  if(length(position)>0) { value_symbols <- value_symbols[-position]}
  position = which(!growth_symbols %in% symbolGuidList)
  if(length(position)>0) { growth_symbols <- growth_symbols[-position]}
  position = which(!core_symbols %in% symbolGuidList)
  if(length(position)>0) { core_symbols <- core_symbols[-position]}
  
  #----------------Create the table of Symbols, their weight, portfolioSymbol.Value and number of units-------------------------------------------------------------------------------------------
  #get the data only for symbols given as input in function
  timeIndexDataSet <- timeIndexDataSet[,c("Date",symbolGuidList) ]
  bin=""
  sumOfValueSymbols = 0
  sumOfCGSymbols = 0
  portfolioValue = 100
  
  if(length(timeIndexDataSet[,1]) > 0 && !is.null(oldSymbolPortfolioUnitsData)){
    
   
    subDataSetValue <-  oldSymbolPortfolioUnitsData[oldSymbolPortfolioUnitsData$Bin =="Value", ]
    currentTimeIndexValueOfValueSymbols <- timeIndexDataSet[1,value_symbols]
  
    
    for(i in seq(2,dim(timeIndexDataSet)[2],1))
    {
      
      unitsFraction = as.numeric(as.character(oldSymbolPortfolioUnitsData[oldSymbolPortfolioUnitsData$SymbolGuid == colnames(timeIndexDataSet)[i], "UnitsFraction"]))
      currentSymbolValue = as.numeric(as.character( timeIndexDataSet[1,i]))
      #print(paste(colnames(timeIndexDataSet)[i], " unitsFraction =" , unitsFraction, "   currentSymbolValue =" ,currentSymbolValue))
      
      if(colnames(timeIndexDataSet[i]) %in% subDataSetValue[,1] )
      {
        sumOfValueSymbols = sumOfValueSymbols + unitsFraction *currentSymbolValue
      }else{
        sumOfCGSymbols = sumOfCGSymbols + unitsFraction *currentSymbolValue
      }
      
      
    }
    
    portfolioValue = sumOfValueSymbols + sumOfCGSymbols
    
  }
  
 
  if(!keepValue){
   
    core_oneSymbolWeight =  (portfolioValue * core_percentage/100)/ length(core_symbols)  
    growth_oneSymbolWeight =  (portfolioValue * growth_percentage/100)/ length(growth_symbols)
    value_oneSymbolWeight =  (portfolioValue * value_percentage/100)/ length(value_symbols)
  }else{
    
    core_oneSymbolWeight = (sumOfCGSymbols * (core_percentage/ (growth_percentage + core_percentage)))/ length(core_symbols)  
    growth_oneSymbolWeight =  (sumOfCGSymbols * (growth_percentage/ (growth_percentage + core_percentage)))/ length(growth_symbols) 
    
  }
  
  #print(paste0("Core w: ",core_oneSymbolWeight, " Value: ",value_oneSymbolWeight, "Growth w :", growth_oneSymbolWeight))
  
  
  
  symbolUnitFractionList <- list()
  symbolList <- list()
  
  symbolPortfolioData <- matrix(ncol = 4)
  colnames(symbolPortfolioData) <- c("SymbolGuid","SymbolTimeIndexValue","UnitsFraction", "Bin")
  

  
  
  if(length(timeIndexDataSet[,1]) > 0)
  {
    for(i in seq(1,length(timeIndexDataSet[1,]),1))
    {
      if(i > 1)
      {
        
        # set up INITIAL Portfolio Symbols UnitsFraction and PortfolioSymbolsValue
        if(!is.na(timeIndexDataSet[1,i]))
        {
          symbolTimeIndexValue = as.double(timeIndexDataSet[1,i])
          
          if(length(value_symbols[value_symbols %in% colnames(timeIndexDataSet)[i]]) >0)
          {
              if(keepValue){
                unitsFraction = as.numeric(as.character(oldSymbolPortfolioUnitsData[oldSymbolPortfolioUnitsData$SymbolGuid == colnames(timeIndexDataSet)[i], "UnitsFraction"]))
                bin = "Value"
               # print(paste("Symbol " , colnames(timeIndexDataSet)[i]," units ", unitsFraction, " Bin Value"))
              }else 
              {
                unitsFraction = value_oneSymbolWeight/ symbolTimeIndexValue  
                bin = "Value"
               # print("-")
              }
          }
          
          if(!is.null(core_symbols) && length(core_symbols[core_symbols %in% colnames(timeIndexDataSet)[i]])>0)
          {
            unitsFraction = core_oneSymbolWeight/ symbolTimeIndexValue  
            bin = "Core"
           # print(paste("Symbol " , colnames(timeIndexDataSet)[i]," units ", unitsFraction, " Bin Core"))
          }
          if(length(growth_symbols[growth_symbols %in% colnames(timeIndexDataSet)[i]])>0)
          {
            unitsFraction = growth_oneSymbolWeight/ symbolTimeIndexValue  
            bin = "Growth"
          }
         
          
          symbolPortfolioData <- rbind(symbolPortfolioData, c( colnames(timeIndexDataSet[i]), 
                                                               formatC(as.numeric(symbolTimeIndexValue), digits = 12, format = "f") , 
                                                               formatC(as.numeric(unitsFraction), digits = 12, format = "f"), bin))
          
          #create a list o symbols and a list of UnitFractions
          symbolUnitFractionList <- c( symbolUnitFractionList, unitsFraction)
          symbolList <- c(symbolList, colnames(timeIndexDataSet[i]))
        }
        
        
      }
    }
    
  }
  
  symbolPortfolioData <- na.omit(symbolPortfolioData)
 
  
  return(symbolPortfolioData)
  
}

#----------------------------------------- Functions for generating quintiles Portfolio with rebalance ---------------------------------------



GenerateRebalancingPortfolioForQuintiles <- function(symbolsTimeIndexfileName, minNumberOfSymbols, rankingFilesName, 
                                                     rankNameForOutputFileNotations, folder, folderMain, workingWith) 
{
  
  folder <- paste0(folderMain,folder,"/")
  #reading benchmark data form file
  
  createIfFolderDontExists(folder)
  #reading ranking data from ranking file
  rankingDataset <- data.frame(read.csv(file =  rankingFilesName, sep=",",header=TRUE,stringsAsFactors=F, fileEncoding="utf-8", check.names=FALSE))
  
  
  date =  firstRowWithEnoughSymbols( rankingDataset, minNumberOfSymbols)
  
  
  #run for at least 1 year
  if( add.month(as.Date(date),1) < as.Date(rankingDataset[dim(rankingDataset)[1] -250,1]))
  {
    
    #this will generate Portfolios starting every month
    fromDate <-  seq(as.Date(date),as.Date(rankingDataset[dim(rankingDataset)[1] -250,1]), by = "month")
    toDate <-  rankingDataset[dim(rankingDataset)[1],1]
    
    
    fromDate <- as.character(fromDate)
    toDate <- as.character(toDate)
    
    
    
     #for eaach Starting date in the vector willgenerate a Portfolio and Summary table
    for(i  in seq(1,length(fromDate),1) )
    {
      
      #portfolio generation
      print(paste0("Generating Portfolio with strarting date: ",fromDate[i]))
      batchName = paste0(fromDate[i],"_to_", toDate,"_", rankNameForOutputFileNotations)
      
      final_summary <- as.data.frame(NA)
      
      
      quintiles = seq(1,100, 1)
      quintileUp = 0
      for(quintile in quintiles)
      {
        
        while (is.na(rankingDataset[rankingDataset$Date == fromDate[i],1 ][1]))
        {
                 fromDate[i]= add.day(fromDate[i],1)
        }
        
        portfolioName <- paste0( quintileUp,"-" ,quintile,"Quintile","_",batchName)
        print(paste0("Generating Portfolio ", portfolioName))
        
        portfolioValues <- GeneratePortfolioEqualWeighted_WithRebalance(fromDate[i], toDate, 
                                                                        rankingDataset,
                                                                        symbolsTimeIndexfileName, 
                                                                        paste0(folder,portfolioName), quintileUp, quintile)
        
        portfolioValues <- as.data.frame(portfolioValues)
        
        colnames(portfolioValues) <- c("Date", "DailyReturn",paste0("P",quintileUp,"-", quintile), "Index")
        
        columns = colnames(final_summary)
        
        final_summary <- cbind(final_summary,portfolioValues[,3])
        colnames(final_summary) <- c(columns,paste0("P",quintileUp,"-", quintile))
        
        
        quintileUp = quintile
        
        
      }
      final_summary[1] <- portfolioValues[1]
      write.csv(final_summary, file = paste0(folder, "Portfolios_TotalValues_Summary_",batchName, ".csv"))
    }
  }
  
  
  
}



GeneratePortfolioEqualWeighted_WithRebalance = function(fromDate, toDate, rankingDataset,
                                                        symbolsTimeIndexfileName, 
                                                        fileAppendix, quintileUp, quintileDown)
{
  
  fileTimeIndexValue <- data.frame(read.csv(file = symbolsTimeIndexfileName, sep=",",header=TRUE,stringsAsFactors=F, fileEncoding="utf-8", check.names=FALSE))
  symbolsTimeIndexValues <- fileTimeIndexValue[fileTimeIndexValue$Date >= fromDate ,]
  
  #column rearange
  symbolGuidList <- colnames(symbolsTimeIndexValues)[2:dim(symbolsTimeIndexValues)[2]]
  
  for( i in symbolGuidList){
    if(any( is.na( symbolsTimeIndexValues[i]))){
      position = which(symbolGuidList == i)
      symbolGuidList <- symbolGuidList[- position]
    }
  }
  
  if(quintileDown <= 2)
  {
    Ptype = "Value"
  }else{
    Ptype = "CoreOrGrowth"
  }
  
  quintileStocks = GetQuintileStocks(rankingDataset, fromDate, quintileDown, quintileUp) 
  
  symbolPortfolioUnitsData <- GenerateUnitsDataSetEW( symbolsTimeIndexValues, quintileStocks, fromDate, 100)
  
  
  symbolPortfolioUnitsData<- as.data.frame(symbolPortfolioUnitsData)
  symbolPortfolioData<- cbind(fromDate, symbolPortfolioUnitsData)
  colnames(symbolPortfolioData) <- c("Inception", colnames(symbolPortfolioUnitsData))
  
  portfolioDailyReturn <- matrix(ncol = 4)
  previousPortfolioValue = 0.0
  symbolUnitsFraction = 0.0
  
  colnames(portfolioDailyReturn)=c( "Date","DailyReturn", "TotalValue", "Index")
  # value rebalance 3 years, growth and core 1 year relanace
  
  ValueRebalanceIndicator <-seq(3, 20,3)
  CGRebalanceIndicators <- seq(1,20,1)
  #CGRebalanceIndicators <- CGRebalanceIndicators[-ValueRebalanceIndicator]
  
  
  # print(symbolPortfolioUnitsData)
  
  if(length(symbolsTimeIndexValues[,1]) > 0)
  {
    noOfRebalances = NULL
    #----------------Create Portfolio Values-------------------------------------------------------------------------------------------
    for(i in seq(1,length(symbolsTimeIndexValues[,1]),1))  
    {
      suma = 0.0  # here will keep the sum of Symbols for each row.
      
      monthsSinceStart = elapsed.months(symbolsTimeIndexValues[i,1], symbolsTimeIndexValues[1,1])
      
      if( ((Ptype == "CoreOrGrowth" && monthsSinceStart %in% (12*CGRebalanceIndicators)) || (Ptype == "Value" && monthsSinceStart %in% (12*ValueRebalanceIndicator)))  &&  ( is.null(noOfRebalances) || !is.element(monthsSinceStart, noOfRebalances ))) 
      {
        #reevaluate the symbols bin after every year
        noOfRebalances <- c(noOfRebalances, monthsSinceStart)
        
        
        #reselect stocks at rebalance time
        quintileStocks = GetQuintileStocks(rankingDataset, symbolsTimeIndexValues[i,1], quintileDown, quintileUp) 
        
        symbolPortfolioUnitsData <- GenerateUnitsDataSetEW( symbolsTimeIndexValues, quintileStocks, symbolsTimeIndexValues[i,1], previousPortfolioValue)
        
        
        symbolPortfolioUnitsData <- as.data.frame(symbolPortfolioUnitsData)
        columnNames <- colnames(symbolPortfolioData)
        symbolPortfolioData<- cbindX(symbolPortfolioData, as.matrix(symbolsTimeIndexValues[i,1]), as.matrix(symbolPortfolioUnitsData))
        colnames(symbolPortfolioData) <- c(columnNames, paste0("Year",monthsSinceStart/12), colnames(symbolPortfolioUnitsData))
        
      
        #print(symbolPortfolioUnitsData)
        #print(paste0("i= " ,i))
      }
      
      
      for(j in seq(1,length(symbolsTimeIndexValues[1,]),1))
      {
        if(j>1 && colnames(symbolsTimeIndexValues[j]) %in% quintileStocks) # needed for dataservice -> && (typeof(symbolsTimeIndexValues[i,j]) == "character") # take only erery second element to be added
        {
          
          symbolUnitsFraction = as.numeric(as.character(symbolPortfolioUnitsData[symbolPortfolioUnitsData$SymbolGuid == colnames(symbolsTimeIndexValues[j]),"UnitsFraction"]))
          if(!is.na(symbolsTimeIndexValues[i,j]))
          {
            suma = suma + as.double(symbolsTimeIndexValues[i,j])* symbolUnitsFraction
          }
        }
      }
      #get Portfolio initial value
      if(i == 1)
      {
        previousPortfolioValue = suma
      }
      
      
      dailyReturn <- (suma-previousPortfolioValue)/previousPortfolioValue*100
      previousPortfolioValue <- suma
      #print(paste0(symbolsTimeIndexValues[i,1], " - ", suma) )
      
      if(!is.na(dailyReturn))
      {
        portfolioDailyReturn<-rbind(portfolioDailyReturn, c( symbolsTimeIndexValues[i,1], formatC(as.numeric(dailyReturn), digits = 12, format = "f") , formatC(as.numeric(suma), digits = 12, format = "f"), convertDateToIndex(symbolsTimeIndexValues[i,1] )))
      }
    }
  }
  
  
  portfolioDailyReturn<-na.omit(portfolioDailyReturn)
  
  #-------------------------------------------------------Save to File Daily Return ---------------------------------------------------------------
  print_portfolioDailyReturn <- as.data.frame(portfolioDailyReturn)
  row.names(print_portfolioDailyReturn)<-NULL
  filename = paste0(fileAppendix,"_DailyReturn",  ".csv")
  write.csv2(print_portfolioDailyReturn , file = filename)
  
  print_symbolPortfolioData <-as.data.frame(symbolPortfolioData)
  row.names(print_symbolPortfolioData)<-NULL
  filename = paste0(fileAppendix, "_Components",  ".csv")
  write.csv2(print_symbolPortfolioData , file = filename ,row.names = FALSE)
  
  return(portfolioDailyReturn)
  
}

GenerateUnitsDataSetEW = function( symbolsTimeIndexValues,  symbolGuidList, evalDate, portfolioTotalValue)
{
  
  
  
  #----------------Create the table of Symbols, their weight, portfolioSymbol.Value and number of units-------------------------------------------------------------------------------------------
  #get the data only for symbols given as input in function
  symbolsTimeIndexValuesOnDate <- symbolsTimeIndexValues[symbolsTimeIndexValues$Date == evalDate ,c("Date",symbolGuidList) ]
  bin=""
  
  numberOfStocks = length(symbolGuidList)
  oneSymbolWeight =  portfolioTotalValue/ numberOfStocks
  
  
  symbolUnitFractionList <- list()
  symbolList <- list()
  
  symbolPortfolioData <- matrix(ncol = 4)
  colnames(symbolPortfolioData) <- c("Date", "SymbolGuid","SymbolTimeIndexValue","UnitsFraction")
  
  
  
  if(length(symbolsTimeIndexValuesOnDate[,1]) > 0)
  {
    for(i in seq(1,length(symbolsTimeIndexValuesOnDate[1,]),1))
    {
      if(i > 1)
      {
        
        # set up INITIAL Portfolio Symbols UnitsFraction and PortfolioSymbolsValue
        if(!is.na(symbolsTimeIndexValuesOnDate[1,i]))
        {
          symbolTimeIndexValue = as.double(symbolsTimeIndexValuesOnDate[1,i])
          unitsFraction = oneSymbolWeight/ symbolTimeIndexValue
          portfolioSymbolValue = unitsFraction * symbolTimeIndexValue
          
          symbolPortfolioData <- rbind(symbolPortfolioData, c(evalDate, colnames(symbolsTimeIndexValuesOnDate[i]), formatC(as.numeric(symbolTimeIndexValue), digits = 12, format = "f") , formatC(as.numeric(unitsFraction), digits = 12, format = "f")))
          
        }
        
      }
    }
    
  }
  
  
  symbolPortfolioData <- na.omit(symbolPortfolioData)
  
  
  return(symbolPortfolioData)
  
}

GetQuintileStocks <- function(rankingDataset, fromDate, quintileDown, quintileUp) {
  startDateRanking = rankingDataset[rankingDataset$Date == fromDate,]
  
  quintileStocks_position = which( startDateRanking<= quintileDown & startDateRanking >  quintileUp)
  quintileStocks = colnames(startDateRanking[,quintileStocks_position])
  if("Date" %in% quintileStocks){ 
    quintileStocks = quintileStocks[-1]
  }
  
  return(quintileStocks)
}




