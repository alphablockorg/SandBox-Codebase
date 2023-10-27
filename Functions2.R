source("Utils.R")
library("dplyr")
source("Functions.R")
source("Summary Code For Continuous Portfolios.R")
source("Drawdown Function.R")

GeneratePortfolioAndMPT_V2 <- function(symbolsTimeIndexfileName, benchmarkTimeIndexFileName, benchmark,
                                    rankingFilesName, rankNameForOutputFileNotations,
                                    folder, folderMain, fromDate, toDate, core_bottom, core_top, value_weight,growth_weight, rebalance =FALSE) 
{
  
  
  folder <- paste0(folderMain,folder,"/")
  #reading benchmark data form file
  benchmarkDataset <-  data.frame(read.csv(file = benchmarkTimeIndexFileName, sep=",",header=TRUE,stringsAsFactors=F, fileEncoding="utf-8", check.names=FALSE))
  if(dim(benchmarkDataset)[2] == 3) {benchmarkDataset = benchmarkDataset[,c(2,3)]}
  benchmarkDataset$Index <- as.Date(benchmarkDataset$Index)
  

  
  createIfFolderDontExists(folder)
  #reading ranking data from ranking file
  rankingDataset <- data.frame(read.csv(file =  rankingFilesName, sep=",",header=TRUE,stringsAsFactors=F, fileEncoding="utf-8", check.names=FALSE))
  
  #finding the first row in the ranking file where {minNumberOfSymbols} have values
  
 
    final_summary <- as.data.frame(NA)
    average_Summary <- as.data.frame(NA)
    k=1
    for(i  in seq(1,length(core_top),1) )
    {
      for(j  in seq(1,length(value_weight),1) )
       {
      
          #reading the ranking data on the day of the begining of the Portfolio
          rankingData <- as.data.frame(rankingDataset[rankingDataset$Date == fromDate,])
          #in case that on the exact day we selected as the begining of the Portoflio, there is no ranking data, we add days until we find a valid date with enough data in the ranking file
          while(dim(rankingData)[1]<1)
          {
            print(paste0("Trying to find a valid ranking for ",fromDate ))
            #maybe weekend or holiday
            fromDate <- as.character( as.Date(add.day(fromDate, 1),format='%Y-%m-%d'))
            rankingData <- as.data.frame(rankingDataset[rankingDataset$Date == fromDate,])
            # toDate <- as.character( as.Date(add.day(toDate, 1),format='%Y-%m-%d'))
          }
          
          benchmarkName = paste0(fromDate,"_to_", toDate,"_",benchmark)
          benchmarkDataset_subset <-  benchmarkDataset[benchmarkDataset$Index >= fromDate & benchmarkDataset$Index <= toDate,]
          benchmarkDataset_subset$Index <- as.Date(benchmarkDataset_subset$Index)
         
        
          rankingData<-rankingData[-1]
          graterThan80 <- which(rankingData>= core_top[i] & rankingData<=100)
          growth_symbols <- colnames(rankingData)[graterThan80]
          
          between_20_80 <- which(rankingData> core_bottom[i] & rankingData<core_top[i])
          core_symbols <- colnames(rankingData)[between_20_80]
          
          lessThan_20 <- which(rankingData<=core_bottom[i] & rankingData>=0)
          value_symbols <- colnames(rankingData)[lessThan_20]
          
            
            
          
          type = paste0("Core",core_bottom[i],"-",core_top[i],"V", value_weight[j],"_G", growth_weight[j])
          portfolioName = paste0(fromDate,"_to_", toDate,"_",type,"Portfolio_", rankNameForOutputFileNotations)
          print(paste0("Generating ", type, "Portfolio for ", portfolioName))
          
          if(rebalance == FALSE)
          {
                    portfolioValues <- GeneratePortfolioUnEqualWeighted_LoadFromFile_WithInputSymbols(fromDate, toDate, 
                                                                                                        value_symbols, 
                                                                                                        value_weight[j],
                                                                                                        core_symbols, 
                                                                                                        growth_symbols,
                                                                                                        growth_weight[j],
                                                                                                        symbolsTimeIndexfileName, 
                                                                                                        paste0(folder,portfolioName))
          }else{
                    portfolioValues <-GeneratePortfolioUnEqualWeighted_WithRebalance(fromDate, toDate, 
                                                                                     rankingDataset, value_weight[j],
                                                                                     growth_weight[j],
                                                                                     symbolsTimeIndexfileName, 
                                                                                     paste0(folder,portfolioName), core_top[i],
                                                                                     core_bottom[i])
          }
          
          
          portfolioValues <- as.data.frame(portfolioValues)
          #rearange the columns
          portfolioValues <- portfolioValues[c("Date","TotalValue", "DailyReturn", "Index")  ]
          #just for  summary code purpose...
          colnames(portfolioValues) <- c("Index", "TotalValue", "RemainingValue", "SymbolsValue")
          
         
          #MPT stats code generator
          summary =  SummaryCodeForContinuousRunningPortfolios(portfolioValues, benchmarkDataset_subset,portfolioName , benchmarkName, folder)
       
          colnames(summary) <- c("", type)
          final_summary <- cbind(final_summary,summary)
          #arrange a temp dataset with the Portfolio results and diff
          {
            temp <- as.matrix( summary[, dim(summary)[2]])
            temp <- rbind(temp,c(1+ 1) )
            colnames(temp) <- type
            
            
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
          if(i == 1 && j ==1)
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
          
          
          
          
        
        
        
        write.csv(average_Summary, file = paste0(folder,"Average_Summary_OnePortfolio_",fromDate,"_",rankNameForOutputFileNotations, ".csv"))
        write.csv(final_summary, file = paste0(folder, "Final_Summary_OnePortfolio_", fromDate,"_",rankNameForOutputFileNotations, ".csv"))
      
    }
  
    }
  
}

