#---------------------- Generate Portfolios and Summary Tables --------------
GeneratePortfolioAndMPT <- function(symbolsTimeIndexfileName, benchmarkTimeIndexFileName, benchmark,minNumberOfSymbols,
                                    rankingFilesName, rankNameForOutputFileNotations,
                                    folder, folderMain, type) 
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
          fromDate <-  seq(as.Date(date),as.Date(rankingDataset[dim(rankingDataset)[1] -250*run_for_years,1]), by = "month")
          toDate <- add.year(add.day(fromDate,1), run_for_years)
          
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
              benchmarkDataset_subset <-  benchmarkDataset[benchmarkDataset$Index >= fromDate[i],]
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
             
             if(toupper(type) == "ALL")
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
              
              
              portfolioValues <- as.data.frame(portfolioValues)
              #rearange the columns
              portfolioValues <- portfolioValues[c("Date","TotalValue", "DailyReturn", "Index")  ]
              #just for  summary code purpose...
              colnames(portfolioValues) <- c("Index", "TotalValue", "RemainingValue", "SymbolsValue")
              
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
              
            
            
            
              if(i == 1)
              {
                average_Summary <- rowMeans(temp, na.rm=TRUE)
                average_Summary <- as.matrix(average_Summary)
                colnames(average_Summary) <- colnames(temp[dim(temp)[2]])
              }else{
                  col_names <- colnames(average_Summary)
                  average_Summary <- cbind(average_Summary, rowMeans(temp, na.rm=TRUE))
                  colnames(average_Summary) <- c(col_names, colnames(temp[dim(temp)[2]]))
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
          portfolioSymbolValue = unitsFraction * symbolTimeIndexValue
          
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

#----------------------Generate Portfolio Mcap Weighted ----------
GeneratePortfolioMcapWeighted = function(fromDate, toDate, 
                                          symbolGuidList, 
                                          mcapDs,
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
  
  if( stringr::str_detect(fileAppendix , "Crypto10") )
  {
      colnames(symbolsTimeIndexValues) <- c("Date",  stringr::str_replace_all(colnames(symbolsTimeIndexValues)[2:dim(symbolsTimeIndexValues)[2]], "[.]", "-") )
  }
  
  
  #----------------Create the table of Symbols, their weight, portfolioSymbol.Value and number of units-------------------------------------------------------------------------------------------
  
  
  # vectorize assign, get and exists functions
  assign_hash <- Vectorize(assign, vectorize.args = c("x", "value"))
  get_hash <- Vectorize(get, vectorize.args = "x")
  exists_hash <- Vectorize(exists, vectorize.args = "x")
  
  #----------------Determine each symbol Weight and UnitsFraction ------------------
  #determine one symbol weight relative to the number of symbols that are going to be part of the P
 
  mcap_sum =sum(as.numeric(mcapDs[,"mcap"]))
 
  mcapDs<- cbind(mcapDs, NA, NA, NA, NA)
  colnames(mcapDs) <- c(colnames(mcapDs[c(1,2)]), "Weight","SymbolValueAtStartDate", "UnitsFraction","PortfolioSymbolsValue" )
  
  mcapDs$Weight <-  (mcapDs[,"mcap"]/mcap_sum) *100
  
  
  #the UnitFraction is calculated at the begining of the P so that is weighted based on Mcap at the Start Date of the Portfolio and will not change
  startPortfolioDate = firstRowWithEnoughSymbols(result , length(symbolGuidList))
  
  startDateValues <- symbolsTimeIndexValues[ symbolsTimeIndexValues$Date == startPortfolioDate,]
  
  #get the data only for symbols given as input in function
 
  for(i in colnames(startDateValues)[-1])
  {
    mcapDs[mcapDs$symbolsList == i, "SymbolValueAtStartDate"] = startDateValues[,i]
  }
  
  mcapDs$UnitsFraction = mcapDs$Weight/mcapDs$SymbolValueAtStartDate
  mcapDs$PortfolioSymbolsValue = mcapDs$UnitsFraction *mcapDs$SymbolValueAtStartDate
  
 
  #----------------Create Portfolio Values-------------------------------------------------------------------------------------------
  
  portfolioDailyReturn <- matrix(ncol = 4)
  previousPortfolioValue = 0.0
  symbolUnitsFraction = 0.0
  
  colnames(portfolioDailyReturn)=c( "Date","DailyReturn", "TotalValue", "Index")
  
  symbolsTimeIndexValues <- symbolsTimeIndexValues[symbolsTimeIndexValues$Date >= startPortfolioDate, ]
  
  rows = length(symbolsTimeIndexValues[,1])
  columns = length(symbolsTimeIndexValues[1,])
  if(rows > 0)
  {
    
    for(i in seq(1,rows,1))  
    {
      suma = 0.0  # here will keep the sum of Symbols for each row(day).
      
      for(j in seq(1,columns,1))
      {
        if(j>1 && colnames(symbolsTimeIndexValues[j]) %in% symbolGuidList) # needed for dataservice -> && (typeof(symbolsTimeIndexValues[i,j]) == "character") # take only erery second element to be added
        {
          symbolUnitsFraction = mcapDs[mcapDs$symbolsList == colnames(symbolsTimeIndexValues[j]) , "UnitsFraction"] 
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
  
  return(portfolioDailyReturn)
  
}
