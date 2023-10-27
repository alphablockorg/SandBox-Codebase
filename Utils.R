library(lubridate)

convertDateToIndex = function(date)
{
  
  date1 = as.Date(as.POSIXlt(date, tz= "EET"), tz= "GMT")
  date2 <- as.Date("1970-01-01")   # this will remain the same date time
  
  secs <- difftime( date1, date2, units = c("secs"))
  
  secs<- secs  + 62135596800
  
  index <- secs/30
  
  return (index)
  
}

convertFromIndexToDate = function(index)
{
  date <- as.character((as.Date(as.POSIXct("01.01.0001 3:00:00",format = "%d.%m.%Y %H:%M:%S") + (as.numeric(index) *30),format="%m/%d/%Y")))
  return(date)
}

rrank<-function(a){
  no_of_stocks = length(a)
  return(round(rank(a, ties.method = "min", na.last=NA),digits = 2)/(no_of_stocks/100))
}

add.year = function(date, nr.years)
{
  date_1y_fwd     <- as.POSIXlt(date)
  date_1y_fwd$year <- date_1y_fwd$year + nr.years
  return(date_1y_fwd)
  
}

add.month =function(date, nr.months)
{
  
  date_verify <- as.POSIXlt(date)
  date_1m_fwd     <- as.POSIXlt(date)
  date_1m_fwd$mon <- date_1m_fwd$mon + nr.months
  
  new_month <- as.numeric(format(date_1m_fwd, "%m"))
  year1 <- as.numeric(format(date_1m_fwd, "%Y"))
  year2 <- as.numeric(format(date_verify, "%Y"))
  
  if( year1 == year2)
  {
    while( new_month - month(date_verify) != nr.months )
    {
      date_1m_fwd$mday <- date_1m_fwd$mday -1 
      new_month <- as.numeric(format(date_1m_fwd, "%m"))
      
    }
  }
  
  return(date_1m_fwd)
}
add.day = function(date, nr.days)
{
  date_1d_fwd     <- as.POSIXlt(date)
  date_1d_fwd$mday <- date_1d_fwd$mday + nr.days
  return(date_1d_fwd)
}
get.year = function(date)
{
  formatedDate  <- as.POSIXlt(date)
  
  return(as.numeric(format(formatedDate, "%Y")))
}  
firstRowWithEnoughSymbols = function( dataSet, nrOfSymbols)
{
  enough = FALSE
  i = 0
  while(!enough)
  {
    suma =  sum(!is.na(dataSet[i,]))
    
    if(suma > nrOfSymbols){    enough = TRUE}
    i = i + 1
  }
  return(as.character( as.Date(dataSet[i,1],format='%Y-%m-%d')) )
}

elapsed.months <- function(end_date, start_date) {
  ed <- as.POSIXlt(end_date)
  sd <- as.POSIXlt(start_date)
  12 * (ed$year - sd$year) + (ed$mon - sd$mon)
}

elapsed.days <- function(end_date, start_date) {
  ed <- as.POSIXlt(end_date)
  sd <- as.POSIXlt(start_date)
  return(difftime(sd,ed,units = c("days")))
}

createIfFolderDontExists = function(folderName)
{
  if (!file.exists(folderName)){
    dir.create(folderName)
  }
}

getRankingFileName = function(rankingToUse, workingWith)
{
  if(workingWith == "S&P100" || workingWith == "S&P500" || workingWith == "TSX60" || workingWith == "BSE100" || workingWith == "Indonesia30" || workingWith == "Crypto10" || workingWith =="India29"|| workingWith == "Crypto30")
  {
    rankingToUse = paste0("Rankings",workingWith, "/", rankingToUse, "_Ranking_", workingWith,".csv")
    
  }else {
    stop("Must input a valid name in workingWith valiable!")
  }
}
getRankingOutputFile = function(rankingToUse)
{
  switch (rankingToUse,
          "Q1" = "1QRank",
          "Q2" = "2QRank", 
          "Q3" = "3QRank", 
          "Q4" = "4QRank", 
          "Q5" = "5QRank",
          "Q6" = "6QRank",
          "Q7" = "7QRank",
          "Q8" = "8QRank",
          "Q12" = "12QRank",
          "Q20" = "20QRank"
  )  
}

generateRanking = function(dataSet, quarter,dataPointsFor1Quarter, productName, fileNameRanking = NULL, fileNameReturns= NULL )
{
  folderToSaveRankings = paste0("Rankings", productName,"/")
  createIfFolderDontExists(folderToSaveRankings) # create the folder if it doesn't exist
  if(is.null(fileNameRanking))
    fileNameRanking = paste0(folderToSaveRankings,"Q",quarter,"_Ranking_",productName,".csv")
  if(is.null(fileNameReturns))
    fileNameReturns = paste0(folderToSaveRankings,"Q",quarter,"_Returns_",productName,".csv")
  
  
  colNumbers = dim(dataSet)[2]
  rowNumbers = dim(dataSet)[1]
  
  Q_Ranking <- data.frame(matrix(nrow = rowNumbers, ncol = colNumbers))
  colnames(Q_Ranking) <-colnames(dataSet)
  Q_Ranking$Date <- dataSet[,1] 
  Q_Returns <- Q_Ranking
  
  for ( rowIndex in seq(1,dim(dataSet)[1],1)) 
  {
    currentDate = as.Date(dataSet[rowIndex,1])
    print(currentDate)
    if(rowIndex > dataPointsFor1Quarter * quarter)
    {
      
      interval =  as.character( as.Date(add.month(currentDate, quarter*(-1)*3 ),format='%Y-%m-%d'))
      row = dataSet[dataSet$Date == interval ,]
      
      # in case there is no data, go to the first row with data
      while(dim(row)[1]<1)
      {
        interval = as.character( as.Date(add.day(interval, -1),format='%Y-%m-%d'))
        row = dataSet[dataSet$Date == interval ,]
      }
      
      for(columnIndex in seq(2,colNumbers,1 ))
      {
        startSymbolValue = as.numeric(as.character(row[1 ,columnIndex]))
        currentSymbolValue = as.numeric(as.character( dataSet[rowIndex,columnIndex]))
        if(!is.na(startSymbolValue) && !is.na(currentSymbolValue))
        {
          symbolRetrunForInterval <- (currentSymbolValue - startSymbolValue)/startSymbolValue *100
          Q_Returns[rowIndex, columnIndex] <- symbolRetrunForInterval
        }
      }
      
      #get the position of the not NA values
      notNulls <- which(Q_Returns[rowIndex, ]!="NA")
      #calculates the ranking to 100 for the elements fount at positions notNulls, and saves them in the Ranking dataset
      #starts from 2 because first column is the date
      if(length(notNulls) > 2)
      {
        Q_Ranking[rowIndex, notNulls[2:length(notNulls)]] <- as.data.frame(t(apply(Q_Returns[rowIndex, notNulls[2:length(notNulls)]], 1,rrank)))
      }
    }
  }
  
  write.csv(Q_Ranking, fileNameRanking, row.names=FALSE)
  write.csv(Q_Returns, fileNameReturns, row.names=FALSE)
  print(paste0("Files saved for ", fileNameRanking))
}


#--------------------  Fill Gaps in TimeSeries -------------------------------------

fillGapsInTimeIndex = function(timeSeriesDS)
{
  
  fillGapsReturnTable <- matrix(nrow=dim(timeSeriesDS)[1])
  i = 2 # first column should alwasy be the date
  
  #for each column, except first(Index)
  for(j in seq(2, dim(timeSeriesDS)[2],1))
  {
    #build a pair of Index and SymbolGuid table and attachet to a commun one
    indexInterval <- matrix(timeSeriesDS[,1])
    one_symbol_timeIndex <-  matrix(timeSeriesDS[,j])
    
    
    
    fillGapsReturnTable <- cbind(fillGapsReturnTable, fillGapsInTimeIndex_OneSymbol(indexInterval, one_symbol_timeIndex, colnames(timeSeriesDS)[j] ) )
    colnames(fillGapsReturnTable)[i+1] <- colnames(timeSeriesDS)[j]
    colnames(fillGapsReturnTable)[i] <- c(colnames(timeSeriesDS)[1])
    i<- i + 2
    print(j)
    
  }
  
  #minus first column that was emplty
  result <- data.frame(fillGapsReturnTable[,-1])
  colnames(result) <- colnames(fillGapsReturnTable)[-1]
  
  result <- result[,c(1,seq(2,dim(result)[2],2))]
  
  return(result)
  
}

fillGapsInTimeIndex_OneSymbol = function (indexInterval, one_symbol_timeIndex, symbolGuid )
{
  
  one_symbol_timeIndex_gapsfilled <- matrix(nrow = dim(indexInterval)[1] ,ncol = 2)
  # colnames(one_symbol_timeIndex_gapsfilled) <- c("Index", symbolGuid)
  values <- na.omit(one_symbol_timeIndex)
  firstFoundValue <- values[1,1]
  firstFoundIndex <- which(!is.na(one_symbol_timeIndex))[1]
  firstFoundIndex_IndexValue <-  indexInterval[firstFoundIndex,1]
  j <- firstFoundIndex
  
  for(i in seq(1,dim(indexInterval)[1],1))
  {
 
      
      if(is.na(one_symbol_timeIndex[i,1]) && i > firstFoundIndex )
      {
        
        one_symbol_timeIndex_gapsfilled[i,1] <- indexInterval[i,1]
        one_symbol_timeIndex_gapsfilled[i,2] <- one_symbol_timeIndex_gapsfilled[i-1,2]
        
      }
      else
      {
        
        one_symbol_timeIndex_gapsfilled[i,1] <- indexInterval[i,1]
        one_symbol_timeIndex_gapsfilled[i,2] <- one_symbol_timeIndex[i,1]
        
      }    
      
    
    
  }
  
  return(one_symbol_timeIndex_gapsfilled)
  
}