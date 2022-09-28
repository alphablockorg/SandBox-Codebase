library(lubridate)

convertDateToIndex = function(date)
{
  
  #Tralio code for creating an Index out of Date uses year as reference date : {1/1/0001 12:00:00 AM}
  # R framework suports Date value starting with 1970-01-01
  #In order to duplicate the code into generating a valid Index for Tralio and using the LoadReferencePointByIndex functions
  # I calculated the seconds from 1/1/0001 to 1970-01-01 and adding them to the calculation of the index
  
  date1 = as.Date(as.POSIXlt(date, tz= "EET"), tz= "GMT")
  date2 <- as.Date("1970-01-01")   # this will remain the same date time
  
  secs <- difftime( date1, date2, units = c("secs"))
  
  secs<- secs  + 62135596800
  
  index <- secs/30
  
  return (index)
  
}

#convertDateToIndex("2011-09-21")


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

createIfFolderDontExists = function(folderName)
{
  if (!file.exists(folderName)){
    dir.create(folderName)
  }
}

getRankingFileName = function(rankingToUse, workingWith)
{
  if(workingWith == "S&P100")
  {
    switch (rankingToUse,
            "Q1" = "RankingsS&P100/Q1_Ranking_S&P100.csv",
            "Q2" = "RankingsS&P100/Q2_Ranking_S&P100.csv", 
            "Q3" = "RankingsS&P100/Q3_Ranking_S&P100.csv", 
            "Q4" = "RankingsS&P100/Q4_Ranking_S&P100.csv", 
            "Q5" = "RankingsS&P100/Q5_Ranking_S&P100.csv",
            "Q6" = "RankingsS&P100/Q6_Ranking_S&P100.csv",
            "Q8" = "RankingsS&P100/Q8_Ranking_S&P100.csv",
            "Q12" = "RankingsS&P100/Q12_Ranking_S&P100.csv",
            "Q20" = "RankingsS&P100/Q20_Ranking_S&P100.csv"
    )  
  }else  if(workingWith == "S&P500"){
    switch (rankingToUse,
            "Q1" = "RankingsS&P500/Q1_Ranking_S&P500.csv",
            "Q2" = "RankingsS&P500/Q2_Ranking_S&P500.csv", 
            "Q3" = "RankingsS&P500/Q3_Ranking_S&P500.csv", 
            "Q4" = "RankingsS&P500/Q4_Ranking_S&P500.csv", 
            "Q5" = "RankingsS&P500/Q5_Ranking_S&P500.csv",
            "Q6" = "RankingsS&P500/Q6_Ranking_S&P500.csv",
            "Q8" = "RankingsS&P500/Q8_Ranking_S&P500.csv",
            "Q12" = "RankingsS&P500/Q12_Ranking_S&P500.csv",
            "Q20" = "RankingsS&P500/Q20_Ranking_S&P500.csv"
    ) 
  }else  if(workingWith == "Crypto10"){
    switch (rankingToUse,
            "Q1" = "RankingsCrypto10/Q1_Ranking_Crypto10.csv",
            "Q2" = "RankingsCrypto10/Q2_Ranking_Crypto10.csv", 
            "Q3" = "RankingsCrypto10/Q3_Ranking_Crypto10.csv", 
            "Q4" = "RankingsCrypto10/Q4_Ranking_Crypto10.csv", 
           
    ) 
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
          "Q8" = "8QRank",
          "Q12" = "12QRank",
          "Q20" = "20QRank"
  )  
}

generateRanking = function(dataSet, quarter,dataPointsFor1Quarter, fileNameRanking, fileNameReturns, productName )
{
  folderToSaveRankings = paste0("Rankings", productName,"/")
  createIfFolderDontExists(folderToSaveRankings) # create the folder if it doesn't exist
  fileNameRanking = paste0(folderToSaveRankings,"Q",quarter,"_Ranking_",productName,".csv")
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