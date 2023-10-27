#converts a TimeIndex file data into a R formated data
#date column must be named Index, or change $Index to $YourDateColumnName

fileName = "S&P500_Benchmark_Index.csv"


timeIndexDataSet <-  data.frame(read.csv(file = fileName, sep=",",header=TRUE,stringsAsFactors=F, fileEncoding="utf-8", check.names=FALSE))

timeIndexDataSet$Index = as.character( as.Date( timeIndexDataSet$Index,format='%m/%d/%Y'))
write.csv(timeIndexDataSet, fileName, row.names = FALSE )
