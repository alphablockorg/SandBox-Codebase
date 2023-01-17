### SUMMARY CODE
###pay attention at: f, c, dateanalysis, endPoints, etc.
library(xts)
library(zoo)
library(dplyr)
source("Drawdown Function.R")


SummaryCodeForContinuousRunningPortfolios = function(portfolioValues, benchmarkDataset, portfolioName, benchmarkName, folder = NULL)
{
  
  ### SUMMARY CODE
  ###pay attention at: f, c, dateanalysis, endPoints, etc.
  
  {
    
    
    # read data according to the format available on Tralio/DownloadSection/PortfolioValuesDataSet
    
    portfolioValues[,1] <- (as.Date(as.POSIXct(portfolioValues[,1])))
    portfolioStarDate <-  portfolioValues[1,1] 
    portfolioEndDate <- portfolioValues[dim(portfolioValues)[1],1] 
    year_start <- get.year(portfolioStarDate)
    year_end <- get.year(portfolioEndDate)
    
    running_months <- elapsed.months(portfolioEndDate, portfolioStarDate)
    
    if(running_months >=12)
    {
      months = running_months %% 12
      if(months >0 )
      {
        years <- (running_months - months)/12
      }else{
        years <- running_months / 12
      }
    }else{
      months = running_months
      years = 0
      running_years = 0
    }
    
    if(years >0 && years <= 4){running_years <- seq(1, years, 1)}
    if(years > 4 && years <10){running_years <- c(1,2,3, 5)}
    if(years >= 10 ){running_years <- c(1,2,3, 5, 10)}
    
    
    ### rescaling of the benchmark
    portfolio_benchmark<- inner_join(portfolioValues[,1:2], benchmarkDataset)
    
    #------------- Indexing the Benchmark Value with 100 on the First Day of The Portfolios ------------------
    
    indexedBench <- portfolio_benchmark[,c(1,3)]  #taking the data and the benchmark value at that data
    indexedBench[1,3] = 100  #adding one column for indexing
    
    for(i in seq(1,dim(indexedBench)[1]-1,1))
    {
      indexedBench[i+1,3]=as.numeric(indexedBench[1,3])/as.numeric(indexedBench[1,2])*as.numeric(indexedBench[i+1,2])           #calculates 100/bench(i)*bench(i+1)
    }
    
    #---------------- Porfolio and Benchmark in one DataSet --------------------------------------------
    temp = cbind(portfolio_benchmark[,1:2], indexedBench[,3])
    temp <- as.xts(temp,order.by=temp[,1])   
    
    temp <- temp[, 2:3]
    
    temp=na.locf(temp, fromLast=TRUE)                           #generic function for replacing each NA with the most recent non-NA prior to it
    temp=na.locf(temp, fromFirst=TRUE)
    portfolioValuesDataSet = temp
    colnames(temp) = c(portfolioName,benchmarkName)
    
    
    temp = portfolioValuesDataSet
    lastRow<-dim(temp)[1]
  
    as.numeric(index(temp)[lastRow]-index(temp))->f
    # an array of numbers holding the difference in Days between last Date of the Portfolio minus FirstDate, minus SecondDate, and so on 
    dailyDifferenceInDateRelativeToStartDate<- as.numeric(index(temp)[lastRow]-index(temp))
    #dailyDifferenceInDateRelativeToStartDate=c(28:0)
    #your.numbers=c(365, 365*2,365*3,365*5,365*10,dailyDifferenceInDateRelativeToStartDate[1])                 #time parameter: 1y, 3y, 5y, 10y
    if(length(running_years) >1)
    {
      your.numbers = c(running_years * 365,dailyDifferenceInDateRelativeToStartDate[1])
      columnNames <- c("",paste0(running_years, " Years"),
                        paste(format(portfolioValues[,1][1], "%Y-%m"),"-",format(portfolioEndDate,"%Y-%m"),sep=""))
                       
      supermat = data.frame(matrix(NA, nrow = 24 ,ncol = length(your.numbers)+1 ))
      #colnames(supermat)=c("","1 Year","2 Years","3 Years","5 Years","10 Years",paste(format(portfolioValues[,1][1], "%Y"),"-",year_end,sep="") )
      colnames(supermat) <- columnNames
    }else{
      your.numbers = c(running_years * 365 )
      if(months > 0 && your.numbers >0) {
        your.numbers <- c(your.numbers, dailyDifferenceInDateRelativeToStartDate[1])
        supermat=data.frame(matrix(NA,nrow=24,ncol=3))
        colnames(supermat) = c("",paste(format(portfolioEndDate, "%Y-%m"),"-",format(add.month(portfolioEndDate,-12),"%Y-%m"),sep=""),
                             # paste(format(add.month(portfolioValues[,1][1],12), "%Y-%m"),"-",format(portfolioEndDate,"%Y-%m"),sep=""), 
                              paste(paste(format(portfolioValues[,1][1], "%Y-%m"),"-",format(portfolioEndDate,"%Y-%m"),sep="")) )
      }
      else{
        your.numbers = elapsed.days(portfolioValues[,1][1], portfolioEndDate)
        supermat=data.frame(matrix(NA,nrow=24,ncol=2))
        #colnames(supermat)=c("","1 Year","2 Years","3 Years","5 Years","10 Years",paste(format(portfolioValues[,1][1], "%Y"),"-",year_end,sep="") )
        colnames(supermat)=c("",paste(format(portfolioValues[,1][1], "%Y-%m"),"-",format(portfolioEndDate,"%Y-%m"),sep="") )
      }
    }
    
    
    row_name = c(portfolioName,benchmarkName)
    supermat[,1]=c("Annualized Returns",row_name,"Volatility",row_name,"Tracking Error",row_name,
                   "Information Ratio",row_name,"Alpha Stat",row_name,"Beta Stat",row_name,"R-Squared",row_name,"Max Drawdown",row_name)
    
    for(mm in seq(1,length(your.numbers),1))
    {
      your.number = your.numbers[mm]
      temp = portfolioValuesDataSet
      
      #print(paste0("mm = ", mm, " My numbers " , length(your.numbers)))
      
      b = 0
    
      position = which(abs(dailyDifferenceInDateRelativeToStartDate - your.number) == min(abs(dailyDifferenceInDateRelativeToStartDate - your.number)))
      dateanalysis = index(temp)[position]
      
      as.numeric(index(temp)[lastRow] - dateanalysis)->c
      b <- (as.numeric(if.any.returns(dailyDifferenceInDateRelativeToStartDate,c)))
      
      dailySeriesForConsideredPeriod = temp[b:nrow(temp),]   # before the endpoint on month
    
      
      endPoints <- endpoints(dailySeriesForConsideredPeriod, on = "months", k = 1)
      temp1 <- dailySeriesForConsideredPeriod[endPoints[2:(length(endPoints)-1)],]
      
      #m=temp
      #monthlyTotalValue=temp1
      
      write.csv(temp1,"temp1.csv",row.names=FALSE)
      write.csv(dailySeriesForConsideredPeriod,"temp.csv",row.names=FALSE) 
      monthlyTotalValue = read.csv("temp1.csv")
      m = read.csv("temp.csv")
      m = rbind(rep(100,ncol(m)),m)
      
      monthlyReturn = data.frame(matrix(NA,nrow=nrow(monthlyTotalValue)-1,ncol=ncol(monthlyTotalValue)))
      for(j in seq(1,ncol(monthlyTotalValue),1))
      {
        for(i in seq(2,nrow(monthlyTotalValue),1))
        {
          monthlyReturn[i-1,j]=((monthlyTotalValue[i,j]/monthlyTotalValue[i-1,j])-1)*100
        }
        
      }
      
      smat = data.frame(matrix(0,nrow=9,ncol=ncol(m)))
      row.names(smat)=c("Ann. Ret","Ann. Vol","TE","IR","Alpha","Beta","R2","Cash","MATE")           #Mean Adjusted Tracking Error
      colnames(smat)=colnames(dailySeriesForConsideredPeriod)
      
      for(i in seq(1,ncol(smat),1))
      { 
        lastDayValue = as.numeric(dailySeriesForConsideredPeriod[nrow(dailySeriesForConsideredPeriod),i])
        firstDayValue = as.numeric(dailySeriesForConsideredPeriod[1,i])
        lastDate = index(dailySeriesForConsideredPeriod)[nrow(dailySeriesForConsideredPeriod)]
        firstDate = index(dailySeriesForConsideredPeriod)[1]
        smat[1,i] = round(100*(((lastDayValue/firstDayValue)^(365/as.numeric((lastDate - firstDate))) )-1),3)
      }
      
      # different way of calculating Volatility : similar to Tralio/Admin
      #sdtempNEW=sd(diff(log((as.numeric(temp[,1])))))*sqrt(252)*100
      
      annualizedStandardDeviation = round(apply(monthlyReturn,2,sd)*sqrt(12),2)
      smat[2,] = annualizedStandardDeviation
      #smat[8,]=round(c(mean((as.numeric(portfolioValues[,3])/as.numeric(portfolioValues[,4]))*100),rep(0,ncol(monthlyReturn)-1)),3)
      
      monthlyDifferenceBtwPortfolioAndBench = monthlyReturn[,1:ncol(monthlyReturn)]-monthlyReturn[,2]
      smat[3,]=round(apply(monthlyDifferenceBtwPortfolioAndBench,2,sd)*sqrt(12),2)
      smat[3,2]=0
      
      mate = monthlyDifferenceBtwPortfolioAndBench^2
      smat[9,]=round(sqrt(colMeans(mate))*sqrt(12),2)
      
      smat[4,] = round(c(as.numeric(smat[1,1])-as.numeric(smat[1,2:ncol(smat)]))/as.numeric(smat[3,]),2)
      smat[4,2]=0
      
    
      for(i in seq(2,ncol(monthlyReturn),1))
      {
      
        yy = monthlyReturn[,1]
        xx = monthlyReturn[,i]
        model = lm(yy~xx)
        
        smat[5,i]=round(model$coef[1],2)
        smat[6,i]=round(model$coef[2],2)
        smat[7,i]=round(summary(model)$adj.r.squared*100,2) 
      }
      
      j=2
      supermat[(2:(j+1)),(mm+1)]=as.numeric(smat[1,1:ncol(smat)])
      j=j+3
      
      supermat[(j:(j+1)),(mm+1)]=as.numeric(smat[2,1:ncol(smat)])
      j=j+3
      
      supermat[(j:(j+1)),(mm+1)]=(as.numeric(smat[3,1:ncol(smat)]))
      j=j+3
      
      supermat[(j:(j+1)),(mm+1)]=(as.numeric(smat[4,1:ncol(smat)]))
      j=j+3
      
      supermat[(j:(j+1)),(mm+1)]=rev(as.numeric(smat[5,1:ncol(smat)]))
      j=j+3
      
      supermat[(j:(j+1)),(mm+1)]=rev(as.numeric(smat[6,1:ncol(smat)]))
      j=j+3
      
      supermat[(j:(j+1)),(mm+1)]=rev(as.numeric(smat[7,1:ncol(smat)]))
      j=j+3
      jsupermat=j
      
      
      portfolio2=data.frame(matrix(NA,nrow=nrow(dailySeriesForConsideredPeriod),ncol=2))
      portfolio2[,1]<-index(dailySeriesForConsideredPeriod)
      portfolio2[,2]=as.numeric(dailySeriesForConsideredPeriod[,1])
      portfolio_values_per_period = portfolio2[,c(1,2)] 
      maxdrop1=10
      if(mm == length(your.numbers) && length(your.numbers)> 3) daport=drawdown.analysis(portfolio_values_per_period,20)  else daport=drawdown.analysis(portfolio_values_per_period,2)
      portfolio2=data.frame(matrix(NA,nrow=nrow(dailySeriesForConsideredPeriod),ncol=2))
      portfolio2[,1]<-index(dailySeriesForConsideredPeriod)
      portfolio2[,2]=as.numeric(dailySeriesForConsideredPeriod[,2])
      benchmark_values_per_period = portfolio2[,c(1,2)] 
      dabench=drawdown.analysis(benchmark_values_per_period,2) 
      matbench=dabench[[1]]
      matport=daport[[1]]
      
      
      if(length(matport) >1)
      {
        for(i in seq(1,nrow(matport),1))
        {
          for(j in seq(1,2,1))
          {
            matport[i,j]=as.character(matport[i,j])
            matport[i,j]=gsub(pattern="/-",replacement="/",x=matport[i,j])
          }
        }
        
        
        #computing max drawdown for the benchmark
        write.csv(matbench,"matbench.csv",row.names=F)
        matbench<-data.frame(read.table("matbench.csv",sep=",",header=TRUE,stringsAsFactors=F))# 
        colnames(matbench)=c("From","To","Days","Max Drawdown")
        j=jsupermat+1
        supermat[j,(mm+1)]= min(round(matbench[,4],4))
        
        #computing max drawdown for the portfolio
        write.csv(matport,"matport.csv",row.names=F)
        matport<-data.frame(read.table("matport.csv",sep=",",header=TRUE,stringsAsFactors=F)) 
        colnames(matport)=c("From","To","Days","Max Drawdown")
        j=j-1
        
        supermat[j,(mm+1)]= min(round(matport[,4],4))
        
        if(mm==length(your.numbers))
        {
          #print("here at ploting")
          if(!is.null(folder))
          {
            pdf(paste0(folder,portfolioName," and ",benchmarkName,"Draw down Curve.pdf",sep=""),width=7,height=5)
          }else{
            pdf(paste(portfolioName," and ",benchmarkName,"Draw down Curve.pdf",sep=""),width=7,height=5)  
          }
          
          plot.ts(dabench[[2]],col=2,axes=F,main="Drawdown Plot",ylab="Drawdown Level",xlab="Time",ylim=c(min(min(dabench[[2]]),min(daport[[2]]))*(1+0.2),0))
          lines(daport[[2]],col=4)
          axis(2)
          l=seq(1,nrow(portfolio2),20)
          axis(1,at=l,labels=portfolio2[l,1],cex.axis=1)
          legend("bottomleft",legend=c(portfolioName,benchmarkName),lty=c(1,1),lwd=c(2,2),col=c("blue","red"),bty="n",cex=1)
          dev.off()
          dd=cbind(daport[[2]],dabench[[2]])
          colnames(dd)=c(portfolioName,benchmarkName)
          
          if(!is.null(folder))
          {
            write.csv(dd,paste0(folder,portfolioName," and ",benchmarkName," Drawdown",".csv",sep=""),row.names= index(dailySeriesForConsideredPeriod))
          }else
          {
            write.csv(dd,paste(portfolioName," and ",benchmarkName," Drawdown",".csv",sep=""),row.names= index(dailySeriesForConsideredPeriod))
          }
          
          # if(supermat[2,5]==supermat[2,6] && supermat[3,5]==supermat[3,6] ) supermat=supermat[,-5]
          # if(supermat[2,4]==supermat[2,5] && supermat[3,4]==supermat[3,5] ) supermat=supermat[,-5]
          #write.csv(supermat,paste(portfolioName," and ",benchmarkName," Summary",".csv",sep=""),row.names=F)
          
        }
        supermat[9,2:ncol(supermat)]=rep('-',length(your.numbers))
        supermat[12,2:ncol(supermat)]=rep('-',length(your.numbers))
        supermat[15,2:ncol(supermat)]=rep('-',length(your.numbers))
        supermat[18,2:ncol(supermat)]=rep('-',length(your.numbers))
        supermat[21,2:ncol(supermat)]=rep('-',length(your.numbers))
      }
    }
    
    
    
  }
  #View(supermat)
  if(!is.null(folder))
  {
    write.csv(supermat,paste0(folder,portfolioName," Summary Table ",portfolioValues[,1][length(portfolioValues[,1])],".csv",sep=""), row.names = FALSE,  na = "")
  }else{
    write.csv(supermat,paste(portfolioName," Summary Table ",portfolioValues[,1][length(portfolioValues[,1])],".csv",sep=""), row.names = FALSE,  na = "")  
  }
  
  ### PORTFOLIO VS BENCHMARK CHART
  
  {
    
   
    
    aaa<-portfolioValues 
    #aaa=aaa[-which(duplicated(aaa[,1])),]
    bbb<-benchmarkDataset 
    bbb<-bbb[(dim(bbb)[1]-dim(aaa)[1]+1):dim(bbb)[1],1:2] 
    bbb[1,3]=100 
    for(i in seq(1,dim(bbb)[1]-1,1)){   
      bbb[i+1,3]=as.numeric(bbb[1,3])/as.numeric(bbb[1,2])*as.numeric(bbb[i+1,2]) 
    } 
    m<-data.frame(matrix(NA,nrow=dim(aaa)[1],ncol=6))  
    m[,1]<-aaa[,1] #dates go in 
    m[,2]<-aaa[,2] 
    m[,3]=bbb[,3] 
    m[,4]=aaa[,2] 
    m[,5]=c(0) 
    m[,6]=bbb[,2] 
    colnames(m)<-c("Date","Portfolio","Benchmark","Portfolio NonIndexed","Cash","Benchmark NonIndexed") 
    
    m[,1]<-as.Date(as.character(m[,1])) #sets the first columns to date format 
    x<-as.xts(m[,2:6],order.by=m[,1])#converts the data to xts format 
    k<-dim(x) #dimensions of x 
    Portfolio<-data.frame(Date=m[,1],as.numeric(x[,1]),as.numeric(x[,2])) #data frame of portfolio=cash 
    
    library(ggplot2)
   
    if(!is.null(folder))
    {
      fileName = paste0(folder,portfolioName," and ",benchmarkName,"PORTFOLIO VS BENCHMARK CHART.png",sep="")
    }else{
      fileName = paste(portfolioName," and ",benchmarkName,"PORTFOLIO VS BENCHMARK CHART.png",sep="")
    }
    png(fileName, width=3000,height=1000)
    
    colnames(Portfolio)[2:3]<-c(portfolioName,benchmarkName)
    f<-ggplot(data=Portfolio,aes(Date))+geom_line(aes(y=Portfolio[,2],col=colnames(Portfolio)[2]))+geom_line(aes(y=Portfolio[,3],col=colnames(Portfolio)[3]))+geom_abline(intercept = 1000000, slope = 0)
    f<-f+xlab("Date")+ylab("Value")
    f<-f+theme(axis.text=element_text(size=15), 
               axis.title=element_text(size=15),
               legend.text=element_text(size=18), legend.position = "bottom")
    f<-f+scale_colour_manual(name="",breaks = c(colnames(Portfolio)[2],colnames(Portfolio)[3] ), values = c( "dodgerblue1","#E54B4B"))
    #f<-f+ggplot()+geom_line(data=Portfolio,aes(x=Date,y=Cash,col="Cash"))
    print(f)
    
    dev.off()
  }
  
  #View(supermat)
  if(!is.null(folder))
  {
    write.csv(supermat,paste0(folder, portfolioName," Summary Table ",portfolioValues[,1][length(portfolioValues[,1])],".csv",sep=""), row.names = FALSE,  na = "")
  }else{
    write.csv(supermat,paste(portfolioName," Summary Table ",portfolioValues[,1][length(portfolioValues[,1])],".csv",sep=""), row.names = FALSE,  na = "")  
  }
  
  file.remove( "matport.csv")
  file.remove("matbench.csv")
  file.remove("temp.csv")
  file.remove("temp1.csv")
  return(supermat)
}

if.any.returns = function(a,b)
{
  temp=0
  while(temp==0)
  {
    for(i in seq(length(a),1,-1))
    {
      if(a[i]==b) 
      {
        #print(a[i])
        temp=i
        break
      }
    }
    b=b+1
  }
  return(temp)
}