### SUMMARY CODE
###pay attention at: f, c, dateanalysis, ep, etc.
library(xts)
library(zoo)
library(dplyr)
source("Drawdown Function.R")
"
PortfolioValuesDataSet <- VCG_portfolioValues
BenchmarkValuesDataSet <- benchmarkDataset
PortfolioName <- portfolioName 
BenchmarkName <- benchmarkName
"

SummaryCode = function(PortfolioValuesDataSet, BenchmarkValuesDataSet, PortfolioName, BenchmarkName, folder = NULL)
{
  
  ### SUMMARY CODE
  ###pay attention at: f, c, dateanalysis, ep, etc.
  
  {
    
    
    # read data according to the format available on Tralio/DownloadSection/PortfolioValuesDataSet
    
    pfvalue <-  PortfolioValuesDataSet
    pfvalue[,1] <- (as.Date(as.POSIXct(pfvalue[,1])))
    portfolioStarDate <-  pfvalue[1,1] 
    portfolioEndDate <- pfvalue[dim(pfvalue)[1],1] 
    year_start <- get.year(portfolioStarDate)
    year_end <- get.year(portfolioEndDate)
    
    if(year_end - year_start <= 4){running_years <- seq(1, year_end - year_start, 1)}
    if(year_end - year_start > 4 && year_end - year_start <10){running_years <- c(1,2,3, 5)}
    if(year_end - year_start >= 10 ){running_years <- c(1,2,3, 5, 10)}
    
    
    bench<-BenchmarkValuesDataSet
    
    
    ### rescaling of the benchmark
    
    portfolio_benchmark<- inner_join(pfvalue[,1:2], bench)
    
    "  p.match = match(as.Date(pfvalue[,1]),as.Date(bench[,1]))                       #determines the common date span
    p.match = na.omit(p.match )    
    temp = pfvalue[p.match,c(Index, TotalValue)]  
    
    d.match = match(as.Date(bench[,1]),as.Date(temp[,1]))                       #determines the common date span
    
    d.match=na.omit(d.match)                                    #if(!is.na(d.match))  / omits the extra data points
    bench=bench[d.match,]                                       #the new bench variable has the same data range with the portfolio value variable
   "
    
    indexedBench <- portfolio_benchmark[,c(1,3)]  #taking the data and the benchmark value at that data
    indexedBench[1,3] = 100  #adding one column for indexing
    
    for(i in seq(1,dim(indexedBench)[1]-1,1))
    {
      indexedBench[i+1,3]=as.numeric(indexedBench[1,3])/as.numeric(indexedBench[1,2])*as.numeric(indexedBench[i+1,2])           #calculates 100/bench(i)*bench(i+1)
    }
    
    temp = cbind(portfolio_benchmark[,1:2], indexedBench[,3])
    temp <- as.xts(temp,order.by=temp[,1])   
    
    temp <- temp[, 2:3]
    
    
    
    temp=na.locf(temp, fromLast=TRUE)                           #generic function for replacing each NA with the most recent non-NA prior to it
    temp=na.locf(temp, fromFirst=TRUE)
    tempmain=temp
    colnames(temp)=c(PortfolioName,BenchmarkName)
    
    rf=1
    temp=tempmain
    ll<-dim(temp)[1]
    as.numeric(index(temp)[ll]-index(temp))->f
    #f=c(28:0)
    #your.numbers=c(365, 365*2,365*3,365*5,365*10,f[1])                 #time parameter: 1y, 3y, 5y, 10y
    if(length(running_years) >1)
    {
      your.numbers = c(running_years * 365 ,f[1])
      
      supermat=data.frame(matrix(NA,nrow=24,ncol=length(your.numbers)+1))
      #colnames(supermat)=c("","1 Year","2 Years","3 Years","5 Years","10 Years",paste(format(pfvalue[,1][1], "%Y"),"-",year_end,sep="") )
      colnames(supermat)=c("",paste0(running_years, " Years"),paste(format(pfvalue[,1][1], "%Y"),"-",year_end,sep="") )
    }else{
      your.numbers = c(running_years * 365 )
      supermat=data.frame(matrix(NA,nrow=24,ncol=2))
      #colnames(supermat)=c("","1 Year","2 Years","3 Years","5 Years","10 Years",paste(format(pfvalue[,1][1], "%Y"),"-",year_end,sep="") )
      colnames(supermat)=c("",paste(format(pfvalue[,1][1], "%Y"),"-",year_end,sep="") )
      
    }
    
    
    rname=c(PortfolioName,BenchmarkName)
    supermat[,1]=c("Annualized Returns",rname,"Volatility",rname,"Tracking Error",rname,
                   "Information Ratio",rname,"Alpha Stat",rname,"Beta Stat",rname,"R-Squared",rname,"Max Drawdown",rname)
    
    for(mm in seq(1,length(your.numbers),1))
    {
      your.number=your.numbers[mm]
      temp=tempmain
      
      #print(paste0("mm = ", mm, " My numbers " , length(your.numbers)))
      
      b=0
      
      if.any.returns<-function(a,b)
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
      
      dateanalysis1=which(abs(f-your.number)==min(abs(f-your.number)))
      dateanalysis=index(temp)[dateanalysis1]
      
      as.numeric(index(temp)[ll]-dateanalysis)-> c
      (as.numeric(if.any.returns(f,c)))-> b
      
      dailySeriesForConsideredPeriod=temp[b:nrow(temp),]   # before the endpoint on month
      temp=temp[b:nrow(temp),]
      
      ep <- endpoints(temp, on = "months", k = 1)
      temp1 <- temp[ep[2:(length(ep)-1)],]
      
      #m=temp
      #mat=temp1
      
      write.csv(temp1,"temp1.csv",row.names=FALSE)
      write.csv(temp,"temp.csv",row.names=FALSE) 
      mat=read.csv("temp1.csv")
      m=read.csv("temp.csv")
      m=rbind(rep(100,ncol(m)),m)
      
      mat1=data.frame(matrix(NA,nrow=nrow(mat)-1,ncol=ncol(mat)))
      for(j in seq(1,ncol(mat),1))
      {
        for(i in seq(2,nrow(mat),1))
        {
          mat1[i-1,j]=((mat[i,j]/mat[i-1,j])-1)*100
        }
        
      }
      
      x1=mat1
      smat=data.frame(matrix(0,nrow=9,ncol=ncol(m)))
      row.names(smat)=c("Ann. Ret","Ann. Vol","TE","IR","Alpha","Beta","R2","Cash","MATE")           #Mean Adjusted Tracking Error
      colnames(smat)=colnames(temp)
      
      for(i in seq(1,ncol(smat),1))
      { 
        smat[1,i]=round(100*(((as.numeric(temp[nrow(temp),i])/as.numeric(temp[1,i]))^(365/as.numeric((index(temp)[nrow(temp)]-index(temp)[1])) ) )-1),3)
      }
      
      # different way of calculating Volatility : similar to Tralio/Admin
      #sdtempNEW=sd(diff(log((as.numeric(temp[,1])))))*sqrt(252)*100
      
      sdtemp=round(apply(x1,2,sd)*sqrt(12),2)
      smat[2,]=sdtemp
      #smat[8,]=round(c(mean((as.numeric(pfvalue[,3])/as.numeric(pfvalue[,4]))*100),rep(0,ncol(x1)-1)),3)
      
      z=x1[,1:ncol(x1)]-x1[,2]
      smat[3,]=round(apply(z,2,sd)*sqrt(12),2)
      smat[3,2]=0
      
      z=(x1[,1:ncol(x1)]-x1[,2])^2
      smat[9,]=round(sqrt(colMeans(z))*sqrt(12),2)
      
      smat[4,]=round(c(as.numeric(smat[1,1])-as.numeric(smat[1,2:ncol(smat)]))/as.numeric(smat[3,]),2)
      smat[4,2]=0
      
      #smat[3,]=round(apply((a[,6]-a[,seq(7,ncol(a),1)]),2,sd),2)
      #smat[9,]=round(sqrt(apply(((a[,6]-a[,seq(7,ncol(a),1)])^2),2,mean)),2)
      #smat[1,3]=round(100*(((m[nrow(m),3]/m[1,3])^(365/as.numeric((d[length(d)]-d[104]))))-1),3)
      #smat[1,4]=round(100*(((m[nrow(m),4]/m[1,4])^(365/as.numeric((d[length(d)]-d[104]))))-1),3)
      #smat[8,]=round(c(mean((as.numeric(portfolio1[,3])/as.numeric(portfolio1[,4]))*100),0,0,0),3)
      #smat[2,]=round(c(sd(x1[,1])*sqrt(12),sd(x1[,2])*sqrt(12),sd(x1[,3])*sqrt(12),sd(x1[,4])*sqrt(12)),3)
      #smat[8,]=round(c(mean((as.numeric(portfolio1[,3])/as.numeric(portfolio1[,4]))*100),0,0,0),3)
      
      for(i in seq(2,ncol(x1),1))
      {
        #yy=diff(log(as.numeric(x2[s,1])))*100
        #xx=diff(log(as.numeric(x2[s,i])))*100
        yy=x1[,1]
        xx=x1[,i]
        model=lm(yy~xx)
        
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
      
      
      portfolio2=data.frame(matrix(NA,nrow=nrow(temp),ncol=2))
      portfolio2[,1]<-index(temp)
      portfolio2[,2]=as.numeric(temp[,1])
      portfolio_values_per_period = portfolio2[,c(1,2)] 
      maxdrop1=10
      if(mm == length(your.numbers) && length(your.numbers)> 3) daport=drawdown.analysis(portfolio_values_per_period,20)  else daport=drawdown.analysis(portfolio_values_per_period,2)
      portfolio2=data.frame(matrix(NA,nrow=nrow(temp),ncol=2))
      portfolio2[,1]<-index(temp)
      portfolio2[,2]=as.numeric(temp[,2])
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
            pdf(paste0(folder,PortfolioName," and ",BenchmarkName,"Draw down Curve.pdf",sep=""),width=7,height=5)
          }else{
            pdf(paste(PortfolioName," and ",BenchmarkName,"Draw down Curve.pdf",sep=""),width=7,height=5)  
          }
          
          plot.ts(dabench[[2]],col=2,axes=F,main="Drawdown Plot",ylab="Drawdown Level",xlab="Time",ylim=c(min(min(dabench[[2]]),min(daport[[2]]))*(1+0.2),0))
          lines(daport[[2]],col=4)
          axis(2)
          l=seq(1,nrow(portfolio2),20)
          axis(1,at=l,labels=portfolio2[l,1],cex.axis=1)
          legend("bottomleft",legend=c(PortfolioName,BenchmarkName),lty=c(1,1),lwd=c(2,2),col=c("blue","red"),bty="n",cex=1)
          dev.off()
          dd=cbind(daport[[2]],dabench[[2]])
          colnames(dd)=c(PortfolioName,BenchmarkName)
          
          if(!is.null(folder))
          {
            write.csv(dd,paste0(folder,PortfolioName," and ",BenchmarkName," Drawdown",".csv",sep=""),row.names= index(temp))
          }else
          {
            write.csv(dd,paste(PortfolioName," and ",BenchmarkName," Drawdown",".csv",sep=""),row.names= index(temp))
          }
          
          # if(supermat[2,5]==supermat[2,6] && supermat[3,5]==supermat[3,6] ) supermat=supermat[,-5]
          # if(supermat[2,4]==supermat[2,5] && supermat[3,4]==supermat[3,5] ) supermat=supermat[,-5]
          #write.csv(supermat,paste(PortfolioName," and ",BenchmarkName," Summary",".csv",sep=""),row.names=F)
          
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
    write.csv(supermat,paste0(folder,PortfolioName," Summary Table ",pfvalue[,1][length(pfvalue[,1])],".csv",sep=""), row.names = FALSE,  na = "")
  }else{
    write.csv(supermat,paste(PortfolioName," Summary Table ",pfvalue[,1][length(pfvalue[,1])],".csv",sep=""), row.names = FALSE,  na = "")  
  }
  
  ### PORTFOLIO VS BENCHMARK CHART
  
  {
    aaa<-PortfolioValuesDataSet 
    #aaa=aaa[-which(duplicated(aaa[,1])),]
    bbb<-BenchmarkValuesDataSet 
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
    colnames(m)<-c("Date","RMI","Benchmark","ORMI Running","Cash","Original Benchmark") 
    
    m[,1]<-as.Date(as.character(m[,1])) #sets the first columns to date format 
    x<-as.xts(m[,2:6],order.by=m[,1])#converts the data to xts format 
    k<-dim(x) #dimensions of x 
    Portfolio<-data.frame(Date=m[,1],as.numeric(x[,1]),as.numeric(x[,2])) #data frame of portfolio=cash 
    
    library(ggplot2)
    colnames(Portfolio)[2:3]<-c("RMI","Benchmark")
    f<-ggplot(data=Portfolio,aes(Date))+geom_line(aes(y=Portfolio[,2],col=colnames(Portfolio)[2]))+geom_line(aes(y=Portfolio[,3],col=colnames(Portfolio)[3]))+geom_abline(intercept = 1000000, slope = 0)
    f<-f+xlab("Date")+ylab("Indices")
    f<-f+theme(axis.text=element_text(size=15), 
               axis.title=element_text(size=15),
               legend.text=element_text(size=18))
    f<-f+scale_colour_manual(name="",breaks = c(colnames(Portfolio)[2],colnames(Portfolio)[3] ), values = c("#E54B4B", "dodgerblue1"))
    #f<-f+ggplot()+geom_line(data=Portfolio,aes(x=Date,y=Cash,col="Cash"))
    f
    
  }
  
  #View(supermat)
  if(!is.null(folder))
  {
    write.csv(supermat,paste0(folder, PortfolioName," Summary Table ",pfvalue[,1][length(pfvalue[,1])],".csv",sep=""), row.names = FALSE,  na = "")
  }else{
    write.csv(supermat,paste(PortfolioName," Summary Table ",pfvalue[,1][length(pfvalue[,1])],".csv",sep=""), row.names = FALSE,  na = "")  
  }
  
  file.remove( "matport.csv")
  file.remove("matbench.csv")
  file.remove("temp.csv")
  file.remove("temp1.csv")
  return(supermat)
}