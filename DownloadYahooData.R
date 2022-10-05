if(!"tidyquant" %in% rownames(installed.packages())){install.packages("tidyquant")}
if(!"dplyr" %in% rownames(installed.packages())){install.packages("dplyr")}
library(tidyquant)
library(dplyr)
source("Functions.R")

workingWith ="Crypto10"
firstDate = as.Date( "1990-01-01")
endDate = as.Date( "2022-09-04")

if(workingWith == "S&P500")
{
  symbolsList<- c("HPE","CFG","SYF","IQV","FTV","APA","OTIS","CTVA","PAYC","DXC","CARR","HWM","BKR","QRVO","INFO","PYPL","WRK","ANET","TMUS","AMD","REG","LW","GL","TER","KEYS","KHC","TFC","HST","CSX","NWL","VTRS","CTLT","RTX","NAVI","WLTW","EXC","J","TT","NCLH","ETSY","LUMN","PEP","EVRG","AMG","STE","HAL","HII","O","CL","CI","IP","HES","CCL","BK","MAR","PTC","MU","AAL","HLT","PRGO","ZTS","LIN","SPGI","TWTR","CDW","FOXA","ALLE","ORCL","ISRG","BKNG","FISV","XRAY","BIIB","NTRS","CERN","FOSL","FFIV","EXPD","VRSN","CTXS","ROST","MCHP","INTU","DLTR","MNST","ADP","LRCX","EXPE","ADBE","AMGN","EA","NTAP","HBAN","AMAT","AAPL","ADI","WDC","CMG","ICE","ROP","BF-B","VMC","AVB","SJM","MTB","EW","WAT","LH","DVA","PSA","BXP","CF","BLL","APD","TAP","HSY","OKE","VNO","PH","L","ETR","EQT","DOV","NOC","CRM","IRM","ED","PXD","CB","BWA","ZBH","LMT","WEC","JWN","ECL","LEG","NEE","CCI","DRI","MCK","HUM","OMC","SYK","MSI","TMO","CBRE","SEE","PEG","A","PLD","IR","CLF","MPC","YUM","PPL","ALL","SO","GIS","OXY","GS","WY","AES","MAS","KMI","IPG","BRK-B","CAT","UNH","CVS","MET","PHM","COP","USB","RF","AIG","KO","C","PFE","GE","BAC","LDOS","HON","AFL","PNC","BIO","AJG","ALB","AME","AOS","ARE","ATO","AZO","APH","AVY","AEE","AON","ABC","ADM","ABT","BLK","BEN","BDX","BAX","BA","AXP","BBY","CHD","CLX","CMA","CMI","CMS","CAH","CNP","CAG","COF","BMY","COO","DRE","ESS","EFX","DTE","DGX","EIX","EMN","FDX","CPB","EL","FE","EOG","DHR","ETN","DUK","EMR","DE","DD","DHI","CVX","DIS","FCX","FL","FRT","IEX","GWW","IFF","FMC","GPC","HRL","HP","GD","WELL","ITW","HRB","IVZ","IBM","HIG","GPS","HD","GLW","HPQ","MAA","MAC","MLM","MKC","K","NI","KMB","MMC","NKE","KIM","LNC","MMM","LEN","JCI","NEM","LLY","MDT","KR","MRO","MCD","LUV","KEY","MO","LOW","MRK","JNJ","MS","JPM","PKG","PNR","PVH","RE","RIG","RJF","ROL","PNW","RL","PKI","PPG","ROK","RHI","ES","RSG","NSC","NUE","NOV","PGR","PG","SLG","TFX","SNA","SHW","SRE","SPG","SWK","STT","SYY","TGT","TJX","SLB","SCHW","T","UHS","WAB","WRB","WST","TXN","VFC","WHR","VTR","UNP","TXT","TRV","UNM","WM","UPS","TSN","VLO","WMT","VZ","WFC","ATVI","CINF","CHRW","COST","CMCSA","CSCO","CTAS","JBHT","HAS","FAST","FITB","INTC","JKHY","KLAC","PAYX","PCAR","PBCT","MSFT","POOL","TSCO","SBUX","QCOM","TROW","XLNX","GPN","MCO","ACN","TPR","AAP","FIS","MOS","PRU","STX","AIZ","CME","EXR","MKTX","CE","MA","AMP","BR","TEL","WU","AWK","AVGO","RRC","STZ","V","PM","AMT","ALK","CRL","CZR","DVN","FRC","FLT","MGM","MTD","RMD","NLSN","DFS","CNC","DLR","DPZ","D","BSX","DAL","DG","APTV","GNRC","EQR","HBI","F","FBHS","GM","HCA","KMX","LVS","KSU","JNPR","LYV","IT","LYB","MHK","MSCI","NOW","ABMD","ALGN","NRG","NVR","ANSS","PWR","ADSK","NDAQ","NFLX","AKAM","ORLY","RCL","CDNS","PSX","AMZN","CTSH","CHTR","CPRT","TDG","DXCM","SIG","TDY","DISH","TYL","URI","META","EBAY","UDR","EQIX","FANG","ENPH","XYL","XOM","FTNT","HOLX","WMB","IDXX","NXPI","GRMN","GILD","NVDA","HSIC","ILMN","INCY","IPGP","MDLZ","LKQ","ODFL","MPWR","PENN","REGN","SBAC","SIVB","SNPS","SWKS","ULTA","NLOK","ZION","TSLA","TTWO","VRTX","TRMB","VRSK","ZBRA","ABBV","LNT","WBA","ANTM","GOOGL","GOOG","KSS","AMCR","LHX","PEAK","BBWI","MRNA")
  index_benchmark <- c("^GSPC")
}else if(workingWith == "S&P100"){
  symbolsList<- c("PYPL","TMUS","KHC","RTX","EXC","PEP","CL","BK","LIN","ORCL","BKNG","BIIB","ADBE","AMGN","AAPL","CRM","LMT","NEE","TMO","SO","GS","BRK-B","CAT","UNH","CVS","MET","COP","USB","AIG","KO","C","PFE","GE","BAC","HON","ABT","BLK","BA","AXP","COF","BMY","FDX","DHR","DUK","EMR","DD","CVX","DOW","DIS","GD","IBM","HD","NKE","MMM","LLY","MDT","MCD","MO","LOW","MRK","JNJ","MS","JPM","PG","SPG","TGT","T","TXN","UNP","UPS","WMT","VZ","WFC","COST","CMCSA","CSCO","INTC","MSFT","SBUX","QCOM","ACN","GOOG","MA","AVGO","V","PM","AMT","F","GM","NFLX","AMZN","CHTR","META","XOM","GILD","NVDA","MDLZ","TSLA","ABBV","WBA","EBAY")
  index_benchmark<- "^OEX"
}else if(workingWith == "Crypto10"){
  
  #crypto has less history, thus the starting point is when all 10 symbols have started trading.
  firstDate = as.Date( "2020-09-23")
  symbolsList <- c( "BTC-USD", "SOL-USD",  "BCH-USD"  , "MATIC-USD", "ALGO-USD"  ,"LINK-USD" , "ETH-USD"  , "LTC-USD"  , "ADA-USD"  , "AVAX-USD" )
  index_benchmark<- "BITW"
}


options("getSymbols.warning4.0"=FALSE)
options("getSymbols.yahoo.warning"=FALSE)

result <- as.data.frame(as.matrix(NA))
colnames(result)<-"Date"

#--------- Read Symbols Data ---------------

for(i in symbolsList )
{ 
  
  tryCatch(
    expr ={
      Y_data = getSymbols(i, from = as.character(firstDate) ,
                          to = endDate ,warnings = FALSE,
                          auto.assign = FALSE)
      
      Y_data <- as.data.frame(as.matrix(Y_data))
      
      Y_data <- cbind(rownames(Y_data), Y_data[,4])
      
      colnames(Y_data)<-c( "Date", i)
      Y_data<- as.data.frame(Y_data)
      
      if(dim(result)[1] < 2){
        result <- Y_data
      }else if(dim(result)[1] < dim(Y_data)[1]){
        result <- Y_data %>% left_join(result)  
      }else{
        result <-result  %>% left_join(Y_data)  
      }
      
      
      
    },
    error =function(e) {   print(paste0("Symbol Missing data from Yahoo " ,i))  }
  )
}

write.csv(result, paste0(workingWith, "TimeIndex.csv"), row.names = FALSE)

#--------- Read Benchmark Data ---------------


  #read from Yahoo
  tryCatch(
    expr ={
      index_ds = getSymbols.yahoo(index_benchmark, from = as.character(firstDate) ,
                                  to = endDate ,warnings = FALSE,
                                  auto.assign = FALSE)
      
      index_ds <- as.data.frame(as.matrix(index_ds))
      
      index_ds <- cbind(rownames(index_ds), index_ds[,4])
      
      colnames(index_ds)<-c( "Index", index_benchmark )
      index_ds<- as.data.frame(index_ds)
      index_ds$Date <  as.POSIXct(as.character(index_ds$Date), format = "%Y-%m-%d")
      
      
    },
    error =function(e) {   print(paste0("Missing benchmark data from Yahoo " ,index_benchmark))  }
  )



  
  write.csv(index_ds, paste0(workingWith,"_Benchmark_Index.csv"), row.names = FALSE)


