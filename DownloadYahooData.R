if(!"tidyquant" %in% rownames(installed.packages())){install.packages("tidyquant")}
if(!"dplyr" %in% rownames(installed.packages())){install.packages("dplyr")}
library(tidyquant)
library(dplyr)
source("Functions.R")

workingWith ="Crypto30"
firstDate = as.Date( "1990-01-01")
endDate = as.Date( "2023-03-29")

if(workingWith == "S&P500")
{
  symbolsList<- c("MMM","AOS","ABT","ABBV","ACN","ATVI","ADM","ADBE","ADP","AAP","AES","AFL","A","APD","AKAM","ALK","ALB","ARE","ALGN","ALLE","LNT","ALL","GOOG","MO","AMZN","AMCR","AMD","AEE","AAL","AEP","AXP","AIG","AMT","AWK","AMP","ABC","AME","AMGN","APH","ADI","ANSS","AON","APA","AAPL","AMAT","APTV","ACGL","ANET","AJG","AIZ","T","ATO","ADSK","AZO","AVB","AVY","BKR","BALL","BAC","BBWI","BAX","BDX","WRB","BRK-B","BBY","BIO","TECH","BIIB","BLK","BK","BA","BKNG","BWA","BXP","BSX","BMY","AVGO","BR","BRO","BF-B","CHRW","CDNS","CZR","CPT","CPB","COF","CAH","KMX","CCL","CARR","CTLT","CAT","CBOE","CBRE","CDW","CE","CNC","CNP","CDAY","CF","CRL","SCHW","CHTR","CVX","CMG","CB","CHD","CI","CINF","CTAS","CSCO","C","CFG","CLX","CME","CMS","KO","CTSH","CL","CMCSA","CMA","CAG","COP","ED","STZ","CEG","COO","CPRT","GLW","CTVA","CSGP","COST","CTRA","CCI","CSX","CMI","CVS","DHI","DHR","DRI","DVA","DE","DAL","XRAY","DVN","DXCM","FANG","DLR","DFS","DISH","DIS","DG","DLTR","D","DPZ","DOV","DOW","DTE","DUK","DD","DXC","EMN","ETN","EBAY","ECL","EIX","EW","EA","ELV","LLY","EMR","ENPH","ETR","EOG","EPAM","EQT","EFX","EQIX","EQR","ESS","EL","ETSY","RE","EVRG","ES","EXC","EXPE","EXPD","EXR","XOM","FFIV","FDS","FAST","FRT","FDX","FITB","FRC","FSLR","FE","FIS","FISV","FLT","FMC","F","FTNT","FTV","FOX","BEN","FCX","GRMN","IT","GEHCV","GEN","GNRC","GD","GE","GIS","GM","GPC","GILD","GL","GPN","GS","HAL","HIG","HAS","HCA","PEAK","HSIC","HSY","HES","HPE","HLT","HOLX","HD","HON","HRL","HST","HWM","HPQ","HUM","HBAN","HII","IBM","IEX","IDXX","ITW","ILMN","INCY","IR","INTC","ICE","IP","IPG","IFF","INTU","ISRG","IVZ","INVH","IQV","IRM","JBHT","JKHY","J","JNJ","JCI","JPM","JNPR","K","KDP","KEY","KEYS","KMB","KIM","KMI","KLAC","KHC","KR","LHX","LH","LRCX","LW","LVS","LDOS","LEN","LNC","LIN","LYV","LKQ","LMT","L","LOW","LUMN","LYB","MTB","MRO","MPC","MKTX","MAR","MMC","MLM","MAS","MA","MTCH","MKC","MCD","MCK","MDT","MRK","META","MET","MTD","MGM","MCHP","MU","MSFT","MAA","MRNA","MHK","MOH","TAP","MDLZ","MPWR","MNST","MCO","MS","MOS","MSI","MSCI","NDAQ","NTAP","NFLX","NWL","NEM","NWSA","NEE","NKE","NI","NDSN","NSC","NTRS","NOC","NCLH","NRG","NUE","NVDA","NVR","NXPI","ORLY","OXY","ODFL","OMC","ON","OKE","ORCL","OGN","OTIS","PCAR","PKG","PARA","PH","PAYX","PAYC","PYPL","PNR","PEP","PKI","PFE","PCG","PM","PSX","PNW","PXD","PNC","POOL","PPG","PPL","PFG","PG","PGR","PLD","PRU","PEG","PTC","PSA","PHM","QRVO","PWR","QCOM","DGX","RL","RJF","RTX","O","REG","REGN","RF","RSG","RMD","RHI","ROK","ROL","ROP","ROST","RCL","SPGI","CRM","SBAC","SLB","STX","SEE","SRE","NOW","SHW","SBNY","SPG","SWKS","SJM","SNA","SEDG","SO","LUV","SWK","SBUX","STT","STLD","STE","SYK","SIVB","SYF","SNPS","SYY","TMUS","TROW","TTWO","TPR","TRGP","TGT","TEL","TDY","TFX","TER","TSLA","TXN","TXT","TMO","TJX","TSCO","TT","TDG","TRV","TRMB","TFC","TYL","TSN","USB","UDR","ULTA","UNP","UAL","UPS","URI","UNH","UHS","VLO","VTR","VRSN","VRSK","VZ","VRTX","VFC","VTRS","VICI","V","VMC","WAB","WBA","WMT","WBD","WM","WAT","WEC","WFC","WELL","WST","WDC","WRK","WY","WHR","WMB","WTW","GWW","WYNN","XEL","XYL","YUM","ZBRA","ZBH","ZION","ZTS")
  index_benchmark <- c("^GSPC")
}else if(workingWith == "S&P100"){
  symbolsList<- c("PYPL","TMUS","KHC","RTX","EXC","PEP","CL","BK","LIN","ORCL","BKNG","BIIB","ADBE","AMGN","AAPL","CRM","LMT","NEE","TMO","SO","GS","BRK-B","CAT","UNH","CVS","MET","COP","USB","AIG","KO","C","PFE","GE","BAC","HON","ABT","BLK","BA","AXP","COF","BMY","FDX","DHR","DUK","EMR","DD","CVX","DOW","DIS","GD","IBM","HD","NKE","MMM","LLY","MDT","MCD","MO","LOW","MRK","JNJ","MS","JPM","PG","SPG","TGT","T","TXN","UNP","UPS","WMT","VZ","WFC","COST","CMCSA","CSCO","INTC","MSFT","SBUX","QCOM","ACN","GOOG","MA","AVGO","V","PM","AMT","F","GM","NFLX","AMZN","CHTR","META","XOM","GILD","NVDA","MDLZ","TSLA","ABBV","WBA","EBAY")
  index_benchmark<- "^OEX"
}else if(workingWith =="Indonesia30"){
  symbolsList<- c("BRPT.JK","ADRO.JK","INCO.JK","INKP.JK","SMGR.JK","INDF.JK","BNLI.JK","CPIN.JK","UNTR.JK","MDKA.JK","BMRI.JK","MYOR.JK","TBIG.JK","GGRM.JK","TPIA.JK","ICBP.JK","BYAN.JK","BBNI.JK","AMRT.JK","ASII.JK","EMTK.JK","HMSP.JK","TOWR.JK","MEGA.JK","BBRI.JK","DNET.JK","TLKM.JK","BBCA.JK","UNVR.JK","ISAT.JK"  )
  index_benchmark<- "^JKSE"
}else if(workingWith =="TSX60"){
  symbolsList<- c("H.TO","SHOP.TO","WCN.TO","WEED.TO","BHC.TO","QSR.TO","EMA.TO","DOL.TO","L.TO","CM.TO","BMO.TO","SU.TO","OTEX.TO","GIL.TO","TRP.TO","TD.TO","CCO.TO","WPM.TO","SLF.TO","ABX.TO","K.TO","TRI.TO","BNS.TO","RY.TO","CAE.TO","T.TO","CNR.TO","BCE.TO","SAP.TO","FNV.TO","SNC.TO","FM.TO","MFC.TO","CSU.TO","FSV.TO","WN.TO","IFC.TO","FTS.TO","ATD.TO","MG.TO","IMO.TO","PPL.TO","AEM.TO","ENB.TO","CVE.TO","CNQ.TO","MRU.TO","AQN.TO","CP.TO","BIPC.TO","CAR-UN.TO","BAM-A.TO","CCL-B.TO","CTC-A.TO","GIB-A.TO","NA.TO","RCI-B.TO","POW.TO","SJR-B.TO","TECK-B.TO")
  
  index_benchmark<- "TX60.TS" # yahoo doesn't have data for this ticker, we will provide the file.
  
}else if(workingWith == "BSE100"){
  symbolsList <- c( "ACC.BO", "ADANIENT.BO","ADANIGREEN.NS","ADANIPORTS.BO","ATGL.NS","ADANITRANS.BO","AMBUJACEM.BO","APOLLOHOSP.BO","ASHOKLEY.BO","ASIANPAINT.BO","AUBANK.BO","AUROPHARMA.BO","ASIANPAINT.BO","DMART.BO","AXISBANK.BO","BAJAJ-AUTO.BO","BAJFINANCE.BO","BAJAJFINSV.NS","BAJAJHLDNG.BO","BANDHANBNK.NS","BERGEPAINT.BO","BHARATFORG.BO","BPCL.BO","BHARTIARTL.BO","BRITANNIA.BO","CHOLAFIN.BO","CIPLA.BO","COALINDIA.BO","COLPAL.BO","CONCOR.BO","CROMPTON.BO","DABUR.BO","DIVISLAB.BO","DLF.BO","DRREDDY.BO","EICHERMOT.BO","GAIL.BO","GODREJCP.BO","GRASIM.BO","HAVELLS.BO","HCLTECH.BO","HDFCBANK.BO","HDFCLIFE.BO","HEROMOTOCO.BO","HINDALCO.BO","HINDPETRO.BO","HINDUNILVR.BO","HDFC.BO","ICICIBANK.BO","ICICIGI.BO","ICICIPRULI.BO","IOC.BO","IGL.BO","INDUSTOWER.NS","INDUSINDBK.BO","NAUKRI.BO","INFY.BO","INDIGO.BO","ITC.BO","JSWSTEEL.BO","JUBLFOOD.BO","KOTAKBANK.BO","LTI.BO","LT.BO","LUPIN.BO","M&M.BO","MARICO.BO","MARUTI.BO","MFSL.BO","MRF.BO","NESTLEIND.BO","NTPC.BO","ONGC.BO","PAGEIND.BO","PETRONET.BO","PIIND.BO","PIDILITIND.BO","PEL.BO","POWERGRID.BO","RELIANCE.BO","SBILIFE.BO","SHREECEM.BO","SRTRANSFIN.BO","SIEMENS.BO","SRF.BO","SBIN.BO","SUNPHARMA.BO","TCS.BO","TATACONSUM.NS","TATAMOTORS.BO","TATAMTRDVR.BO","TATAPOWER.BO","TATASTEEL.BO","TECHM.BO","TITAN.BO","ULTRACEMCO.BO","UPL.BO","VEDL.BO","VOLTAS.BO","WIPRO.BO","ZEEL.BO" )
  index_benchmark<- "BSE-100.BO"
  
}else if(workingWith == "Crypto10"){
  
  #crypto has less history, thus the starting point is when all 10 symbols have started trading.
  firstDate = as.Date( "2020-09-23")
  symbolsList <- c( "BTC-USD", "SOL-USD",  "BCH-USD"  , "MATIC-USD", "ALGO-USD"  ,"LINK-USD" , "ETH-USD"  , "LTC-USD"  , "ADA-USD"  , "AVAX-USD" )
  index_benchmark<- "BITW"
}else if(workingWith == "Crypto30"){
  
  #crypto has less history, thus the starting point is when all  symbols have started trading.
  firstDate = as.Date( "2020-09-23")
  symbolsList <- c( "LTC-USD","ADA-USD","AAVE-USD","ALGO-USD","APT21794-USD","ATOM-USD","AVAX-USD","AXS-USD","BIT11221-USD","BUSD-USD","CHZ-USD","DAI-USD","DOGE-USD","ETC-USD",
                    "FIL-USD","FLOW-USD","ICP-USD","LEO-USD","LUNc-USD","OKB-USD","SAND-USD","SHIB-USD","SOL-USD","THETA-USD","TON11419-USD","USDT-USD","VET-USD","XRP-USD","XTZ-USD","ZEC-USD")
  
                                                                                                                                                                       
  index_benchmark<- "BITW"
}


options("getSymbols.warning4.0"=FALSE)
options("getSymbols.yahoo.warning"=FALSE)

result <- as.data.frame(as.matrix(NA))
colnames(result)<-"Date"

#--------- Read Symbols Data ---------------
j=0
for(i in symbolsList )
{ 
  j = j+1
  print(j)
  tryCatch(
    expr ={
      Y_data = getSymbols(i, from = as.character(firstDate) ,
                          to = endDate ,warnings = FALSE,
                          auto.assign = FALSE)
      
      Y_data <- as.data.frame(as.matrix(Y_data))
      
     
      Y_data <- Y_data[Y_data[,4] >0 ,]
      
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


if(workingWith == "BSE100"){
  result =  fillGapsInTimeIndex(result)
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


