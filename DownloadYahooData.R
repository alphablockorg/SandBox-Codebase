if(!"tidyquant" %in% rownames(installed.packages())){install.packages("tidyquant")}
if(!"dplyr" %in% rownames(installed.packages())){install.packages("dplyr")}
library(tidyquant)
library(dplyr)
source("Functions.R")

workingWith ="S&P500"
firstDate = as.Date( "2000-01-03")
endDate = as.Date( "2023-10-16")

if(workingWith == "S&P500")
{
  symbolsList<- c("MBC",	"HPE",	"CFG",	"KDP",	"SYF",	"IQV",	"FTV",	"APA",	"MTCH",	"OTIS",	"CTVA",	"SEDG",	"AMCR",	"GEN",	"PAYC",	"DXC",	"CARR",	"HWM",	"CEG",	"QRVO",	"GRMN",	"PYPL",	"WRK",	"EG",	"INVH",	"ANET",	"TMUS",	"OGN",	"BKR",	"AMD",	"CDAY",	"HON",	"BALL",	"REG",	"WBD",	"RVTY",	"LW",	"GL",	"TER",	"KEYS",	"PARA",	"VICI",	"COO",	"KHC",	"TFC",	"HST",	"CSX",	"CTRA",	"LHX",	"NWL",	"VTRS",	"CTLT",	"RTX",	"EXC",	"J",	"AXON",	"PEAK",	"TT",	"NCLH",	"ETSY",	"META",	"ON",	"AEP",	"PEP",	"STE",	"HAL",	"HII",	"COR",	"O",	"CL",	"CI",	"IP",	"HES",	"CCL",	"BK",	"MAR",	"PTC",	"MU",	"AAL",	"HLT",	"ZTS",	"LIN",	"SPGI",	"FI",	"CDW",	"FOX",	"ALLE",	"ORCL",	"ISRG",	"BKNG",	"XRAY",	"BIIB",	"NTRS",	"FFIV",	"EXPD",	"WYNN",	"VRSN",	"ROST",	"MCHP",	"INTU",	"DLTR",	"MNST",	"ADP",	"LRCX",	"EXPE",	"ADBE",	"AMGN",	"EA",	"NTAP",	"HBAN",	"AMAT",	"AAPL",	"ADI",	"WDC",	"CMG",	"ICE",	"BF-B",	"VMC",	"AVB",	"SJM",	"MTB",	"EW",	"WAT",	"LH",	"DVA",	"PSA",	"BXP",	"CF",	"APD",	"TAP",	"HSY",	"OKE",	"VNO",	"PH",	"L",	"ETR",	"EQT",	"DOV",	"NOC",	"CRM",	"IRM",	"ED",	"PXD",	"CB",	"BWA",	"ZBH",	"LMT",	"WEC",	"ECL",	"LEG",	"NEE",	"CCI",	"DRI",	"MCK",	"HUM",	"OMC",	"SYK",	"MSI",	"TMO",	"CBRE",	"SEE",	"PEG",	"A",	"PLD",	"IR",	"MPC",	"YUM",	"PPL",	"ALL",	"SO",	"GIS",	"OXY",	"GS",	"WY",	"AES",	"MAS",	"KMI",	"IPG",	"BRK-B",	"CAT",	"UNH",	"CVS",	"MET",	"PHM",	"COP",	"USB",	"RF",	"AIG",	"KO",	"C",	"PFE",	"GE",	"BAC",	"LDOS",	"AFL",	"PNC",	"BIO",	"AJG",	"ALB",	"AME",	"AOS",	"ARE",	"ATO",	"AZO",	"APH",	"AVY",	"AEE",	"AON",	"ADM",	"ABT",	"BLK",	"BEN",	"BDX",	"BAX",	"BA",	"AXP",	"BBY",	"BRO",	"CHD",	"CLX",	"CMA",	"CMI",	"CMS",	"CAH",	"CNP",	"CAG",	"COF",	"BMY",	"CPT",	"ESS",	"FDS",	"FICO",	"EFX",	"DTE",	"DGX",	"EIX",	"EMN",	"FDX",	"CPB",	"EL",	"FE",	"EOG",	"DHR",	"ETN",	"DUK",	"EMR",	"DE",	"DD",	"DHI",	"CVX",	"DOW",	"DIS",	"FCX",	"FRT",	"IEX",	"GWW",	"IFF",	"FMC",	"GPC",	"HRL",	"GD",	"WELL",	"ITW",	"IVZ",	"IBM",	"HIG",	"GPS",	"HD",	"GLW",	"HPQ",	"MAA",	"MLM",	"MKC",	"K",	"NI",	"KMB",	"MMC",	"NKE",	"KIM",	"LNC",	"MMM",	"LEN",	"JCI",	"NEM",	"LLY",	"MDT",	"KR",	"MRO",	"MCD",	"LUV",	"KEY",	"MO",	"LOW",	"MRK",	"JNJ",	"MS",	"JPM",	"PKG",	"PNR",	"PVH",	"RJF",	"ROL",	"PNW",	"RL",	"PPG",	"ROK",	"RHI",	"ES",	"RSG",	"PCG",	"NSC",	"NUE",	"PGR",	"PG",	"TFX",	"SNA",	"SHW",	"SRE",	"SPG",	"SWK",	"STT",	"SYY",	"TGT",	"TJX",	"SLB",	"SCHW",	"T",	"UHS",	"WAB",	"WRB",	"WST",	"TXN",	"VFC",	"WHR",	"VTR",	"UNP",	"TXT",	"TRV",	"WM",	"UPS",	"TSN",	"VLO",	"WMT",	"VZ",	"WFC",	"CINF",	"CHRW",	"COST",	"CMCSA",	"CSCO",	"CTAS",	"JBHT",	"HAS",	"FAST",	"FITB",	"INTC",	"JKHY",	"NDSN",	"KLAC",	"PAYX",	"PCAR",	"NWSA",	"MSFT",	"POOL",	"STLD",	"TECH",	"TSCO",	"SBUX",	"QCOM",	"TROW",	"GPN",	"BG",	"MCO",	"ACN",	"TPR",	"AAP",	"FIS",	"MOS",	"PRU",	"STX",	"AIZ",	"CME",	"EXR",	"MKTX",	"CE",	"GOOG",	"MA",	"AMP",	"BR",	"TEL",	"AWK",	"AVGO",	"STZ",	"V",	"PM",	"AMT",	"ALK",	"CRL",	"CZR",	"DVN",	"FLT",	"MGM",	"MTD",	"RMD",	"BX",	"DFS",	"CNC",	"DLR",	"DPZ",	"D",	"BSX",	"DAL",	"DG",	"APTV",	"GNRC",	"EQR",	"EPAM",	"F",	"GM",	"HCA",	"KMX",	"LVS",	"JNPR",	"LYV",	"IT",	"LYB",	"MHK",	"MOH",	"MSCI",	"NOW",	"ACGL",	"ALGN",	"NRG",	"ANSS",	"PWR",	"ADSK",	"NDAQ",	"NFLX",	"AKAM",	"ORLY",	"RCL",	"CDNS",	"PSX",	"AMZN",	"CTSH",	"CHTR",	"CPRT",	"CSGP",	"TDG",	"TRGP",	"DXCM",	"TDY",	"DISH",	"TYL",	"URI",	"EBAY",	"UAL",	"UDR",	"EQIX",	"FANG",	"ENPH",	"XYL",	"XOM",	"FTNT",	"HOLX",	"WMB",	"IDXX",	"NXPI",	"GILD",	"NVDA",	"FSLR",	"HSIC",	"ILMN",	"INCY",	"MDLZ",	"LKQ",	"ODFL",	"MPWR",	"PENN",	"REGN",	"PODD",	"SBAC",	"SNPS",	"SWKS",	"ULTA",	"ZION",	"TSLA",	"TTWO",	"VRTX",	"TRMB",	"VRSK",	"ZBRA",	"ABBV",	"UAA",	"LNT",	"ELV",	"MRNA",	"BBWI",	"WBA",	"PANW" )  
  index_benchmark <- c("SPY")
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
  firstDate = as.Date( "2020-09-18")
  # symbolsList <- c( "LTC-USD","ADA-USD","AAVE-USD","ALGO-USD","APT21794-USD","ATOM-USD","AVAX-USD","AXS-USD","BIT11221-USD","BUSD-USD","CHZ-USD","DAI-USD","DOGE-USD","ETC-USD",
  #                   "FIL-USD","FLOW-USD","ICP-USD","LEO-USD","LUNc-USD","OKB-USD","SAND-USD","SHIB-USD","SOL-USD","THETA-USD","TON11419-USD","USDT-USD","VET-USD","XRP-USD","XTZ-USD","ZEC-USD")
  symbolsList <- c("XRP-USD","MIOTA-USD","EOS-USD","BUSD-USD","SHIB-USD","SOL-USD","UNI7083-USD", "BTC-USD","POLY-USD", "MATIC-USD","MKR-USD",
                   "ALGO-USD","LINK-USD","XLM-USD","ETH-USD","DOT-USD","LTC-USD","ADA-USD","TRX-USD","CRV-USD", "AXS-USD","AVAX-USD","ETH-USD",
                   "NEAR-USD","DOGE-USD","XTZ-USD","ZEC-USD","ATOM-USD","FIL-USD","SAND-USD") 
                                                                                                                                                                       
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


if(workingWith == "BSE100" || workingWith == "S&P500"){
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


