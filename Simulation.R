library(clipr)
library(gdata)
library(base)
library(xlsx)
library (Hmisc)
library(TTR)
library(forecast)
library(pastecs)
library(stats)
library(VIF)
library(randomForest)
require(FNN)
require(data.table)
require(TunePareto)
require(rpart)
library(jsonlite)
library(httr)
library(digest)
library(Quandl)
library(readr)
require(zoo)
library(Rcpp)
library(anytime)

#MG
#Blockchain API call function: 
#Examples for usage: api.blockchain("transactions-per-second",list(timespan="5weeks",rollingAverage="8hours",format="json"))
#api.blockchain("transactions-per-second",list(timespan="5weeks",rollingAverage="8hours",start="2016-03-16"format="csv"))
api.blockchain <- function(typeofchart, args = list()) {
  type=paste0(typeofchart,"?")
  string=paste0("https://api.blockchain.info/charts/",type)
  if(length(args)>0){
    for(i in 1:length(args)){
      string=paste0(string,'&',names(args)[[i]],'=',args[[i]])
    }
  }
  print(string)
  ret <- GET(string)
  stop_for_status(ret)
  content(ret)
}

#This function below takes the daily data from corresponding files.

#Daha genericlestirilmedi.girilen tarihler arasinda almali datayi.
GetData<-function(timespan, startdate,issixmonths, whichdate){
  #3 The average block size in MB
  avgblocksize <- api.blockchain("avg-block-size",list(timespan=timespan,start=startdate,format="csv"))  #2016-03-19
  colnames(avgblocksize)[1] <- "date"
  colnames(avgblocksize)[2] <- "size"
  avgblocksize <- avgblocksize$size
  
  #4 The total size of all block headers and transactions. Not including database indexes.
  blocksize <- api.blockchain("blocks-size",list(timespan=timespan,start=startdate,format="csv"))  #2016-03-19
  colnames(blocksize)[1] <- "date"
  colnames(blocksize)[2] <- "size"
  blocksize <- blocksize$size
  
  
  #5 Cost Per Transaction: Miners revenue divided by the number of transactions
  costpertrans <- api.blockchain("cost-per-transaction",list(timespan=timespan,start=startdate,format="csv"))  #2016-03-19
  colnames(costpertrans)[1] <- "date"
  colnames(costpertrans)[2] <- "cost"
  costpertrans <- costpertrans$cost
  
  
  #6 Cost % of Transaction Volume: Miners revenue as percentage of the transaction volume
  costpertransper <- api.blockchain("cost-per-transaction-percent",list(timespan=timespan,start=startdate,format="csv"))  #2016-03-19
  colnames(costpertransper)[1] <- "date"
  colnames(costpertransper)[2] <- "cost"
  costpertransper <- costpertransper$cost
  
  #7 Difficulty: A relative measure of how difficult it is to find a new block. The difficulty is adjusted periodically as a function of how much hashing power has been deployed by the network of miners.
  difficulty <- api.blockchain("difficulty",list(timespan=timespan,start=startdate,format="csv"))  #2016-03-19
  colnames(difficulty)[1] <- "date"
  colnames(difficulty)[2] <- "diff"
  difficulty <- difficulty$diff
  
  #8 Estimated Transaction Value: The total estimated value of transactions on the Bitcoin blockchain (does not include coins returned to sender as change).
  esttransvol <- api.blockchain("estimated-transaction-volume",list(timespan=timespan,start=startdate,format="csv"))  #2016-03-19
  colnames(esttransvol)[1] <- "date"
  colnames(esttransvol)[2] <- "volume"
  esttransvol <- esttransvol$volume
  
  #9 Estimated USD Transaction Value: The Estimated Transaction Value in USD value
  esttransvolusd <- api.blockchain("estimated-transaction-volume-usd",list(timespan=timespan,start=startdate,format="csv"))  #2016-03-19
  colnames(esttransvolusd)[1] <- "date"
  colnames(esttransvolusd)[2] <- "volume"
  esttransvolusd <- esttransvolusd$volume
  
  #10 Hash Rate: The estimated number of tera hashes per second (trillions of hashes per second) the Bitcoin network is performing.
  hashrate <- api.blockchain("hash-rate",list(timespan=timespan,start=startdate,format="csv"))  #2016-03-19
  colnames(hashrate)[1] <- "date"
  colnames(hashrate)[2] <- "hashrate"
  hashrate <- hashrate$hashrate
  
  #11 Market Capitalization: The total USD value of bitcoin supply in circulation, as calculated by the daily average market price across major exchanges.
  marketcap <- api.blockchain("market-cap",list(timespan=timespan,start=startdate,format="csv"))  #2016-03-19
  colnames(marketcap)[1] <- "date"
  colnames(marketcap)[2] <- "marketcap"
  marketcap <- marketcap$marketcap
  
  #12 Median Confirmation Time: The median time for a transaction to be accepted into a mined block and added to the public ledger (note: only includes transactions with miner fees).
  medconfirtime <- api.blockchain("median-confirmation-time",list(timespan=timespan,start=startdate,format="csv"))  #2016-03-19
  colnames(medconfirtime)[1] <- "date"
  colnames(medconfirtime)[2] <- "confirmtime"
  medconfirtime <- medconfirtime$confirmtime
  
  #13 Confirmed Transactions Per Day: The number of daily confirmed Bitcoin transactions.
  transactions <- api.blockchain("n-transactions",list(timespan=timespan,start=startdate,format="csv"))  #2016-03-19
  colnames(transactions)[1] <- "date"
  colnames(transactions)[2] <- "ntrans"
  transactions <- transactions$ntrans
  
  #14 Number Of Transactions Excluding Chains Longer Than 100
  transexcludinglonger100 <- api.blockchain("n-transactions-excluding-chains-longer-than-100",list(timespan=timespan,start=startdate,format="csv"))  #2016-03-19
  colnames(transexcludinglonger100)[1] <- "date"
  colnames(transexcludinglonger100)[2] <- "ntrans100"
  transexcludinglonger100 <- transexcludinglonger100$ntrans100
  
  #15 Number of Transactions Excluding Popular Addresses
  transexcludingpopular <- api.blockchain("n-transactions-excluding-popular",list(timespan=timespan,start=startdate,format="csv"))  #2016-03-19
  colnames(transexcludingpopular)[1] <- "date"
  colnames(transexcludingpopular)[2] <- "ntransExPopular"
  transexcludingpopular <- transexcludingpopular$ntransExPopular
  
  #16 Average Number Of Transactions Per Block
  transperblock <- api.blockchain("n-transactions-per-block",list(timespan=timespan,start=startdate,format="csv"))  #2016-03-19
  colnames(transperblock)[1] <- "date"
  colnames(transperblock)[2] <- "ntransperblock"
  transperblock <- transperblock$ntransperblock
  
  
  #17 Total Number of Transactions
  transtotal <- api.blockchain("n-transactions-total",list(timespan=timespan,start=startdate,format="csv"))  #2016-03-19
  colnames(transtotal)[1] <- "date"
  colnames(transtotal)[2] <- "transtotal"
  transtotal <- transtotal$transtotal
  
  #18 Number Of Unique Addresses Used
  uniqueaddresses <- api.blockchain("n-unique-addresses",list(timespan=timespan,start=startdate,format="csv"))  #2016-03-19
  colnames(uniqueaddresses)[1] <- "date"
  colnames(uniqueaddresses)[2] <- "nunique"
  uniqueaddresses <- uniqueaddresses$nunique
  
  #19 Output Value: The total value of all transaction outputs per day (includes coins returned to the sender as change).
  outputvolume <- api.blockchain("output-volume",list(timespan=timespan,start=startdate,format="csv"))  #2016-03-19
  colnames(outputvolume)[1] <- "date"
  colnames(outputvolume)[2] <- "outputvalue"
  outputvolume <- outputvolume$outputvalue
  
  #20 Bitcoins in circulation: The total number of bitcoins that have already been mined; in other words, the current supply of bitcoins on the network.
  totalbitcoins <- api.blockchain("total-bitcoins",list(timespan=timespan,start=startdate,format="csv"))  #2016-03-19
  colnames(totalbitcoins)[1] <- "date"
  colnames(totalbitcoins)[2] <- "totalbitcoins"
  totalbitcoins <- totalbitcoins$totalbitcoins
  
  
  #21 USD Exchange Trade Volume: The total USD value of trading volume on major bitcoin exchanges
  tradevolume <- api.blockchain("trade-volume",list(timespan=timespan,start=startdate,format="csv"))  #2016-03-19
  colnames(tradevolume)[1] <- "date"
  colnames(tradevolume)[2] <- "trade_volume"
  tradevolume <- tradevolume$trade_volume
  
  
  #22 Total Transaction Fees: The total value of all transaction fees paid to miners (not including the coinbase value of block rewards).
  transfees <- api.blockchain("transaction-fees",list(timespan=timespan,start=startdate,format="csv"))  #2016-03-19
  colnames(transfees)[1] <- "date"
  colnames(transfees)[2] <- "fee"
  transfees <- transfees$fee
  
  
  #23 Total Transaction Fees in USD: The total value of all transaction fees paid to miners (not including the coinbase value of block rewards).
  transfeesusd <- api.blockchain("transaction-fees-usd",list(timespan=timespan,start=startdate,format="csv"))  #2016-03-19
  colnames(transfeesusd)[1] <- "date"
  colnames(transfeesusd)[2] <- "fee_usd"
  transfeesusd <- transfeesusd$fee_usd
  
  
  #24 Miners Revenue: Total value of coinbase block rewards and transaction fees paid to miners.
  minersrevenue <- api.blockchain("miners-revenue",list(timespan=timespan,start=startdate,format="csv"))  #2016-03-19
  colnames(minersrevenue)[1] <- "date"
  colnames(minersrevenue)[2] <- "miners_rev"
  minersrevenue <- minersrevenue$miners_rev
  
  
  #42
  #Using Quandl
  DollarToChineseYuan<-Quandl("CUR/CNY", api_key="bkosXkpWMbzo6XkWzFKm", start_date=startdate, end_date= whichdate-1-issixmonths)
  DollarToChineseYuan<-rev(DollarToChineseYuan$RATE)
  DollarToChineseYuanchange = 0
  for(i in 1:length(DollarToChineseYuan)-1){
    DollarToChineseYuanchange[i]=(DollarToChineseYuan[i+1]-DollarToChineseYuan[i])/DollarToChineseYuan[i]
  }
  
  AllInfo<-cbind(avgblocksize,blocksize, costpertrans, costpertransper, difficulty, esttransvol, esttransvolusd, hashrate, marketcap, medconfirtime, transactions, transexcludinglonger100, transexcludingpopular, transperblock,transtotal, uniqueaddresses, outputvolume, totalbitcoins, tradevolume, transfees, transfeesusd, minersrevenue, DollarToChineseYuanchange)
  return(AllInfo)
}


#This function arranges the not-sizable-data in a way that we can use them in different time intervals.
#Please enter the intervalwanted in minutes.
DatagivenTimeInterval<-function(AllData,intervalwanted){
  eachvalue=24*60/intervalwanted
  AllDatawithinterval=paste("AllData",intervalwanted,sep="-")
  AllDatawithinterval=AllData[rep(seq_len(nrow(AllData)), each=eachvalue),]
  return(AllDatawithinterval)
}

#This function helps to fetch data from poloniex.com
api.poloniex.public <- function(command, args = list()) {
  string=paste0("https://poloniex.com/public?command=",command)
  if(length(args)>0){
    for(i in 1:length(args)){
      string=paste0(string,'&',names(args)[[i]],'=',args[[i]])
    }
  }
  #  print(string)
  ret <- POST(string)
  stop_for_status(ret)
  content(ret)
}

#This function below converts a given date to unix timestamp. Acceptable inputs: "YYYY-MM-DD HH:MM:SS"
DateUnixTimestampConverter<-function(date){
  unixtimetamp=as.numeric(as.POSIXct(date,tz="GMT"))
  return(unixtimetamp)
}

#This function below takes the time limits and the interval that we want to fetch the data, and gets the "data from Poloniex"(BTC-ETH Rate and USD-BTC Rate) accordingly.
#300(5 min), 900(15 min), 1800(30 min), 7200(2 hours), 14400(4 hours), and 86400(1 day) are possible periods to get the data from Poloniex.
#Please enter the intervalwanted in minutes.
PoloniexBTC_ETH_RATE<-function(intervalwanted,startdate,enddate){
  BTC_ETH_RATE=paste("BTC_ETHRate",intervalwanted,sep="-")
  BTC_ETH_RATEWEIGHTED=paste("BTC_ETHWeighted",intervalwanted,sep="-")
  
  periodtogetdata=intervalwanted*60
  
  BTC_ETH_RATE=api.poloniex.public("returnChartData", list(currencyPair = "BTC_ETH",start=startdate,end=enddate,period=periodtogetdata))
  BTC_ETH_RATE=rbindlist(BTC_ETH_RATE)
  BTC_ETH_RATEWEIGHTED=cbind(date=BTC_ETH_RATE$date,weightedaverage=BTC_ETH_RATE$weightedAverage)
  BTC_ETH_RATEWEIGHTED = as.data.frame(BTC_ETH_RATEWEIGHTED)
  return(BTC_ETH_RATEWEIGHTED)
}
PoloniexUSD_BTC_RATE<-function(intervalwanted,startdate,enddate){
  USD_BTC_RATE=paste("USD_BTCRate",intervalwanted,sep="-")
  USD_BTC_RATEWEIGHTED=paste("USD_BTCWeighted",intervalwanted,sep="-")
  
  periodtogetdata=intervalwanted*60
  
  USD_BTC_RATE=api.poloniex.public("returnChartData", list(currencyPair = "USDT_BTC",start=startdate,end=enddate,period=periodtogetdata))
  USD_BTC_RATE=rbindlist(USD_BTC_RATE)
  USD_BTC_RATEWEIGHTED=cbind(date=USD_BTC_RATE$date,weightedaverage=USD_BTC_RATE$weightedAverage)
  USD_BTC_RATEWEIGHTED = as.data.frame(USD_BTC_RATEWEIGHTED)
  return(USD_BTC_RATEWEIGHTED)
}

PoloniexUSD_ETH_RATE<-function(intervalwanted,startdate,enddate){
  USD_ETH_RATE=paste("USD_ETH_RATE",intervalwanted,sep="-")
  USD_ETH_RATEWEIGHTED=paste("USD_ETHWeighted",intervalwanted,sep="-")
  
  periodtogetdata=intervalwanted*60
  
  USD_ETH_RATE=api.poloniex.public("returnChartData", list(currencyPair = "USDT_ETH",start=startdate,end=enddate,period=periodtogetdata))
  USD_ETH_RATE=rbindlist(USD_ETH_RATE)
  USD_ETH_RATEWEIGHTED=cbind(date=USD_ETH_RATE$date,weightedaverage=USD_ETH_RATE$weightedAverage)
  USD_ETH_RATEWEIGHTED = as.data.frame(USD_ETH_RATEWEIGHTED)

  return(USD_ETH_RATEWEIGHTED)
}

ChangeoftheRates<-function(AllDatagiveninterval,intervalwanted,startdate,enddate){
  BTC_ETHRATEWEIGHTED<-PoloniexBTC_ETH_RATE(intervalwanted,startdate,enddate)
  USD_BTC_RATEWEIGHTED<-PoloniexUSD_BTC_RATE(intervalwanted,startdate,enddate)
  USD_ETH_RATEWEIGHTED<-PoloniexUSD_ETH_RATE(intervalwanted,startdate,enddate)
  lastindex=nrow(USD_BTC_RATEWEIGHTED)
  
  
  USD_BTCratechange = 1
  for(i in 1:lastindex-1){
    USD_BTCratechange[i]=(USD_BTC_RATEWEIGHTED[i+1,2]-USD_BTC_RATEWEIGHTED[i,2])/USD_BTC_RATEWEIGHTED[i,2]
 
  }
  if(any(is.na(USD_BTCratechange))){
    USD_BTCratechange[which(is.na(USD_BTCratechange))]=0}
 
  AllDatagiveninterval = cbind(AllDatagiveninterval,USD_BTCratechange)
  
  BTC_ETHrateschange = 1
  for(i in 1:lastindex-1){
    BTC_ETHrateschange[i]=(BTC_ETHRATEWEIGHTED[i+1,2]-BTC_ETHRATEWEIGHTED[i,2])/BTC_ETHRATEWEIGHTED[i,2]
     
    }
  if(any(is.na(BTC_ETHrateschange))){
    BTC_ETHrateschange[which(is.na(BTC_ETHrateschange))]=0}
  AllDatagiveninterval= cbind(BTC_ETHrateschange, AllDatagiveninterval)
  
  USD_ETHratechange = 1
  for(i in 1:lastindex-1){
    USD_ETHratechange[i]=(USD_ETH_RATEWEIGHTED[i+1,2]-USD_ETH_RATEWEIGHTED[i,2])/USD_ETH_RATEWEIGHTED[i,2]
  
    
  }
  
  if(any(is.na(USD_ETHratechange))){
  USD_ETHratechange[which(is.na(USD_ETHratechange))]=0}

  AllDatagiveninterval=cbind(AllDatagiveninterval,USD_ETHratechange)
  
  
  return(AllDatagiveninterval)
}


AddingLagsforBTC_ETH<-function(AllDatagiveninterval,intervalwanted,howmanylags,whataboutdailylag,whataboutweeklylag,startdate,enddate){
  
  AllDatagiveninterval<-ChangeoftheRates(AllDatagiveninterval,intervalwanted,startdate,enddate)
  thelength = nrow(AllDatagiveninterval)
  for(i in 1:howmanylags){
    rateschangelag=0
    rateschangelag[1:i]=0
    for(k in (i+1):thelength){
      rateschangelag[k] = (AllDatagiveninterval[,1])[k-i]
    }
    AllDatagiveninterval=cbind(AllDatagiveninterval,rateschangelag)
    colnames(AllDatagiveninterval)[ncol(AllDatagiveninterval)] <- paste0("rateschangelag",i)
    
  }
  
  if(whataboutdailylag==TRUE){
    rateschangelagdaily=0
    oneday<-1440/intervalwanted
    rateschangelagdaily[1:oneday]=0
    for(i in (oneday+1):thelength){
      rateschangelagdaily[i] = (AllDatagiveninterval[,1])[i-oneday]
    }
    
    AllDatagiveninterval = cbind(AllDatagiveninterval, rateschangelagdaily)
  }
  if(whataboutweeklylag==TRUE){
    rateschangelagweekly=0
    oneweek<-10080/intervalwanted
    rateschangelagweekly[1:oneweek]=0
    for(i in (oneweek+1):thelength){
      rateschangelagweekly[i] = (AllDatagiveninterval[,1])[i-oneweek]
    }
    
    AllDatagiveninterval = cbind(AllDatagiveninterval, rateschangelagweekly)
  }
  return(AllDatagiveninterval)
}

FinalizeDataBeforeMethods<-function(AllDatagiveninterval,intervalwanted,howmanylags,whataboutdailylag,whataboutweeklylag,startdate,enddate){
  AllDatagiveninterval<-AddingLagsforBTC_ETH(AllDatagiveninterval,intervalwanted,howmanylags,whataboutdailylag,whataboutweeklylag,startdate,enddate)
  
  if(whataboutweeklylag==TRUE){
    oneweek<-10080/intervalwanted
    AllDataFinal = AllDatagiveninterval[-(1:oneweek),]
    whichway=AllDataFinal[,1]>0
    AllDataFinal[,1] = whichway
    AllDataFinal=as.data.frame(AllDataFinal)
    AllDataFinal[,1]=as.factor(AllDataFinal[,1])
    return(AllDataFinal)
  }
  else if(whataboutdailylag==TRUE){
    oneday<-1440/intervalwanted
    AllDataFinal = AllDatagiveninterval[-(1:oneday),]
    whichway=AllDataFinal[,1]>0
    AllDataFinal[,1] = whichway
    AllDataFinal=as.data.frame(AllDataFinal)
    AllDataFinal[,1]=as.factor(AllDataFinal[,1])
    return(AllDataFinal)
  }
  else{
    whichway=AllDataFinal[,1]>0
    AllDataFinal[,1] = whichway
    AllDataFinal=as.data.frame(AllDataFinal)
    AllDataFinal[,1]=as.factor(AllDataFinal[,1])
    return(AllDataFinal)
  }
}

RealTimeLags<-function(intervalwanted,timee){

  
  now<-DateUnixTimestampConverter(timee)
  min30<-DateUnixTimestampConverter(timee)-1800
  min60<-DateUnixTimestampConverter(timee)-3600
  day1pre<-DateUnixTimestampConverter(timee)-88200
  day1<-DateUnixTimestampConverter(timee)-86400
  week1pre<-DateUnixTimestampConverter(timee)-606600
  week1<-DateUnixTimestampConverter(timee)-604800
  otherratesfrompoloniex=matrix(0,nrow=1,ncol=2)
  USD_BTC_RATEWEIGHTED=PoloniexUSD_BTC_RATE(intervalwanted,min30,now)
  USD_ETH_RATEWEIGHTED=PoloniexUSD_ETH_RATE(intervalwanted,min30,now)
 
  USD_BTCratechange=(USD_BTC_RATEWEIGHTED[2,2]-USD_BTC_RATEWEIGHTED[1,2])/USD_BTC_RATEWEIGHTED[1,2]
  USD_ETHratechange=(USD_ETH_RATEWEIGHTED[2,2]-USD_ETH_RATEWEIGHTED[1,2])/USD_ETH_RATEWEIGHTED[1,2]
  if(is.na(USD_ETHratechange)){
    print("O tarihte, ratechange 0ladik.")
    USD_ETHratechange=0
  }
  if(is.na(USD_BTCratechange)){
    print("O tarihte, ratechange 0ladik.")
    USD_BTCratechange=0
  }

  
  
  otherratesfrompoloniex[,1]=USD_BTCratechange
  otherratesfrompoloniex[,2]=USD_ETHratechange
  colnames(otherratesfrompoloniex)<-c("USD_BTCratechange","USD_ETHratechange")
  
  BTC_ETHRATEWEIGHTED=PoloniexBTC_ETH_RATE(intervalwanted,min60,now)

  lastindex=nrow(BTC_ETHRATEWEIGHTED)
  BTC_ETHrateschange = 1
  for(i in 1:lastindex-1){
    BTC_ETHrateschange[i]=(BTC_ETHRATEWEIGHTED[i+1,2]-BTC_ETHRATEWEIGHTED[i,2])/BTC_ETHRATEWEIGHTED[i,2]
  }
  BTC_ETHRATEWEIGHTED2=PoloniexBTC_ETH_RATE(intervalwanted,day1pre,day1)
  BTC_ETHrateschange2=(BTC_ETHRATEWEIGHTED2[2,2]-BTC_ETHRATEWEIGHTED2[1,2])/BTC_ETHRATEWEIGHTED2[1,2]
  BTC_ETHRATEWEIGHTED3=PoloniexBTC_ETH_RATE(intervalwanted,week1pre,week1)
  BTC_ETHrateschange3=(BTC_ETHRATEWEIGHTED3[2,2]-BTC_ETHRATEWEIGHTED3[1,2])/BTC_ETHRATEWEIGHTED3[1,2]
  if(is.na(BTC_ETHrateschange[1])){
    print("O tarihte, ratechange 0ladik.")
    BTC_ETHrateschange[1]=0
  }
  if(is.na(BTC_ETHrateschange[2])){
    BTC_ETHrateschange[2]=0
    print("O tarihte, ratechange 0ladik.")
  }
  if(is.na(BTC_ETHrateschange2)){
    BTC_ETHrateschange2=0
    print("O tarihte, ratechange 0ladik.")
  }
  if(is.na(BTC_ETHrateschange3)){
    BTC_ETHrateschange3=0
    print("O tarihte, ratechange 0ladik.")
  }
  
  lags=matrix(0,nrow=1,ncol=5)
  lags[,1]=BTC_ETHrateschange[3]
  lags[,2]=BTC_ETHrateschange[2]
  lags[,3]=BTC_ETHrateschange[1]
  lags[,4]=BTC_ETHrateschange2
  lags[,5]=BTC_ETHrateschange3
  colnames(lags)<-c("rateschangelag1","rateschangelag2","rateschangelag3","rateschangelagdaily","rateschangelagweekly")
  
  poloniexdata<-cbind(otherratesfrompoloniex,lags)
  return (poloniexdata)
  
}  
  # while(any(is.na(poloniexdata))){
  #   print("I am in while to get rid of NAs")
  #   now<-DateUnixTimestampConverter(timee)
  #   min30<-DateUnixTimestampConverter(timee)-1800
  #   min60<-DateUnixTimestampConverter(timee)-3600
  #   day1pre<-DateUnixTimestampConverter(timee)-88200
  #   day1<-DateUnixTimestampConverter(timee)-86400
  #   week1pre<-DateUnixTimestampConverter(timee)-606600
  #   week1<-DateUnixTimestampConverter(timee)-604800
  #   otherratesfrompoloniex=matrix(0,nrow=1,ncol=2)
  #   USD_BTC_RATEWEIGHTED=PoloniexUSD_BTC_RATE(intervalwanted,min30,now)
  #   USD_ETH_RATEWEIGHTED=PoloniexUSD_ETH_RATE(intervalwanted,min30,now)  
  #   
  #   
  #   USD_BTCratechange=(USD_BTC_RATEWEIGHTED[2,2]-USD_BTC_RATEWEIGHTED[1,2])/USD_BTC_RATEWEIGHTED[1,2]
  #   USD_ETHratechange=(USD_ETH_RATEWEIGHTED[2,2]-USD_ETH_RATEWEIGHTED[1,2])/USD_ETH_RATEWEIGHTED[1,2]
  #   otherratesfrompoloniex[,1]=USD_BTCratechange
  #   otherratesfrompoloniex[,2]=USD_ETHratechange
  #   colnames(otherratesfrompoloniex)<-c("USD_BTCratechange","USD_ETHratechange")
  #   
  #   BTC_ETHRATEWEIGHTED=PoloniexBTC_ETH_RATE(intervalwanted,min60,now)
  #   
  #   lastindex=nrow(BTC_ETHRATEWEIGHTED)
  #   BTC_ETHrateschange = 1
  #   for(i in 1:lastindex-1){
  #     BTC_ETHrateschange[i]=(BTC_ETHRATEWEIGHTED[i+1,2]-BTC_ETHRATEWEIGHTED[i,2])/BTC_ETHRATEWEIGHTED[i,2]
  #   }
  #   BTC_ETHRATEWEIGHTED2=PoloniexBTC_ETH_RATE(intervalwanted,day1pre,day1)
  #   BTC_ETHrateschange2=(BTC_ETHRATEWEIGHTED2[2,2]-BTC_ETHRATEWEIGHTED2[1,2])/BTC_ETHRATEWEIGHTED2[1,2]
  #   BTC_ETHRATEWEIGHTED3=PoloniexBTC_ETH_RATE(intervalwanted,week1pre,week1)
  #   BTC_ETHrateschange3=(BTC_ETHRATEWEIGHTED3[2,2]-BTC_ETHRATEWEIGHTED3[1,2])/BTC_ETHRATEWEIGHTED3[1,2]
  #   
  #   lags=matrix(0,nrow=1,ncol=5)
  #   lags[,1]=BTC_ETHrateschange[3]
  #   lags[,2]=BTC_ETHrateschange[2]
  #   lags[,3]=BTC_ETHrateschange[1]
  #   lags[,4]=BTC_ETHrateschange2
  #   lags[,5]=BTC_ETHrateschange3
  #   colnames(lags)<-c("rateschangelag1","rateschangelag2","rateschangelag3","rateschangelagdaily","rateschangelagweekly")
  #   
  #   poloniexdata<-cbind(otherratesfrompoloniex,lags)
  # }


#please enter trainingperiod in days, and intervalwanted in minutes
TradewithRandomForest<-function(AllDataFinal){
  rforest = randomForest(BTC_ETHrateschange~.,AllDataFinal)
  return(rforest)
}

PredictwithRF<-function(rforestobj,input){
  
  RFpredictions<-predict(rforestobj, input, type="prob")
  return(RFpredictions)
}

PredictwithRF<-function(rforestobj,input){
  
  RFpredictions<-predict(rforestobj, input, type="prob")
  return(RFpredictions)
} 

PredictwithLR<-function(LRobj,input){
  
  LRPredictions=predict(LRobj,input,type='response')
  return(LRPredictions)
}


#please enter trainingperiod in days, and intervalwanted in minutes
TradewithLogisticRegression<-function(AllDataFinal){
  nothing = glm(BTC_ETHrateschange~1,AllDataFinal, family=binomial)
  glm.fit=glm(BTC_ETHrateschange~., data=AllDataFinal,family=binomial )
  forward = step(nothing, scope=list(lower=formula(nothing),upper=formula(glm.fit)), direction="forward", trace = 0)
  glmforward=glm(formula(forward), data=AllDataFinal,family=binomial)
  return(glmforward)
}

GetDailyInput<-function(startdate,whichdate){
  inputrow<-GetData("2days",startdate,0,whichdate)
  return(inputrow)
}

ReturnofYesterdayTrade<-function(PredProb,RealExRatesChange,intervalwanted){
  capital= array(1,50)
  threshold = 0.30 #threshold initial
  howmanypreds = 24*(60/intervalwanted)-1
  k=1
  
  for(j in 1: 50){
    for(i in 1:howmanypreds){
      if(PredProb[i,1] >= threshold){
        capital[k] = capital[k] + capital[k]*RealExRatesChange[i]
      }
    }
    threshold = threshold + 0.01
    k = k + 1
  }
  
  return(capital)
}

WhichModel <- function(PredMatRF, PredMatLR, intervalwanted,whichdate){
  # real exchange rate'i cek ve change'e cevir, RealExRatesChange
  if(sum(PredMatLR)==0 && sum(PredMatRF)==0){
    alpha <- 0.5
    
    index <- 0.5
    
    ret <- c(alpha, index)
  }
  else{
    startdate=DateUnixTimestampConverter(paste0(whichdate-1, sep= " ", "00:00:00"))
    enddate=DateUnixTimestampConverter(paste0(whichdate-1, sep= " ", "23:45:00"))
    
    RealExRates = matrix(0, nrow = (24*(60/intervalwanted)), ncol = 2)
    
    RealExRates <- PoloniexBTC_ETH_RATE(intervalwanted,startdate,enddate)
    
    RealExRatesChange = 1
    for(i in 1:(24*(60/intervalwanted)-1)){
      RealExRatesChange[i]=(RealExRates[i+1,2]-RealExRates[i,2])/RealExRates[i,2]
    }
    
    ReturnsFromRF = array(0,50)
    ReturnsFromLR = array(0,50)
    ReturnsFromRF = ReturnofYesterdayTrade(PredMatRF, RealExRatesChange, intervalwanted)
    ReturnsFromLR = ReturnofYesterdayTrade(PredMatLR, RealExRatesChange, intervalwanted)
    
    index <- which.max(ReturnsFromRF + ReturnsFromLR)
    alpha <- ReturnsFromRF[index] / (ReturnsFromRF[index] + ReturnsFromLR[index])
    
    index <- 0.29 + (index*0.01)
    
    ret <- c(alpha, index)
  }
  return(ret) 
}

ReturnofRandomTrade<-function(testperiod,LR){
  ran<-sample(16800,8400) #0dan 16800e kadar 8400 sayi generate et, randomly,  farkli
  
  capital=1
  counterbuy= 0
  
  for(i in ran){
    capital = capital + capital*LR$realizedchangerate[i] 
    counterbuy = counterbuy + 1
  }
  
}
ReturnofConstantTrade<-function(testperiod,LR){
  capital=1
  counterbuy= 0
  
  
  for(i in 1:16800){
    capital = capital + capital*LR$realizedchangerate[i]
    counterbuy = counterbuy + 1
  }
  
}
#This function below makes live trade with the help of previous functions implemented.
api.poloniex <- function(key, secret, command, args = list()) {
  req <- c(list(
    command = command,
    nonce = round(as.numeric(Sys.time()) * 1e4, 0)),
    args)
  # print(req)
  ret <- POST("https://poloniex.com/tradingApi",
              add_headers(key=key, sign=hmac(secret, httr:::compose_query(req), "sha512")),
              body = req,
              encode = "form")
  content(ret)
}


buyETH<-function(){
  print("BuyETH")
  tradestatus=TRUE
  key="7O6W4DE3-ARK58PI3-AFJ9QP0I-O7NOQTGO"
  secret="c67f56ded29bf5baeae7b669d6e0da05f9af1dcf4d2fd8fc3f4c886cb4f91ecbab858f54464903568a19a113c17e509280afc23916af5e9e5b6b7d1331cb3d94"
  balance=api.poloniex(key,secret,"returnBalances")
  titles=names(balance)
  balance=as.numeric(balance)
  BTC_ETH_balance=data.table(BTC=balance[which(titles=="BTC")],ETH=balance[which(titles=="ETH")],XRP=balance[which(titles=="XRP")])
  amount=BTC_ETH_balance$BTC
  for(i in 1:5){
    tradestatus=TRUE
    ethTicker=api.poloniex.public("returnTicker", list())
    amountofETH=((amount*0.99)/as.numeric(ethTicker$BTC_ETH$last))
    resultofbuyingETC<-api.poloniex(key,secret,"buy",list(currencyPair ="BTC_ETH",rate=ethTicker$BTC_ETH$lowestAsk,amount=amountofETH,immediateOrCancel=1))
    if("error" %in% names(resultofbuyingETC)){
      print("BuyETH cancelation")
      tradestatus=FALSE #cancelation
      }
    else{
      break
    }
  }
  
  if(tradestatus){
    a =as.character(Sys.time())
    write.table(cbind(a, amountofETH),"ManualTradeHistory.csv",append=T, sep=",",col.names = F, row.names = F)
  }
  return(tradestatus)
}
buyBTC<-function(){
  print("BuyBTC")
  tradestatus=TRUE
  key="" #fill these
  secret="" #fill these
  balance=api.poloniex(key,secret,"returnBalances")
  titles=names(balance)
  balance=as.numeric(balance)
  BTC_ETH_balance=data.table(BTC=balance[which(titles=="BTC")],ETH=balance[which(titles=="ETH")])
  amount=BTC_ETH_balance$ETH
  ethTicker=api.poloniex.public("returnTicker", list())
  resultofbuyingBTC<-api.poloniex(key,secret,"sell",list(currencyPair ="BTC_ETH",rate=ethTicker$BTC_ETH$highestBid,amount=amount,immediateOrCancel=1))
  if("error" %in% names(resultofbuyingBTC)){
    print("BuyBTC cancelation")
    tradestatus=FALSE #cancelation
  }
  
  return(tradestatus)
}


#MAIN
Sys.setenv(TZ='GMT')
intervalwanted=15
trainingperiod = 181
howmanylags=3
whataboutdailylag=TRUE
whataboutweeklylag=TRUE


isDataCollectedTrained = FALSE
isInitialized = FALSE
isDecided = TRUE
isTrade = FALSE
isFirst = TRUE
predcount = 0
oneday<-24*60*60 #86400
min15 <- 60*15 #900


PredMatRF = matrix(0,nrow=24*(60/intervalwanted)-1,ncol=1)
PredMatLR = matrix(0,nrow=24*(60/intervalwanted)-1,ncol=1)

write.table(NULL,"ManualTradeHistory.csv",sep=",",col.names = F,row.names = F)
write.table(NULL,"Simulation.csv", sep=",",col.names = F, row.names = F)
time <- 1481672700
while(1==1){
  time <- time + 19
  print(time)
  if (time > 1494805500){
    break
  }
  now <- as.POSIXct(time, origin="1970-01-01", tz="GMT")
  nowatday <- time %% oneday
  whichdate <- as.Date(as.POSIXct(time, origin="1970-01-01", tz="GMT"))
  
  # to start the day
  if((nowatday >= 85500) && (nowatday < 85620) && isFirst == TRUE){
    write.table(paste0("The day has begun.The time is: ",now),"Simulation.csv",append=TRUE, sep=",",col.names = F, row.names = F)
    isDataCollectedTrained = FALSE
    isInitialized = FALSE
    isDecided = TRUE
    isFirst = FALSE
    predcount = 0
  
  }

  # to collect 6 month data
  if((nowatday > 10) && isDataCollectedTrained==FALSE){
    write.table(paste0("Data collection for training is started.The time is: ",now),"Simulation.csv",append=TRUE, sep=",",col.names = F, row.names = F)
    #poloniex
    startdate=DateUnixTimestampConverter(paste0(whichdate - trainingperiod - 2, sep= " ", "23:45:00"))
    enddate=DateUnixTimestampConverter(paste0(whichdate-1, sep= " ", "23:45:00")) 
    #blockchain.info and ether
    timespan = paste0(trainingperiod+2, "days")
    startdate2 = whichdate - trainingperiod - 3
    AllInfo<-GetData(timespan, startdate2,1, whichdate)
    AllDatagiveninterval<-DatagivenTimeInterval(AllInfo,intervalwanted)
    AllDataFinal<-FinalizeDataBeforeMethods(AllDatagiveninterval,intervalwanted,howmanylags,whataboutdailylag,whataboutweeklylag,startdate,enddate)
    write.table(paste0("Training is started.The time is: ",now),"Simulation.csv",append=TRUE, sep=",",col.names = F, row.names = F)
    RFobject=TradewithRandomForest(AllDataFinal)
    LRobject=TradewithLogisticRegression(AllDataFinal)
    write.table(paste0("Training is finished.The time is: ",now),"Simulation.csv",append=TRUE, sep=",",col.names = F, row.names = F)
    isInitialized = FALSE
    isDataCollectedTrained = TRUE
    
    
  }
  
  # to collect daily data
  if((nowatday > 60) && (nowatday < 120) && isInitialized==FALSE && isDataCollectedTrained == TRUE){
    write.table(paste0("Data collection for daily input is started.The time is: ",now),"Simulation.csv",append=TRUE, sep=",",col.names = F, row.names = F)
    inputrow=GetDailyInput(whichdate-2, whichdate) #00.00yi gectik
    write.table(paste0("Data collection for daily input is finished.The time is: ",now),"Simulation.csv",append=TRUE, sep=",",col.names = F, row.names = F)
    # Dunun prediction matrix ve exchange rate datasina bakarak hangi model ve threshold'a karar ver.
    ret <- WhichModel(PredMatRF, PredMatLR, intervalwanted,whichdate)
    alpha <- ret[1]
    threshold <- ret[2]
    isDecided = FALSE
    isInitialized = TRUE
    isFirst = TRUE
    print("We got the daily input")
  }
  
  if((nowatday %% min15) < 60 && isInitialized==TRUE){
    isDecided = FALSE
  }
  
  
  
  if((nowatday %% min15) > 100 && isDecided == FALSE && isInitialized==TRUE && isTrade==FALSE){
    write.table(paste0("Each 15 min get the lagged input.The time is: ",now),"Simulation.csv",append=TRUE, sep=",",col.names = F, row.names = F)
    print("15 min loop is okay")
    afterinputrow=cbind(inputrow,RealTimeLags(intervalwanted,now))
    afterinputrow=as.data.frame(afterinputrow)
    write.table(paste0("Prediction has started.The time is: ",now),"Simulation.csv",append=TRUE, sep=",",col.names = F, row.names = F)
    RFprediction=PredictwithRF(RFobject,afterinputrow)
    LRprediction=PredictwithLR(LRobject,afterinputrow)
    predcount <- predcount + 1
    write.table(paste0("Prediction has finished.The time is: ",now),"Simulation.csv",append=TRUE, sep=",",col.names = F, row.names = F)
    PredMatRF [predcount,1] <- RFprediction[1,2]
    PredMatLR [predcount,1] <- LRprediction
    
    
    ### bu predictionlarla ve decision making algoritmasiyla bir karara var.
    
    if((RFprediction[1,2]*alpha + LRprediction*(1-alpha)) >= threshold){
      isTrade=TRUE
      
      write.table(as.character(now),"ManualTradeHistory.csv",append=T, sep=",",col.names = F, row.names = F)
      write.table(paste0("The decision has been made, ",isTrade,". Time is: ",now),"Simulation.csv",append=TRUE, sep=",",col.names = F, row.names = F)
      # tradestatus=buyETH()
      # if(tradestatus==FALSE){
      #   isTrade=FALSE
      # }
    
      write.table(paste0("Trade of BTC to ETH has finished.The time is: ",now),"Simulation.csv",append=TRUE, sep=",",col.names = F, row.names = F)
      
    }
    
    isDecided = TRUE
  }
  
  if((nowatday %% min15) > 880 && isTrade == TRUE){
    isTrade = FALSE
    write.table(paste0("We are selling ETH for sure.The time is: ",now),"Simulation.csv",append=TRUE, sep=",",col.names = F, row.names = F)
    # tradestatus=buyBTC()
    # if(tradestatus==FALSE){
    #   isTrade=TRUE
    # }
    
    write.table(paste0("Trade of ETH to BTC has finished.The time is: ",now),"Simulation.csv",append=TRUE, sep=",",col.names = F, row.names = F)
  }
  
}
