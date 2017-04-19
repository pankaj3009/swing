#v1.2
#Support backtest using options
library(caret)
library(nnet)
library(doParallel)
library(RcppRoll)
library(TTR)
library(rredis)
library(log4r)
library(RTrade)
library(RQuantLib)
options(scipen=999)

args.commandline=commandArgs(trailingOnly=TRUE)
if(length(args.commandline)>0){
  args<-args.commandline
}

# args<-c("2","swing01","3")
# args[1] is a flag for model building. 0=> Build Model, 1=> Generate Signals in Production 2=> Backtest and BootStrap 4=>Save BOD Signals to Redis
# args[2] is the strategy name
# args[3] is the redisdatabase
redisConnect()
redisSelect(1)
if(length(args)>1){
  static<-redisHGetAll(toupper(args[2]))
}else{
  static<-redisHGetAll("SWINGA")
}
newargs<-unlist(strsplit(static$args,','))
if(length(args)<=1 && length(newargs>1)){
  args<-newargs
}
redisClose()

kUseSystemDate<-as.logical(static$UseSystemDate)
kWriteToRedis <- as.logical(static$WriteToRedis)
kGetMarketData<-as.logical(static$GetMarketData)
kDataCutOffBefore<-static$DataCutOffBefore
kBackTestStartDate<-static$BackTestStartDate
kBackTestEndDate<-static$BackTestEndDate
kFNODataFolder <- static$FNODataFolder
kNiftyDataFolder <- static$NiftyDataFolder
kTimeZone <- static$TimeZone
kIndex<-static$Index
kMLFile<-static$MLFile
kBrokerage<-as.numeric(static$SingleLegBrokerageAsPercentOfValue)/100
kPerContractBrokerage=as.numeric(static$SingleLegBrokerageAsValuePerContract)
kContractSize=as.numeric(static$ContractSize)
kMaxContracts=as.numeric(static$MaxContracts)
kHomeDirectory=static$HomeDirectory
kExchangeMargin=as.numeric(static$ExchangeMargin)
kDrawdownPercentThreshold=as.numeric(static$DrawdownPercentThreshold)
kDrawdownDaysThreshold=as.numeric(static$DrawdownDaysThreshold)
kDrawdownCost=as.numeric(static$DrawdownCost)
kRecoveryBonus=as.numeric(static$RecoveryBonus)
kMoneyManagement=as.numeric(static$MoneyManagement)
kBackTestEndDate=strftime(adjust("India",as.Date(kBackTestEndDate, tz = kTimeZone),bdc=2),"%Y-%m-%d")
kBackTestStartDate=strftime(adjust("India",as.Date(kBackTestStartDate, tz = kTimeZone),bdc=0),"%Y-%m-%d")
setwd(kHomeDirectory)

today<-strftime(Sys.Date(),tz=kTimeZone,format="%Y-%m-%d")
logger <- create.logger()
logfile(logger) <- static$LogFile
level(logger) <- 'INFO'
levellog(logger,"INFO",args)
###### Load Data #############
kairos.symbol <- unlist(strsplit(kIndex, split = "_"))[1]
endtime <- format(Sys.time(), format = "%Y-%m-%d %H:%M:%S")
md <- data.frame()
if (file.exists(paste(kNiftyDataFolder,kIndex, ".Rdata", sep = ""))) {
  load(paste(kNiftyDataFolder,kIndex, ".Rdata", sep = ""))
  start <- strftime(md[nrow(md), c("date")] + 1, tz = kTimeZone, "%Y-%m-%d %H:%M:%S")
} else{
  start <- "2012-10-21 09:15:00"
}
if(kGetMarketData){
  temp <-
    kGetOHLCV(
      paste("symbol", tolower(kairos.symbol), sep = "="),
      df=md,
      start = start,
      end = endtime,
      timezone = kTimeZone,
      name = "india.nse.index.s4.daily",
      ts = c("open", "high", "low","settle", "volume"),
      aggregators = c("first", "max", "min", "last", "sum"),
      aValue = "1",
      aUnit = "days",
      splits = data.frame(
        date = as.POSIXct(character(), tz = kTimeZone),
        symbol = character(),
        oldshares = numeric(),
        newshares = numeric()
      )
    )
  if (nrow(temp) > 0) {
    temp$symbol <- kIndex
  }
  md<-temp
  save(md, file = paste(kNiftyDataFolder,kIndex, ".Rdata", sep = "")) # save new market data to disc
}
if (length(args) > 1 && args[1]==1) {
  #Backtesting with today's data from realtime sources
  newrow <- getPriceArrayFromRedis(9,"NSENIFTY_IND___","tick","close",paste(today, " 09:12:00"))
  newrow <-
    data.frame(
      "symbol" = "NSENIFTY",
      "date" = newrow$date[1],
      "open" = newrow$open[1],
      "high" = newrow$high[1],
      "low" = newrow$low[1],
      "settle" = newrow$settle[1],
      "close" = newrow$settle[1],
      "volume" = 0,
      "aopen" = newrow$open[1],
      "ahigh" = newrow$high[1],
      "alow" = newrow$low[1],
      "asettle" = newrow$settle[1],
      "aclose" = newrow$settle[1],
      "avolume" = 0,
      "splitadjust" = 1
    )
  redisString<-paste(newrow$date[1],newrow$open[1],newrow$high[1],newrow$low[1],newrow$settle[1],newrow$volume[1],sep=",")
  levellog(logger,"INFO",paste(args[2], redisString, sep = ":"))
  
  md <- rbind(md, newrow)
}

if (nrow(md) > 0) {
  #change col name of settle to close, if temp is returned with data
  md<-md[, !(names(md) %in% c("close","aclose"))]
  colnames(md) <- c( "date","open","high","low","close","volume","symbol","splitadjust",
                     "aopen","ahigh","alow","aclose","avolume")
}

md <- unique(md) # remove duplicate rows
load(kMLFile)

##### 1. Calculate Indicators ########

trend <- Trend(md$date, md$high, md$low, md$close)
md$trend <- trend$trend
sd <- roll_sd(md$close, 10) * sqrt(9 / 10)
NA9Vec <- rep(NA, 9)
sd <- c(NA9Vec, sd)
md$closezscore <- (md$close - SMA(md$close, 10)) / sd
md$highzscore <-  (md$high - SMA(md$high, 10)) / c(NA9Vec, roll_sd(md$high, 10) * sqrt(9 /10))
md$lowzscore <-  (md$low - SMA(md$low, 10)) / c(NA9Vec, roll_sd(md$low, 10) * sqrt(9 / 10))
ma <- SMA(md$close, 10)
md$mazscore <-  (ma - SMA(ma, 10)) / c(NA9Vec, roll_sd(ma, 10) * sqrt(9 / 10))
daysinupswing = BarsSince(trend$updownbar <= 0)
daysindownswing = BarsSince(trend$updownbar >= 0)
md$swing = ifelse(daysinupswing > 0, 1, 0)
md$daysinuptrend = BarsSince(trend$trend <= 0)
md$daysindowntrend = BarsSince(trend$trend >= 0)
md$daysintrend = ifelse(trend$trend == 1,md$daysinuptrend,
                        ifelse(trend$trend == -1, md$daysindowntrend, 0))
md$daysinswing = daysinupswing + daysindownswing
md$atr <- ATR(md[, c("high", "low", "close")], 10)[, 2]
md <- na.omit(md)
trainingsize = sum(md$date < "2013-01-01")
a <-(md$daysintrend - mean(md$daysintrend[md$date < "2013-01-01"])) / (3 * sd(md$daysintrend[md$date <"2013-01-01"]) * sqrt((trainingsize - 1) / trainingsize) / (2 * pi))
md$softmax_daysintrend <- 1 / (1 + exp(-a))
b <-  (md$daysinswing - mean(md$daysinswing[md$date < "2013-01-01"])) / (3 * sd(md$daysinswing[md$date <"2013-01-01"]) * sqrt((trainingsize - 1) / trainingsize) / (2 * pi))
md$softmax_daysinswing <- 1 / (1 + exp(-b))
md$dayreturn <- (log(md$close) - log(Ref(md$close, -1))) *  100

####### 2. Generate Buy/Sell Arrays ##########

md$predict.raw <- predict(fit, md, type = 'prob')
md$predict.class <- predict(fit, md)

md$buy = as.numeric(md$predict.class) == 2
md$sell = as.numeric(md$predict.class) == 1
md$short = as.numeric(md$predict.class) == 1
md$cover = as.numeric(md$predict.class) == 2
md$buy = ExRem(md$buy, md$sell)
md$sell = ExRem(md$sell, md$buy)
md$short = ExRem(md$short, md$cover)
md$cover = ExRem(md$cover, md$short)
md$buyprice <- md$close
md$sellprice <- md$close
md$shortprice <- md$close
md$coverprice <- md$close
md$inlongtrade <- Flip(md$buy, md$sell)
md$inshorttrade <- Flip(md$short, md$cover)

###### 3. Create SL/TP Array ############
md$stoploss1 <-  ifelse(md$inlongtrade == 1,md$close - md$low,
                        ifelse(md$inshorttrade == 1, md$high - md$close, 0))
md$stoploss2 <-  ifelse(md$inlongtrade == 1 | md$inshorttrade == 1,0.5 * md$atr,0)
md$stoplosslevel <- pmin(md$stoploss1, md$stoploss2)

##### 4. Generate Cash Trades #########
startindex <- which(md$date == kBackTestStartDate)
mdsubset<-md[startindex:nrow(md),]
signals <- ApplyStop(mdsubset, mdsubset$stoplosslevel)
trades <- GenerateTrades(signals)
trades$brokerage <- 2*kBrokerage
trades$netpercentprofit <-
  trades$percentprofit - trades$brokerage
equity <-
  CalculatePortfolioEquityCurve("NSENIFTY",
                                mdsubset,
                                trades,
                                rep(kMaxContracts*kContractSize, nrow(md) - startindex),
                                brokerage = kBrokerage)

##### Generate Derived Trades ########
returns <-  trades$netpercentprofit
bars <- trades$bars
amendedsize<-RTrade::calcDerivedContracts(dddays=kDrawdownDaysThreshold,ddamt=kDrawdownPercentThreshold,recoverybonus=kRecoveryBonus,ddcost=kDrawdownCost,stop=0,margin=1,charttitle="swing01",
                                          returnvector=returns,
                                          tradebarvector=bars,
                                          contractsize=kMaxContracts,
                                          derivedleg=1)
tmp<-data.frame(date=trades$entrytime,amendedsize=amendedsize)
tmp1<-merge(tmp,mdsubset,by="date",all=TRUE)
amendedsize<-na.locf(tmp1$amendedsize)
#make amendedsize of same length as nrow(mdsubset)
amendedsize<-c(rep(head(amendedsize,1),nrow(tmp1)-length(amendedsize)),amendedsize)
derivedequity <-
  CalculatePortfolioEquityCurve(kIndex, mdsubset, trades, amendedsize *
                                  kContractSize, brokerage = kBrokerage)


########### Generate Option Trades ################

signals$strike<-round((signals$buyprice+signals$shortprice) / 100) * 100
signals$currentmonthexpiry <- as.Date(sapply(signals$date, getExpiryDate), tz = kTimeZone)
nextexpiry <- as.Date(sapply(
  as.Date(signals$currentmonthexpiry + 20, tz = kTimeZone),
  getExpiryDate
), tz = kTimeZone)
signals$entrycontractexpiry <- as.Date(ifelse(
  businessDaysBetween("India",as.Date(signals$date, tz = kTimeZone),signals$currentmonthexpiry) <= 3,
  nextexpiry,signals$currentmonthexpiry),tz = kTimeZone)
optionSignals<-optionTradeSignalsLongOnly(signals,kFNODataFolder,kNiftyDataFolder)
optionTrades<-GenerateTrades(optionSignals)
optionTrades<-optionTrades[order(optionTrades$entrytime),]
for(i in 1:nrow(optionTrades)){
  if(optionTrades$exittime[i]<"2000-01-01"){
    optionTrades$exittime[i]<-strptime(NA,format="%Y-%m-%d",tz=kTimeZone)
  }
}
if(kMoneyManagement){
  amendedsizedf<-data.frame(date=mdsubset$date,close=mdsubset$close,size=amendedsize)
}else{
  amendedsize<-kMaxContracts
  amendedsizedf<-data.frame(date=mdsubset$date,close=mdsubset$close,size=amendedsize)
}
optionTrades$size<-merge(optionTrades,amendedsizedf,by.x="entrytime",by.y="date")$size*kContractSize
optionTrades$underlying<-merge(optionTrades,amendedsizedf,by.x="entrytime",by.y="date")$close

pricediff<-optionTrades$exitprice-optionTrades$entryprice
absprofit<-ifelse(grepl("BUY",optionTrades$trade),pricediff,-pricediff)
optionTrades$percentprofit<-absprofit/optionTrades$underlying
optionTrades$brokerage <-  (2*kPerContractBrokerage/kContractSize)/optionTrades$entryprice
optionTrades$netpercentprofit <-  optionTrades$percentprofit - optionTrades$brokerage


# update option prices for trades today
# update entry prices
out <- which(as.Date(optionTrades$entrytime,tz=kTimeZone) == Sys.Date())
if(length(out)>0){
  for(o in 1:length(out)){
    index<-out[o]
    newrow <- getPriceArrayFromRedis(9,optionTrades$symbol[index],"tick","close",paste(today, " 09:12:00"))
    if(nrow(newrow)==1){
      optionTrades$entryprice[index]<-newrow$settle[1]
      optionTrades$exitprice[index]<-newrow$settle[1]
    }
  }
}



# update exit prices
out <- which(as.Date(optionTrades$exittime,tz=kTimeZone) == Sys.Date()|is.na(optionTrades$exittime))
if(length(out)>0){
  for(o in 1:length(out)){
    index<-out[o]
    newrow <- getPriceArrayFromRedis(9,optionTrades$symbol[index],"tick","close",paste(today, " 09:12:00"))
    if(nrow(newrow)==1){
      optionTrades$exitprice[index]<-newrow$settle[1]
    }
  }
}

if(length(args)>1 && args[1]==1){
  save(md,file=paste(args[2],"md.Rdata",sep="."))
  save(optionTrades,file=paste(args[2],"optionTrades.Rdata",sep="."))
  
}


########### SAVE SIGNALS TO REDIS #################

entrysize <- 0
exitsize <- 0

if (kUseSystemDate) {
  today = Sys.Date()
} else{
  today = advance("India",dates=Sys.Date(),n=-1,timeUnit = 0,bdc=2)
}
yesterday=advance("India",dates=today,n=-1,timeUnit = 0,bdc=2)

entrycount = which(as.Date(optionTrades$entrytime,tz=kTimeZone) == today)
exitcount = which(as.Date(optionTrades$exittime,tz=kTimeZone) == today)

if (length(exitcount) > 0 && kWriteToRedis && args[1]==1) {
  redisConnect()
  redisSelect(args[3])
  out <- optionTrades[which(as.Date(optionTrades$exittime,tz=kTimeZone) == today),]
  uniquesymbols=NULL
  for (o in 1:nrow(out)) {
    if(length(grep(out[o,"symbol"],uniquesymbols))==0){
      uniquesymbols[length(uniquesymbols)+1]<-out[o,"symbol"]
      startingposition = GetCurrentPosition(out[o, "symbol"], optionTrades,position.on=yesterday,trades.till=yesterday)
      redisString = paste(out[o, "symbol"],
                          abs(startingposition),
                          ifelse(startingposition>0,"SELL","COVER"),
                          0,
                          abs(startingposition),
                          sep = ":")
      redisRPush(paste("trades", args[2], sep = ":"),
                 charToRaw(redisString))
      levellog(logger,
               "INFO",
               paste(args[2], redisString, sep = ":"))
    }
  }
  redisClose()
}


if (length(entrycount) > 0 && kWriteToRedis && args[1]==1) {
  redisConnect()
  redisSelect(args[3])
  out <- optionTrades[which(as.Date(optionTrades$entrytime,tz=kTimeZone) == today),]
  uniquesymbols=NULL
  for (o in 1:nrow(out)) {
    if(length(grep(out[o,"symbol"],uniquesymbols))==0){
      uniquesymbols[length(uniquesymbols)+1]<-out[o,"symbol"]
      startingposition = GetCurrentPosition(out[o, "symbol"], optionTrades,trades.till=yesterday,position.on = today)
      todaytradesize=GetCurrentPosition(out[o, "symbol"], optionTrades)-startingposition
      redisString = paste(out[o, "symbol"],
                          abs(todaytradesize),
                          ifelse(todaytradesize>0,"BUY","SHORT"),
                          0,
                          abs(startingposition),
                          sep = ":")
      redisRPush(paste("trades", args[2], sep = ":"),
                 charToRaw(redisString))
      levellog(logger,
               "INFO",
               paste(args[2], redisString, sep = ":"))
      
    }
    
  }
  redisClose()
}


############### SAVE BOD STOP LEVELS TO REDIS #############
if (kWriteToRedis &  length(args) > 1 && args[1]==4 ) {
  levellog(logger, "INFO", "Saving BOD levels to Redis")
  redisConnect()
  redisSelect(as.numeric(args[3]))
  size <- ifelse(is.na(optionTrades[nrow(optionTrades),'exittime']),optionTrades[nrow(optionTrades),'size'],0)
  strategyside <- ifelse(grepl("BUY",optionTrades[nrow(optionTrades),'trade']), "BUY", ifelse(grepl("SHORT",optionTrades[nrow(optionTrades),'trade']), "SHORT", "AVOID"))
  symbol <- ifelse(nrow(optionTrades>0),optionTrades[nrow(optionTrades),'symbol'],"")
  buyindices<-which(signals$buy>=1)
  shortindices<-which(signals$short>=1)
  buyindex<-tail(buyindices,1)
  shortindex<-tail(shortindices,1)
  slpoints <- mdsubset[max(buyindex,shortindex), c("stoplosslevel")]
  underlying <- mdsubset[max(buyindex,shortindex), c("close")]
  if (size > 0) {
    redisRPush(paste("recontrades", args[2], sep = ":"), charToRaw(
      paste(
        symbol,
        size,
        strategyside,
        slpoints,
        underlying,
        sep = ":"
      )
    ))
    levellog(
      logger,
      "INFO",
      paste(
        args[2],
        symbol,
        size,
        strategyside,
        slpoints,
        underlying,
        sep = ":"
      )
    )
  }
  redisClose()
}

############### BOOTSTRAP #############

if (length(args)>1 && args[1] == 2) {
  #Bootstrap
  trades<-trades[complete.cases(optionTrades),]
  optionTrades<-optionTrades[complete.cases(optionTrades),]
  #  par(mfrow = c(2, 4))
  returns <-
    trades[trades$entrytime >= "2013-04-01" & trades$entrytime <= "2016-03-31" , c("netpercentprofit")]
  derivedreturns <-
    optionTrades[optionTrades$entrytime >= "2013-04-01" & optionTrades$entrytime <= "2016-03-31" , c("netpercentprofit")]
  
  bars <- trades[trades$entrytime >= "2013-04-01" & trades$entrytime <= "2016-03-31", c("bars")]
  count <- length(returns)

  bootstrap(dddays=kDrawdownDaysThreshold,ddamt=kDrawdownPercentThreshold,recoverybonus=kRecoveryBonus,ddcost=kDrawdownCost,stop=0,margin=1,charttitle=args[2],
            returnvector=returns,
            derivedreturn=derivedreturns,
            tradebarvector=bars,
            samples=1000,
            samplesize=as.integer(count/3),
            contractsize=kMaxContracts,
            derivedleg=kMoneyManagement)
}

############### METRICS ##############
if (length(args)>1 && args[1] == 2) {
  #Derived Trades
  pnl <- pnl<-data.frame(bizdays=as.Date(mdsubset$date,tz=kTimeZone),realized=0,unrealized=0,brokerage=0)
  cumpnl<-CalculateDailyPNL(optionTrades[complete.cases(optionTrades),],pnl,kFNODataFolder,kPerContractBrokerage/75,per.contract.brokerage = TRUE)
  cumpnl$daily.pnl <- (cumpnl$realized + cumpnl$unrealized-cumpnl$brokerage) - Ref(cumpnl$realized + cumpnl$unrealized-cumpnl$brokerage, -1)
  cumpnl$daily.pnl <- ifelse(is.na(cumpnl$daily.pnl), 0, cumpnl$daily.pnl)
  cumpnl$daily.return <- cumpnl$daily.pnl*100/(mdsubset$close*kContractSize*75)
  cumpnl$daily.return <- ifelse(is.na(cumpnl$daily.return), 0, cumpnl$daily.return)
  cumpnl$daily.return <- ifelse(is.infinite(cumpnl$daily.return), 0, cumpnl$daily.return)
  grosspnl<-(optionTrades$exitprice-optionTrades$entryprice)*optionTrades$size
  brokerage<- 2*optionTrades$size*kPerContractBrokerage/75
  optionTrades$grosspnl<-ifelse(grepl("BUY",optionTrades$trade),grosspnl,-grosspnl)
  optionTrades$brokerage<-brokerage
  NetProfit<-sum(optionTrades$grosspnl-optionTrades$brokerage)
  AverageMargin=sum(optionTrades$entryprice*optionTrades$size*optionTrades$bars)/sum(optionTrades$bars)
  fy.tmp <- seq( as.POSIXct('2013-04-01'), length=10, by='year')
  cumpnl$fiscalyear=(2014:2023)[ findInterval(as.POSIXct(cumpnl$bizdays,tz=kTimeZone),fy.tmp) ]
  optionTrades$fiscalyear=(2014:2023)[ findInterval(optionTrades$entrytime,fy.tmp) ]
  fiscal.pnl<-tapply(cumpnl$daily.pnl,cumpnl$fiscalyear,sum)
  fiscal.sharpe<-tapply(cumpnl$daily.return,cumpnl$fiscalyear,sharpe)
  fiscal.pnl.recon<-tapply(optionTrades$grosspnl-optionTrades$brokerage,optionTrades$fiscalyear,sum)
  fiscal.margin.num<-tapply(optionTrades$size*optionTrades$entryprice*optionTrades$bars,optionTrades$fiscalyear,sum)
  fiscal.margin.den<-tapply(optionTrades$bars,optionTrades$fiscalyear,sum)
  fiscal.margin<-fiscal.margin.num/fiscal.margin.den
  fiscal.winratio.num<-tapply((optionTrades$grosspnl-optionTrades$brokerage)>0,optionTrades$fiscalyear,sum)
  fiscal.winratio.den<-tapply(optionTrades$grosspnl,optionTrades$fiscalyear,length)
  fiscal.winratio=fiscal.winratio.num/fiscal.winratio.den  
  fiscal.underlying.max<-tapply(optionTrades$entryprice,optionTrades$fiscalyear,max)
  
  reporting<-as.data.frame(fiscal.sharpe)
  reporting=data.frame(reporting,as.data.frame(fiscal.winratio)$fiscal.winratio)
  reporting=data.frame(reporting,as.data.frame(fiscal.pnl)$fiscal.pnl)
  reporting=data.frame(reporting,as.data.frame(fiscal.margin)$fiscal.margin)
  reporting=data.frame(reporting,as.data.frame(fiscal.underlying.max)$fiscal.underlying.max*kContractSize*kMaxContracts)
  colnames(reporting)<-c("Sharpe","WinRatio","NetP&L","AverageMarginUtil","BudgetedMargin")
  reporting$Sharpe<-format(round(reporting$Sharpe,2),nsmall = 2)
  reporting$WinRatio<-format(round(reporting$WinRatio*100,2),nsmall = 2)
  reporting$`NetP&L`<-formatC(reporting$`NetP&L`,format="d", big.mark=',')
  reporting$AverageMarginUtil<- formatC(reporting$AverageMarginUtil,format="d", big.mark=',')
  reporting$BudgetedMargin<- formatC(reporting$BudgetedMargin,format="d", big.mark=',')
}

############### BUILD MODEL #############

if (length(args)>1 && args[1] == 0 )
  # Build Model
{
  seed <- .Random.seed
  #save(".Random.seed",file="random_state_seed.Rdata") ## save current state
  setwd("C:/Users/Pankaj/Documents/Seafile/ML-Coursera/R")
  load("NSENIFTY.Rdata")
  data <- md
  trend <- Trend(data$date, data$high, data$low, data$close)
  data$trend <- trend$trend
  data$daysinupswing <- BarsSince(trend$updownbar <= 0)
  data$daysindownswing <- BarsSince(trend$updownbar >= 0)
  daysinuptrend <- BarsSince(trend$trend <= 0)

  daysindowntrend <- BarsSince(trend$trend >= 0)

  data$daysoutsidetrend <- BarsSince(trend$trend != 0)
  data$daysintrend <- ifelse(trend$trend == 1,
                            daysinuptrend,
                            ifelse(trend$trend == -1, daysindowntrend, 0))

  sd <- roll_sd(md$close, 10) * sqrt(9 / 10)
  NA9Vec <- rep(NA, 9)
  sd <- c(NA9Vec, sd)
  data$closezscore <- (data$close - SMA(data$close, 10)) / sd
  data$highzscore <-
    (data$high - SMA(data$high, 10)) / c(NA9Vec, roll_sd(data$high, 10) * sqrt(9 /
                                                                                 10))
  data$lowzscore <-
    (data$low - SMA(data$low, 10)) / c(NA9Vec, roll_sd(data$low, 10) * sqrt(9 /
                                                                              10))
  ma <- SMA(data$close, 10)
  data$mazscore <-
    (ma - SMA(ma, 10)) / c(NA9Vec, roll_sd(ma, 10) * sqrt(9 /
                                                            10))
  data$adx <-
    ADX(data[, c("high", "low", "close")])[, c("ADX")]
  updownbar <- Ref(trend$updownbar, 1)
  updownbar[nrow(data)] <- 0
  outsidebar <- Ref(trend$outsidebar, 1)
  outsidebar[nrow(data)] <- 0
  data$y <-
    ifelse(outsidebar == 1, 2, ifelse(updownbar == -1, 0, 1))

  data <- na.omit(data)
  trainingdata <- data[data$date < "2013-01-01",]

  #train nn
  td <-
    trainingdata[, names(trainingdata) %in% c(
      "trend",
      "daysinupswing",
      "daysindownswing",
      "daysoutsidetrend",
      "daysintrend",
      "closezscore",
      "highzscore",
      "lowzscore",
      "mazscore",
      "y"
    )]

  #factor y
  td$y <- factor(td$y, labels = c("down", "up", "avoid"))
  td <-
    caret::upSample(x = td[,!names(td) %in% c("y")], y = td$y, yname = "y")
  #z<-preProcess(td[,!names(td)%in%c("y")], method = c("ica"),n.comp=7)
  #trainTransformed <- predict(z, td[,!names(td)%in%c("y")])
  trainTransformed <- td[,!names(td) %in% c("y")]
  fitControl <-
    caret::trainControl(
      method = "repeatedcv",
      number = 10,
      repeats = 10,
      classProbs = TRUE,
      returnData = TRUE,
      verboseIter = TRUE
    )
  grid <- data.frame(size = seq(17, 21, 2), decay = c(0.1))
  #grid<-rbind(grid,data.frame(size=seq(3,7,2),decay=c(0.1)))
  cl <- makeCluster(detectCores())
  registerDoParallel(cl)
  fit <-
    caret::train(
      x = trainTransformed,
      y = td[, dim(td)[2]],
      method = "nnet",
      maxit = 1000,
      trControl = fitControl,
      tuneGrid = grid
    )
  stopCluster(cl)
  fileName <- paste("fit_", "NSENIFTY", "swing,v1.2.Rdata", sep = "_")
  fit$seed <- seed
  save(fit, file = fileName)

  #Statistics
  shortlist <- rownames(fit$bestTune)
  Accuracy <- fit$results[shortlist,]$Accuracy
  AccuracySD <- fit$results[shortlist,]$AccuracySD
  print(Accuracy)
  td.predict.class <- predict(fit, trainTransformed)
  confusion.matrix.training <-
    caret::confusionMatrix(td.predict.class, td$y)
  print(confusion.matrix.training)

  #Validation
  vd <- data[data$date >= "2013-01-01",]
  vd$y <- factor(vd$y, labels = c("down", "up", "avoid"))
  #validationTransformed=predict(z,vd)
  validationTransformed <- vd[,!names(vd) %in% c("y")]
  validation.predict.raw <-
    predict(fit, validationTransformed, type = 'prob')
  validation.predict.class <-
    predict(fit, validationTransformed)
  confusion.matrix.validation <-
    caret::confusionMatrix(validation.predict.class, vd$y)
  print(mean(validation.predict.class == vd$y))
  print(confusion.matrix.validation)

  importance <- varImp(fit, scale = FALSE)
  print(importance)
} else{
  print("Skipped Model Creation")
}
