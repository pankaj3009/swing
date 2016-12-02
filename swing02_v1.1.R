library(caret)
library(nnet)
library(doParallel)
library(RcppRoll)
library(TTR)
library(rredis)
library(log4r)
library(RTrade)
library(zoo)

writeToRedis = TRUE
getmarketdata=TRUE
dataCutOffBefore="2015-01-01"
BackTestStartDate="2016-04-01"
BackTestEndDate="2017-03-31"

#Uncomment the code below for testing
#writeToRedis=FALSE
#args<-c("1","swing02","3","NSENIFTY_IND___","2016-11-18", "8097", "8128.75", "8048.4", "8077", "0")
#args<-c("1","swing02","3","NSENIFTY_IND___")
# args[1] is a flag for model building. 0=> Build Model, 1=> Backtest 2=> Backtest and BootStrap
# args[2] is the strategy name
# args[3] is the redisdatabase
# args[4],args[5],args[6],args[7],args[8],args[9],args[10] are symbol,date,open high low close volume


logger <- create.logger()
logfile(logger) <- 'base.log'
level(logger) <- 'INFO'
levellog(logger, "INFO", paste(args, collapse = ','))
###### BACKTEST ##############

###### Load Data #############

kairossymbol = unlist(strsplit(args[4], split = "_"))[1] # get the exchange symbol
endtime = format(Sys.time(), format = "%Y-%m-%d %H:%M:%S")
md = data.frame() # create placeholder for market data
if (file.exists(paste(args[4], ".Rdata", sep = ""))) {
  #Load image if it exists to save time
  load(paste(args[4], ".Rdata", sep = ""))
  start = strftime(md[nrow(md), c("date")] + 1, tz = "Asia/Kolkata", "%Y-%m-%d %H:%M:%S")
} else{
  # if image does not exist, give a fixed time
  start = "2012-10-21 09:15:00"
}
if(getmarketdata){
temp <-
  kGetOHLCV(
    paste("symbol", tolower(kairossymbol), sep = "="),
    df=md,
    start = start,
    end = endtime,
    timezone = "Asia/Kolkata",
    name = "india.nse.index.s4.daily",
    ts = c("open", "high", "low", "settle", "volume"),
    aggregators = c("first", "max", "min", "last", "sum"),
    aValue = "1",
    aUnit = "days",
    splits = data.frame(
      date = as.POSIXct(character(), tz = "Asia/Kolkata"),
      symbol = character(),
      oldshares = numeric(),
      newshares = numeric()
      )
  )
if (nrow(temp) > 0) {
  # change col name of settle to close, if temp is returned with data
  #colnames(temp) <- c("date", "open", "high", "low", "close", "volume","symbol")
  temp$symbol = args[4]
}
#md <- rbind(md, temp)
md<-temp
save(md, file = paste(args[4], ".Rdata", sep = "")) # save new market data to disc

levellog(logger, "INFO", paste("endtime=", endtime, sep = ''))
# load(paste(args[4],".Rdata",sep=""))
}
if (nrow(md) > 0) {
  #change col name of settle to close, if temp is returned with data
  colnames(md) <- c( "date","open","high","low","close","volume","symbol","splitadjust",
                     "aopen","ahigh","alow","aclose","avolume")
}

if (length(args) == 10 &
    as.POSIXct(args[5], format = "%Y-%m-%d") > md[nrow(md), c("date")]) {
  # we have ohlc for today. Add to nsenifty
  newrow <-
    data.frame(
      "symbol" = args[4],
      "date" = as.POSIXct(args[5], format = "%Y-%m-%d"),
      "open" = as.numeric(args[6]),
      "high" = as.numeric(args[7]),
      "low" = as.numeric(args[8]),
      "close" = as.numeric(args[9]),
      "volume" = as.numeric(args[10]),
      "aopen" = as.numeric(args[6]),
      "ahigh" = as.numeric(args[7]),
      "alow" = as.numeric(args[8]),
      "aclose" = as.numeric(args[9]),
      "avolume" = as.numeric(args[10]),
      "splitadjust"=1
    )
  
  md <- rbind(md, newrow)
}
md <- unique(md) # remove duplicate rows
load(paste("fit", kairossymbol, "swing02", "v1.0.Rdata", sep =
             "_"))


##### 1. Calculate Indicators ########

trend <- Trend(md$date, md$high, md$low, md$close)
md$trend <- trend$trend
sd <- roll_sd(md$close, 10) * sqrt(9 / 10)
NA9Vec <- rep(NA, 9)
sd <- c(NA9Vec, sd)
md$closezscore <- (md$close - SMA(md$close, 10)) / sd
md$highzscore <-
  (md$high - SMA(md$high, 10)) / c(NA9Vec, roll_sd(md$high, 10) * sqrt(9 /
                                                                         10))
md$lowzscore <-
  (md$low - SMA(md$low, 10)) / c(NA9Vec, roll_sd(md$low, 10) * sqrt(9 / 10))
ma <- SMA(md$close, 10)
md$mazscore <-
  (ma - SMA(ma, 10)) / c(NA9Vec, roll_sd(ma, 10) * sqrt(9 / 10))
daysinupswing = BarsSince(trend$updownbar <= 0)

daysindownswing = BarsSince(trend$updownbar >= 0)

md$swing = ifelse(daysinupswing > 0, 1, 0)

md$daysinuptrend = BarsSince(trend$trend <= 0)

md$daysindowntrend = BarsSince(trend$trend >= 0)

md$daysintrend = ifelse(trend$trend == 1,
                        md$daysinuptrend,
                        ifelse(trend$trend == -1, md$daysindowntrend, 0))

md$daysinswing = daysinupswing + daysindownswing

md$atr <- ATR(md[, c("high", "low", "close")], 10)[, 2]
md <- na.omit(md)
trainingsize = sum(md$date < "2013-01-01")
a <-
  (md$daysintrend - mean(md$daysintrend[md$date < "2013-01-01"])) / (3 * sd(md$daysintrend[md$date <
                                                                                             "2013-01-01"]) * sqrt((trainingsize - 1) / trainingsize) / (2 * pi))
md$softmax_daysintrend <- 1 / (1 + exp(-a))
b <-
  (md$daysinswing - mean(md$daysinswing[md$date < "2013-01-01"])) / (3 * sd(md$daysinswing[md$date <
                                                                                             "2013-01-01"]) * sqrt((trainingsize - 1) / trainingsize) / (2 * pi))
md$softmax_daysinswing <- 1 / (1 + exp(-b))
md$dayreturn <- (log(md$close) - log(Ref(md$close, -1))) *
  100

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
md$stoploss1 <-
  ifelse(
    md$inlongtrade == 1,
    md$close - md$low,
    ifelse(md$inshorttrade == 1, md$high - md$close, 0)
  )
md$stoploss2 <-
  ifelse(md$inlongtrade == 1 |
           md$inshorttrade == 1,
         0.5 * md$atr,
         0)

md$stoplosslevel = pmin(md$stoploss1, md$stoploss2)



##### 4. Generate Trades #########
startindex = which(md$date == BackTestStartDate)
mdsubset<-md[startindex:nrow(md),]
signals <- ApplyStop(mdsubset, mdsubset$stoplosslevel)
trades <- GenerateTrades(signals)
trades$brokerage <-
  (trades$entryprice * 0.0002 + trades$exitprice * 0.0002) / trades$entryprice
trades$netpercentprofit <-
  trades$percentprofit - trades$brokerage
equity <-
  CalculatePortfolioEquityCurve("NSENIFTY_IND___",
                       mdsubset,
                       trades,
                       rep(1050, nrow(md) - startindex),
                       brokerage = 0.0002)

##### Custom Rules ########
# 0. Parameters
numbercontracts = 14
drawdownDaysThreshold = 60


# 1. Generate drawdown bars vector.
returns <-
  trades[trades$entrytime >= "2016-04-01" , c("netpercentprofit")]
bars <- trades[trades$entrytime >= "2016-04-01", c("bars")]
amendedsize<-RTrade::calcDerivedContracts(dddays=0,ddamt=0.1,recoverybonus=0,ddcost=0,stop=0,margin=1,charttitle="swing01",
                                          returnvector=returns,
                                          tradebarvector=bars,
                                          contractsize=14,
                                          derivedleg=1)

tmp<-data.frame(date=trades$entrytime,amendedsize=amendedsize)
tmp1<-merge(tmp,md,by="date",all=TRUE)
amendedsize=na.locf(tmp1$amendedsize)

derivedequity <-
  CalculatePortfolioEquityCurve("NSENIFTY_IND___", mdsubset, trades, amendedsize *
                         75, brokerage = 0.0002)

########### SAVE SIGNALS TO REDIS #################

if (writeToRedis & length(args) == 10 &
    as.POSIXct(args[5], format = "%Y-%m-%d") == md[nrow(md), c("date")]) {
  levellog(logger, "INFO", "Saving trades to Redis")
  redisConnect()
  redisSelect(as.numeric(args[3]))
  bsell = signals[nrow(signals), c("sell")]
  bcover = signals[nrow(signals), c("cover")]
  strategyside = ifelse(bsell == 1, "SELL", ifelse(bcover ==
                                                     1, "COVER", "AVOID"))

  strategysize = ifelse(strategyside != "AVOID", abs(equity[nrow(derivedequity) -
                                                              1, c("contracts")]), 0)
  if (strategysize > 0) {
    #push to redis if bsell or bcover are ==1. Other values are intra-day stops
    redisRPush(paste("trades", args[2], sep = ":"), charToRaw(paste(
      args[4], strategysize, strategyside, 0, strategysize,sep = ":"
    )))
    levellog(logger,
             "INFO",
             paste(args[4], strategysize, strategyside, 0,strategysize, sep = ":"))
  }

  #save last entry action to redis
  bbuy = signals[nrow(signals), c("buy")]
  bshort = signals[nrow(signals), c("short")]
  strategyside = ifelse(bbuy, "BUY", ifelse(bshort, "SHORT", "AVOID"))

  strategysize = ifelse(strategyside != "AVOID", as.character(amendedsize[length(amendedsize)] * 75), 0)
  slpoints = md[nrow(md), c("stoplosslevel")]
  if (strategysize > 0) {
    redisRPush(paste("trades", args[2], sep = ":"), charToRaw(
      paste(args[4], strategysize, strategyside, slpoints,0, sep = ":")
    ))
    levellog(logger,
             "INFO",
             paste(args[4], strategysize, strategyside, slpoints,0, sep = ":"))
  }
  redisClose()
}


############### SAVE BOD STOP LEVELS TO REDIS #############
if (writeToRedis &
    length(args) == 4 &
    args[1] == 1) {
  # we have args[1]=1,args[2]=strategyname and args[3]==redisdatabase
  levellog(logger, "INFO", "Saving BOD levels to Redis")

  redisConnect()
  redisSelect(as.numeric(args[3]))
  #bbuy = signals[nrow(signals), c("buy")]
  #bshort = signals[nrow(signals), c("short")]
  #strategyside = ifelse(bbuy, "BUY", ifelse(bshort, "SHORT", "AVOID"))
  bbuy = length(grep("Long",trades[nrow(trades),c("trade")]))
  bshort = length(grep("Short",trades[nrow(trades),c("trade")]))
  strategyside = ifelse(bbuy>0, "BUY", ifelse(bshort>0, "SHORT", "AVOID"))

  buyindices=which(signals$buy>=1)
  shortindices=which(signals$short>=1)
  buyindex=tail(buyindices,1)
  shortindex=tail(shortindices,1)

  strategysize = ifelse(strategyside != "AVOID", as.character(amendedsize[length(amendedsize)] * 75), 0)
  slpoints = mdsubset[max(buyindex,shortindex), c("stoplosslevel")]
  underlying = mdsubset[max(buyindex,shortindex), c("close")]
  if (strategysize > 0) {
    redisRPush(paste("recontrades", args[2], sep = ":"), charToRaw(
      paste(
        args[4],
        strategysize,
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
        "swing02",
        args[4],
        strategysize,
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

if (args[1] == 2) {
  #Bootstrap
  returns <-
    trades[trades$entrytime >= "2013-01-01" & trades$entrytime < "2016-01-01" , c("netpercentprofit")]
  bars <- trades[trades$entrytime >= "2013-01-01" & trades$entrytime < "2016-01-01", c("bars")]
  count <- length(returns)
  print(count)
  bootstrap(dddays=0,ddamt=0.10,recoverybonus=0,ddcost=0,stop=0,margin=1,charttitle="swing02",
            returnvector=returns,
            tradebarvector=bars,
            samples=1000,
            samplesize=as.integer(count/3),
            contractsize=7,
            derivedleg=1)
  }
#545 trades in 3.5 years till 23 Aug 2016.
  # 545/3.5 = 155 trades / year



#par(mfrow=c(2,1))
#plot(y=equity$profit,x=equity$date,type="l")
#plot(y=derivedequity$profit,x=derivedequity$date,type="l")

############### BUILD MODEL #############

if (args[1] == 0 & length(args) == 1)
  # Build Model
{
  seed <- .Random.seed
  #save(".Random.seed",file="random_state_seed.Rdata") ## save current state
  setwd("C:/Users/Pankaj/Documents/Seafile/ML-Coursera/R")
  load("NSENIFTY_IND___.Rdata")
  data <- md
  trend = Trend(data$date, data$high, data$low, data$close)
  data$trend <- trend$trend
  data$daysinupswing <- BarsSince(trend$updownbar <= 0)
  data$daysindownswing = BarsSince(trend$updownbar >= 0)
  daysinuptrend = BarsSince(trend$trend <= 0)

  daysindowntrend = BarsSince(trend$trend >= 0)

  data$daysoutsidetrend = BarsSince(trend$trend != 0)
  data$daysintrend = ifelse(trend$trend == 1,
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
  updownbar[nrow(data)] = 0
  outsidebar <- Ref(trend$outsidebar, 1)
  outsidebar[nrow(data)] = 0
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
  td$y = factor(td$y, labels = c("down", "up", "avoid"))
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
  fileName = paste("fit_", "NSENIFTY", "swing,v1.2.Rdata", sep = "_")
  fit$seed <- seed
  save(fit, file = fileName)

  #Statistics
  shortlist = rownames(fit$bestTune)
  Accuracy = fit$results[shortlist,]$Accuracy
  AccuracySD = fit$results[shortlist,]$AccuracySD
  print(Accuracy)
  td.predict.class <- predict(fit, trainTransformed)
  confusion.matrix.training <-
    caret::confusionMatrix(td.predict.class, td$y)
  print(confusion.matrix.training)

  #Validation
  vd <- data[data$date >= "2013-01-01",]
  vd$y = factor(vd$y, labels = c("down", "up", "avoid"))
  #validationTransformed=predict(z,vd)
  validationTransformed = vd[,!names(vd) %in% c("y")]
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
