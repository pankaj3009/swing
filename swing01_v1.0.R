library(caret)
library(nnet)
library(pROC)
library(ptw)
library(doParallel)
library(RcppRoll)
library(TTR)
library(Rcpp)
library(rredis)
library(log4r)
source("swing/kDB.R")
source("swing/BootstrapFunctions.R")
sourceCpp("swing/RAmibroker.cpp")
#set.seed(998)

#Uncomment the code below for testing

# commandArgs <- function() {
#         c(
#                 "1",
#                 "swing01",
#                 "3",
#                 "NSENIFTY_IND___",
#                 "2016-07-07",
#                 "8378.8",
#                 "8498.3",
#                 "8164.95",
#                 "8170.1",
#                 "0"
#                 
#         )
# }

# commandArgs <- function() {
#         c(
#                 "1",
#                 "swing01",
#                 "3",
#                 "NSENIFTY_IND___"
# 
#         )
# }
# 
# 
# args = commandArgs()
writeToRedis=TRUE
# args[1] is a flag for model building. 0=> Build Model, 1=> Backtest 2=> Backtest and BootStrap
# args[2] is the strategy name
# args[3] is the redisdatabase
# args[4],args[5],args[6],args[7],args[8],args[9],args[10] are symbol,date,open high low close volume
# args[11] = true, => append at end

logger <- create.logger()
logfile(logger) <- 'base.log'
level(logger) <- 'INFO'
levellog(logger, "INFO", paste(args,collapse=','))
###### BACKTEST ##############

###### Load Data #############

if (args[1] == 1 | args[1]==2) {
        kairossymbol=unlist(strsplit(args[4],split="_"))[1]
        endtime=format(Sys.time(),format="%Y-%m-%d %H-%M-%S")
        md <-
                kGetOHLCV(
                        start = "1990-05-21 09:15:00",
                        end = endtime,
                        timezone = "Asia/Kolkata",
                        name = "india.nse.index.s4.daily",
                        aName = NULL,
                        aValue = NULL,
                        aUnit = NULL,
                        paste("symbol",tolower(kairossymbol),sep="=")
                )
        md$symbol = args[4]
        levellog(logger, "INFO", paste("endtime=",endtime,sep=''))
        #save(md,file=paste(args[4],".Rdata",sep=""))
        #load("swing/NSENIFTY_IND___.Rdata")
        if (length(args) == 10 &
            as.POSIXct(args[5], format = "%Y-%m-%d") > md[nrow(md), c("date")]) {
                # we have ohlc for today. Add to nenifty
                newrow <-
                        data.frame(
                                "symbol" = args[4],
                                "date" = as.POSIXct(args[5], format = "%Y-%m-%d"),
                                "open" = as.numeric(args[6]),
                                "high" = as.numeric(args[7]),
                                "low" = as.numeric(args[8]),
                                "close" = as.numeric(args[9]),
                                "volume" = as.numeric(args[10])
                        )
                md <- rbind(md, newrow)
        }
        md <- unique(md) # remove duplicate rows
        if (!exists("fit")) {
                load(paste("swing/fit",kairossymbol,"swing01","v1.0.Rdata",sep="_"))          
        }
        
        ##### 1. Calculate Indicators ########
        trend = Trend(md$date, md$high, md$low, md$close)
        md$trend <- trend$trend
        md$daysinupswing <- BarsSince(trend$updownbar <= 0)
        md$daysindownswing = BarsSince(trend$updownbar >= 0)
        daysinuptrend = BarsSince(trend$trend <= 0)
        
        daysindowntrend = BarsSince(trend$trend >= 0)
        
        md$daysoutsidetrend = BarsSince(trend$trend != 0)
        md$daysintrend = ifelse(trend$trend == 1,
                                daysinuptrend,
                                ifelse(trend$trend == -1, daysindowntrend, 0))
        
        sd <- roll_sd(md$close, 10) * sqrt(9 / 10)
        NA9Vec <- rep(NA, 9)
        sd <- c(NA9Vec, sd)
        md$closezscore <- (md$close - SMA(md$close, 10)) / sd
        md$highzscore <-
                (md$high - SMA(md$high, 10)) / c(NA9Vec, roll_sd(md$high, 10) *
                                                         sqrt(9 / 10))
        md$lowzscore <-
                (md$low - SMA(md$low, 10)) / c(NA9Vec, roll_sd(md$low, 10) *
                                                       sqrt(9 / 10))
        ma <- SMA(md$close, 10)
        md$mazscore <-
                (ma - SMA(ma, 10)) / c(NA9Vec, roll_sd(ma, 10) * sqrt(9 / 10))
        md$adx <- ADX(md[, c("high", "low", "close")])[, c("ADX")]
        
        ####### 2. Generate Buy/Sell Arrays ##########
        
        md$predict.raw <- predict(fit, md, type = 'prob')
        md$predict.class <- predict(fit, md)
        
        md$buy = as.numeric(md$predict.class) == 2
        md$sell = as.numeric(md$predict.class) == 1 |
                as.numeric(md$predict.class) == 3
        md$short = as.numeric(md$predict.class) == 1
        md$cover = as.numeric(md$predict.class) == 2 |
                as.numeric(md$predict.class) == 3
        
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
        md$atr <- ATR(md[, c("high", "low", "close")], 10)[, 2]
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
        startindex = which(md$date == "2013-01-01")
        signals <- GenerateSignals(md, md$stoplosslevel)
        trades <- GenerateTrades(signals)
        trades$brokerage <-
                (trades$entryprice * 0.0002 + trades$exitprice * 0.0002) / trades$entryprice
        trades$netpercentprofit <- trades$percentprofit - trades$brokerage
        equity <-
                CalculateEquityCurve("NSENIFTY_IND___",
                                     md[startindex:nrow(md), ],
                                     trades,
                                     rep(525, nrow(md) - startindex),
                                     brokerage = 0.0002)
        
        ##### Custom Rules ########
        # 0. Parameters
        numbercontracts = 7
        drawdownDaysThreshold = 60
        
        
        # 1. Generate drawdown bars vector.
        equityreturn <- diff(equity$profit, 1)
        equityreturn <- c(NA, equityreturn)
        equityreturn <-
                equityreturn / 4000000 # covert to a percentage on a constant denominator. This is driven by the assumption under dddays calculator
        # We are starting from 1st Jan 2013 to calculate drawdowns days. So subset equity return from this date.
        #equityreturn<-equityreturn[4267:nrow(md)]
        equityreturn[1] = 0
        ddbars <-
                calcDrawdownDayCount(seq(1, length(equityreturn), 1), equityreturn, rep(1, length(equityreturn)))
        drawdowns <-
                data.frame(
                        "date" = equity$date,
                        "profit" = equity$profit,
                        "ddbars" = ddbars
                )
        amendedsize <-
                round(
                        numbercontracts - pmin(drawdownDaysThreshold, ddbars) * numbercontracts /
                                drawdownDaysThreshold
                )
        amendedsize <- shift(amendedsize, 1)
        amendedsize[1] = numbercontracts
        derivedequity <-
                CalculateEquityCurve("NSENIFTY_IND___", md[startindex:nrow(md), ], trades, amendedsize *
                                             75, brokerage = 0.0002)
        
        ########### SAVE SIGNALS TO REDIS #################
        
        if (length(args) == 10 &
            as.POSIXct(args[5], format = "%Y-%m-%d") == md[nrow(md), c("date")]) {
                levellog(logger, "INFO", "Saving trades to Redis")
                redisConnect()
                redisSelect(as.numeric(args[3]))  
                bsell=signals[nrow(signals),c("sell")]
                bcover=signals[nrow(signals),c("cover")] 
                strategyside=ifelse(bsell,"SELL",ifelse(bcover,"COVER","AVOID"));
                strategysize=ifelse(strategyside!="AVOID",abs(equity[nrow(derivedequity)-1,c("contracts")]),0)
                if(strategysize>0){
                        redisRPush(paste("trades",args[2],sep=":"),charToRaw(paste(args[4], strategysize,strategyside,0, sep = ":")))
                        levellog(logger, "INFO", paste(args[4], strategysize,strategyside,0, sep = ":"))
                }
                
                #save last entry action to redis
                bbuy=signals[nrow(signals),c("buy")]
                bshort=signals[nrow(signals),c("short")] 
                strategyside=ifelse(bbuy,"BUY",ifelse(bshort,"SHORT","AVOID"));
                strategysize=ifelse(strategyside!="AVOID",as.character(amendedsize[length(amendedsize)] * 75),0)
                slpoints=md[nrow(md),c("stoplosslevel")]
                if(strategysize>0){
                        redisRPush(paste("trades",args[2],sep=":"),charToRaw(paste(args[4], strategysize,strategyside,slpoints, sep = ":")))
                        levellog(logger, "INFO", paste(args[4], strategysize,strategyside,slpoints, sep = ":"))
                }
                redisClose()
        }
        
        
        ############### SAVE BOD STOP LEVELS TO REDIS #############
        if (length(args) == 4 & args[1]==1 ){ # we have args[1]=1,args[2]=strategyname and args[3]==redisdatabase
                levellog(logger, "INFO", "Saving BOD levels to Redis")
                
                redisConnect()
                redisSelect(as.numeric(args[3]))  
                bbuy=signals[nrow(signals),c("buy")]
                bshort=signals[nrow(signals),c("short")] 
                strategyside=ifelse(bbuy,"BUY",ifelse(bshort,"SHORT","AVOID"));
                strategysize=ifelse(strategyside!="AVOID",as.character(amendedsize[length(amendedsize)] * 75),0)
                slpoints=md[nrow(md),c("stoplosslevel")]
                if(strategysize>0){
                        redisRPush(paste("recontrades",args[2],sep=":"),charToRaw(paste(args[4], strategysize,strategyside,slpoints, sep = ":")))
                        levellog(logger, "INFO", paste(args[4], strategysize,strategyside,slpoints, sep = ":"))
                }
                redisClose()
        }
}


############### BOOTSTRAP #############

if(args[1]==2){ #Bootstrap
        par(mfrow=c(2,4))
        returns<-trades[trades$entrytime>="2013-01-01",c("netpercentprofit")]
        bars<-trades[trades$entrytime>="2013-01-01",c("bars")]
        count<-length(returns)
        bootstrap(returns,bars,1000,401,60)
}


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
        load("swing/NSENIFTY_IND___.Rdata")
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
        data$mazscore <- (ma - SMA(ma, 10)) / c(NA9Vec, roll_sd(ma, 10) * sqrt(9 /
                                                                                       10))
        data$adx <- ADX(data[, c("high", "low", "close")])[, c("ADX")]
        updownbar <- RowShift(trend$updownbar, 1)
        updownbar[nrow(data)] = 0
        outsidebar <- RowShift(trend$outsidebar, 1)
        outsidebar[nrow(data)] = 0
        data$y <- ifelse(outsidebar == 1, 2, ifelse(updownbar == -1, 0, 1))
        
        data <- na.omit(data)
        trainingdata <- data[data$date < "2013-01-01", ]
        
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
                caret::upSample(x = td[, !names(td) %in% c("y")], y = td$y, yname = "y")
        #z<-preProcess(td[,!names(td)%in%c("y")], method = c("ica"),n.comp=7)
        #trainTransformed <- predict(z, td[,!names(td)%in%c("y")])
        trainTransformed <- td[, !names(td) %in% c("y")]
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
        Accuracy = fit$results[shortlist, ]$Accuracy
        AccuracySD = fit$results[shortlist, ]$AccuracySD
        print(Accuracy)
        td.predict.class <- predict(fit, trainTransformed)
        confusion.matrix.training <-
                caret::confusionMatrix(td.predict.class, td$y)
        print(confusion.matrix.training)
        
        #Validation
        vd <- data[data$date >= "2013-01-01", ]
        vd$y = factor(vd$y, labels = c("down", "up", "avoid"))
        #validationTransformed=predict(z,vd)
        validationTransformed = vd[, !names(vd) %in% c("y")]
        validation.predict.raw <-
                predict(fit, validationTransformed, type = 'prob')
        validation.predict.class <- predict(fit, validationTransformed)
        confusion.matrix.validation <-
                caret::confusionMatrix(validation.predict.class, vd$y)
        print(mean(validation.predict.class == vd$y))
        print(confusion.matrix.validation)
        
        importance <- varImp(fit, scale = FALSE)
        print(importance)
} else{
        print("Skipped Model Creation")
}
