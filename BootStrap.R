source("BootstrapFunctions.R")
# Set working directory
#setwd("C:/Users/Pankaj/Documents/Seafile/ML-Coursera/R/swing")
setwd("/home/psharma/Seafile/ML-Coursera/R/swing")
set.seed(567)
#drawdownDaysThreshold=60 #After 60 days of drawdown, no new trades.
plot.new()
frame()
par(mfrow=c(2,4))

# Load data exported from amibroker trade list
f1=c("NSENIFTY_trades.old.csv") #this is the training sample
f2=c("swing_v0.9.csv")     #this is the validation sample
trade.train<-read.csv(paste("",f1,sep=""), header=TRUE,stringsAsFactors = F)
trade.validate<-read.csv(paste("",f2,sep=""), header=TRUE,stringsAsFactors = F)
return.train<-trade.train$X..Profit
return.validate<-trade.validate$X..Profit
profit.validate=trade.validate$Profit
profit.validate=trade.train$Profit
bar.train=trade.train$X..bars-1
bar.validate=trade.validate$X..bars-1
date.train=as.Date(trade.train$Date,format="%d-%b-%y")
date.validate=as.Date(trade.validate$Date,format="%d-%b-%y")




# print(paste("Max monthly Profit: ",max(return.validate.sim.monthlyreturn)))
# print(paste("Max monthly Loss: ",min(return.validate.sim.monthlyreturn)))
# print(paste("Max monthly Profit: ",max(return.validate.sim.monthlyreturn)))
# print(paste("Average Profit: ",mean(return.validate.sim.monthlyreturn)))
# 
# 
# ##### EXTRA ###########
# #check holding period and return distribution
# jointdata<-cbind(return.validate,bar.validate)
# jointpdf <- bkde2D(jointdata, bandwidth=c(BW.ref(return.validate, method = "KS-SJ"), BW.ref(bar.validate, method = "KS-SJ")))
# contour(jointpdf$x1, jointpdf$x2, jointpdf$fhat)
# persp(jointpdf$fhat)
# cov(bar.validate,return.validate)
# #low covariance between holiding period and returns!

# #Calculate CI Method #1
# return.hat=mean(trade.new.meanvec)
# return.se=sd(trade.new.meanvec)
# mean.error=qnorm(0.95)*return.se
# left=trade.new.meanvec-mean.error
# right=trade.new.meanvec+mean.error
# a.m1<-which(left<=mean(trade.new.meanvec))
# b.m1<-which(right>=mean(trade.new.meanvec))
# ci.normal=length(trade.new.meanvec[intersect(a.m1,b.m1)])/length(trade.new.meanvec)
# print(length(trade.new.meanvec[intersect(a.m1,b.m1)]))
# print(ci.normal)
# 
# #Calculate CI Method #2
# left<-quantile(trade.new.meanvec,probs=0.025)
# right<-quantile(trade.new.meanvec,probs=0.975)
# a.m2<-which(left<=trade.new.meanvec)
# b.m2<-which(right>=trade.new.meanvec)
# ci.quantile=length(meanvec[intersect(a.m2,b.m2)])/length(trade.new.meanvec)
# print(length(trade.new.meanvec[intersect(a.m2,b.m2)]))
# print(ci.quantile)
# #For 95% of samples, the sample mean will lie beween the confidence interval range.
# 
# 
# #what can we infer about population mean?
# #probability that population mean is in the range
# #TBD
# 
# 
# #now merge trades.new to "update" population
# trade.pop.return<-c(trade.old.return,trade.new.return)
# trade.pop.density<-density(trade.pop.return)
# plot(trade.pop.density,col="black",ylim=range(trade.old.density$y,trade.new.density$y))
# lines (trade.old.density,col="red",ylim=range(trade.old.density$y,trade.new.density$y))
# lines (density(trade.new.return),col="blue",ylim=range(trade.old.density$y,trade.new.density$y))

# #check if there is any correlation
# acf(trade.train$X..Profit) # no autocorrelation. 
# # This leads to assumption that trades are independent.
# Box.test(trade.train$X..Profit)

# #plot densities of return
# return.train.density<-density(return.train)
# return.validate.density<-density(return.validate)
# plot(return.train.density,col="red",ylim=range(return.train.density$y,return.validate.density$y))
# lines (return.validate.density,col="blue",ylim=range(return.train.density$y,return.validate.density$y))
# return.train.cdf=kCDF(return.train)
# return.validate.cdf=kCDF(return.validate)
# #plot(trade.old.cdf,col="red")
# #plot(trade.cdf,col="blue")
# #trade.old.winprob=1-trade.old.cdf$Fhat[which(trade.old.cdf$x>0)[1]]
# #trade.winprob=1-trade.cdf$Fhat[which(trade.cdf$x>0)[1]]
