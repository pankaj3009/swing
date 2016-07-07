library(caret)
library(nnet)
library(pROC)
library(ptw)
library(doParallel)

#set.seed(998)
seed<-.Random.seed
save(".Random.seed",file="random_state_seed.Rdata") ## save current state
setwd("C:/Users/Pankaj/Documents/Seafile/ML-Coursera/R/swing")
f=c("NSENIFTY.csv")
data<-read.csv(paste("",f,sep=""), header=TRUE);
data$Date<-as.Date(data$Date,format="%d-%m-%Y")
drops <- c("cv_insidebar","cv_outsidebar")
data<-data[ , !(names(data) %in% drops)]
data[data==-100001] = NA
data<-na.omit(data)
trainingdata<-data[data$Date<as.Date('01-01-2013',"%d-%m-%Y"),]
# Q 1 - is y depended on trend? Answer: mildly
# p(y=0)=0.42,            p(y=1)=0.49,          p(y=2)=0.09
# p(y=0|trend=-1)= 0.50   p(y=1|trend=1)=0.55   

sample1<-trainingdata[,c("trend","y")]
prob_y_0=table(sample1$y)[1]/length(sample1$y)
prob_y_1=table(sample1$y)[2]/length(sample1$y)
prob_y_2=table(sample1$y)[3]/length(sample1$y)
prob_y_0_trend_0=table(sample1[sample1$trend==0,]$y)[1]/sum(sample1$trend==0)
prob_y_1_trend_0=table(sample1[sample1$trend==0,]$y)[2]/sum(sample1$trend==0)
prob_y_2_trend_0=table(sample1[sample1$trend==0,]$y)[3]/sum(sample1$trend==0)
prob_y_0_trend_p1=table(sample1[sample1$trend==1,]$y)[1]/sum(sample1$trend==1)
prob_y_1_trend_p1=table(sample1[sample1$trend==1,]$y)[2]/sum(sample1$trend==1)
prob_y_2_trend_p1=table(sample1[sample1$trend==1,]$y)[3]/sum(sample1$trend==1)
prob_y_0_trend_n1=table(sample1[sample1$trend==-1,]$y)[1]/sum(sample1$trend==-1)
prob_y_1_trend_n1=table(sample1[sample1$trend==-1,]$y)[2]/sum(sample1$trend==-1)
prob_y_2_trend_n1=table(sample1[sample1$trend==-1,]$y)[3]/sum(sample1$trend==-1)
prob_trend_ne0=length(sample1[sample1$trend!=0,]$y)/dim(sample1)[1]


# Q 2 - what is the mean and SD of daysintrend. What is the distribution of days in trend?
# Ans: mean=7.28, sd=6.81
# distribution is exponential
sample2<-trainingdata[,c("trend","daysintrend","y")]
sample2=sample2[sample2$daysintrend>0,]
prob_daysintrend<-hist(sample2$daysintrend,breaks=seq(1,100,1),freq=FALSE,right=FALSE)
prob_daysintrend_trend_p1<-hist(sample2[sample2$trend==1,]$daysintrend,breaks=seq(1,100,1),freq=FALSE,right=FALSE)
prob_daysintrend_trend_n1<-hist(sample2[sample2$trend==-1,]$daysintrend,breaks=seq(1,100,1),freq=FALSE,right=FALSE)
binlength=length(prob_daysintrend$density)
ev_daysintrend<-sum(prob_daysintrend$density*prob_daysintrend$breaks[1:binlength])
ev_daysintrend_trend_p1<-sum(prob_daysintrend_trend_p1$density*prob_daysintrend_trend_p1$breaks[1:binlength])
ev_daysintrend_trend_n1<-sum(prob_daysintrend_trend_n1$density*prob_daysintrend_trend_n1$breaks[1:binlength])
sd_daysintrend<-sd(sample2$daysintrend)
sd_daysintrend_trend_p1<-sd(sample2[sample2$trend==1,]$daysintrend)
sd_daysintrend_trend_n1<-sd(sample2[sample2$trend==-1,]$daysintrend)
zscore_daysintrend<-(sample2$daysintrend-ev_daysintrend)/sd_daysintrend
a<-(sample2$daysintrend-ev_daysintrend)/(3*sd_daysintrend/(2*pi))
daysintrend_softmax<-1/(1+exp(-a))

# Q3: how are daysinupswing and uptrend related?
sample3<-trainingdata[,c("trend","daysintrend","swing","daysinswing","y")]
sample3=sample3[sample3$daysintrend>0,]
prob_daysintrend<-hist(sample3$daysintrend,breaks=seq(1,100,1),freq=FALSE,right=FALSE)
prob_daysinupswing_trend_p1<-hist(sample3[sample3$trend==1,]$daysinswing,breaks=seq(1,100,1),freq=FALSE,right=FALSE)
prob_daysintrend_trend_n1<-hist(sample2[sample2$trend==-1,]$daysintrend,breaks=seq(1,100,1),freq=FALSE,right=FALSE)
binlength=length(prob_daysintrend$density)
ev_daysintrend<-sum(prob_daysintrend$density*prob_daysintrend$breaks[1:binlength])
ev_daysintrend_trend_p1<-sum(prob_daysintrend_trend_p1$density*prob_daysintrend_trend_p1$breaks[1:binlength])
ev_daysintrend_trend_n1<-sum(prob_daysintrend_trend_n1$density*prob_daysintrend_trend_n1$breaks[1:binlength])
sd_daysintrend<-sd(sample2$daysintrend)
sd_daysintrend_trend_p1<-sd(sample2[sample2$trend==1,]$daysintrend)
sd_daysintrend_trend_n1<-sd(sample2[sample2$trend==-1,]$daysintrend)
zscore_daysintrend<-(sample2$daysintrend-ev_daysintrend)/sd_daysintrend
a<-(sample2$daysintrend-ev_daysintrend)/(3*sd_daysintrend/(2*pi))
daysintrend_softmax<-1/(1+exp(-a))

# Q 4: Create softmax for daysinswing
b<-(sample3$daysinswing-mean(sample3$daysinswing))/(3*sd(sample3$daysinswing)/(2*pi))
daysinswing_softmax<-1/(1+exp(-b))
prob_daysinswing<-hist(trainingdata$daysinswing,breaks=seq(1,100,1),freq=FALSE,right=FALSE)

#enhance data frame
a<-(trainingdata$daysintrend-mean(trainingdata$daysintrend))/(3*sd(trainingdata$daysintrend)/(2*pi))
trainingdata$softmax_daysintrend<-1/(1+exp(-a))

b<-(trainingdata$daysinswing-mean(trainingdata$daysinswing))/(3*sd(trainingdata$daysinswing)/(2*pi))
trainingdata$softmax_daysinswing<-1/(1+exp(-b))

#train nn
td<-trainingdata[,names(trainingdata)%in%c("trend","softmax_daysintrend",
                                           "swing",
                                           "softmax_daysinswing",
                                           "closezscore",
                                           "highzscore",
                                           "lowzscore",
                                           "mazscore",
                                           "dayreturn",
                                           "y"
                                           )];
#factor y
td$y=factor(td$y,labels=c("down","up"))
td<-caret::upSample(x = td[,!names(td)%in%c("y")],y = td$y, yname = "y")
#z<-preProcess(td[,!names(td)%in%c("y")], method = c("ica"),n.comp=7)
#trainTransformed <- predict(z, td[,!names(td)%in%c("y")])
trainTransformed<-td[,!names(td)%in%c("y")]
fitControl <- caret::trainControl(method = "repeatedcv", number = 10,repeats = 10,classProbs=TRUE,returnData=TRUE,verboseIter=TRUE)
grid <- data.frame(size=seq(17,21,2),decay=c(0.1))
#grid<-rbind(grid,data.frame(size=seq(3,7,2),decay=c(0.1)))
cl <- makeCluster(detectCores())
registerDoParallel(cl)
fit<-caret::train(x=trainTransformed,y=td[,dim(td)[2]],method="nnet",maxit=1000,trControl=fitControl,tuneGrid=grid)
stopCluster(cl)
fileName=paste("fit_",strsplit(f,"[.]")[[1]][1],"_caret_3.Rdata",sep="")
fit$seed<-seed
save(fit,file=fileName)
#fileName=paste("pca_",strsplit(f,"[.]")[[1]][1],"_caret_3.Rdata",sep="")
#save(z,file=fileName)

#Statistics
shortlist=rownames(fit$bestTune)
Accuracy=fit$results[shortlist,]$Accuracy
AccuracySD=fit$results[shortlist,]$AccuracySD
print(Accuracy)
td.predict.class<-predict(fit,trainTransformed)
confusion.matrix.training<-caret::confusionMatrix(td.predict.class,td$y)
print(confusion.matrix.training)

#Validation
vd<-data[data$Date>=as.Date('01-01-2016',"%d-%m-%Y"),]
a<-(vd$daysintrend-mean(trainingdata$daysintrend))/(3*sd(trainingdata$daysintrend)/(2*pi))
vd$softmax_daysintrend<-1/(1+exp(-a))

b<-(vd$daysinswing-mean(trainingdata$daysinswing))/(3*sd(trainingdata$daysinswing)/(2*pi))
vd$softmax_daysinswing<-1/(1+exp(-b))

vd$y=factor(vd$y,labels=c("down","up"))
#validationTransformed=predict(z,vd)
validationTransformed=vd[,!names(vd)%in%c("y")]
validation.predict.raw<-predict(fit,validationTransformed,type='prob')
validation.predict.class<-predict(fit,validationTransformed)
confusion.matrix.validation<-caret::confusionMatrix(validation.predict.class,vd$y)
print(mean(validation.predict.class==vd$y))
print(confusion.matrix.validation)

importance <- varImp(fit, scale=FALSE)
print(importance)