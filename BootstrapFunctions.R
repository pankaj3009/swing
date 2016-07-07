library(binhf)

###### Functions ##############
# RowShift -> Shifts vector ahead by shiftLen. To shift ahead by 1 and make element[1]=NA, use shiftLen=-1
# FillInTheBlanks -> Fills a vector spaces by prior value in vector
# insertRow -> Inserts a row in a DF, at the provided index r
# calcDrawdownDayCount -> returns drawdown day corresponding to each input row 
# calcDrawdownVector -> Returns a vector of inderminate size containing duration of all drawdowns in the sample
# calcDrawdownValues -> Returns the drawdown currency amount for each input row.
# bootstrap -> bootstraps a sample for simulated returns

RowShift <- function(x, shiftLen = 1L) {
        r <- (1L + shiftLen):(length(x) + shiftLen)
        r[r<1] <- NA
        return(x[r])
}

FillInTheBlanks <- function(S,value) {
        #http://stackoverflow.com/questions/1782704/propagating-data-within-a-vector
        ## NA or 0 in S are replaced with observed values
        
        ## accepts a vector possibly holding NA values and returns a vector
        ## where all observed values are carried forward and the first is
        ## also carried backward.  cfr na.locf from zoo library.
        L <- !is.na(S) & !(S==value)
        #L <- !(S==value |is.na(S))
        c(S[L][1], S[L])[cumsum(L)+1]
}

insertRow <- function(existingDF, newrow, r) {
#Inserts a row in a DF, at the provided index r
  if(r>0){
    existingDF[seq(r+1,nrow(existingDF)+1),] <- existingDF[seq(r,nrow(existingDF)),]
    existingDF[r,] <- newrow
    existingDF
  }else{
    existingDF[r+1,]<-newrow
    existingDF
  }
}

calcDrawdownDayCount<-function(orderingvector,returns,tradedays){
  #Returns drawdown day corresponding to each return.
  orderedreturns<-returns[orderingvector]
  orderedtradedays<-tradedays[orderingvector]
  drawdowndays<-rep(1,length(orderingvector))
  highwatermark=0
  equity=cumsum(orderedreturns)
  if(equity[1]>=0){
    drawdowndays[0]=0
  }
  for(i in 2:length(orderingvector)){
    if(equity[i]>highwatermark){
      highwatermark=equity[i]
      #print(paste0("i: highwatermark:",i,",",highwatermark))                        
      drawdowndays[i]=0
    }
    #}
    #for(i in 2:length(orderingvector)){
    if(drawdowndays[i]>0){
      #print(paste0("in loop:",i))
      drawdowndays[i]=drawdowndays[i-1]+orderedtradedays[i]
    }
  }
  drawdowndays
}

calcDrawdownVector<-function(orderingvector,returns,tradedays){
  #Returns a vector of inderminate size containing duration of all drawdowns in the sample
  orderedreturns<-returns[orderingvector]
  orderedtradedays<-tradedays[orderingvector]
  drawdowndays<-rep(1,length(orderingvector))
  df<- data.frame(ddstart=numeric(),
                  ddend=numeric(), 
                  dddays=numeric(), 
                  stringsAsFactors=FALSE) 
  highwatermark=0
  ddstart=0
  indrawdown=FALSE
  equity=cumsum(orderedreturns)
  if(equity[1]>=0){
    drawdowndays[0]=0
  }else{
    indrawdown=TRUE
  }
  for(i in 2:length(orderingvector)){
    if(equity[i]>=highwatermark){
      highwatermark=equity[i]
      if(indrawdown){
        ddend=cumsum(orderedtradedays)[i-1]
        newrow<-c(ddstart,ddend,ddend-ddstart+1)
        #          print(paste0("insertrow,dim",dim(df)[1]))
        df[dim(df)[1]+1,]<-newrow
        indrawdown=FALSE
      }
      drawdowndays[i]=0
      
    }else if (!indrawdown) {
      indrawdown=TRUE
      #print(paste0("indrawdown",i))
      ddstart=cumsum(orderedtradedays)[i] 
    }
  }
  df
}

calcDrawdownValues<-function(orderingvector,returns){
  #Returns the drawdown currency amount.
  orderedreturns<-returns[orderingvector]
  drawdownvalues<-rep(0,length(orderingvector))
  highwatermark=0
  lowwatermark=0
  drawdowndays<-rep(1,length(orderingvector))
  equity=cumsum(orderedreturns)
  #print(orderedreturns)
  #print(equity)
  if(equity[1]>=0){
    drawdowndays[0]=0
    highwatermark=equity[1]
    lowwatermark=0
  }else{
    drawdownvalues[1]=equity[1]
    lowwatermark=equity[1]
  }
  for(i in 2:length(orderingvector)){
    #print(paste("i : ",i))
    if(equity[i]>=highwatermark){
      highwatermark=equity[i]
      lowwatermark=0
      drawdowndays[i]=0
      drawdownvalues[i]=0
      #print(paste("i,highwatermark : ",i,highwatermark))
      #print(paste("drawdownvalues : ",drawdownvalues[i]))
      
    }else{
      drawdownvalues[i]=max(drawdownvalues[i-1],highwatermark-equity[i])
      drawdowndays[i]=drawdowndays[i-1]+1;
      #print(paste("i,orderedreturns : ",i,orderedreturns[i]))
      #print(paste("i,drawdownvalues : ",i,drawdownvalues[i]))
      
    }
  }
  drawdownvalues
}

bootstrap<-function(returnvector,tradebarvector,samples,samplesize,drawdownDaysThreshold){
  set.seed(567)
  return.validate=returnvector
  bar.validate=tradebarvector
  samplelist <- list()
  for(i in 1:samples){
    samplelist[[i]]=sample.int(length(return.validate),samplesize,replace=TRUE)
  }
  return.validate.simind <- data.frame(samplelist)
  colnames(return.validate.simind)<-seq(1,samples,1)
  rownames(return.validate.simind)<-seq(1,samplesize,1)
  return.validate.sim<-apply(return.validate.simind,2,function(x) return.validate[x])
  return.validate.sim.mean<-vector("numeric",length = samples)
  for(i in 1:samples){
    return.validate.sim.mean<-apply(return.validate.sim,2,mean)
  }
  
  #Plot(Row1, Col1)
  return.validate.sim.periodreturn=apply(return.validate.sim,2,sum)
  #hist(return.validate.sim.mean*samplesize,breaks="FD",prob=TRUE,ylab="relative frequency",xlab="avg return",main="Expected Absolute % Return")
  hist(return.validate.sim.periodreturn,breaks="FD",prob=TRUE,ylab="relative frequency",xlab="avg return",main="Expected Absolute % Return")
  lines(density(return.validate.sim.periodreturn),lwd=2,col="blue")
  mtext(side=3,text=(paste("Expected Return: ",round(mean(return.validate.sim.periodreturn),2), "sd: ",round(sd(return.validate.sim.periodreturn),2))),cex=0.75)
  
  #calculate drawdowns
  #bootstrap 450 trades and simulate drawdown
  
  ddlengthlist<-list()
  ddrunlist<-list()
  samplelist <- list()
  ddvaluelist<-list()
  ddtradeslist<-list()
  set.seed(567)
  for(i in 1:samples){
    samplelist[[i]]=sample.int(length(return.validate),samplesize,replace=TRUE)
  }
  return.validate.simind <- data.frame(samplelist)
  colnames(return.validate.simind)<-seq(1,samples,1)
  rownames(return.validate.simind)<-seq(1,samplesize,1)
  
  for(i in 1:samples){
    ddlength=calcDrawdownVector(return.validate.simind[,i],return.validate,bar.validate)
    ddlengthlist[[i]]=ddlength$dddays
    ddrun=calcDrawdownDayCount(return.validate.simind[,i],return.validate,bar.validate)
    ddrunlist[[i]]=ddrun
    ddvalue=calcDrawdownValues(return.validate.simind[,i],return.validate)
    ddvaluelist[[i]]=ddvalue
    ddtrades<-calcDrawdownVector(return.validate.simind[,i],return.validate,rep(1,samplesize))
    ddtradeslist[[i]]<-ddtrades
  }
  
  ddlengthraw<-unlist(ddlengthlist)
  ddtradesraw<-unlist(ddtradeslist)
  
  ddrun.validate.sim=data.frame(ddrunlist)
  colnames(ddrun.validate.sim)<-seq(1,samples,1)
  rownames(ddrun.validate.sim)<-seq(1,samplesize,1)
  
  ddvalue.validate.sim=data.frame(ddvaluelist)
  colnames(ddvalue.validate.sim)<-seq(1,samples,1)
  rownames(ddvalue.validate.sim)<-seq(1,samplesize,1)
  
  #plot 2(Row1, Col 2)
  ddrun.validate.sim.mean=sapply(ddrun.validate.sim,mean)
  hist(ddrun.validate.sim.mean,breaks="FD",prob=TRUE,ylab="relative frequency",xlab="drawdown days",main="Drawdown Days")
  lines(density(ddrun.validate.sim.mean),lwd=2,col="blue")
  mtext(side=3,text=(paste("99 Percentile: ",round(quantile(ddlengthraw,0.99),0))),cex=0.75)
  
  #plot 3(Row1, Col 3)
  ddrun.validate.sim.max=sapply(ddrun.validate.sim,max)
  hist(ddrun.validate.sim.max,breaks="FD",prob=TRUE,ylab="relative frequency",xlab="max drawdown days",main="Max Drawdown Days")
  lines(density(ddrun.validate.sim.max),lwd=2,col="blue")
  mtext(side=3,text=(paste("Max Drawdown Days: ",max(ddrun.validate.sim.max)," 95 Percentile: ",quantile(ddrun.validate.sim.max,0.95))),cex=0.75)
  
  #plot 4(Row1, Col 4)
  ddvalue.validate.sim.max=sapply(ddvalue.validate.sim,max)
  hist(ddvalue.validate.sim.max,breaks="FD",prob=TRUE,ylab="relative frequency",xlab="max drawdown %",main="Drawdown Percent")
  lines(density(ddvalue.validate.sim.max),lwd=2,col="blue")
  mtext(side=3,text=(paste("Max Drawdown %: ",round(max(ddvalue.validate.sim.max),2)," 95 Percentile: ",round(quantile(ddvalue.validate.sim.max,0.95),2))),cex=0.75)
  
  
  # derived trades mapping
  returnlist <- list()
  ddrunlist<-list()
  ddvaluelist<-list()
  ddlengthlist<-list()
  ddtradeslist<-list()
  notrades<-numeric()
  for(i in 1:samples){
    return.validate.sim<-return.validate[return.validate.simind[,i]]
    bar.validate.sim<-bar.validate[return.validate.simind[,i]]
    position.validate.sim<-round(7-pmin(drawdownDaysThreshold,ddrun.validate.sim[,i])*7/drawdownDaysThreshold)
    position.validate.sim<-shift(position.validate.sim,1)
    position.validate.sim[1]<-7
    notrades<-sum(position.validate.sim == 0)
    deriv.return.validate.sim=return.validate.sim*position.validate.sim/7
    #deriv.ddrun.validate.sim<-calcDrawdownDayCount(return.validate.simind[,i],deriv.return.validate.sim,bar.validate)
    deriv.ddrun.validate.sim<-calcDrawdownDayCount(seq(1,samplesize,1),deriv.return.validate.sim,bar.validate.sim)
    returnlist[[i]]=deriv.return.validate.sim
    ddrunlist[[i]]=deriv.ddrun.validate.sim
    #    ddvalue=calcDrawdownValues(return.validate.simind[,i],deriv.return.validate.sim)
    ddvalue=calcDrawdownValues(seq(1,samplesize,1),deriv.return.validate.sim)
    ddvaluelist[[i]]=ddvalue
    ddlength=calcDrawdownVector(seq(1,samplesize,1),deriv.return.validate.sim,bar.validate.sim)
    ddlengthlist[[i]]=ddlength$dddays
    ddtrades<-calcDrawdownVector(seq(1,samplesize,1),deriv.return.validate.sim,rep(1,samplesize))
    ddtradeslist[[i]]<-ddtrades
  }
  
  ddlengthadj=unlist(ddlengthlist)
  ddtradesadj<-unlist(ddtradeslist)
  
  deriv.return.validate.sim=data.frame(returnlist)
  colnames(deriv.return.validate.sim)<-seq(1,samples,1)
  rownames(deriv.return.validate.sim)<-seq(1,samplesize,1)
  
  deriv.ddrun.validate.sim=data.frame(ddrunlist)
  colnames(deriv.ddrun.validate.sim)<-seq(1,samples,1)
  rownames(deriv.ddrun.validate.sim)<-seq(1,samplesize,1)
  
  deriv.ddvalue.validate.sim=data.frame(ddvaluelist)
  colnames(deriv.ddvalue.validate.sim)<-seq(1,samples,1)
  rownames(deriv.ddvalue.validate.sim)<-seq(1,samplesize,1)
  
  #plot 5(Row2, Col 1)
  deriv.return.validate.sim.periodreturn=apply(deriv.return.validate.sim,2,sum)
  hist(deriv.return.validate.sim.periodreturn,breaks="FD",prob=TRUE,ylab="relative frequency",xlab="avg return",main="Expected Absolute % Return")
  lines(density(deriv.return.validate.sim.periodreturn),lwd=2,col="blue")
  mtext(side=3,text=(paste("Expected Return: ",round(mean(deriv.return.validate.sim.periodreturn),2), "sd: ",round(sd(deriv.return.validate.sim.periodreturn),2))),cex=0.75)
  
  #plot 6(Row2, Col 2)
  deriv.ddrun.validate.sim.mean=sapply(deriv.ddrun.validate.sim,mean)
  hist(deriv.ddrun.validate.sim.mean,breaks="FD",prob=TRUE,ylab="relative frequency",xlab="drawdown days",main="Average Drawdown days")
  lines(density(deriv.ddrun.validate.sim.mean),lwd=2,col="blue")
  mtext(side=3,text=(paste("99 Percentile: ",round(quantile(ddlengthadj,0.99),0))),cex=0.75)
  
  #plot 7(Row2, Col 3)
  deriv.ddrun.validate.sim.max=sapply(deriv.ddrun.validate.sim,max)
  hist(deriv.ddrun.validate.sim.max,breaks="FD",prob=TRUE,ylab="relative frequency",xlab="max drawdown days",main="Max Drawdown days")
  lines(density(deriv.ddrun.validate.sim.max),lwd=2,col="blue")
  mtext(side=3,text=(paste("Max Drawdown Days: ",max(deriv.ddrun.validate.sim.max)," 95 Percentile: ",quantile(deriv.ddrun.validate.sim.max,0.95))),cex=0.75)
  
  #plot 8(Row2, Col 4)
  deriv.ddvalue.validate.sim.max=sapply(deriv.ddvalue.validate.sim,max)
  hist(deriv.ddvalue.validate.sim.max,breaks="FD",prob=TRUE,ylab="relative frequency",xlab="max drawdown %",main="Drawdown Percent")
  lines(density(deriv.ddvalue.validate.sim.max),lwd=2,col="blue")
  mtext(side=3,text=(paste("Max Drawdown %: ",round(max(deriv.ddvalue.validate.sim.max),2)," 95 Percentile: ",round(quantile(deriv.ddvalue.validate.sim.max,0.95),2))),cex=0.75)
 
  #create dataframe of metrics
  # row 1 is with 100% allocation
  # row 2 is with allocated scaled by dd days
  metrics=data.frame(sleepperc=numeric(2),maxddavg=numeric(2),maxddmax=numeric(2),maxddmin=numeric(2),maxdd95per=numeric(2),
                     maxdd99perc=numeric(2),retavg=numeric(2),retmax=numeric(2),retmin=numeric(2),retworst95perc=numeric(2),
                     retworst99perc=numeric(2),dddaysavg=numeric(2),dddaysmax=numeric(2),dddays95perc=numeric(2),
                     dddays99perc=numeric(2),ddtradesavg=numeric(2)
                     )
  metrics$sleepperc[1]=0
  metrics$sleepperc[2]=mean(notrades)/samplesize
  metrics$maxddavg[1]=mean(ddrun.validate.sim.max)
  metrics$maxddavg[2]=mean(deriv.ddrun.validate.sim.max)
  metrics$maxddmax[1]=max(ddrun.validate.sim.max)
  metrics$maxddmax[2]=max(deriv.ddrun.validate.sim.max)
  metrics$maxddmin[1]=min(ddrun.validate.sim.max)
  metrics$maxddmin[2]=min(deriv.ddrun.validate.sim.max)
  metrics$maxdd95perc[1]=round(quantile(ddrun.validate.sim.max,0.95),0)
  metrics$maxdd95perc[2]=round(quantile(deriv.ddrun.validate.sim.max,0.95),0)
  metrics$maxdd99perc[1]=round(quantile(ddrun.validate.sim.max,0.99),0)
  metrics$maxdd99perc[2]=round(quantile(deriv.ddrun.validate.sim.max,0.99),0)
  metrics$retavg[1]=round(mean(return.validate.sim.periodreturn),2)
  metrics$retavg[2]=round(mean(deriv.return.validate.sim.periodreturn),2)
  metrics$retmax[1]=round(max(return.validate.sim.periodreturn),2)
  metrics$retmax[2]=round(max(deriv.return.validate.sim.periodreturn),2)
  metrics$retmin[1]=round(min(return.validate.sim.periodreturn),2)
  metrics$retmin[2]=round(min(deriv.return.validate.sim.periodreturn),2)
  metrics$retworst95perc[1]=round(quantile(return.validate.sim.periodreturn,0.05),1)
  metrics$retworst95perc[2]=round(quantile(deriv.return.validate.sim.periodreturn,0.05),1)
  metrics$retworst99perc[1]=round(quantile(return.validate.sim.periodreturn,0.01),1)
  metrics$retworst99perc[2]=round(quantile(deriv.return.validate.sim.periodreturn,0.01),1)
  metrics$dddaysavg[1]=round(mean(ddrun.validate.sim.mean),0)
  metrics$dddaysavg[2]=round(mean(deriv.ddrun.validate.sim.mean),0)
  metrics$dddaysmax[1]=round(max(ddrun.validate.sim.mean),0)
  metrics$dddaysmax[2]=round(max(deriv.ddrun.validate.sim.mean),0)
  metrics$dddays95perc[1]=round(quantile(ddlengthraw,0.95),0)
  metrics$dddays95perc[2]=round(quantile(ddlengthadj,0.95),0)
  metrics$dddays99perc[1]=round(quantile(ddlengthraw,0.99),0)
  metrics$dddays99perc[2]=round(quantile(ddlengthadj,0.99),0)
  metrics$ddtradesavg[1]=round(mean(ddtradesraw),0)
  metrics$ddtradesavg[2]=round(mean(ddtradesadj)-mean(notrades),0)
  metrics
  
}