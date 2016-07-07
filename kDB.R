library(httr)
library(jsonlite)

ref<-function(x,shift){
        # x has to be a vector. Not a matrix or dataframe!!
        if(shift<0){
                addition<-rep(NA,abs(shift))
                r1<-seq(length(x)-abs(shift)+1,length(x),1)
                out<-x[-r1]
                out<-append(out,addition,0)                
                
        }else if(shift>0){
                addition<-rep(NA,abs(shift))
                r1<-seq(1,abs(shift),1)
                out<-x[-r1]
                out<-append(out,addition,length(x))                
        }else{
         x
        }
}

kQueryBody<-function(start,end,metrics,add.to=NULL){
    if(is.null(add.to)){
    result<-list(start,end,list(metrics))
    if(is(start,"list") && is(end,"list")){
    names(result)<-c("start_relative","end_relative","metrics")
    }else if(is(start,"list")){
        names(result)<-c("start_relative","end_absolute","metrics")
    }else if(is(end,"list")){
        names(result)<-c("start_absolute","end_relative","metrics")        
    }else{
        names(result)<-c("start_absolute","end_absolute","metrics")
    }
}
else{
    result<-append(add.to$metrics,metrics)
}

result

}

kDate<-function(x,y=NULL){    
    if(is.null(y)){
    result<-unbox(x)   
    }else{
        x<-list(unbox(x),unbox(y))
        names(x)<-c("value","unit")
        result<-x
    }
    result
}

kMetrics<-function(tags,name,aggregators){
    if(!is.null(aggregators)){
        result<-list(tags,unbox(name),list(aggregators))
        names(result)<-c("tags","name","aggregators")
        
    }else{
        result<-list(tags,unbox(name))
        names(result)<-c("tags","name")
    }
    result
}

kAggregators<-function(x,y,z){
    sampling<-list(unbox(y),unbox(z))
    names(sampling)<-c("value","unit")
    result<-list(unbox(x),sampling)
    names(result)<-c("name","sampling")
    result
}

kTags<-function(...,a){
    input<-list(...)[-length(list(...))]
    #print("input")
    #print(input)
    #print("length input")
    #print(length(input))
    tags<-list()
    names<-vector()
    for(i in 1:length(input)){
        #print("loop #")
        #print(i)
        namevalue<-input[[i]]
        #print("namevalue")
        #print(namevalue)
        name<-strsplit(as.character(input[[i]]),"=")[[1]][[1]]
        #print("name")
        #print(name)
        value<-strsplit(as.character(input[[i]]),"=")[[1]][[2]]
        #value<-strsplit(as.character(value),split=",")
        #print("value")
        #print(value)
        tags<-c(tags,list(value))
        names<-c(names,name)
    }
    names(tags)<-names
    
    #print("tags")
    #print(tags)
    tags
}

kGetData<-function(start,end,timezone,name,aName=NULL,aValue=NULL,aUnit=NULL,...){
#library(httr)
#library(jsonlite)
#startUnix<-startUnix<-as.numeric(as.POSIXct("2014-11-03 12:14:00 Asia/Kolkata"))*1000
#endUnix<-startUnix<-as.numeric(as.POSIXct("2014-11-03 12:13:00 Asia/Kolkata"))*1000
  #print(start); print(end);print(timezone);print(name);print(aName);print(aValue);print(aUnit);print(as.list(...))
startUnix<-as.numeric(as.POSIXct(paste(start,timezone)))*1000
endUnix<-as.numeric(as.POSIXct(paste(end,timezone)))*1000
start<-kDate(startUnix)
end<-kDate(endUnix)
#start<-kDate(5,"minutes")
#end<-kDate(1,"minutes")

if(!is.null(aName)){
    aggr<-kAggregators(aName,aValue,aUnit)    
}
tags<-kTags(...,NULL)
if(!is.null(aName)){
    metrics<-kMetrics(tags,name,aggr)
    
}else{
    metrics<-kMetrics(tags,name,NULL)
    
}
query<-kQueryBody(start,end,metrics)
myjson <- toJSON(query, pretty=TRUE)
#cat(myjson)
r<-POST("http://91.121.165.108:8085/api/v1/datapoints/query",body=myjson,encode="json",verbose())
#print(content(r))
m<-matrix(unlist(content(r)$queries[[1]]$results[[1]]$values),ncol=2,byrow=TRUE)
d<-data.frame(m)
#d[,1]<-d[,1]/1000
d[,1]<-as.POSIXct(d[,1]/1000, origin="1970-01-01", tz="Asia/Kolkata")
d
}

kGetOHLCV<-function(start,end,timezone,name,aName=NULL,aValue=NULL,aUnit=NULL,...){
  startUnix<-as.numeric(as.POSIXct(paste(start,timezone)))*1000
  endUnix<-as.numeric(as.POSIXct(paste(end,timezone)))*1000
  start<-kDate(startUnix)
  end<-kDate(endUnix)
  if(!is.null(aName)){
    aggr<-kAggregators(aName,aValue,aUnit)    
  }
  tags<-kTags(...,NULL)
  out<-matrix()

  for(i in 1:5){
    
    if(i==1){
      tempname=paste(name,"open",sep=".")
    }else if(i==2){
      tempname=paste(name,"high",sep=".")
      } else if(i==3){
      tempname=paste(name,"low",sep=".")
      }else if(i==4){
      tempname=paste(name,"settle",sep=".")
      }else if(i==5){
      tempname=paste(name,"volume",sep=".")
    }
    
  if(!is.null(aName)){
    metrics<-kMetrics(tags,tempname,aggr)
    
  }else{
    metrics<-kMetrics(tags,tempname,NULL)
    
  }
  query<-kQueryBody(start,end,metrics)
  myjson <- toJSON(query, pretty=TRUE)
#  print(myjson)
#  cat(i)
  r<-POST("http://91.121.165.108:8085/api/v1/datapoints/query",body=myjson,encode="json")
  #print(content(r))
  m<-matrix(unlist(content(r)$queries[[1]]$results[[1]]$values),ncol=2,byrow=TRUE)
  if(i==1){
      colnames(m)<-c("date","open")
  }else if(i==2){
      colnames(m)<-c("date","high")
  } else if(i==3){
      colnames(m)<-c("date","low")
  }else if(i==4){
      colnames(m)<-c("date","close")
  }else if(i==5){
      colnames(m)<-c("date","volume")
  }
  out <- merge(out, m, by = 1, all = TRUE)
  }
  #colnames(out)<-c("date","open","high","low","close","volume")
  colnames(out)<-c("date","open","high","low","close","volume")
  out<-out[rowSums(is.na(out))!=6, ]
  d<-data.frame(out)
  #d[,1]<-d[,1]/1000
  d[,1]<-as.POSIXct(d[,1]/1000, origin="1970-01-01", tz="Asia/Kolkata")
  
  d
}
#a <- merge(Matrix1, Matrix2, by = c("Col1", "Col3"), all = TRUE)
#r<-kGetOHLCV(start="2014-10-30 00:00:00",end="2014-11-05 00:00:00",timezone="Asia/Kolkata",name="india.nse.future.s1.1min",aName=NULL,aValue=NULL,aUnit=NULL,"symbol=nsenifty","expiry=20141127")
#result<-content(r)$queries[[1]]$results[[1]]$values
#r<-kGetOHLCV(start="2014-10-21 09:15:00",end="2014-10-21 15:30:00",timezone="Asia/Kolkata",name="india.nse.equity.s1.1sec",aName=NULL,aValue=NULL,aUnit=NULL,"symbol=vakrangee")
#r<-kGetOHLCV(start="2014-11-26 00:13:00",end="2014-11-26 15:30:00",timezone="Asia/Kolkata",name="india.nse.future.s1.1min",aName=NULL,aValue=NULL,aUnit=NULL,"symbol=nsenifty","expiry=20141127")
#r<-kGetOHLCV(start="2015-01-08 09:15:00",end="2015-01-08 15:30:00",timezone="Asia/Kolkata",name="india.nse.future.s1.1sec",aName=NULL,aValue=NULL,aUnit=NULL,"symbol=nsenifty","expiry=20150129")
#r<-kGetOHLCV(start="1990-05-21 09:15:00",end="2014-10-21 15:30:00",timezone="Asia/Kolkata",name="india.nse.index.s4.daily",aName=NULL,aValue=NULL,aUnit=NULL,"symbol=nsenifty")
#sum(abs(diff(r$close)))
#which(is.na(r$close))