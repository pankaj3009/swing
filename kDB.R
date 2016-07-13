library(httr)
library(jsonlite)

ref <- function(x, shift) {
  # x has to be a vector. Not a matrix or dataframe!!
  if (shift < 0) {
    addition <- rep(NA, abs(shift))
    r1 <- seq(length(x) - abs(shift) + 1, length(x), 1)
    out <- x[-r1]
    out <- append(out, addition, 0)
    
  } else if (shift > 0) {
    addition <- rep(NA, abs(shift))
    r1 <- seq(1, abs(shift), 1)
    out <- x[-r1]
    out <- append(out, addition, length(x))
  } else{
    x
  }
}

kQueryBody <- function(start, end, metrics, add.to = NULL) {
  if (is.null(add.to)) {
    result <- list(start, end, list(metrics))
    if (is(start, "list") && is(end, "list")) {
      names(result) <- c("start_relative", "end_relative", "metrics")
    } else if (is(start, "list")) {
      names(result) <- c("start_relative", "end_absolute", "metrics")
    } else if (is(end, "list")) {
      names(result) <- c("start_absolute", "end_relative", "metrics")
    } else{
      names(result) <- c("start_absolute", "end_absolute", "metrics")
    }
  }
  else{
    result <- append(add.to$metrics, metrics)
  }
  
  result
  
}

kDate <- function(x, y = NULL) {
  if (is.null(y)) {
    result <- unbox(x)
  } else{
    x <- list(unbox(x), unbox(y))
    names(x) <- c("value", "unit")
    result <- x
  }
  result
}

kMetrics <- function(tags, name, aggregators) {
  if (!is.null(aggregators)) {
    result <- list(tags, unbox(name), list(aggregators))
    names(result) <- c("tags", "name", "aggregators")
    #print(result)
    
  } else{
    result <- list(tags, unbox(name))
    names(result) <- c("tags", "name")
    #print("wrong loop")
    #print(result)
  }
  result
}

kAggregators <- function(x, y, z) {
  sampling <- list(unbox(y), unbox(z))
  names(sampling) <- c("value", "unit")
  result <- list(unbox(x), unbox(c("true")),sampling)
  names(result) <- c("name", "align_start_time","sampling")
  return(result)
}

kTags <- function(..., a) {
  input <- list(...)[-length(list(...))]
  # print("input")
  # print(input)
  # print("length input")
  # print(length(input))
  tags <- list()
  names <- vector()
  for (i in 1:length(input)) {
    #print("loop #")
    #print(i)
    namevalue <- input[[i]]
    #print("namevalue")
    #print(namevalue)
    name <- strsplit(as.character(input[[i]]), "=")[[1]][[1]]
    #print("name")
    #print(name)
    value <- strsplit(as.character(input[[i]]), "=")[[1]][[2]]
    #value<-strsplit(as.character(value),split=",")
    #print("value")
    #print(value)
    tags <- c(tags, list(value))
    names <- c(names, name)
  }
  names(tags) <- names
  
  #    print("tags")
  #    print(tags)
  tags
}

kGetOHLCV <-
  function(start,
           end,
           timezone,
           name,
           ts = c("open", "high", "low", "close", "settle")
           ,
           aggregators = c("first", "max", "min", "last", "sum"),
           aValue = NULL,
           aUnit = NULL,
           filepath="",
           ...) {
    symbolchange <-
      read.csv("swing/symbolchange.csv",
               header = TRUE,
               stringsAsFactors = FALSE)
    input = strsplit(list(...)[[1]], "=")[[1]][2]
    md <- data.frame()
    symbollist <-
      linkedsymbols(symbolchange$SM_KEY_SYMBOL,
                    symbolchange$SM_NEW_SYMBOL,
                    toupper(input))
    for (j in length(symbollist):1) {
      a <- list(...)
      a[[1]] = paste("symbol", tolower(symbollist[j]), sep = "=")
      newargs = paste(a, sep = ",")
      startUnix <-
        as.numeric(as.POSIXct(paste(start, timezone))) * 1000
      endUnix <- as.numeric(as.POSIXct(paste(end, timezone))) * 1000
      startLong <- kDate(startUnix)
      endLong <- kDate(endUnix)
      tags <- kTags(newargs, NULL)
      out <- matrix()
      aggr = list()
      for (i in 1:length(ts)) {
        tempname = paste(name, ts[i], sep = ".")
        aggr <- kAggregators(aggregators[i], aValue, aUnit)
        
        if (!is.null(aggr)) {
          metrics <- kMetrics(tags, tempname, aggr)
        } else{
          metrics <- kMetrics(tags, tempname, NULL)
        }
        query <- kQueryBody(startLong, endLong, metrics)
        myjson <- toJSON(query, pretty = TRUE)
        print(paste("retrieving",ts[i],"for",symbollist[j], sep = " "))
        r <-
          POST(
            "http://91.121.165.108:8085/api/v1/datapoints/query",
            body = myjson,
            encode = "json"
          )
        if (content(r)$queries[[1]]$sample_size > 0) {
          m <-
            matrix(
              unlist(content(r)$queries[[1]]$results[[1]]$values),
              ncol = 2,
              byrow = TRUE
            )
          colnames(m) <- c("date", ts[i])
          out <- merge(out, m, by = 1, all = TRUE)
          
        }
      }
      if (dim(out)[2] == length(ts) + 1) {
        # generate quotes only if the #columns = # elements in ts
        colnames(out) <- c("date", ts)
        out <- out[rowSums(is.na(out)) != 6, ]
        d <- data.frame(out)
        md <- rbind(md, d)
      }
      
    }
    if (nrow(md) > 0) {
      md[, 1] <-
        as.POSIXct(md[, 1] / 1000, origin = "1970-01-01", tz = "Asia/Kolkata")
      md$symbol<-symbollist[1]
    }
    #handle splits
    md<-processSplits(md,"swing/splits.csv",symbollist)
    if(!missing(filepath)){
      save(md,file=paste(filepath,symbollist[1],".Rdata",sep=""))
    }
    md
  }

processSplits<-function(md,splitinfofile,symbollist){
  if (nrow(md) > 0) {
    splits <- read.csv(splitinfofile,
                       header = TRUE,
                       stringsAsFactors = FALSE)
    splits$date <- as.POSIXct(splits$date, format = "%Y-%m-%d")
    for (i in 1:length(symbollist)) {
      print(paste("Processing Split for symbol",symbollist[i],sep=" "))
      subset <- splits[splits$symbol == symbollist[i], ]
      if(nrow(subset)>0){
        for (j in 1:nrow(subset)) {
          if ("open" %in% colnames(md)) {
            md$open <-
              ifelse(
                md$date < subset$date[j],
                md$open * subset$oldshares[j] / subset$newshares[j],
                md$open
              )
          }
          if ("high" %in% colnames(md)) {
            md$high <-
              ifelse(
                md$date < subset$date[j],
                md$high * subset$oldshares[j] / subset$newshares[j],
                md$high
              )
          }
          if ("low" %in% colnames(md)) {
            md$low <-
              ifelse(
                md$date < subset$date[j],
                md$low * subset$oldshares[j] / subset$newshares[j],
                md$low
              )
          }
          if ("close" %in% colnames(md)) {
            md$close <-
              ifelse(
                md$date < subset$date[j],
                md$close * subset$oldshares[j] / subset$newshares[j],
                md$close
              )
          }
          if ("settle" %in% colnames(md)) {
            md$settle <-
              ifelse(
                md$date < subset$date[j],
                md$settle * subset$oldshares[j] / subset$newshares[j],
                md$settle
              )
          }
          if ("volume" %in% colnames(md)) {
            md$volume <-
              ifelse(
                md$date < subset$date[j],
                md$volume * subset$newshares[j] / subset$oldshares[j],
                md$volume
              )
          }
          
        }
      }

      
    }
    
  }
  md
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