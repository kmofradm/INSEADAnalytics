
# rm(list=ls()) # Clean up the memory, if we want to rerun from scratch
# source("helpersSet1.R")

getdata.fromscratch = 1

website_used = "yahoo" # can be "yahoo" or other ( see help(getSymbols) ). Depending on the website we may need to change the stock tickers' representation
mytickers = c("SPY", "AAPL")  # Other tickers for example are "GOOG", "GS", "TSLA", "FB", "MSFT", 
startDate = "2001-01-01"

if (getdata.fromscratch){
  # Get SPY first, to get all trading days
  tmp<-as.matrix(try(getSymbols(Symbols="SPY",from = startDate,src = website_used, auto.assign=FALSE)))
  StockPrices=matrix(rep(0,nrow(tmp)*length(mytickers)), ncol=length(mytickers))
  colnames(StockPrices)<-mytickers; 
  rownames(StockPrices)<-rownames(tmp)
  StockVolume=StockPrices
  StockPrices[,1] <- tmp[,6]
  
  for (ticker_index in 1:length(mytickers)){
    ticker_to_get = mytickers[ticker_index]
    print(paste("\nDownloading ticker ", ticker_to_get, " ..."))
    tmpdata<-as.matrix(try(getSymbols(Symbols=ticker_to_get,from = startDate,auto.assign=FALSE)))
    if (!inherits(tmpdata, "try-error"))
    {
      therownames=intersect(rownames(tmpdata),rownames(StockPrices))
      tmpdata[is.na(tmpdata)] <- 0
      StockPrices[therownames,ticker_index]<-tmpdata[therownames,6] # adjusted close price
      StockVolume[therownames,ticker_index]<-tmpdata[therownames,5] # shares volume for now - need to convert to dollars later
    } else {
      cat(ticker_to_get," NOT found")
    }
  }
  # Get the daily returns now. Use the simple percentage difference approach 
  StockReturns= ifelse(head(StockPrices,-1)!=0, (tail(StockPrices,-1)-head(StockPrices,-1))/head(StockPrices,-1),0) # note that this removes the first day as we have no way to get the returns then!
  rownames(StockReturns)<-tail(rownames(StockPrices),-1) # adjust the dates by 1 day now
  
  # Now remove the first day from the other data, too
  StockPrices = StockPrices[rownames(StockReturns),]
  StockVolume = StockPrices[rownames(StockReturns),]
  colnames(StockPrices)<-mytickers
  colnames(StockVolume)<-mytickers
  colnames(StockReturns)<-mytickers
  
  save(StockReturns,StockPrices,StockVolume, file = "DataSet1.Rdata")
} else {
  load("DataSet1.Rdata")
}

website_used = "yahoo" # can be "yahoo" or other ( see help(getSymbols) ). Depending on the website we may need to change the stock tickers' representation
mytickers2 = c("SPY", "AAPL","YHOO","GOOG","TSLA")  # Other tickers for example are "GOOG", "GS", "TSLA", "FB", "MSFT", 
startDate2 = "2012-07-10"
endDate = "2013-07-10"

if (getdata.fromscratch){
  # Get SPY first, to get all trading days
  tmp2<-as.matrix(try(getSymbols(Symbols="SPY",from = startDate2, to = endDate, src = website_used, auto.assign=FALSE)))
  StockPrices2=matrix(rep(0,nrow(tmp2)*length(mytickers2)), ncol=length(mytickers2))
  colnames(StockPrices2)<-mytickers2; 
  rownames(StockPrices2)<-rownames(tmp2)
  StockVolume2=StockPrices2
  StockPrices2[,1] <- tmp2[,6]
  
  for (ticker_index in 1:length(mytickers2)){
    ticker_to_get = mytickers2[ticker_index]
    print(paste("\nDownloading ticker ", ticker_to_get, " ..."))
    tmpdata2<-as.matrix(try(getSymbols(Symbols=ticker_to_get,from = startDate2,auto.assign=FALSE)))
    if (!inherits(tmpdata2, "try-error"))
    {
      therownames=intersect(rownames(tmpdata2),rownames(StockPrices2))
      tmpdata2[is.na(tmpdata2)] <- 0
      StockPrices2[therownames,ticker_index]<-tmpdata2[therownames,6] # adjusted close price
      StockVolume2[therownames,ticker_index]<-tmpdata2[therownames,5] # shares volume for now - need to convert to dollars later
    } else {
      cat(ticker_to_get," NOT found")
    }
  }
  # Get the daily returns now. Use the simple percentage difference approach 
  StockReturns2= ifelse(head(StockPrices2,-1)!=0, (tail(StockPrices2,-1)-head(StockPrices2,-1))/head(StockPrices2,-1),0) # note that this removes the first day as we have no way to get the returns then!
  rownames(StockReturns2)<-tail(rownames(StockPrices2),-1) # adjust the dates by 1 day now
  
  # Now remove the first day from the other data, too
  StockPrices2 = StockPrices2[rownames(StockReturns2),]
  StockVolume2 = StockPrices2[rownames(StockReturns2),]
  colnames(StockPrices2)<-mytickers2
  colnames(StockVolume2)<-mytickers2
  colnames(StockReturns2)<-mytickers2
  
  save(StockReturns2,StockPrices2,StockVolume2, file = "DataSet1.Rdata")
} else {
  load("DataSet1.Rdata")
}


