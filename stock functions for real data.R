## 
## library(quantmod)
## library(ggplot2)
## library(googleVis)
##



source("stock functions.r")


#########################################################################################################
#########################################################################################################
###                                                                                                   ###
###                                                                                                   ###

if (FALSE){  ## Do not run!!


CODE <- c(paste("000",1:9,sep=""),paste("00",10:99,sep=""),paste("0",100:999,sep=""),as.character("1000":"9999"))
ttt0 <- system.time({    price.CODE <- getStockData( years=c(201406), codes=CODE )    })
ttt0 <- print(paste("Took",round(ttt0[3]/60,2),"min.",sep=""))
length( price.CODE$S )
stockcodes <- names(  price.CODE$S )
save( stockcodes,price.CODE,ttt0,  file="All_StockNames_with_PriceData_basedon_date_201406.RData")


load("All_StockNames_with_PriceData_basedon_date_201406.RData")



load("VolumeDataFiles_201406.RData")



ttt1 <- system.time({    VData <- getVolumeData_ALL( dates=c(201401:201406) )    })

ttt1 <- system.time({    VData <- getVolumeData_ALL( DATA=VData, dates=c(201406) )    })
ttt1 <- print(paste("Took",round(ttt1[3]/60,2),"min.",sep=""))
save( VData,ttt1, file="VolumeDataFiles_201406.RData")


ttt2 <- system.time({    VM.stock <- Vol.fcn(Rnames=NULL,data=VData)    })
ttt2 <- print(paste("Took",round(ttt2[3]/60,2),"min.",sep=""))
save( VData,ttt1, VM.stock,ttt2, file="VolumeDataFiles_201406.RData")


snames    <- c();  for(k in 1:length(VData$V)){  snames <- union(snames,VData$V[[k]][,2])  };  snames <- sort(snames);  length(snames)
ttt3 <- system.time({    price <- getStockData( years=c(2014), codes=snames, keep.dataFile=F )    })
ttt3 <- print(paste("Took",round(ttt3[3]/60,2),"min.",sep=""))
save( VData,ttt1, VM.stock,ttt2, price,ttt3,  file="VolumeDataFiles_201406.RData")


ttt4 <- system.time({    priceData <- getStockData( PRICE=price.CODE, years=c(2014), codes=stockcodes, keep.dataFile=F )    })
ttt4 <- print(paste("Took",round(ttt4[3]/60,2),"min.",sep=""))
save( VData,ttt1, VM.stock,ttt2, price,ttt3, priceData,ttt4,stockcodes,  file="VolumeDataFiles_201406.RData")


ttt5 <- system.time({    VM.price <- VMP.fcn( VMS=VM.stock, PDATA=price, stock=NULL, dates=NULL )    })
ttt5 <- print(paste("Took",round(ttt5[3]/60,2),"min.",sep=""))
save( VData,ttt1, VM.stock,ttt2, price,ttt3, priceData,ttt4,stockcodes, VM.price,ttt5,  file="VolumeDataFiles_201406.RData")

setdiff(stockcodes,names(priceData$S))


ttt6 <- system.time({    VM.priceData <- VMP.fcn( VMS=VM.stock, PDATA=priceData, stock=NULL, dates=NULL )    })
ttt6 <- print(paste("Took",round(ttt6[3]/60,2),"min.",sep=""))
save( VData,ttt1, VM.stock,ttt2, price,ttt3, priceData,ttt4,stockcodes, VM.price,ttt5, VM.priceData,ttt6,  file="VolumeDataFiles_201406.RData")


length(price$S)
length(priceData$S)
ttt1;  ttt2;  ttt3;  ttt4;  ttt5;  ttt6;


setdiff(names(VM.stock),names(price$S))
getName(setdiff(names(VM.stock),names(price$S)))

} ## Do not run!!






cat('\n','============================================================','\n',
    '\n','  Do remember to:  load("VolumeDataFiles_201406.RData").    ','\n',
    '\n','============================================================','\n')

###                                                                                                   ###
###                                                                                                   ###
#########################################################################################################
#########################################################################################################







#########################################################################################################
#########################################################################################################
###                                                                                                   ###
###                                                                                                   ###



###                                                                                                   ###
###                                                                                                   ###
#########################################################################################################
#########################################################################################################







#########################################################################################################
#########################################################################################################
###                                                                                                   ###
###                                                                                                   ###


###                                                                                                   ###
###                                                                                                   ###
#########################################################################################################
#########################################################################################################










#########################################################################################################
#########################################################################################################
###                                                                                                   ###
###  VM.price is the data set that combines Volume data and price data categorized by stock names.    ###
###                                                                                                   ###

VMP.fcn <- function( VMS=VM.stock, PDATA=NULL, stock=c("2206"), dates=c(2014) ) {

  Vnames <- names(VMS)
  if (is.null(stock)) {  if(is.null(PDATA)){  stock <- Vnames  }else{  stock <- names(PDATA$S)  }  }
  if (is.null(PDATA)) {  PDATA <- getStockData( years=dates, codes=stock )  }
  idx <- which(!is.element(stock,names(PDATA$S)))
  if (length(idx )>0) {  PDATA <- getStockData( PRICE=PDATA, years=price$TradingDates, codes=stock[idx] )  }
  Pnames <- names(PDATA$S )

  VM.price <- list()
  PID <- which(!is.na(pmatch(stock,Vnames)))
  if (length(PID)>0) {
    for(k in PID){
print(stock[k])
      sdata <- VMS[[which(is.element(Vnames,stock[k]))]]
      sdates <- sdata[,1]
      pdata <- PDATA$S[[which(is.element(Pnames,stock[k]))]][[2]]
      PDATES <- pdata[,1]
##    for(jj in 1:length(PDATES)){  tmp <- strsplit(PDATES[jj]," ")[[1]];  PDATES[jj] <- tmp[length(tmp)]  }
      tmp <- matrix(unlist(strsplit(PDATES,"/")),ncol=3,byrow=T)
      pdates <- paste((as.double(tmp[,1])+1911),tmp[,2],tmp[,3],sep="")
      VMP <- cbind(sdata[1:2,],pdata[1:2,-1])[0,]
      for(i in 1:length(pdates)){
          idx <- which(is.element(sdates,pdates[i]))
          if (length(idx)>0) {  tmp <- cbind(sdata[idx,],pdata[i,-1]); rownames(tmp) <- NULL;  VMP[i,] <- tmp
          }else{                VMP[i,11:18] <- pdata[i,-1]                }
      }
      VMP$date <- PDATES
      VM.price[[ stock[k] ]] <- VMP
    }
  }
  stockN <- stock[which(is.na(pmatch(stock,Vnames)))]
  PIDN <- which(!is.na(pmatch(stockN,Pnames)))
  if (length(PIDN)>0) {
    for(k in PIDN){
print(stockN[k])
      pdata <- PDATA$S[[which(is.element(Pnames,stockN[k]))]][[2]]
      PDATES <- pdata[,1]
##    for(jj in 1:length(PDATES)){  tmp <- strsplit(PDATES[jj]," ")[[1]];  PDATES[jj] <- tmp[length(tmp)]  }
      VMP <- data.frame("date"=PDATES,NA,NA,NA,NA,NA,NA,NA,NA,NA,pdata[,-1])
      names(VMP) <- c("date","證券代號","證券名稱","外資買進股數","外資賣出股數","投信買進股數","投信賣出股數","自營商買進股數",
                      "自營商賣出股數","三大法人買賣超股數","成交股數","成交金額","開盤價","最高價","最低價","收盤價","漲跌價差","成交筆數" )
      VM.price[[ stockN[k] ]] <- VMP
    }
  }
  VM.price
}

###                                                                                                   ###
###                                                                                                   ###
#########################################################################################################
#########################################################################################################




#########################################################################################################
#########################################################################################################
###                                                                                                   ###
###                                                                                                   ###

getName <- function(stock, data=VData){

  data <- data$V
  snames <- c()
  for(k in 1:length(data)){  snames <- union(snames,data[[k]][,2])  }
  snames <- sort(snames)
  length(snames) 
  stocknames <- cbind(snames,"")
  for(k in 1:nrow(stocknames)){
    for(j in 1:length(data)){
      idx <- which(is.element(data[[j]][,2],stocknames[k,1]))
      if (length(idx)>0) {  stocknames[k,2] <- data[[j]][idx,3]; break  }
    }
  }
  stock <- cbind(stock,name="")
  for(k in 1:nrow(stock)){
    idx <- which(is.element(stocknames[,1],stock[k]))
    if (length(idx)>0){  stock[k,2] <- stocknames[idx,2]  }
  }
  data.frame(stock)
}
##
##

getName <- function(stock, data=priceData){
  snames <- sort(names(data$S))
  length(snames) 
  stocknames <- cbind(snames,NAMES="")
  for(k in 1:nrow(stocknames)){
     tmp <- strsplit(data$S[[k]]$des," ")[[1]][2]
     tmp <- strsplit(tmp,":")[[1]]
     stocknames[k,2] <- tmp[length(tmp)]
  }
  stock <- cbind(stock,name="")
  for(k in 1:nrow(stock)){
    idx <- which(is.element(stocknames[,1],stock[k]))
    if (length(idx)>0){  stock[k,2] <- stocknames[idx,2]  }
  }
  data.frame(stock)
}

##
##  getName(c("2206","1103","2498"))
##
###                                                                                                   ###
###                                                                                                   ###
#########################################################################################################
#########################################################################################################











#########################################################################################################
#########################################################################################################
###                                                                                                   ###
###  construct Volume data categorized by each stock                                                  ###
###                                                                                                   ###
Vol.fcn <- function(Rnames=NULL,data=VData){
print("construct Volume data categorized by each stock")
    data <- data$V
    if (is.null(Rnames)) {
      Rnames <- c()
      for(k in 1:length(data)){  Rnames <- union(Rnames,data[[k]][,2])  }
      Rnames <- sort(Rnames);  
    }
    Result <- list()
    for(j in  1:length(Rnames)){
      Rtmp <- data[[1]][1:3,]
      for(k in 1:length(data)){
        if( sum(is.element(data[[k]][,2],Rnames[j]))>0 ){
          Rtmp[k,] <- data[[k]][which(is.element(data[[k]][,2],Rnames[j])),]
        }else{
          Rtmp[k,] <- NA
        }
      }
      Rtmp <- Rtmp[order(as.double(Rtmp[,1]),decreasing=T),]
      rownames(Rtmp) <- NULL
      Result[[eval(Rnames[j])]] <- Rtmp
    }
    Result
}
###                                                                                                   ###
###                                                                                                   ###
#########################################################################################################
#########################################################################################################








#########################################################################################################
#########################################################################################################
###                                                                                                   ###
###  filter.fcn is the function to filter the stocks within the given dates (dates)                   ###
###  above or below the given boundries (bdy) according to the criterion,                             ###
###  "ratio" or differences (diff).                                                                   ###
###                                                                                                   ###

filter.fcn <- function(dates=(20140501:20140508), bdy=c(9.99,300)[1], criterion=c("ratio","diff")[1], data=VData ){

    dates <- as.character(dates)
    DATES <- data$T
    dir=c("pos","neg","both")
    idx <- c()
    for(k in 1:length(dates)){  idx <- c(idx,which(is.element(DATES,dates[k])))  }
    print(DATES[idx])
    STOCK <- list()
if (criterion=="ratio") {
    for(i in 1:length(dir)){
        stock <- matrix(NA,1,2)[0,]
        if(dir[i]=="both"){  for(k in idx){ tmp <- data$V[[k]][,4]/(VData$V[[k]][,5]+(VData$V[[k]][,5]==0)*1000);
                                            TMP <- data$V[[k]][,5]/(VData$V[[k]][,4]+(VData$V[[k]][,4]==0)*1000);  stock <- rbind(stock, VData$V[[k]][c(which(tmp>bdy),which(TMP>bdy)),2:3]) }  }
        if(dir[i]=="pos" ){  for(k in idx){ tmp <- data$V[[k]][,4]/(VData$V[[k]][,5]+(VData$V[[k]][,5]==0)*1000);  stock <- rbind(stock, VData$V[[k]][  which(tmp>bdy),2:3]) }  }
        if(dir[i]=="neg" ){  for(k in idx){ tmp <- data$V[[k]][,5]/(VData$V[[k]][,4]+(VData$V[[k]][,4]==0)*1000);  stock <- rbind(stock, VData$V[[k]][  which(tmp>bdy),2:3]) }  }
        stock <- stock[order(stock[,1]),]
        count <- sort(table(as.matrix(stock[,1])),decreasing=T)
        sname <- data.frame("stock"=names(count),"name"=NA,count)
        for(k in 1:nrow(sname)){ sname$name[k] <- stock[which(is.element(stock[,1],as.character(sname$stock[k])))[1],2] }
        STOCK[[i]] <- sname
    }
}else{
    bdy <- bdy*1000
    for(i in 1:length(dir)){
        stock <- matrix(NA,1,2)[0,]
        if(dir[i]=="both"){  for(k in idx){ tmp <- data$V[[k]][,4]-data$V[[k]][,5];
                                            TMP <- data$V[[k]][,5]-data$V[[k]][,4];  stock <- rbind(stock, VData$V[[k]][c(which(tmp>bdy),which(TMP>bdy)),2:3]) }  }
        if(dir[i]=="pos" ){  for(k in idx){ tmp <- data$V[[k]][,4]-data$V[[k]][,5];  stock <- rbind(stock, VData$V[[k]][  which(tmp>bdy),2:3]) }  }
        if(dir[i]=="neg" ){  for(k in idx){ tmp <- data$V[[k]][,5]-data$V[[k]][,4];  stock <- rbind(stock, VData$V[[k]][  which(tmp>bdy),2:3]) }  }
        stock <- stock[order(stock[,1]),]
        count <- sort(table(as.matrix(stock[,1])),decreasing=T)
        sname <- data.frame("stock"=names(count),"name"=NA,count)
        for(k in 1:nrow(sname)){ sname$name[k] <- stock[which(is.element(stock[,1],as.character(sname$stock[k])))[1],2] }
        STOCK[[i]] <- sname
    }
}
    STOCK[[3]]$buy  <- 0
    STOCK[[3]]$sell <- 0
    stock.both <- as.character(STOCK[[3]]$stock)
    stock.buy  <- as.character(STOCK[[1]]$stock)
    stock.sell <- as.character(STOCK[[2]]$stock)
    for(j in 1:nrow(STOCK[[1]])){  STOCK[[3]]$buy[ which(is.element(stock.both,stock.buy[ j]))] <- STOCK[[1]]$count[j]  }
    for(j in 1:nrow(STOCK[[2]])){  STOCK[[3]]$sell[which(is.element(stock.both,stock.sell[j]))] <- STOCK[[2]]$count[j]  }
    names(STOCK[[1]])[3] <- "buy"
    names(STOCK[[2]])[3] <- "sell"
    STOCK[[3]] <- (STOCK[[3]][order(STOCK[[3]][,3]*100+STOCK[[3]][,4]*10+STOCK[[3]][,5],decreasing=T),])
    STOCK[[4]] <- (STOCK[[3]][order(STOCK[[3]][,3]*000+(STOCK[[3]][,4]+1)*10-STOCK[[3]][,5],decreasing=T),])

    for(k in 1:length(dir)){
        action <- if(dir[k]=="pos"){"bought in"}else{ if(dir[k]=="neg"){"sold out "}else{"bought in and sold out"} }
        print(paste("There are",nrow(STOCK[[k]]),"stocks",action,"within these",length(idx),"bargain days."))
    }
    STOCK
}
###                                                                                                   ###
###  stock <- filter.fcn(dates=(20140501:20140508),bdy=c(9.99),criterion="ratio",data=VData)           ###
###                                                                                                   ###
#########################################################################################################
#########################################################################################################



















#########################################################################################################
#########################################################################################################
###                                                                                                   ###
###  cbdplot is a function to create plots specifically for combined data, VM.price.                  ###
###                                                                                                   ###

cbdplot <- function( wData=VM.price, stocks=NULL, dates=c("103/05/02","103/05/08"), sub=c("103/03/03","103/06/03"), dim=c(1,1), 
                     pdfname="plots of combined data of volume and price.pdf", show.plot=T, COLORS=c('darkred','darkgreen'), WDH=16, HGT=8 ){
##
##  包含外陸資買賣成交量的圖
##

    wnames <- names(wData)
    if (is.null(stocks)) {     stocks <- names(wData)  }
    if (is.integer(stocks)) {  stocks <- names(wData)[stocks]  }
    if (!is.null(pdfname) & show.plot) {  pdf(pdfname,width=WDH,height=HGT ) }
    if (!(dim[1]==1 & dim[2]==1)){  par(mfrow=c(dim[1],dim[2]))  }
    nn <- dim[1]*2*dim[2]
    def.par <- par(no.readonly = TRUE) # save default, for resetting...
    layout(matrix(1:nn,ncol=dim[2]),height=rep(c(3,2),dim[1]))
    for(k in 1:length(stocks)){
print(stocks[k])
      idx <- which(is.element(wnames,stocks[k]))
      if (length(idx)>0) {
        cdata <- wData[[idx]]
        stockname <- cdata[which(!is.na(cdata[,3]))[1],3]
        stockid <- names(wData)[idx]
        subid <- sort(which(is.element(cdata[,1],sub)))

        if (length(subid)==2) {   cdata <- cdata[subid[1]:subid[2],]    }else{
                                  if(!is.null(sub)){ print("check setting of 'sub' of stocks[",k,"]",sep="")  }    }
        LL=4
        bardata <- cdata[,c(4,5)]; bardata[is.na(bardata)] <- 0; rownames(bardata) <- cdata[,1]; bardata <- t(as.matrix(bardata))
        vlines <- which(is.element(cdata[,1],dates))

        par(mar = c(0,3,2,2), "bg"='gray10' )
        candle <- ggChartSeries( cdata, col=COLORS )
        plot(-10,xlim=c(1,nrow(cdata)+2),ylim=range(candle[,2:5]),ylab="", xaxt='n', yaxt='n', main=paste("Price and volume of",names(wData)[idx]), col.main='olivedrab' )
        box(col='lightskyblue')
        my.at <- seq(min(candle[,2:5]),max(candle[,2:5]),l=LL+1)
        axis(2, at = my.at, labels = rep("",length(my.at)), col='lightskyblue'  )  #,las=3
        text( cex=.8, x=-5, my.at+(my.at[2]-my.at[1])/3, labels=round(my.at,2), srt=90, adj=1.2, xpd=TRUE, col='lightskyblue' )  ## (nrow(candle)/10)
        xdata <- 1:nrow(candle)
        segments(xdata,candle$low,xdata,candle$high,col='grey25' )
        rect(xdata-.4,candle$candleLower,xdata+.4,candle$candleUpper,col=candle$fill )
        lines( xdata, candle$ma1W, col='orange')
        lines( xdata, candle$ma1M, col='purple')
        lines( xdata, candle$ma1Q, col='pink')
        abline(v=c(-10,vlines),col=4)
        abline(v=round(seq(1,ncol(bardata),l=LL*2))+0,col='gray20')
        legend( "topleft", lty=c(1,1,1,0), lwd=rep(1,3), col=c('orange','purple','pink'), text.col='lightskyblue', box.col='lightskyblue',
          legend=c(paste("Moving",c("WA","MA","QA")),paste("closing=",candle$close[length(candle$close)],sep=""))
        )
        par(mar = c(3,3,0,2) )
        plot(0,xlim=c(1,nrow(cdata)+2),ylim=range(c(bardata[1,],-bardata[2,])),ylab="", xaxt='n', yaxt='n',pch='' )
        box(col='lightskyblue')
        rect(xdata-.4,0,xdata+.4,bardata[1,],col='darkred' )
        rect(xdata-.4,-bardata[2,],xdata+.4,0,col='darkgreen' )
        abline(v=c(-10,vlines),h=0,col=4)
        abline(v=round(seq(1,ncol(bardata),l=LL*2))+0,col='gray20')
        legend( "topleft", text.col='lightskyblue', box.col='lightskyblue',
          legend=paste("Max.",c("buy  in ","sell out"),"volume =",round(c(max(bardata[1,]),max(bardata[2,]))/1000,1), "lot(s)")
        )
        my.at <- seq(-max(bardata[2,]),max(bardata[1,]),l=LL+1)[-(LL+1)]
        axis(2, at = my.at, labels = rep("",length(my.at)), las=3, col='lightskyblue' )
        text( cex=.8, x=-5, y=c(my.at+(my.at[3]-my.at[2])/5), labels=round(my.at/1000,2), srt=90, adj=1.2, xpd=TRUE, col='lightskyblue' )  ## (nrow(candle)/10)
        vlines <- c(vlines,round(seq(1,ncol(bardata),l=LL*2)))
        text( cex=.8, vlines+1, y=-max(bardata[2,]), labels=cdata[vlines,1], srt=50, adj=1.2, xpd=TRUE, col='lightskyblue' )

      }else{   print(paste("No data for stock[",k,"], ",stocks[k],sep=""))   }
    }
    if (!is.null(pdfname) & show.plot) {  dev.off()  }
    par(def.par)  #- reset to default
##  print("包含外陸資買賣成交量的圖")
}
###                                                                                                   ###
###  par(mfrow=c(1,3))                                                                                ###
###  cbdplot( stocks=1:3, pdfname=NULL)                                                               ###
###                                                                                                   ###
#########################################################################################################
#########################################################################################################







#########################################################################################################
#########################################################################################################
###                                                                                                   ###
###                                                                                                   ###
###                                                                                                   ###
library(quantmod)

ggChartSeries <- function( data=VM.price[[1]], col=c('red','green') ){

  tmp <- matrix(unlist(strsplit(as.character(data$date),"/")),ncol=3,byrow=T)
  tmp[,1] <- as.double(tmp[,1])+1911
  tmp <- data.frame(tmp)
  data$date <- do.call(paste,c(tmp,sep="-"))

  date  <- as.Date(data$date)
  open  <- as.vector(data$'開盤價')
  high  <- as.vector(data$'最高價')
  low   <- as.vector(data$'最低價')
  close <- as.vector(data$'收盤價')
 
#Then build the data frame
  xSubset <-data.frame('date'=date,'open'=open,'high'= high,'low'=low,'close'=close)

#Add Moving Averages
  xSubset$ma1Q <-  SMA(xSubset$close, min( 62,nrow(xSubset)) )
  xSubset$ma1M <-  SMA(xSubset$close, min( 21,nrow(xSubset)) )
  xSubset$ma1W <-  SMA(xSubset$close, min( 5,nrow(xSubset)) )
  xSubset$ma1Q[which(is.na(xSubset$ma1Q))] <- xSubset$ma1Q[which(!is.na(xSubset$ma1Q))[1]]
  xSubset$ma1M[which(is.na(xSubset$ma1M))] <- xSubset$ma1M[which(!is.na(xSubset$ma1M))[1]]
  xSubset$ma1W[which(is.na(xSubset$ma1W))] <- xSubset$ma1W[which(!is.na(xSubset$ma1W))[1]]

  xSubset$candleLower <- pmin(xSubset$open, xSubset$close)
  xSubset$candleUpper <- pmax(xSubset$open, xSubset$close)
  xSubset$fill <- ''
  xSubset$fill[xSubset$open < xSubset$close] = col[1]
  xSubset$fill[xSubset$fill ==''] = col[2]

  xSubset
}
##
## ggChartSeries( data=VM.price[[1]] )
##

###                                                                                                   ###
###  
###                                                                                                   ###
#########################################################################################################
#########################################################################################################










#########################################################################################################
#########################################################################################################
###                                                                                                   ###
###  
###                                                                                                   ###

##
## start <- Sys.Date()-200
## end <- Sys.Date()
## ggChartSeries( VM.price[[1]][,c(1,13:16,11)],Sys.Date()-200,Sys.Date() )
##
ggChartSeriesPlot <- function( x=cdata[,c(1,13:16,11)], start, end){

# the below is done redundantly for ease of maintenance later on
#First, strip OHLC data (need to vectorize)

  tmp <- matrix(unlist(strsplit(x[,1],"/")),ncol=3,byrow=T)
  tmp[,1] <- as.double(tmp[,1])+1911
  tmp <- data.frame(tmp)
  x[,1] <- do.call(paste,c(tmp,sep="-"))

  date  <- as.Date(x[,1])
  open  <- as.vector(x[,2])
  high  <- as.vector(x[,3])
  low   <- as.vector(x[,4])
  close <- as.vector(x[,5])
 
#Then build the data frame
  xSubset <-data.frame('date'=date,'open'=open,'high'= high,'low'=low,'close'=close)

dim(xSubset)

#We want to construct our candlesticks  
  xSubset$candleLower <- pmin(xSubset$open, xSubset$close)
  xSubset$candleMiddle <- NA
  xSubset$candleUpper <- pmax(xSubset$open, xSubset$close)
  xSubset$fill <- ''
  xSubset$fill[xSubset$open < xSubset$close] = 'white'
  xSubset$fill[xSubset$fill ==''] = 'red'
 
#Add Moving Averages
  xSubset$ma062 <-  SMA(xSubset$close, min( 62,nrow(xSubset)) )
  xSubset$ma021 <-  SMA(xSubset$close, min( 21,nrow(xSubset)) )

  xSubset$ma062[which(is.na(xSubset$ma062))] <- xSubset$ma062[which(!is.na(xSubset$ma062))[1]]
  xSubset$ma021[which(is.na(xSubset$ma021))] <- xSubset$ma021[which(!is.na(xSubset$ma021))[1]]

#Trim Data
  xSubset <-subset(xSubset, xSubset$date > start & xSubset$date < end)
 
#Graphing Step
  g <- ggplot(xSubset, aes(x=date, lower=candleLower, middle=candleMiddle, upper=candleUpper, ymin=low, ymax=high)) 
  g <- g + geom_boxplot(stat='identity', aes(group=date, fill=fill))
  g <- g + geom_line(aes(x=date, y=ma021))+ geom_line(aes(x=date, y=ma062))
  g 
}



###                                                                                                   ###
###  
###                                                                                                   ###
#########################################################################################################
#########################################################################################################










