





#########################################################################################################
#########################################################################################################
###                                                                                                   ###
###                                                                                                   ###

if (FALSE){  ## Do not run!!


CODE <- c(paste("000",1:9,sep=""),paste("00",10:99,sep=""),paste("0",100:999,sep=""),as.character("1000":"9999"))
ttt0 <- system.time({    price.CODE <- getStockData( years=c(201409), codes=CODE )    })
ttt0 <- print(paste("Took",round(ttt0[3]/60,2),"min.",sep=""))
length( price.CODE$S )
stockcodes <- names(  price.CODE$S )
save( stockcodes,price.CODE,ttt0,  file="All_StockNames_with_PriceData_basedon_date_201409.RData")



load("All_StockNames_with_PriceData_basedon_date_201408.RData")

load("VolumeDataFiles_201408.RData")



ttt1 <- system.time({    VData <- getVolumeData_ALL( DATA=VData, dates=c(201408) )    })
ttt1 <- print(paste("Took",round(ttt1[3]/60,2),"min.",sep=""))
save( VData,ttt1, file="VolumeDataFiles_201408.RData")


ttt2 <- system.time({    VM.stock <- Vol.fcn(Rnames=NULL,data=VData)    })
ttt2 <- print(paste("Took",round(ttt2[3]/60,2),"min.",sep=""))
save( VData,ttt1, VM.stock,ttt2, file="VolumeDataFiles_201408.RData")


snames    <- c();  for(k in 1:length(VData$V)){  snames <- union(snames,VData$V[[k]][,2])  };  snames <- sort(snames);  length(snames)

names(price)
names(price.CODE)
length(price[[2]])
length(price.CODE[[2]])
price[[1]][[10]]
price.CODE[[1]][[10]]

ttt3 <- system.time({    price <- getStockData( PRICE=price, years=c(201409), codes=snames, keep.dataFile=F )    })
ttt3 <- print(paste("Took",round(ttt3[3]/60,2),"min.",sep=""))
save( VData,ttt1, VM.stock,ttt2, price,ttt3,  file="VolumeDataFiles_201408.RData")


ttt4 <- system.time({    priceData <- getStockData( PRICE=price.CODE, years=c(2014), codes=stockcodes, keep.dataFile=F )    })
ttt4 <- print(paste("Took",round(ttt4[3]/60,2),"min.",sep=""))
save( VData,ttt1, VM.stock,ttt2, price,ttt3, priceData,ttt4,stockcodes,  file="VolumeDataFiles_201408.RData")

ttt5 <- system.time({    VM.price <- VMP.fcn( VMS=VM.stock, PDATA=price, stock=NULL, dates=NULL )    })
ttt5 <- print(paste("Took",round(ttt5[3]/60,2),"min.",sep=""))
save( VData,ttt1, VM.stock,ttt2, price,ttt3, priceData,ttt4,stockcodes, VM.price,ttt5,  file="VolumeDataFiles_201408.RData")

setdiff(stockcodes,names(priceData$S))


ttt6 <- system.time({    VM.priceData <- VMP.fcn( VMS=VM.stock, PDATA=priceData, stock=NULL, dates=NULL )    })
ttt6 <- print(paste("Took",round(ttt6[3]/60,2),"min.",sep=""))
save( VData,ttt1, VM.stock,ttt2, price,ttt3, priceData,ttt4,stockcodes, VM.price,ttt5, VM.priceData,ttt6,  file="VolumeDataFiles_201408.RData")

ttt1;  ttt2;  ttt3;  ttt4;  ttt5;  ttt6;


} ## Do not run!!


###                                                                                                   ###
###                                                                                                   ###
#########################################################################################################
#########################################################################################################




getwd()


load("VolumeDataFiles_201408.RData")
objects()


source("stock functions for real data.r")
##  source("stock functions.r")




#########################################################################################################
#########################################################################################################
###                                                                                                   ###
###  VData,  VM.stock, price, and  VM.price  are all data stored before.                              ###
###                                                                                                   ###
###  VData is the Vloume data categorized by each date.                                               ###
###  VM.stock is the Vloume data categorized by each stock.                                           ###
###  price is the Vloume data categorized by each stock.                                              ###
###  VM.price is the comvined Vloume and price data categorized by each stock.                        ###
###                                                                                                   ###
###                                                                                                   ###


###
###  source("stock functions for real data.r")
###

names(VData)
names(VM.stock)  ## names of working stocks
names(price$S )  ## names of working stocks

length(VM.stock)
length(price$S)
length(priceData$S)
length(VM.price)
length(VM.priceData)




length( VData$V )          ## number of dates from "20140301" to "20140531"
length( names(VM.stock) )  ## number of stocks
length( names(price$S)  )  ## number of stocks
names(VM.price)

t(sapply(VData$V,dim))
VData$V[[1]][1:10,]
price$S[[1]]$S[1:10,]
VM.stock[[1]][,1:5]
VM.stock[[2]][1:10,]
VM.price[[2]][1:10,]
VM.price[[2]][36:98,]

VM.price[[1]][36:98,][1:5,]
par(mfrow=c(2,3))
cbdplot( stocks=1:6, pdfname=NULL)

##
## cbdplot( wData=VM.priceData, stocks=NULL, dates=c("103/05/08","103/05/22","103/06/09"), sub=NULL, pdfname="All stock data.pdf", dim=c(1,1))
##
VM.priceData[[14]][,c(1,16)]
VM.priceData[[15]][,c(1,16)]

STOCKNAMES <- getName(names(VM.priceData))
STOCKNAMES[1:100,]

SID=c("3582")
SID=c("4999")
SID=c("1101")
SID=c("2331")
SID=c("5356")

SID=c("4746")
tmpppp <- VMP.fcn(VMS=VM.stock,stock=SID,dates=c(2014:2014))
tmpppp <- VMP.fcn(VMS=VM.stock,stock=SID,dates=c(2010:2014))
tmpppp[[1]][,1]
dim(tmpppp[[1]])
cbdplot( wData=tmpppp, stocks=SID, dates=c("103/05/08","103/05/22","103/06/09"), sub=NULL, pdfname=NULL, dim=c(1,1))
tmpppp[[1]][1:5,]
tmpppp[[1]][nrow(tmpppp[[1]])-9:0,]

cbdplot( wData=tmpppp, stocks=SID, dates=c("103/05/08","103/05/22","103/06/09"), sub=NULL, pdfname=NULL, dim=c(1,1))
cbdplot( wData=VM.priceData, stocks=SID, dates=c("103/05/08","103/05/22","103/06/09"), sub=NULL, pdfname=NULL, dim=c(1,1))


kk=1:4+12
cbdplot( wData=VM.priceData, stocks=names(VM.priceData)[kk], dates=c("103/05/08","103/05/22","103/06/09"), sub=NULL, pdfname=NULL, dim=c(2,2))







stock4746 <- getStockData( years=c(2014), codes=c("4746") )
tmpppp <- VMP.fcn(VMS=VM.stock,PDATA=stock4746,stock=c("4746"),dates=c(2014))
cbdplot( wData=tmpppp, stocks=NULL, dates=NULL, sub=NULL, pdfname=NULL, dim=c(1,1))

stock4746[[1]][[1]][[2]]
stock4746[[1]][[1]][[2]][nrow(stock4746[[1]][[1]][[2]]):1,][1:5,]







SID <- c("3260","2477")
SID <- c("0080","0058")
tmpppp <- VMP.fcn(VMS=VM.stock,stock=SID,dates=c(2014))
cbdplot( wData=tmpppp, stocks=NULL, dates=c("103/05/08","103/05/22","103/06/09"), sub=NULL, pdfname="test.pdf", dim=c(1,1))


stock2330 <- getStockData(years=c(2000:2014),codes=c("2330"))
tmpppp <- VMP.fcn(VMS=VM.stock,PDATA=stock2330,stock=c("2330"),dates=c(2000:2014))
cbdplot( wData=tmpppp, stocks=NULL, dates=NULL, sub=NULL, pdfname=NULL, dim=c(1,1))

cbdplot( wData=tmpppp, stocks=NULL, dates=c("103/05/08","103/05/22","103/06/09"), sub=NULL, pdfname="test2330.pdf", dim=c(1,1))



stock.tmp <- getStockData(years=c(2010:2014),codes=c("2820"))
tmpppp <- VMP.fcn(VMS=VM.stock,PDATA=stock.tmp,stock=c("2820"),dates=c(2010:2014))
cbdplot( wData=tmpppp, stocks=NULL, dates=NULL, sub=NULL, pdfname=NULL, dim=c(1,1))




getName(stock=c("0055"))



source("stock functions for real data.r")

###                                                                                                   ###
###                                                                                                   ###
#########################################################################################################
#########################################################################################################


price$S[[1]][[1]]
price$S[[1]][[2]]

length(price$S)
length(price.CODE$S)

length(snames)
length(stockcodes)
length(union(snames,stockcodes))
length(intersect(snames,stockcodes))


setdiff(intersect(snames,stockcodes),names(price$S))
setdiff(names(price$S),intersect(snames,stockcodes))
names(price$S)[1:5]


setdiff(1:6,1:5*2)
setdiff(1:5*2,1:6)


source("stock functions for real data.r")

#########################################################################################################
#########################################################################################################
###                                                                                                   ###
###  外陸資佔其可持股比例低高排序                                                                          ###
###                                                                                                   ###

SD2014.1 <- ShareData(Date="20140623")
SD2014.2 <- ShareData(Date="20140623",Sub=F)
dim(SD2014.1$M1)
dim(SD2014.2$M1)


SD2014 <- ShareData(Date="20140623")
dim(SD2014$M1)
SD2014$M1[,c(1:2,4:9)]
names(SD2014$M1)[5:6]

SPD <- SD2014$M1[order(SD2014$M1[,6]/rowSums(SD2014$M1[,5:6]),decreasing=T),-c(3,10:11)]   ## 外陸資佔其可持股比例低高排序
dim(SPD)
SPD[1:10,]

ShareData(code=c("4915","2712","2723","2727"))$M1[,-c(3,9:11)]
SD2014$M1[1:30+450,c(1:2,7:8)]








SID <- SPD[c(1:12,1+nrow(SPD)-12:1),][,1];  SID
tmp.pData <- getStockData(years=c(2014),code=SID)
tmp.wData <- VMP.fcn( PDATA=tmp.pData, stock=SID, dates=c(2014))
names(tmp.wData)
cbdplot( wData=tmp.wData, stocks=SID, dates=c("103/05/08","103/05/22","103/06/09"), sub=NULL, pdfname="test.pdf", dim=c(1,1))
getName(c("2540","2348","1516","6172"))
which(is.element(snames,c("2540","2348","1516","6172")))
intersect(snames,c("2540","2348","1516","6172"))


tmp.S <- getStockData(years=c(2014),codes=c("2540","2348","1516","6172","3582"))

tmp.S$S[[2]]$S[1:5,]

sapply(tmp.S$S[[2]]$S[,-1],class)

tmpppp <- VMP.fcn(PDATA=tmp.S,stock=SID[1:2],dates=c(2014))
tmpppp <- VMP.fcn(PDATA=tmp.S,stock=NULL,dates=c(2014))
cbdplot( wData=tmpppp, stocks=NULL, dates=c("103/05/08","103/05/22","103/06/09"), sub=NULL, pdfname="test55.pdf", dim=c(1,1))



for (kk in 1:length(VData$Vol) ){  print(c( kk, which(is.element(VData$Vol[[kk]][,2],c("2540","2348","1516","6172"))) ))  }

##
cbdplot( wData=tmp.wData, stocks=SID[1], dates=c("103/05/08","103/05/22","103/06/09"), sub=NULL, pdfname=NULL, dim=c(1,1))
cbdplot( wData=tmp.wData, stocks=SID[1:4+4*0], dates=c("103/05/08","103/05/22","103/06/09"), sub=NULL, pdfname=NULL, dim=c(1,3))
cbdplot( wData=tmp.wData, stocks=SID[1:4+4*1], dates=c("103/05/08","103/05/22","103/06/09"), sub=NULL, pdfname=NULL, dim=c(1,3))
cbdplot( wData=tmp.wData, stocks=SID[1:4+4*2], dates=c("103/05/08","103/05/22","103/06/09"), sub=NULL, pdfname=NULL, dim=c(1,2))
##

tmp.5522 <- VMP.fcn(stock=c("5522","2206"),dates=c(2014))
cbdplot( wData=tmp.5522, stocks=NULL, dates=c("103/05/08","103/05/22","103/06/09"), sub=NULL, pdfname="test5522.pdf", dim=c(1,1))




###                                                                                                   ###
###                                                                                                   ###
#########################################################################################################
#########################################################################################################



source("stock functions for real data.r")



#########################################################################################################
#########################################################################################################
###                                                                                                   ###
###  Use the function below to filter to stocks within the given dates, dates=(20140501:20140508).    ###
###                                                                                                   ###

stock <- filter.fcn(dates=(20140501:20140508),bdy=c(29.99),criterion="ratio",data=VData)
Rnames <- c(rownames(stock[[1]])[1:12],rownames(stock[[2]])[12:1]); length(Rnames)
cbdplot( stocks=Rnames, sub=c("103/03/03","103/06/03"), pdfname="Price and volume data.pdf", dim=c(1,2))
stock[[1]][1:20,]
stock[[2]][1:20,]
stock[[3]][1:20,]
stock[[4]][1:20,]



##
## http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf  ## R colors
##


source("stock functions for real data.r")
source("stock functions.r")


cbdplot( stocks=c("2206"), dates=c("103/05/08","103/05/22","103/06/09"), sub=NULL, pdfname="三陽.pdf", dim=c(1,1),WDH=12,HGT=8) 

tmp.wData <- VMP.fcn(stock=c("2206"),dates=c(2013:2014))
cbdplot( wData=tmp.wData, stocks=c("2206"), dates=c("103/05/08","103/05/22","103/06/09"), sub=NULL, pdfname=NULL, dim=c(1,1))
VM.price[[2]][1:20,]


cbdplot( stocks=NULL, sub=NULL, pdfname=NULL, dim=c(1,4)) 


cbdplot( stocks=c("1103","4999"), sub=NULL, pdfname="test.pdf", dim=c(1,2)) 


cbdplot( stocks=c("1103"), sub=c("103/03/03","103/06/03"), pdfname=NULL, dim=c(1,1))
cbdplot( stocks=c("3582","4999"), sub=NULL, pdfname=NULL, dim=c(1,2))
cbdplot( stocks=c("1103","0056","2498"), sub=c("103/03/03","103/06/03"), pdfname=NULL, dim=c(1,3))

cbdplot( stocks=c("2010","2417"), sub=NULL, pdfname=NULL, dim=c(1,2))
Rnames <- c(rownames(stock[[1]])[1:12],rownames(stock[[2]])[12:1]); length(Rnames)
cbdplot( stocks=Rnames, dim=c(2,2), pdfname="plots of combined data of volume and price.pdf" )
Rnames[1:12]
getName(Rnames)


which(is.element(rownames(stock[[2]]),"2417"))

tmp <- getStockData( years=c(2013,2014), codes=c("3498"), TransformData = T )
tmp <- getStockData( years=c(2013,2014), codes=c("3058","2601"), TransformData = T )


tmp <- getStockData( years=c(2012,2013,2014), codes=c("2206"), TransformData = T )
MakePlots( wData=tmp, stocks=NULL, nr=348, dates=c("103/05/08","103/05/22","103/06/09"), pdfname=NULL, show.plot=T, SINGLE=F, dim=c(1,2) ) 

par(mfrow=c(1,2))
MakePlots( wData=price, stocks=c("3058","2601"), nr=348, dates=c("102/01/03","103/05/02" ), pdfname=NULL, show.plot=T, SINGLE=F, dim=c(2,2) ) 
MakePlots( wData=price, stocks=c("0058","1109","1219"), nr=348, dates=c("102/01/03","103/05/02" ), pdfname=NULL, show.plot=T, SINGLE=F, dim=c(2,3) ) 



getName( stock=c("3058","2601","2010","0058") )

price$S[[10]]


tmp <- getStockData( years=c(201401:201403), codes=c("3058","2601"), TransformData = T )
TMP <- getStockData( PRICE=tmp, years=c(201404:201405), codes=c("3058","2601"), TransformData = T )
tmp$S[[2]][[2]]
TMP$S[[2]][[2]]
dim( tmp$S[[2]][[2]] )
dim( TMP$S[[2]][[2]] )


c("5519","8996")
length(names(price$S))
length(names(PRICE$S))
names(PRICE$S)[-which(is.na(pmatch(names(PRICE$S),c("5519","8996"))))]
names(price$S)[-which(is.na(pmatch(names(price$S),c("5519","8996"))))]

PRICE <- getStockData( PRICE=price, years=c(201406), codes=snames, TransformData = T )
which(sapply(1:length(PRICE$S),function(k){nrow(PRICE$S[[k]][[2]])})!=103)
names(PRICE$S)[which(sapply(1:length(PRICE$S),function(k){nrow(PRICE$S[[k]][[2]])})!=103)]



###                                                                                                   ###
###                                                                                                   ###
#########################################################################################################
#########################################################################################################




c("5285","6409","6414","8150")

which(is.element(names(price.test$S),c("5285","6409","6414","8150")))

ttt <- getStockData( years=c(2014), codes=c("5285","6409","6414","8150"), keep.dataFile=F )


getName(c("5285","6409","6414","8150"))





Rtmp <- getStockData( years=c(2013,2014), codes=Rnames, TransformData = T )
length(Rtmp$S)
par(mfcol=c(2,6))
for(k in 1:6){ plot(price$S[[k]]$S[,7]) }
for(k in 1:6){ plot(price$S[[k]]$S[-1,7]-price$S[[k]]$S[-length(price$S[[k]]$S[,7]),7]) }

par(mfcol=c(3,6))
for(k in 1:6){
  plot(price$S[[k]]$S[,7],type='l')
  plot(price$S[[k]]$S[-1,7]-price$S[[k]]$S[-length(price$S[[k]]$S[,7]),7],type='l')
  acf(price$S[[k]]$S[-1,7]-price$S[[k]]$S[-length(price$S[[k]]$S[,7]),7])
}
windows(); par(mfrow=c(1,3))
plot(YY,type='l')
acf(YY)
pacf(YY)




YY <- c(2,3)
for(t in 3:315){
  YY[t] <- .8*YY[t-1]-.3*YY[t-2]+rnorm(1,0,.3)
}
YY <- YY[-(1:15)]
windows(); par(mfrow=c(1,3))
plot(YY,type='l')
acf(YY)
pacf(YY)


eigen(matrix(c(.7,1,.2,0),2,2))



#########################################################################################################
#########################################################################################################
###                                                                                                   ###
###                                                                                                   ###


tmp.fcn <- function(data=price,kset=1:100,pset=4:2,N=20,pdfname=NULL,show.plot=T,dim=c(1,1),trajectory=c("diff","real","both")[2]){
##  data <- Rtmp
  kset <- kset[1:min(length(kset),length(data$S))]
  error <- matrix(0,length(kset),7)
  colnames(error) <- c("Diff.obs","Diff.est","Diff.Percentage","Same.Sign","order","Confidence","Var")

  error <- array(0,dim=c(length(kset),length(pset),7))
  dimnames(error)[[1]] <- paste("k=",kset,sep="")
  dimnames(error)[[2]] <- paste("p=",pset,sep="")
  dimnames(error)[[3]] <- c("Diff.obs","Diff.est","Diff.Percentage","Same.Sign","order","Confidence","Var")


  estset <- list()
  if (!is.null(pdfname) & show.plot) {  pdf(pdfname,width=20,height=9 ) }
  if (dim[1]==1 & dim[2]==1){ }else{  par(mfrow=c(dim[1],dim[2]))  }


  for(k in 1:length(kset)){
  for(p in 1:length(pset)){
    data0 <- data$S[[kset[k]]]$S
    cdata   <- data0[which(rowSums(data0[,-1])>0),7]
    tmpdata <- cdata[2:length(cdata)]-cdata[2:length(cdata)-1]   ## assume diff.stock.price follows AR model.
    KK <- length(tmpdata)-pset[p]
if (KK>5) {
    tt <- ar(tmpdata[1:KK])
    est <- tmpdata[1:KK]+tt$res
    est[1:tt$order] <- tmpdata[1:tt$order]
    pdt <- predict(tt, n.ahead=pset[p])
    est <- c(est, pdt$pred)
    est <- cbind( tmpdata[1:(pset[p]+KK)], est )
    real <- cbind(cumsum(c(cdata[1],est[,1])),cumsum(c(cdata[1],est[,2])))
    EST <- cbind( est, round( (est[,1]-est[,2]),3) )
    colnames(EST) <- c("Obs.","Est.","Res.")
##  error[k,p,1:2] <- c( EST[KK+1,1], max(EST[(KK+1):nrow(EST),2])) 
    error[k,p,1:2] <- EST[KK+1,1:2] 
    error[k,p,3] <- error[k,p,2]/error[k,p,1] 
    error[k,p,4] <- (ifelse(error[k,p,1]>0,1,-1)*ifelse(error[k,p,2]>0,1,-1))>0
    error[k,p,5] <- tt$order
    error[k,p,6] <- if( max(pdt$pred-2*pdt$se)>0 ){ 3 }else{ if( max(pdt$pred-1*pdt$se)>0 ){ 2 }else{ if( max(pdt$pred-tt$var)>0 ){ 1 }else{ 0 } } }
    error[k,p,7] <- tt$var 
    rr <- range(c(c(EST[max(1,KK-N+pset[p]):nrow(EST),1:2]),pdt$pred-1*pdt$se,pdt$pred+1*pdt$se))
if (sum(is.element(c("diff","both"),trajectory))>0){
    matplot( max(1,KK-N+pset[p]):nrow(EST), EST[max(1,KK-N+pset[p]):nrow(EST),1:2], type='l', lty=1, ylim=c(rr[1],rr[2]), xaxt='n', xlab="" ); abline(h=0,col=8)
##  mtext(substr(data$S[[kset[k]]][[1]],1,9))  ## uncomment to print stock name
    my.at <- c(round(seq(max(1,KK-N+pset[p]),nrow(EST),l=8)),KK)
    axis(1, at = my.at, labels = data0[my.at+1,1],las=3 )
    title( main=paste("1st pred. error = ",round(EST[KK+1,1]-EST[KK+1,2],3)," ( ",round(error[k,3],3),"% ) ; order = ",tt$order," ; p = ",pset[p],".",sep="") )
    lines(KK+1:pset[p],pdt$pred-1*pdt$se,lty=3,col=4)
    lines(KK+1:pset[p],pdt$pred+1*pdt$se,lty=3,col=4)
    lines(KK+1:pset[p],pdt$pred-1*tt$var,lty=2,col=6)
    lines(KK+1:pset[p],pdt$pred+1*tt$var,lty=2,col=6)
    abline(v=KK+.5,lty=1,col=3);  abline(v=1:(KK+pset[p]),lty=2);
}
if (sum(is.element(c("real","both"),trajectory))>0){
    matplot( max(1,KK-N+pset[p]):nrow(real), real[max(1,KK-N+pset[p]):nrow(real),], type='l',lty=1,xaxt='n',xlab="")
##  mtext(substr(data$S[[kset[k]]][[1]],1,9))  ## uncomment to print stock name
    my.at <- c(round(seq(max(1,KK-N+pset[p]),nrow(real),l=8)),KK+1)
    axis(1, at = my.at, labels = data0[my.at,1],las=3 )
    crnb <- round( (max(real[KK+(1:pset[p]),2])-real[KK+1,2])/real[KK+1,1]*100, 3)
    crns <- round( (min(real[KK+(1:pset[p]),2])-real[KK+1,2])/real[KK+1,1]*100, 3)
    tmpresult <- if(crns<0){ paste("Sell on the ",which(real[KK+(1:pset[p]),2]-real[KK+1,2]<0)[1]-1,"th day ; order = ",tt$order," ; p = ",pset[p],".",sep="") }else{""}
    title( if(crnb>0){ paste("Buy today ( +",crnb,"% ) ; order = ",tt$order," ; p = ",pset[p],".",sep="") }else{ tmpresult } )
    abline(v=KK+1.5,lty=1,col=3);  abline(v=1:(KK+pset[p]+1),lty=2);
}
    estset[[k]] <- list(diff=round(EST,3),real=round(real,3))
} # if (KK>5)
  } # pset
  } # kset
  if (!is.null(pdfname) & show.plot) {  dev.off()  }
  list(error=round(error,3),estset=estset)
}
####
####  ttt <- tmp.fcn(kset=1:100,pdfname="prediction plots 100.pdf")
####  ttt
####
dim( stocknames )
stocknames[21:30,]
(price$S[[24]][[1]])
ttt <- tmp.fcn(kset=1:3+8*2,pset=9:7,dim=c(2,3),traj="both")
ttt <- tmp.fcn(kset=1:3+8*2,pset=6:4,dim=c(2,3),traj="both")
ttt <- tmp.fcn(kset=1:3+8*2,pset=3:1,dim=c(2,3),traj="both")

ttt <- tmp.fcn(kset=c(1:1000),pset=10:1+1,dim=c(2,5),traj="real",pdfname="prediction__1-5.pdf")
ttt$error
ttt$error[1,,]
ttt$error[,1,]



windows()
ttt <- tmp.fcn(kset=1:8+8*2,pset=8,dim=c(2,8),traj="both")
ttt$error
u=2
ttt$estset[[u]]$diff[(nrow(ttt$estset[[u]]$diff)-p):nrow(ttt$estset[[u]]$diff),]
ttt$estset[[u]]$real[(nrow(ttt$estset[[u]]$real)-p):nrow(ttt$estset[[u]]$real),]

####
####
ttt <- tmp.fcn(kset=1:2,p=8,N=20,dim=c(1,2),data=Rtmp)
ttt$error
ttt <- tmp.fcn(kset=1,N=20,dim=c(1,1),data=Rtmp)
ttt$error
####
####
ttt <- tmp.fcn(kset=4,p=28,N=40,data=price);  ttt$error;
####
####
tmp <- getStockData( years=c(2013,2014), codes=c("3498","1210"), TransformData = T )
windows()
par(mfrow=c(2,2))
MakePlots( wData=tmp, stocks=NULL, nr=100, dates=c("102/01/03","103/05/02" ), pdfname=NULL, show.plot=T, SINGLE=F )
par(mfrow=c(2,5))
k=1
  ttt11 <- tmp.fcn(kset=k,p=11,N=20,data=tmp)
  ttt10 <- tmp.fcn(kset=k,p=10,N=20,data=tmp)
  ttt9  <- tmp.fcn(kset=k,p=9 ,N=20,data=tmp)
  ttt8  <- tmp.fcn(kset=k,p=8 ,N=20,data=tmp)
  ttt7  <- tmp.fcn(kset=k,p=7 ,N=20,data=tmp)
  ttt6  <- tmp.fcn(kset=k,p=6 ,N=20,data=tmp)
  ttt5  <- tmp.fcn(kset=k,p=5 ,N=20,data=tmp)
  ttt4  <- tmp.fcn(kset=k,p=4 ,N=20,data=tmp)
  ttt3  <- tmp.fcn(kset=k,p=3 ,N=20,data=tmp)
  ttt2  <- tmp.fcn(kset=k,p=2 ,N=20,data=tmp)
####
####
pdf("predicting difference data 02.pdf",width=24,height=10)
tttset <- list()
par(mfrow=c(2,5))
for(k in 1:length(Rtmp$S)){
  ttt11 <- tmp.fcn(kset=k,p=11,N=20,data=Rtmp)
  ttt10 <- tmp.fcn(kset=k,p=10,N=20,data=Rtmp)
  ttt9  <- tmp.fcn(kset=k,p=9 ,N=20,data=Rtmp)
  ttt8  <- tmp.fcn(kset=k,p=8 ,N=20,data=Rtmp)
  ttt7  <- tmp.fcn(kset=k,p=7 ,N=20,data=Rtmp)
  ttt6  <- tmp.fcn(kset=k,p=6 ,N=20,data=Rtmp)
  ttt5  <- tmp.fcn(kset=k,p=5 ,N=20,data=Rtmp)
  ttt4  <- tmp.fcn(kset=k,p=4 ,N=20,data=Rtmp)
  ttt3  <- tmp.fcn(kset=k,p=3 ,N=20,data=Rtmp)
  ttt2  <- tmp.fcn(kset=k,p=2 ,N=20,data=Rtmp)
  tttset[[k]] <- rbind(ttt7$error,ttt6$error,ttt5$error,ttt4$error,ttt3$error,ttt2$error)
  names(tttset[[k]]) <- Rtmp$S[[k]][[1]]
}
dev.off()
####
####
####
####
test.fcn <- function(idx=1,DATA=price,n0=50,m0=11:1,dim=NULL){
if (is.null(dim)){
  if (length(m0)<8){  par(mfrow=c(2,4))  }else{  if (length(m0)<12){  par(mfrow=c(2,6))  }else{  par(mfrow=c(3,6))  }  }
}else{
  par(mfrow=c(dim[1],dim[2]))
}
  tset <- list()
  info.table <- matrix(0,length(m0),12)
  for(t in 1:length(m0)){
    tset[[t]] <- tmp.fcn(kset=idx,p=m0[t],N=n0,data=DATA);
    current <- tset[[t]]$est[nrow(tset[[t]]$est)-m0[t],1]
    mdiff <- round(max(tset[[t]]$est[(nrow(tset[[t]]$est)-m0[t]+1):nrow(tset[[t]]$est),2]-current),3)
    mtext(paste("p =",m0[t],"; diff.max =",mdiff));
    diff <- tset[[t]]$est[nrow(tset[[t]]$est),]
    info.table[t,] <- c(m0[t],tset[[t]]$error,current,diff)
  }
  plot( info.table[,1], info.table[,ncol(info.table)],type='l' )
  title(paste("idx =",idx),outer=T,line=-1)
  info.table <- cbind(info.table,info.table[,11]-info.table[,9])
  colnames(info.table) <- c("p",colnames(tset[[t]]$error),"current","final","est","diff","diff2")
  round( info.table, 3)
}
##
##
test.fcn(idx=1,m0=c(17:1))
test.fcn(idx=2,m0=c(17:1))
test.fcn(idx=3,m0=c(17:1))
test.fcn(idx=4,m0=c(17:1))
test.fcn(idx=5,m0=c(17:1))
test.fcn(idx=6,m0=c(17:1))
####
####
####
which(ttt$error[,4]>0)
round(sum(ttt$error[,4]>0)/nrow(ttt$error),2)
####
####


tt$residuals
tt$aic
names(tt)
tt[[14]]
predict(tt, n.ahead = 1)$pred










##
tmp <- getRatioData()

names(tmp)
length(tmp$Vo)
dim(tmp$VM)
tmp$VM[1,1:5,]
tmp$Vo[[1]][1:5,]

###                                                                                                   ###
###                                                                                                   ###
#########################################################################################################
#########################################################################################################



which(is.element(wnames,stocks))
Rnames[setdiff(1:24,which(is.element(stocks,wnames)))]
[1] "No data for stock[3], 6409"
[1] "No data for stock[8], 8150"


names(price$S)
which(is.element(names(price$S),c("6049","8051")))


TBI.R <- getStockData( years=c(2014), codes=c("3474","3260"), TransformData = T )
par(mfrow=c(1,2))
MakePlots( wData=TBI.R, stocks=NULL, nr=248, dates=c(" 103/05/02"," 103/05/08"), pdfname=NULL, show.plot=T, SINGLE=T ) 
cbdplot( stocks=c("3474","3260"), sub=NULL, pdfname=NULL, dim=c(1,2))
cbdplot( stocks=c("3474"), sub=NULL, pdfname=NULL, dim=c(1,1))

iddd <- which(is.element(names(VM.stock),"3474"))
tmpdata <- VM.stock[[iddd]]
cbind(tmpdata[,1],tmpdata[,4:5],tmpdata[,4]-tmpdata[,5],cumsum(tmpdata[,4]-tmpdata[,5]))
TBI.R$S[[1]][[2]]

tmpdata




VM.stock[[1]][1:5,]
VM.price[[1]][36:98,1:5][1:5,]


for(k in 1:length(VM.stock)){  VM.stock[[k]] <- VM.stock[[k]][nrow(VM.stock[[k]]):1,]  }
k=1
VM.stock[[k]][1:10,]
VM.stock[[k]][1:10,]


length(VData$V)
dim(VData$V[[1]])


length(price$S)
price$S[[1]][[1]]


k=1
VM.stock[[1]][1:5,]
VM.stock[[2]][1:5,]

VM.price[[1]][1:5+35,]
price$S[[1]][[2]][1:5+35,]






tmpdata[,1:5]


which(is.element(names(VM.price),"3260"))
which(is.element(names(VM.stock),"3260"))

which(is.element(snames,"3260"))


for(k in 1:length(VData$V)){  print( which(is.element(VData$V[[k]][,2],"3260")) )  }
for(k in 1:length(VData$V)){  print( which(is.element(VData$V[[k]][,3],"威剛")) )  }

for(k in 1:length(VData$V)){  print( which(is.element(VData$V[[k]][,3],"聯電")) )  }




Rfcn <- function(k){ cbind( VData$V[[k]][,1:5], "ratio"=round(VData$V[[k]][,4]/(VData$V[[k]][,5]+(VData$V[[k]][,5]==0)*1000),2)) }
tmp <- Rfcn(1)
tmp[(tmp[,6]>9.99|tmp[,6]<.1)&(tmp[,6]!=0),]
sum(tmp[,6]>9.99|tmp[,6]<.1)
sum((tmp[,6]>9.99|tmp[,6]<.1)&(tmp[,6]!=0))
sum(tmp[,6]==0)


tmp     <- VData$V[[k]][which(abs(VData$V[[k]][,4]-VData$V[[k]][,5])>  300000 ),]
tmp.pos <- VData$V[[k]][which(   (VData$V[[k]][,4]-VData$V[[k]][,5])>  300000 ),]
tmp.neg <- VData$V[[k]][which(   (VData$V[[k]][,4]-VData$V[[k]][,5])<(-300000)),]
dim(tmp)
dim(tmp.pos)
dim(tmp.neg)
tmp
tmp.pos
tmp.neg




stock <- filter.fcn(dates=(20140501:20140508),bdy=c(9.99,.1),criterion="ratio",data=VData)
stock[[4]]
sapply(stock,dim)
stock[[1]][1:20,]
stock[[2]][1:20,]
stock[[3]][1:20,]
stock[[4]][1:20,]
cbind(stock[[1]][1:20,],stock[[2]][1:20,])
cbind(stock[[3]][1:60,],stock[[4]][1:60,])
colSums(stock[[3]][,3:5]!=0)


source("stock functions.r")
Rnames <- rownames(stock[[4]])
TBI.R <- getStockData( years=c(2014), codes=Rnames[1:20], TransformData = T )
MakePlots( wData=TBI.R, stocks=NULL, nr=248, dates=c(" 103/05/02"," 103/05/08"), pdfname='TopBuyIn1Q.R_stocks_trend_plots_2014.pdf', WDH=20, HGT=9 ) 

MakePlots( wData=TBI.R, stocks=NULL, nr=248, dates=c(" 103/05/02"," 103/05/08"), pdfname=NULL, WDH=20, HGT=9, show.plot=T ) 







k=1
VData$V[[k]][1:20,]




Rnames <- rownames(stock[[4]])[c(1:10,nrow(stock[[4]])-19:0)]
data <- VData$V
Result <- list()
for(j in  1:length(Rnames)){
  Rtmp <- data[[1]][1:63,]
  for(k in 1:length(data)){
    if( sum(is.element(data[[k]][,2],Rnames[j]))>0 ){
      Rtmp[k,] <- data[[k]][which(is.element(data[[k]][,2],Rnames[j])),]
    }else{
      print(c(j,k))
      Rtmp[k,] <- NA
    }
  }
  Result[[j]] <- Rtmp
}
TBI.R <- getStockData( years=c(2014), codes=Rnames, TransformData = T )
c(j,k)


length(Result)
KK=10
Result[[KK]]
TBI.R$S[[KK]]$S[36:98,]
KK=10
tmp <- cbind( Result[[KK]][,-10], TBI.R$S[[KK]]$S[36:98,7:8] );  tmp[,4:9] <- round(log(tmp[,4:9]+1),2)
## par(mfrow=c(2,3))
matplot(cbind(tmp[,4]-tmp[,5],tmp[,10]),type='l',lty=1); abline(h=0,col=4);  abline(v=c(43,47),col=3);
matplot(cbind(tmp[,6]-tmp[,7],tmp[,10]),type='l',lty=1); abline(h=0,col=4);  abline(v=c(43,47),col=3);
matplot(cbind(tmp[,8]-tmp[,9],tmp[,10]),type='l',lty=1); abline(h=0,col=4);  abline(v=c(43,47),col=3);
####
####


KKset <- c(1:5,16:20)
KKset <- c(1:10)
KKset <- c(11:20)
KKset <- c(21:30)
par(mfrow=c(2,5))
for(KK in KKset){
    tmp <- cbind( Result[[KK]][,-10], TBI.R$S[[KK]]$S[36:98,7:8] );  tmp[,4:9] <- round(log(tmp[,4:9]+1),2)
    coe <- 1  ## (range(na.omit(tmp[,4]-tmp[,5]))[2]-range(na.omit(tmp[,4]-tmp[,5]))[1])/(range(tmp[,10]-tmp[63,10])[2]-range(tmp[,10]-tmp[63,10])[1])
    matplot(cbind(tmp[,4]-tmp[,5],(tmp[,10]-tmp[63,10]*0)*coe),type='l',lty=1 );
##  title(main=paste(tmp[1,2],tmp[63,10]));
    title(main=paste(tmp[1,2],tmp[1,3],tmp[63,10]));
    abline(h=c(0,tmp[63,10]),col=4);  abline(v=c(43,47),col=3);
}
##
##
KK=28
tmp <- cbind( Result[[KK]][,-10], TBI.R$S[[KK]]$S[36:98,7:8] );  tmp[,4:9] <- round(log(tmp[,4:9]+1),2)
tmp


summary(VData$V[[1]])




TBI.second <- getStockData( years=c(2013,2014), codes=c("2613","4934"), TransformData = T )
par(mfrow=c(1,2))
MakePlots( wData=TBI.second, stocks=NULL, nr=248, dates=c(" 103/05/02"," 103/05/08"), pdfname=NULL, WDH=20, HGT=9, show.plot=T,SINGLE=T ) 







VData <- cbind(VData$V[[k]][,4]-VData$V[[k]][,5],VData$V[[k]][,6]-VData$V[[k]][,7],VData$V[[k]][,8]-VData$V[[k]][,9],VData$V[[k]][,10])
matplot( VData, type='l',col=1:4)
cor(VData)



VData.1 <- VData.2 <- VData.3 <- VData.4 <- VData
for(k in 1:length(VData$V)){
  VData.1$V[[k]] <- VData$V[[k]][order(VData$V[[k]][,4]-VData$V[[k]][,5],decreasing=T),]
  VData.2$V[[k]] <- VData$V[[k]][order(VData$V[[k]][,6]-VData$V[[k]][,7],decreasing=T),]
  VData.3$V[[k]] <- VData$V[[k]][order(VData$V[[k]][,8]-VData$V[[k]][,9],decreasing=T),]
  VData.4$V[[k]] <- VData$V[[k]][order(VData$V[[k]][,10],decreasing=T),]
}
VData.1$V[[1]][1:10,]
VData.2$V[[1]][1:10,]
VData.3$V[[1]][1:10,]
VData.4$V[[1]][1:10,]



VData$V[[1]][1:10,]
VM.tmp1 <- data.frame(t(sapply( VData$V, function(obj){  obj[which.max(obj[,4]-obj[,5]),]  })))
VM.tmp2 <- data.frame(t(sapply( VData$V, function(obj){  obj[which.max(obj[,6]-obj[,7]),]  })))
VM.tmp3 <- data.frame(t(sapply( VData$V, function(obj){  obj[which.max(obj[,8]-obj[,9]),]  })))
VM.tmp4 <- data.frame(t(sapply( VData$V, function(obj){  obj[which.max(obj[,10])       ,]  })))

VM.tmp1  ## 當天外陸資買賣差最大者
VM.tmp2  ## 當天投信顧買賣差最大者
VM.tmp3  ## 當天自營商買賣差最大者
VM.tmp4  ## 當天三大法人買賣差最大者


TopBuyIn1Q.name  <- names(sort(table(unlist(cbind(VM.tmp1[,3],VM.tmp2[,3],VM.tmp3[,3],VM.tmp4[,3]))),decreasing=T))
TopBuyIn1Q.count <- sort(table(unlist(cbind(VM.tmp1[,2],VM.tmp2[,2],VM.tmp3[,2],VM.tmp4[,2]))),decreasing=T)
TopBuyIn1Q <- names(TopBuyIn1Q.count)
data.frame(cbind( TopBuyIn1Q, TopBuyIn1Q.name, TopBuyIn1Q.count ))




source("stock functions.r")
TBI <- getStockData( years=c(2014), codes=TopBuyIn1Q[1:20], TransformData = T )
MakePlots( wData=TBI, stocks=NULL, nr=248, dates=c(" 102/12/10"), pdfname='TopBuyIn1Q_stocks_trend_plots_2014.pdf', WDH=20, HGT=9 ) 
names(TBI)
class(TBI$S)
length(TBI$S)
TBI$S[[1]]
VM.tmp1

x=10
which(unlist(VM.tmp1[,2])==TopBuyIn1Q[x])
which(unlist(VM.tmp2[,2])==TopBuyIn1Q[x])
which(unlist(VM.tmp3[,2])==TopBuyIn1Q[x])
which(unlist(VM.tmp4[,2])==TopBuyIn1Q[x])

VM.tmp1[which(unlist(VM.tmp1[,2])==TopBuyIn1Q[x]),]
VM.tmp2[which(unlist(VM.tmp2[,2])==TopBuyIn1Q[x]),]
VM.tmp3[which(unlist(VM.tmp3[,2])==TopBuyIn1Q[x]),]
VM.tmp4[which(unlist(VM.tmp4[,2])==TopBuyIn1Q[x]),]


##
TBI.1 <- getStockData( years=c(2013,2014), codes=TopBuyIn1Q[1], TransformData = T )
##

Date.id <- unlist(VM.tmp1$date)[ sort(union(c(
which(unlist(VM.tmp1[,2])==TopBuyIn1Q[1]),which(unlist(VM.tmp2[,2])==TopBuyIn1Q[1]),
which(unlist(VM.tmp3[,2])==TopBuyIn1Q[1]),which(unlist(VM.tmp4[,2])==TopBuyIn1Q[1])),NULL)) ]

Date.id <- unlist(VM.tmp1[which(unlist(VM.tmp1[,2])==TopBuyIn1Q[1]),1])

Dates <- paste(" 103",sapply(Date.id,function(x){substr(x,5,6)}),sapply(Date.id,function(x){substr(x,7,8)}),sep="/");  Dates
MakePlots( wData=TBI.1, stocks=NULL, nr=248, dates=c(" 103/03/06",Dates), pdfname='TopBuyIn1Q.1_stocks_trend_plots_2014.pdf', WDH=20, HGT=9 ) 

MakePlots( wData=TBI.1, stocks=NULL, nr=248, dates=c(Dates), pdfname=NULL, WDH=20, HGT=9, show.plot=T, SINGLE=T ) 










source("stock functions.r")
SML <- getStockData( years=c(2013,2014), codes=c("2311","5522","8083","6146"), TransformData = T )
MakePlots( wData=SML, stocks=NULL, nr=248,dates=c(" 102/12/10"), pdfname='Sample1_stock_trend_plots_2013_now.pdf', WDH=20, HGT=9 ) 


SML$S[[1]][[1]]
SML$S[[2]][[1]]
SML$S[[3]][[1]]


code=codes[1]; code
code=codes[3]; code



source("stock functions.r")
CODES <- c("8083","3213","8109","5230","8103","6292","6146","4745","1527","2731","3323","5305")
MaInNine <- getStockData( years=c(2014), codes=CODES, TransformData = T )
MakePlots( wData=MaInNine, stocks=NULL, nr=248, dates=c("102/12/10","103/03/06"), bds=c(23.35,26.75,45,55,100,145,150,160,200), 
           pdfname='MaInNine_stock_trend_plots_2013_now.pdf', WDH=20, HGT=9, show.plot=T ) 



Types.tmp <- getTypesData( dates=c(20140520:20140524), TransformData = T, filename=NULL ) 
Types.tmp
matplot( t(Types.tmp$VM[,,"成交股數"]), type='l')


Market.tmp <- getMarketData( dates=c(201404:201406), TransformData = T, filename=NULL ) 
Market.tmp$MD


Volume.tmp <- getVolumeData_ALL( dates=c(201403,201404,201405), TransformData = T )
names(Volume.tmp)
length(Volume.tmp$V)
sapply(Volume.tmp$V,dim)
Volume.tmp$V[[1]][1:20,]


May.tmp <- t(sapply(Volume.tmp$V,function(obj){
  tmp <- obj[,4]-obj[,5]
  obj[which.max(tmp),]
}))
May.tmp





































