






##
## ref : http://www.twse.com.tw/ch/trading/exchange/STOCK_DAY/STOCK_DAYMAIN.php
## ref : http://www.gretai.org.tw/ch/stock/aftertrading/daily_trading_info/st43_download.php?d=103/04&stkno=8083&s=0,asc,0
## 個股日成交資訊：
##
##

getStockData <- function( PRICE=NULL, years=c(201205,201206,201207), codes=c("4746","2498","2845","8083") ) {

  keep.dataFile=F
  TransformData=T 
  tt <- print(Sys.time())
  if( is.null(years) ){  years <- paste(substr(tt,1,4),substr(tt,6,7),sep="")  }
  if( is.null(PRICE) ){  SNAME <- NULL;  which.type <- cbind(union(codes,SNAME),"")
  }else{                 SNAME <- names(PRICE$StockDatabase)
                         if (length(setdiff(codes,SNAME))>0) {
                           which.type <- rbind(PRICE$which.type,cbind(setdiff(codes,SNAME),""))
                         }else{
                           which.type <- PRICE$which.type
                         }
  }
  StockDatabase <- list()

  YEARS <- list()
  for (i in 1:length(years)) {
      if (length(strsplit(as.character(years[i]),"")[[1]])==4) {
          YEARS[[i]] <- paste(years[i],c(paste("0",1:9,sep=""),10:12),sep="")
      }else{
          YEARS[[i]] <- years[i]
      }
  }

  idx <- which(!(unlist(YEARS)>paste(substr(tt,1,4),substr(tt,6,7),sep="")))
  years <- as.character(sort(unlist(YEARS)[idx],decreasing=T)); years
  if( substr(tt, 9,10)=="01"&as.double(substr(tt,12,13))<15 ){  years <- years[-1]  }


  for (code in union(codes,SNAME)) {
  type <- 0

try({

  if (is.element(code,setdiff(SNAME,codes))) {
      StockDatabase[[ eval(code) ]] <- PRICE$StockData[[ eval(code) ]]
  }else{

    TradingDates <- c()
    StockData <- list()
    Data <- matrix(0,0,9)
##  各股日成交資訊  http://www.twse.com.tw/ch/trading/exchange/STOCK_DAY/STOCK_DAYMAIN.php
    str1 <- "http://www.twse.com.tw/ch/trading/exchange/STOCK_DAY/STOCK_DAY_print.php?genpage=genpage/Report"
    str2 <- "_F3_1_8_"
    str3 <- ".php&type=csv"
    url <- paste(str1, years[1], "/", years[1], str2, code, str3, sep = "")
    lines <- readLines(url,warn=F)

    if ( length(lines)>2 & length(strsplit(lines[3],",")[[1]])>8 ) {
##  上市股票

        print(code)
        type <- 1
        for (date in years) {
            Now <- strsplit(as.character(Sys.time()),"-")[[1]]
            Now <- as.double(paste(Now[1],Now[2],sep=""))
            if ( as.double(date)>Now ) break
            str1 <- "http://www.twse.com.tw/ch/trading/exchange/STOCK_DAY/STOCK_DAY_print.php?genpage=genpage/Report"
            str2 <- "_F3_1_8_"
            str3 <- ".php&type=csv"
            url <- paste(str1, date, "/", date, str2, code, str3, sep = "")
            fileName <- paste("pastdata/Stock_", code,"__",date, ".txt", sep = "")  ## fileName <- paste("Stock_", code, ".txt", sep = "")
            lines <- readLines(url,warn=F)
            if (length(lines)>2 & length(strsplit(lines[3],",")[[1]])>8 ) {
              TradingDates <- c(TradingDates,date)
              StockName <- lines[1:2]
              content <- lines[2:length(lines)]
              writeLines(content, fileName)
              data <- read.table(fileName, header = T, sep=",")
              IDX <- union(which(is.element(data[,4],c("--","0"))),which(data[,4]==0))
              if (length(IDX)>0) {
                if (min(IDX)==1){ IDX <- IDX[-1] }
                for (i in IDX){  data[i,4:7] <- data[i-1,4:7]  }
              }
              for ( k in c(1:3,4:7,9)) { data[,k] <- as.character(data[,k]) }
              data8 <- rep(0,nrow(data))
              if ( !(class(data[,8]) == "numeric") ){
                for(kk in 1:nrow(data)){
                  tmp <- strsplit(as.character(data[kk,8]),"")[[1]]
                  tmp <- gsub("X",replacement="",tmp)
                  data8[kk] <- as.double(apply(matrix(tmp,ncol=1),2,paste,collapse=""))
                }
                data[,8] <- data8
              }
              Data <- rbind(Data, data[nrow(data):1,])

              if (!keep.dataFile) {   file.remove(fileName)  }
            } ## if length(lines)>2
        } ## date

##  上市股票
    }else{
##  上櫃股票

        type <- 2
        for (date in years) {

            Now <- strsplit(as.character(Sys.time()),"-")[[1]]
            Now <- as.double(paste(Now[1],Now[2],sep=""))
            if ( as.double(date)>Now ) break
            str1 <- "http://www.gretai.org.tw/ch/stock/aftertrading/daily_trading_info/st43_download.php?d="
            str2 <- "&stkno="
            str3 <- "&s=0,asc,0"
            ym <- paste(as.double(substr(date,1,4))-1911,substr(date,5,6),sep="/")
            url <- paste(str1, ym, str2, code, str3, sep = "")
            fileName <- paste("pastdata/Stock_", code,"__",date, ".txt", sep = "")  ## fileName <- paste("Stock_", code, ".txt", sep = "")
            lines <- readLines(url,warn=F)
            lines[4] <- paste(lines[4],code,lines[3],lines[1],sep=" ")
            lines <- lines[-c(1:3,length(lines))]

            if (length(lines)>6) {

              if (date==years[1]){    print(code)    }

              TradingDates <- c(TradingDates,date)
              StockName <- lines[1:2]
              content <- lines[2:length(lines)]
              writeLines(content, fileName)
              data <- read.table(fileName, header = T, sep=",")
              IDX <- union(which(is.element(data[,4],c("--","0"))),which(data[,4]==0))
              if (length(IDX)>0) {
                if (min(IDX)==1){ IDX <- IDX[-1] }
                for (i in IDX){  data[i,4:7] <- data[i-1,4:7]  }
              }
              for ( k in c(1:3,4:7,9)) { data[,k] <- as.character(data[,k]) }
              data8 <- rep(0,nrow(data))
              if ( !(class(data[,8]) == "numeric") ){
                for(kk in 1:nrow(data)){
                  tmp <- strsplit(as.character(data[kk,8]),"")[[1]]
                  tmp <- gsub("X",replacement="",tmp)
                  data8[kk] <- as.double(apply(matrix(tmp,ncol=1),2,paste,collapse=""))
                }
                data[,8] <- data8
              }
              Data <- rbind(Data, data[nrow(data):1,])

              for (k in c(2:3)) { 
                IDX <- which(sapply(strsplit(Data[,k],","),length)>1)
                for(i in IDX){
                  tmp <- strsplit(as.character(Data[i,k]),"")[[1]]
                  if ( sum(is.element(tmp,","))>0 ) {    tmp <- gsub(",",replacement="",tmp)    }
                  Data[i,k] <- apply(matrix(tmp,ncol=1),2,paste,collapse="")
                }
              }

              if (!keep.dataFile) {   file.remove(fileName)  }
            } ## if length(lines)>2
        } ## date
        for (k in c(2:3)) {    Data[,k] <- as.character(as.double(Data[,k])*1000)    }

##  上櫃股票
    }

    if (nrow(Data)>0) {
      Data <- Data[nrow(Data):1,]
      which.type[pmatch(code,which.type[,1]),2] <- type
      des <- strsplit(StockName[1]," ")[[1]][2:4]
      StockData$descp <- paste(des[1],des[2],strsplit(des[3],split="\"")[[1]])
      names(Data) <- c("日期","成交股數","成交金額","開盤價","最高價","最低價","收盤價","漲跌價差","成交筆數")
##    names(Data) <- strsplit(StockName[2],",")[[1]]
      nr <- nrow(Data)
      if (TransformData) {
        for (k in c(2:3,4:7,9)) { 
          IDX <- which(sapply(strsplit(Data[,k],","),length)>1)
          for(i in IDX){
            tmp <- strsplit(as.character(Data[i,k]),"")[[1]]
            if ( sum(is.element(tmp,","))>0 ) {    tmp <- gsub(",",replacement="",tmp)    }
            Data[i,k] <- apply(matrix(tmp,ncol=1),2,paste,collapse="")
          }
        }
        IDX <- which(is.element(Data[,4],c("--",0)))
        if (length(IDX)>0){   if (IDX[1]==1){  Data[1,-1] <- 0; IDX <- IDX[-1]  }  }
        for (i in IDX){  Data[i,4:7] <- Data[i-1,4:7]  }
        for (k in 2:9) { Data[,k] <- as.double( Data[,k] ) }
      }
      idx <- which(is.element(SNAME,code))
      if (length(idx)>0) {
        nidx <- which(is.element(PRICE$StockDatabase[[idx]][[2]][,1],Data[,1]))
        if (length(nidx)>0) {
          Data <- rbind(PRICE$StockDatabase[[idx]][[2]][-nidx,],Data)
        }else{
          Data <- rbind( PRICE$StockDatabase[[idx]][[2]],Data)
        }
      }

##
##  replace miss data by previous data
IID <- which(rowSums(Data[,-1])!=0)
NID <- which(rowSums(Data[,-1])==0)
if (length(NID)>0) {
  if(NID[1]==1){    Data[1,-1] <- Data[IID[1],-1]
                    if (length(NID)>1) { for (i in 2:length(NID)) { Data[NID[i],-1] <- Data[NID[i]-1,-1] } }
  }else{            for (i in 1:length(NID)) { Data[NID[i],-1] <- Data[NID[i]-1,-1] }
  }
}
##  replace miss data by previous data
##
      Data[,1] <- gsub(" ",replacement="", Data[,1])
      tmp <- matrix(as.double(unlist(strsplit(Data[,1],"/"))),ncol=3,byrow=T)
      StockData$StockData <- Data[order(tmp%*%c(10000,100,1)),]
      StockDatabase[[ eval(code) ]] <- StockData

    }else{ ## if nrow(Data)>0
      idx <- which(is.element(SNAME,code))
      if (length(idx)>0){
        which.type[pmatch(code,which.type[,1]),2] <- PRICE$which.type[pmatch(code,PRICE$which.type[,1]),2]
        StockDatabase[[ eval(code) ]] <- PRICE$StockDatabase[[idx]]
      }
    }

  } ## if (is.element(code,setdiff(SNAME,codes))) {


}) ## try

  } ## codes
  if (!is.null(PRICE)){    TradingDates <- union(PRICE$TradingDates,TradingDates)    }

  which.type <- matrix(which.type[pmatch(names(StockDatabase),which.type[,1]),],ncol=2)
  which.type[which(which.type[,2]==1),2] <- "上市"
  which.type[which(which.type[,2]==2),2] <- "上櫃"

##  print(paste("TradingDates :",length(TradingDates),"months"))
##  print(paste(TradingDates,"",sep=""))

  list( StockDatabase=StockDatabase, TradingDates=TradingDates, which.type=which.type )

}

## 
## d0 <- getStockData(years=c("201401"), codes="1101", F, T)
## d0
## 







##
## source("stock functions.r")
##
## tmpS <- getStockData(years=201408:201409)
## tmpS[[1]][[1]]
##
## tt0 <- system.time(  tmpV0 <- getVolumeData(dates=c(20140801:20140912))      );   tt0
## ttA <- system.time(  tmpVA <- getVolumeData_ALL(dates=c(20140801:20140912))  );  ttA
##
## tmpV0[[1]][[1]][1:20,]
## tmpVA[[1]][[1]][1:20,]
##
##


##
##
## ref : http://www.twse.com.tw/ch/trading/fund/TWT43U/TWT43U.php   
##
## 三大法人買賣超日報(股)：
##
##
##
getVolumeData_ALL <- function( DATA=NULL, dates=c(20140120:20140124), TransformData=T, Sub=T, filename=NULL ) {  ##    print("三大法人買賣超")
    print( Sys.time() )

    Dates <- list()
    for (i in 1:length(dates)) {
        tmp <- strsplit(as.character(dates[i]), "")[[1]]
        if (length(tmp)==8) {    Dates[[i]] <- as.character(dates[i])
        }else{                   Dates[[i]] <- paste(dates[i], c(paste("0",1:9,sep=""),10:31),sep="")        }
    }
    Dates <- sort(unlist(Dates),decreasing=F)
    Now <- strsplit(as.character(Sys.time())," ")[[1]][1]
    Now <- as.double(paste(strsplit(Now,"-")[[1]],sep="",collapse=""))
    Dates <- Dates[which(as.double(Dates)<Now)]
    if (is.null(data)) {    TradingDates <- c()
    }else{                  TradingDates <- DATA$TradingDates
                            Dates <- setdiff(Dates,DATA$TradingDates)        }
    if (is.null(DATA)) {    VolumeData <- list()    }else{    VolumeData <- DATA$Volume    }

    ##
    ## TMP <- readLines( "http://www.twse.com.tw/ch/trading/fund/T86/print.php?edition=ch&filename=genpage/201401/20140123_2by_issue.dat&type=csv&select2=ALL" )
    str1 <- "http://www.twse.com.tw/ch/trading/fund/T86/print.php?edition=ch&filename=genpage/"
    str2 <- "_2by_issue.dat&type=csv&select2=ALL"
    ##
    for (date in Dates) {

        month <- substr(date,1,6)
        url <- paste(str1, month, "/", date, str2, sep = "")
        fileName <- "fileName.txt"
        content <- readLines(url, warn=F)

        if (length(content)>2) {
          print(date)
          TradingDates <- c(TradingDates,date)
          lines <- content[-which(sapply(strsplit(content,"\""),length)==1)]
          writeLines(lines, fileName)
          data <- read.table(fileName, header = F, sep=",")
          colnames(data) <- strsplit(content[2],",")[[1]]
          file.remove(fileName)
          data <- cbind(date,data)
          for (k in 1:ncol(data)){  data[,k] <- as.character(data[,k])  }
          ##
          idx <- which(sapply(strsplit(data[,2],"="),length)==2)
          if (length(idx)>0) {  data[idx,2] <- matrix(unlist(strsplit(data[idx,2],"=")),ncol=2,byrow=TRUE)[,2]  }
          if (Sub) {  data <- data[sapply(strsplit(data[,2],""),length)==4,]  }
          nr <- nrow(data)
          if (TransformData) {
            for (k in c(4:ncol(data))) { 
              IDX <- which(sapply(strsplit(data[,k],","),length)>1)
              for(i in IDX){
                tmp <- strsplit(as.character(data[i,k]),"")[[1]]
                if ( sum(is.element(tmp,","))>0 ) {  tmp <- gsub(",",replacement="",tmp)  }
                data[i,k] <- apply(matrix(tmp,ncol=1),2,paste,collapse="")
              }
            }
            for (k in 4:ncol(data)) { data[,k] <- as.double( data[,k] ) }
          }
          VolumeData[[eval(date)]] <- data
        }
    } ## date
    list( Volume=VolumeData, TradingDates=TradingDates )
}
##
##
## d4 <- getVolumeData_ALL( dates=c(20140611:20140613) )
## d5 <- getVolumeData_ALL( DATA=d4, dates=c(20140609:20140617) )
## 
## 
## names(d4)
## names(d4$V)
## names(d5$V)
## d5[[2]]
## length(d5$V)
## dim(d5$V[[1]])
## d5$V[[1]][1:20,]
##
##







##
##
## Type=1:
## ref : http://www.twse.com.tw/ch/trading/fund/TWT38U/TWT38U.php
## 外資及陸資買賣超彙總表：
##
## Type=2:
## ref : http://www.twse.com.tw/ch/trading/fund/TWT44U/TWT44U.php
## 投信買賣超彙總表：
##
## Type=3:
## ref : http://www.twse.com.tw/ch/trading/fund/TWT43U/TWT43U.php
## 自營商買賣超彙總表(含避險)：
##
##
##

getVolumeData <- function( Type=c(1,2,3), dates=c(20140120:20140124), TransformData=T, KK=100, filename=NULL ) {

    Dates <- list()
    for (i in 1:length(dates)) {
        tmp <- strsplit(as.character(dates[i]), "")[[1]]
        if (length(tmp)==6) {     Dates[[i]] <- paste(dates[i], c(paste("0",1:9,sep=""),10:31),sep="")
        }else{                    Dates[[i]] <- dates[i]        }
    }
    Dates <- sort(unlist(Dates),decreasing=F)
    Now <- strsplit(as.character(Sys.time())," ")[[1]][1]
    Now <- as.double(paste(strsplit(Now,"-")[[1]],sep="",collapse=""))
    Dates <- Dates[which(as.double(Dates)<Now)]
    names <- c("外資及陸資買賣超","投信買賣超","自營商買賣超")


    VolumeList <- list()
    for(type in as.double(Type)){
        print(names[type])
        VolumeData <- list()
        v=0
        ## TMP <- readLines( "http://www.twse.com.tw/ch/trading/fund/TWT38U/TWT38U_print.php?edition=ch&filename=genpage/A20120813.dat&type=csv")
        ## TMP <- readLines( "http://www.twse.com.tw/ch/trading/fund/TWT44U/TWT44U_print.php?edition=ch&filename=genpage/A20120813.dat&type=csv")
        ## TMP <- readLines( "http://www.twse.com.tw/ch/trading/fund/TWT43U/TWT43U_print.php?edition=ch&filename=genpage/A20120813.dat&type=csv")
        if (type==1){          str1 <- "http://www.twse.com.tw/ch/trading/fund/TWT38U/TWT38U_print.php?edition=ch&filename=genpage/A"
        }else{  if (type==2){  str1 <- "http://www.twse.com.tw/ch/trading/fund/TWT44U/TWT44U_print.php?edition=ch&filename=genpage/A"
                }else{         str1 <- "http://www.twse.com.tw/ch/trading/fund/TWT43U/TWT43U_print.php?edition=ch&filename=genpage/A"      }
        }
        str2 <- ".dat&type=csv"
        TradingDates <- c()
        ##
        for (date in Dates) {
            url <- paste(str1, date, str2, sep = "")
            fileName <- "fileName.txt"
            lines <- readLines(url, warn=F)
            if (length(lines)>1) {
              print(date)
              v <- v+1
              TradingDates <- c(TradingDates,date)
              lines <- lines[-which(sapply(strsplit(lines,"\""),length)==1)]
              writeLines(lines, fileName)
              data <- read.table(fileName, header = F, sep=",")
              file.remove(fileName)
              for (k in 1:ncol(data)){  data[,k] <- as.character(data[,k])  }
              if (type==3) {  data <- cbind(date,data)  }else{  data[,1] <- date  }
              ##
              if (TransformData) {
                  idx <- which(sapply(strsplit(data[,2],"="),length)==2)
                  if (length(idx)>0) {  data[idx,2] <- matrix(unlist(strsplit(data[idx,2],"=")),ncol=2,byrow=TRUE)[,2]  }
                  for (k in c(4:ncol(data))) { 
                    IDX <- which(sapply(strsplit(data[,k],","),length)>1)
                    for(i in IDX){
                      tmp <- strsplit(as.character(data[i,k]),"")[[1]]
                      if ( sum(is.element(tmp,","))>0 ) {  tmp <- gsub(",",replacement="",tmp)  }
                      data[i,k] <- apply(matrix(tmp,ncol=1),2,paste,collapse="")
                    }
                    data[,k] <- as.double( data[,k] )
                  }  ## for (k)
              } ## if (TransformData)
              colnames(data) <- c("Date","Stock","Name","BuyIn","SellOut","Diff")
              rownames(data) <- NULL
              VolumeData[[v]] <- data
            }  ## if (length(lines)>1)
        } ## date
        VolumeList[[ eval(names[type]) ]] <- VolumeData
    } ## for(t in 1:length(Type))

    VolumeMatrix <- list()
    if (length(Type)==3){
        for (k in 1:length(VolumeList[[1]])){
          VolumeMatrix[[k]] <- data.frame(matrix(NA,KK*2,4*3))
          for (j in 1:2){
            tmp <- TMP <- idx <- list()
            if(j==1){  for(i in 1:3){  tmp[[i]] <- VolumeList[[i]][[k]][VolumeList[[i]][[k]]$Diff>0,3]  }
            }else{     for(i in 1:3){  tmp[[i]] <- VolumeList[[i]][[k]][VolumeList[[i]][[k]]$Diff<0,3]  }    }
            tmp[[4]] <- intersect(tmp[[1]],intersect(tmp[[2]],tmp[[3]]))
            for (i in 1:4){  idx[[i]] <- 1:min(length(tmp[[i]]),KK)  }
            for (i in 1:3){  VolumeMatrix[[k]][idx[[i]]+ifelse(j==1,0,KK),1:3+(i-1)*3] <- VolumeList[[i]][[k]][is.element(VolumeList[[i]][[k]]$Name,tmp[[i]]),][idx[[i]],c(2,3,6)]
                             TMP[[i]] <- VolumeList[[i]][[k]][is.element(VolumeList[[i]][[k]]$Name,tmp[[4]]),][idx[[4]],c(2,3,6)]
                             TMP[[i]] <- TMP[[i]][order(TMP[[i]]$Name),]      }

            TMP[[4]] <- cbind(TMP[[1]][,1:2],"Diff"=rowSums(cbind(TMP[[1]]$Diff,TMP[[2]]$Diff,TMP[[3]]$Diff)))
            TMP[[4]] <- TMP[[4]][order(abs(TMP[[4]]$Diff),decreasing=T),]
            VolumeMatrix[[k]][idx[[4]]+ifelse(j==1,0,KK),10:12] <- as.matrix(TMP[[4]])
          } ## for j
          colnames( VolumeMatrix[[k]] ) <- c("外陸資.code","外陸資.name","外陸資.diff","投信.code","投信.name","投信.diff","自營商.code","自營商.name","自營商.diff","交集.code","交集.name","交集.diff")
          VolumeMatrix[[k]] <- cbind("Date"=TradingDates[k],VolumeMatrix[[k]])
        } ## for k
    } ## if (length(Type)==3)
##VolumeMatrix[[1]]

    VolumeList[[ "VolumeMatrix" ]] <- VolumeMatrix
    VolumeList[[ "交易日期" ]] <- TradingDates
    print( names(VolumeList) )
    VolumeList
}


## 
## d1 <- getVolumeData( dates=c(201406) )
##
## 
## names(d1)  ## d1 <- getVolumeData( dates=c(201406) )
## length(d1[[4]])
## dim(d1[[4]][[1]])
## d1[[4]][[1]][1:20,]
##
##











##
##
## http://www.twse.com.tw/ch/trading/exchange/FMTQIK/FMTQIK2.php?STK_NO=&myear=2014&mmon=02&type=csv
##
## 市場日成交資訊:
##
##
## tmpM <- getMarketData()
## tmpM
##


getMarketData <- function( dates=c(201401:201402), TransformData = T, filename=NULL ) {
    print("市場日成交資訊")
    VolumeData <- list()
    v=0
    DATES <- list()
    for (i in 1:length(dates)) {
      if (length(strsplit(as.character(dates[i]),"")[[1]])==4) {
        DATES[[i]] <- paste(dates[i],c(paste("0",1:9,sep=""),10:12),sep="")
      }else{
        DATES[[i]] <- dates[i]
      }
    }
    Dates <- as.character(sort(unlist(DATES)))
    Now <- Sys.time()
    Now <- as.double(paste(substr(Now,1,4),substr(Now,6,7),sep=""))
    Dates <- Dates[which(as.double(Dates)<=Now)]
    MD <- matrix(0,0,6)
    TradingDates <- c()
    ##
    ## TMP <- readLines( "http://www.twse.com.tw/ch/trading/exchange/FMTQIK/FMTQIK2.php?STK_NO=&myear=2014&mmon=02&type=csv" )
    str1 <- "http://www.twse.com.tw/ch/trading/exchange/FMTQIK/FMTQIK2.php?STK_NO=&myear="
    str2 <- "&mmon="
    str3 <- "&type=csv"
    ##
    for (date in Dates) {
        url <- paste(str1, substr(date,1,4), str2, substr(date,5,6), str3, sep = "")
        fileName <- "fileName.txt"
        content <- readLines(url, warn=F)

        if (length(content)>2) {
          TradingDates <- c(TradingDates,date)
          v <- v+1
          lines <- content[-which(sapply(strsplit(content,"\""),length)<3)]
          writeLines(lines, fileName)
          data <- read.table(fileName, header = F, sep=",")
          colnames(data) <- strsplit(content[2],",")[[1]]
          file.remove(fileName)
          for (k in 1:ncol(data)){  data[,k] <- as.character(data[,k])  }
          ##
          if (TransformData) {
            for (k in c(2:ncol(data))) { 
              IDX <- which(sapply(strsplit(data[,k],","),length)>1)
              for(i in IDX){
                tmp <- strsplit(as.character(data[i,k]),"")[[1]]
                if ( sum(is.element(tmp,","))>0 ) {  tmp <- gsub(",",replacement="",tmp)  }
                data[i,k] <- apply(matrix(tmp,ncol=1),2,paste,collapse="")
              }
            }
            for (k in 2:ncol(data)) { data[,k] <- as.double( data[,k] ) }
          }
          MD <- rbind(MD,data)
          VolumeData[[v]] <- data
        }
    } ## date
    list( Volume=VolumeData, MD=MD, TradingDates=TradingDates )
}
## 
## 
## tmp <- getMarketData( dates=c(201401:201406), TransformData = T, filename=NULL )
## tmp$V
## tmp$MD
## 













##
##  Two functions to summarize volumn overbought or oversold :
##
Volume_matrix <- function( vdata, idx ){
    if(idx=="Buy"){  stocks <- vdata$CB  }else{  stocks <- vdata$CS  }
    volumn <- matrix(0,length(stocks),length(vdata$Volume))
    rownames(volumn) <- stocks
    colnames(volumn) <- vdata$TradingDates
    nc <- ncol(vdata$Volume[[1]])
    for(k in 1:ncol(volumn)){
      for(j in 1:nrow(volumn)){
        idx <- which(is.element(vdata$Volume[[k]][,2],stocks[j]))
        if(length(idx)>1){  print(c(k,j,stocks[j],c(as.matrix(vdata$Volume[[k]][idx,]))))  }
        if(length(idx)>0){  volumn[j,k] <- vdata$Volume[[k]][idx[1],nc]  }
      }
    }
    volumn <- volumn[order(abs(rowSums(volumn)),decreasing=TRUE),]
    list(volumn=volumn,count=rowSums(volumn>0))
}
##
##
Volume_Summary <- function( cdata ){
    VM1 <- Volume_matrix(cdata,"Buy")
    VM2 <- Volume_matrix(cdata,"Sell")
    volumn <- rbind(VM1$volumn,VM2$volumn )
    volumn <- volumn[order(rowSums(volumn>0),decreasing=TRUE),]
    count <- rowSums(volumn>0)
    volumn <- cbind(volumn,"rowsum"=rowSums(volumn),count)
}
##
##
Volume_sum <- function( data, stock=c("1101","2002"), names=NULL ){
    cdata <- data[[1]]
    volumn <- array(0, dim=c(length(cdata$V), length(stock),length(data)) )
    rown <- c()
    for(j in 1:length(cdata$V)){  rown <- c(rown,cdata$V[[j]][1,1])  }
    colnames(volumn) <- stock
    rownames(volumn) <- rown
    for(i in 1:length(data)){
      cdata <- data[[i]]
      for(j in 1:length(cdata$V)){
        for(k in 1:length(stock)){
          idx <- which(is.element(cdata$V[[j]][,2],stock[k]))
          if (length(idx)>0){ volumn[j,k,i] <- cdata$V[[j]][idx[1],6] }
        }
      }
    }
    if (is.null(names)){ names <- 1:length(data) }
    dimnames(volumn) <- list(rown,stock,names)
    volumn
}
##
##
##












##
## ref : http://www.twse.com.tw/ch/trading/fund/TWT43U/TWT43U.php
##
##
## 三大法人買賣金額統計表：
##
## tmpA <- getAmountData()
## tmpA
##

getAmountData <- function( dates=c(20140120:20140124), TransformData = T, filename=NULL ) { 
    print("三大法人買賣金額")
    VolumeData <- list()
    v=0
    Dates <- list()
    for (i in 1:length(dates)) {
        tmp <- strsplit(as.character(dates[i]), "")[[1]]
        if (length(tmp)==4) {     Dates[[i]] <- paste(dates[i], c(paste("0",1:9,sep=""),10:12),sep="")
        }else{                    Dates[[i]] <- dates[i]        }
    }
    dates <- sort(unlist(Dates),decreasing=TRUE)
    Dates <- list()
    for (i in 1:length(dates)) {
        tmp <- strsplit(as.character(dates[i]), "")[[1]]
        if (length(tmp)==8) {    Dates[[i]] <- dates[i]
        }else{                   Dates[[i]] <- paste(dates[i], c(paste("0",1:9,sep=""),10:31),sep="")         }
    }
    Dates <- sort(unlist(Dates),decreasing=TRUE)
    Now <- strsplit(as.character(Sys.time())," ")[[1]][1]
    Now <- as.double(paste(strsplit(Now,"-")[[1]],sep="",collapse=""))
    Dates <- Dates[which(as.double(Dates)<Now)]
    TradingDates <- c()
    ##
    ## TMP <- readLines( "http://www.twse.com.tw/ch/trading/fund/BFI82U/BFI82U_print.php?begin_date=20140206&end_date=20140205&report_type=day&language=ch&save=csv")
    str1 <- "http://www.twse.com.tw/ch/trading/fund/BFI82U/BFI82U_print.php?begin_date="
    str2 <- "&end_date="
    str3 <- "&report_type=day&language=ch&save=csv"
    ##
    for (date in Dates) {
        url <- paste(str1, date, str2, date, str3, sep = "")
        fileName <- "fileName.txt"
        contents <- readLines(url, warn=F)
        if (length(contents)>1) {
          TradingDates <- c(TradingDates,date)
          v <- v+1
          lines <- contents[-(1:2)]
          lines <- lines[which(sapply(strsplit(lines,"\""),length)>1)]
          writeLines(lines, fileName)
          data <- read.table(fileName, header = F, sep=",")
          file.remove(fileName)
          data <- cbind(date,data)
          for (k in 1:ncol(data)){  data[,k] <- as.character(data[,k])  }
          if (TransformData) {
            for (k in c(3:5)) {
              IDX <- which(sapply(strsplit(data[,k],","),length)>1)
              for(i in IDX){
                tmp <- strsplit(as.character(data[i,k]),"")[[1]]
                if ( sum(is.element(tmp,","))>0 ) {  tmp <- gsub(",",replacement="",tmp)  }
                data[i,k] <- apply(matrix(tmp,ncol=1),2,paste,collapse="")
              } # for i
              data[,k] <- as.double( data[,k] )
            } # for k
          }
          colnames(data) <- c("交易日期",strsplit(contents[2],",")[[1]])
          VolumeData[[v]] <- data
       } # if(length(lines)>1)

    } ## date
    VM <- array(0,dim=c(length(TradingDates),4,3))
    for(k in 1:length(TradingDates)){ VM[k,,] <- matrix(as.double(as.matrix(VolumeData[[k]][,-(1:2)])),nrow(VolumeData[[1]])) }
    dimnames(VM) <- list( TradingDates, VolumeData[[1]][,2], names(VolumeData[[1]][,-(1:2)]) )
    print(paste("TradingDates :",length(TradingDates),"days"))
    if(!is.null(filename)){ save(VolumeData,file=filename) }
    list( Volume=VolumeData, VM=VM, TradingDates=TradingDates )
}
##
## 
## d5 <- getAmountData( dates=c(20140610:20140624) )
## 
## d5
##





##
## ref : "http://www.twse.com.tw/ch/trading/exchange/BFIAMU/BFIAMU2.php?input_date=103/02/06&type=csv"
##
## 各類指數日成交量值：
##
##
## tmpT <- getTypesData()
## tmpT
##

getTypesData <- function( dates=c(20140120:20140124), TransformData = T, filename=NULL ) { 
    print("各類指數日成交量值")
    VolumeData <- list()
    v=0
    Dates <- list()
    for (i in 1:length(dates)) {
        tmp <- strsplit(as.character(dates[i]), "")[[1]]
        if (length(tmp)==8) {    Dates[[i]] <- dates[i]
        }else{                   Dates[[i]] <- paste(dates[i], c(paste("0",1:9,sep=""),10:31),sep="")         }
    }
    Dates <- sort(unlist(Dates),decreasing=TRUE)
    Now <- strsplit(as.character(Sys.time())," ")[[1]][1]
    Now <- as.double(paste(strsplit(Now,"-")[[1]],sep="",collapse=""))
    Dates <- Dates[which(as.double(Dates)<Now)]
    TradingDates <- c()
    ##
    ## TMP <- readLines( "http://www.twse.com.tw/ch/trading/exchange/BFIAMU/BFIAMU2.php?input_date=103/02/06&type=csv")
    str1 <- "http://www.twse.com.tw/ch/trading/exchange/BFIAMU/BFIAMU2.php?input_date="
    str2 <- "&type=csv"
    ##
    for (date in Dates) {
        DATE <- paste(c(as.double(substr(date,1,4))-1911,"/",substr(date,5,6),"/",substr(date,7,8)),sep="",collapse="")
        url <- paste(str1, DATE, str2, sep = "")
        fileName <- "fileName.txt"
        contents <- readLines(url, warn=F)
        if (length(contents)>1) {
          TradingDates <- c(TradingDates,date)
          v <- v+1
          lines <- contents[-(1:2)]
          lines <- lines[which(sapply(strsplit(lines,"\""),length)>1)]
          writeLines(lines, fileName)
          data <- read.table(fileName, header = F, sep=",")
          file.remove(fileName)
          data <- cbind(date,data)
          for (k in 1:ncol(data)){  data[,k] <- as.character(data[,k])  }
          if (TransformData) {
            for (k in c(3:5)) {
              IDX <- which(sapply(strsplit(data[,k],","),length)>1)
              for(i in IDX){
                tmp <- strsplit(as.character(data[i,k]),"")[[1]]
                if ( sum(is.element(tmp,","))>0 ) {  tmp <- gsub(",",replacement="",tmp)  }
                data[i,k] <- apply(matrix(tmp,ncol=1),2,paste,collapse="")
              } # for i
              data[,k] <- as.double( data[,k] )
            } # for k
          }
          colnames(data) <- c("交易日期",strsplit(contents[2],",")[[1]])
          VolumeData[[v]] <- data
       } # if(length(lines)>1)

    } ## date
    VM <- array(0,dim=c(length(TradingDates),nrow(VolumeData[[1]]),ncol(VolumeData[[1]])-2))
    for(k in 1:length(TradingDates)){ VM[k,,] <- matrix(as.double(as.matrix(VolumeData[[k]][,-(1:2)])),nrow(VolumeData[[1]])) }
    dimnames(VM) <- list( TradingDates, VolumeData[[1]][,2], names(VolumeData[[1]][,-(1:2)]) )
    print(paste("TradingDates :",length(TradingDates),"days"))
    if(!is.null(filename)){ save(VolumeData,file=filename) }
    list( Volume=VolumeData, VM=VM, TradingDates=TradingDates )
}
##
## 
## d6 <- getTypesData( dates=c(20140610:20140624) )
## 
## d6$Volume[[1]]
##











##
## ref : "http://www.twse.com.tw/ch/trading/exchange/BWIBBU/BWIBBU_d.php?input_date=103/01/02"
##
## 個股日本益比、殖利率及股價淨值比：
##
##
##
getRatioData <- function( dates=NULL, filename=NULL ) { 
   ##  dates=c(20140615:20140616)
    print("個股日本益比、殖利率及股價淨值比")
    VolumeData <- list()
    v=0
    if (is.null(dates)) {
      cdate <- Sys.Date()
      ctime <- date()
      if (substr(ctime,12,13)<20) {
        if (substr(ctime,1,3)=="Mon"){
          dd <- as.double(substr(cdate,9,10))-3
          dates <- paste(substr(cdate,1,4),substr(cdate,6,7),paste(ifelse(dd<10,"0",""),dd,sep=""),sep="")
        }else{
          dd <- as.double(substr(cdate,9,10))-1
          dates <- paste(substr(cdate,1,4),substr(cdate,6,7),paste(ifelse(dd<10,"0",""),dd,sep=""),sep="")
        }
      }else{
        dates <- paste(substr(cdate,1,4),substr(cdate,6,7),substr(cdate,9,10),sep="")
      }
    }
    Dates <- list()
    for (i in 1:length(dates)) {
        tmp <- strsplit(as.character(dates[i]), "")[[1]]
        if (length(tmp)==8) {    Dates[[i]] <- dates[i]
        }else{                   Dates[[i]] <- paste(dates[i], c(paste("0",1:9,sep=""),10:31),sep="")         }
    }
    Dates <- sort(unlist(Dates),decreasing=TRUE)

    Now <- as.character(Sys.Date())
    Now <- as.double(paste(strsplit(Now,"-")[[1]],sep="",collapse=""))
    if (as.double(substr(date(),12,13))>20) { Now <- Now+1 }
    Dates <- as.character(Dates[which(as.double(Dates)<Now)])
    TradingDates <- c()
    str1 <- "http://www.twse.com.tw/ch/trading/exchange/BWIBBU/BWIBBU_d.php?input_date="
    ##

    for (date in Dates) {

        DATE <- paste(c(as.double(substr(date,1,4))-1911,"/",substr(date,5,6),"/",substr(date,7,8)),sep="",collapse="")
        url <- paste(str1, DATE, sep = "")
        fileName <- "fileName.txt"
        contents <- readLines(url, warn=F)
        tmp1 <- strsplit(contents,"依證券代碼排序查詢")
        idx <- which(sapply(tmp1,length)==2)[1]

        if (length(which(sapply(tmp1,length)==2))>0) {

          tmp2 <- unlist(strsplit(tmp1[[idx]][1],"<div align=center class=til_2>"))[2]
          current.date <- paste(as.double(substr(tmp2,1,3))+1911,substr(tmp2,5,6),substr(tmp2,9,10),sep="")

          if (current.date==date) {
            TradingDates <- c(TradingDates,date)
            v <- v+1

            tmp1 <- strsplit(contents,"證券名稱")
            idx <- which(sapply(tmp1,length)==2)[1]
            tmp2 <- unlist(strsplit(tmp1[[idx]][2],"<td height=20 class=basic2 align=center>"))[-1]
            tmp3 <- unlist(strsplit(tmp2,"</td><td height=20 class=basic2 align=right>"))
            tmp4 <- unlist(strsplit(unlist(strsplit(tmp3,"</td></tr><tr bgcolor=#FFFFFF>")),"</td>"))
            tmp4[length(tmp4)-9:0]
            tmp5 <- tmp4[-length(tmp4)]
            tmp6 <- matrix(tmp5,ncol=5,byrow=T)
            tmp6[tmp6=="-"] <- NA

            for(k in 3:5){ tmp6[,k] <- as.double(tmp6[,k]) }
            data <- data.frame(cbind(date,tmp6))
            names(data) <- c("交易日期","證券代號","證券名稱","本益比","殖利率(%)","股價淨值比")
            if( length(Dates)==1 ){    }
            VolumeData[[v]] <- data
         } # if(length(lines)>1)
       }else{
          print("Please enter a valid date.")
       }

    } ## date
    VM <- array(0,dim=c(length(TradingDates),nrow(VolumeData[[1]]),ncol(VolumeData[[1]])-3))
    for(k in 1:length(TradingDates)){ VM[k,,] <- matrix(as.double(as.matrix(VolumeData[[k]][,-(1:3)])),nrow(VolumeData[[1]])) }
    dimnames(VM) <- list( TradingDates, VolumeData[[1]][,2], names(VolumeData[[1]][,-(1:3)]) )
    print(paste("TradingDates :",length(TradingDates),"days"))
    if(!is.null(filename)){ save(VolumeData,file=filename) }
    list( Volume=VolumeData, VM=VM, TradingDates=TradingDates )
}
##
## 
## d7 <- getRatioData( dates=NULL )
##
## d7$Volume[[1]][1:20,]
##
## d7$Volume[[1]]
##








## http://www.twse.com.tw/ch/trading/fund/MI_QFIIS/MI_QFIIS.php

ShareData <- function(Date="20140623",Sub=TRUE,code=NULL){
## tmp <- readLines("C:/Users/wshsieh/Desktop/Mis/Stock/R scripts/MI_QFIIS-20120102.csv")
    library(stringr)
    tmp <- readLines(paste(getwd(),"/SharesData/MI_QFIIS-",Date,".csv",sep=""))
    tmpData <- tmp[6:(length(tmp)-6)]
    idx1 <- which(sapply(strsplit(tmpData,"="),length)==2)
    idx2 <- which(sapply(strsplit(tmpData,"="),length)==1)
    tmp1 <- gsub(" ","",c(paste(sapply(idx1,function(x){strsplit(tmpData[idx1],"\"")[[x]][2]}),
    sapply(idx1,function(k){substr(tmpData[idx1],10,str_length(tmpData[idx1]))[k]}),sep=""),tmpData[idx2]))
    tmp2 <- matrix(unlist(strsplit(tmp1,"\"")),ncol=length(strsplit(tmp1[1],"\"")[[1]]),byrow=T)[,-seq(3,13,2)]
    tmp3 <- data.frame(cbind(matrix(unlist(strsplit(tmp2[,1],",")),ncol=3,byrow=T),
                             tmp2[,2:8],matrix(unlist(strsplit(tmp2[,9],",")),ncol=3,byrow=T)[,3]))
    colnames(tmp3) <- c("證券代號","證券名稱","國際證券編碼","發行股數",
        "外陸資尚可投資股數","全體外陸資持有股數","外陸資尚可投資比率","全體外陸資持股比率",
        "外陸資共用法令投資上限比率","陸資法令投資上限比率","上次申報異動日期")
    if (Sub) {    tmp3 <- tmp3[which(sapply(strsplit(as.character(tmp3[,1]),""),length)==4),]    }
    tmp4 <- tmp3
    for(k in c(1:3,11)){  tmp4[,k] <- as.character(tmp3[,k])  }
    for(k in 4:10){  tmp4[,k] <- as.double(gsub(",","",as.character(tmp3[,k])))  }

    idx <- pmatch(code,tmp4[,1])
    if (length(idx)>0) {  tmp4 <- tmp4[idx,]  }

    list(M1=tmp4,M2=tmp3)
}
##
## SD2012 <- ShareData(Date="20120102")
## SD2014 <- ShareData(Date="20140102")
## SD2014 <- ShareData(Date="20140617")
## SD2012$M1[1:10,]
## SD2014$M1[1:10,]
## SD2014$M1[1:20,-c(3,10:11)]
## SD2014$M1[order(SD2014$M1[,8]/SD2014$M1[,9],decreasing=T),-c(3,10:11)]  ## 外陸資佔其可持股比例高低排序
##
##








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






## library(cairo)
MakePlots_Old <- function ( wData=S1, stocks=NULL, nr=248, vlines=c(0), dates=NULL, bds=c(-10), WDH=20, HGT=9, 
                        pdfname="stock_trend_plots_2006_now.pdf", SINGLE=FALSE, show.plot=T, dim=c(1,1) ) {
  names <- c()
  wData <- wData$StockDatabase
  for(k in 1:length(wData)){   names <- c(names,strsplit(wData[[k]][[1]]," ")[[1]][1])  }
  if (is.null(stocks)) {  stocks <- names  }
  if (!is.null(pdfname) & show.plot) {  pdf(pdfname,width=WDH,height=HGT) }
##if (!is.null(pdfname) & show.plot) {  cairo_pdf(pdfname,width=WDH,height=HGT) }
  def.par <- par(no.readonly = TRUE) # save default, for resetting...
  if (!(dim[1]==1 & dim[2]==1)){  par(mfcol=c(dim[1],dim[2]))  }

  idxset <- pmatch(stocks,names)
##  getName( stock=stocks )

  for (k in idxset) {

    if (length(wData[[k]])>0 & nrow(wData[[k]]$S)>1) {

      bdata <- wData[[k]][[2]]
      wdata <- mdata <- qdata <- bdata
      if (nrow(bdata)> 4) {
        for(dd in nrow(bdata): 5){ wdata[dd,4:7] <- colMeans(bdata[dd:(dd- 4),4:7]) }
        for(dd in 4:7){  wdata[1: 4,dd] <- wdata[ 5,dd]  }   }
      if (nrow(bdata)>20) {
        for(dd in nrow(bdata):21){ mdata[dd,4:7] <- colMeans(bdata[dd:(dd-20),4:7]) }
        for(dd in 4:7){  mdata[1:20,dd] <- mdata[21,dd]  }   }
      if (nrow(bdata)>62) {
        for(dd in nrow(bdata):63){ qdata[dd,4:7] <- colMeans(bdata[dd:(dd-62),4:7]) }
        for(dd in 4:7){  qdata[1:62,dd] <- qdata[63,dd]  }   }
      nr <- min(nr,nrow(bdata)-1)
      idx <- nrow(bdata)-(nr:1)+1
      cdata <- bdata[idx,]
      int4 <- min(nr-1,256);   coefY  <- round(lm( cdata[(nr-int4):nr,7]~(x<-0:int4))$coef,2)
      if (nr>  4) {   int1 <-   5-1;  coefw  <- round(lm( cdata[(nr-int1):nr,7]~(x<-0:int1))$coef,2)  }else{  int1 <- int4;  coefw <- coefY  }
      if (nr> 44) {   int2 <-  45-1;  coefm  <- round(lm( cdata[(nr-int2):nr,7]~(x<-0:int2))$coef,2)  }else{  int2 <- int4;  coefm <- coefY  }
      if (nr>124) {   int3 <- 125-1;  coefy  <- round(lm( cdata[(nr-int3):nr,7]~(x<-0:int3))$coef,2)  }else{  int3 <- int4;  coefy <- coefY  }
      ##
      VLINES <- as.double(vlines[1:(length(vlines)-as.double(vlines[length(vlines)])-1)])
      ##
      if (!SINGLE) {
        matplot(cbind(bdata[,4:7],mdata[,4:7],qdata[,4:7]),type='l',lty=c(rep(1,4),rep(2,4),rep(3,4)),col=c(5,6,8,1),lwd=c(2,1,1,2),main=paste(names[k],"; closing price",cdata[nr,7]),xaxt="n" )
        openday <- c(which(is.element(cdata[,1],c(" 101/01/30"," 102/02/18",dates))),nrow(bdata)-VLINES+1)
        openday <- openday[openday>0]
        my.at <- c(0:2*round(nrow(bdata)/3)+1,nrow(cdata)-(nr-1),nrow(cdata),openday)
        axis(1, at = my.at, labels = cdata[,1][my.at],las=2 )
        lines( (nrow(bdata)-rep(1,2)*int4),range(cdata[,4:7]),col=2,lwd=2)
        lines( (nrow(bdata)-rep(0,2)*int4),range(cdata[,4:7]),col=2,lwd=2)
        lines( (nrow(bdata)-c(int4,0)),range(cdata[,4:7])[1]%x%c(1,1),col=2,lwd=2)
        lines( (nrow(bdata)-c(int4,0)),range(cdata[,4:7])[2]%x%c(1,1),col=2,lwd=2)
        if(length(openday)>0) {  abline(v=c(openday),col=4)  }
        abline(h=bds,col=8)
      }
      ##
      matplot(cbind(cdata[,4:7],mdata[idx,4:7],qdata[idx,4:7]),type='l',lty=c(rep(1,4),rep(2,4),rep(3,4)),col=c(5,6,8,1),lwd=c(2,1,1,2),main=paste(names[k],"; closing price",cdata[nr,7]),xaxt="n" )
      openday <- c(which(is.element(cdata[,1],c("101/01/30","102/02/18",dates))),nr-VLINES+1)
      openday <- openday[openday>0]
      legend( ifelse(length(openday)==0,10,max(openday)+10), max(cdata[,7]), c("opening","highest","lowest","closing"),lty=1,col=c(5,6,8,1),lwd=c(2,1,1,2))
      mtext( paste("last day :",round(cdata[nr,7]-cdata[nr-1,7],2),"; last week :",coefw[2],"; last (2,6,12) months : (",coefm[2],",",coefy[2],",",coefY[2],")" ))
      my.at <- union(c(nr-int4,nr-int3,nr-int2,nr-int1,nr,openday),NULL)
      axis(1, at = my.at, labels = cdata[,1][my.at],las=2 )
      abline(v=c(my.at,nr-int1),col=2)
      if(length(openday)>0) {  abline(v=c(openday),col=4)  }
      sc1 <- 2;  sc2 <- 2;  sc3 <- 2;  sc4 <- 2;
      lines( (nr-int1)+c(0-sc1,int1+sc1),coefw[2]*c(0-sc1,int1+sc1)+coefw[1], col=c(2,3)[ifelse(coefw[2]<0,2,1)], lwd=2 )
      lines( (nr-int2)+c(0-sc2,int2+sc2),coefm[2]*c(0-sc2,int2+sc2)+coefm[1], col=c(2,3)[ifelse(coefm[2]<0,2,1)], lwd=2 )
      lines( (nr-int3)+c(0-sc3,int3+sc3),coefy[2]*c(0-sc3,int3+sc3)+coefy[1], col=c(2,3)[ifelse(coefy[2]<0,2,1)], lwd=2 )
      lines( (nr-int4)+c(0-sc4,int4+sc4),coefY[2]*c(0-sc4,int4+sc4)+coefY[1], col=c(2,3)[ifelse(coefY[2]<0,2,1)], lwd=2 )
      abline(h=bds,col=8)
    } ## if length(wData[[k]])>0
  } ## for k
  if (!is.null(pdfname) & show.plot) {  dev.off()  }
  par(def.par)  #- reset to default
}
##
##




##
##  S1 <- getStockData( PRICE=NULL, years=c(201401:201409), codes=c("4746","2498","2845","8083"))
##

MakePlots <- function ( wData=S1, stocks=NULL, dates=c("103/05/05","103/08/06"), Range=NULL, Xpos=NULL, ylines=0:0,
                        pdfname="stock_trend_plots_2006_now.pdf", WDH=20, HGT=9, show.plot=T, dim=c(1,1) ) {

  names <- c()
  wdata <- wData$StockDatabase
  for(k in 1:length(wdata)){   names <- c(names,strsplit(wdata[[k]][[1]]," ")[[1]][1])  }
  if (is.null(stocks)) {  stocks <- names  }
  if (!is.null(pdfname) & show.plot) {  pdf(pdfname,width=WDH,height=HGT) }
  def.par <- par(no.readonly = TRUE) # save default, for resetting...
  if (!(dim[1]==1 & dim[2]==1)){  par(mfcol=c(dim[1],dim[2]))  }
  idxset <- na.omit(pmatch(stocks,names))

  COLORS = c('darkred','darkgreen')
  par( "bg"='gray10' )

  for (k in idxset) {

        cdata <- wdata[[k]][[2]][,c(1,4:7)]
        if(!is.null(Range)){
          if(length(Range)==1){
            cdata <- cdata[which(!(cdata[,1]<Range[1])),]
          }else{
            cdata <- cdata[which((!(cdata[,1]<Range[1]))&!(cdata[,1]>Range[2])),]
          }
        }
        names(cdata)[1] <- 'date'
        candle <- ggChartSeries( cdata, col=COLORS )
        plot(-10,xlim=c(1,nrow(cdata)+2),ylim=range(candle[,2:5]),xlab="",ylab="", xaxt='n', yaxt='n', main=paste("Price and volume of",names(wdata)[k]), col.main='olivedrab' )
        box(col='lightskyblue')
##      my.at <- c( seq(min(candle[,2:5]),max(candle[,2:5]),l=4), candle$close[length(candle$close)], ylines)
        my.at <- c( seq(min(candle[,2:5]),max(candle[,2:5]),l=4), ylines)  ##, candle$close[length(candle$close)]
        axis(2, at = my.at, labels = rep("",length(my.at)), col='lightskyblue'  )
        dd <- (my.at[2]-my.at[1])/5
        if( is.null(Xpos) ){ Xpos=-(nrow(candle)/15) }
        text( cex=.8, x=Xpos, my.at+dd, labels=round(my.at,2), srt=90, adj=1.2, xpd=TRUE, col='lightskyblue' )
        y0 <- my.at[1]-dd

        idx <- which(is.element(cdata$date,dates))
        my.at <- c( NULL, round(seq(1,nrow(cdata),l=11)) )
        axis(1, at = my.at, labels = rep("",length(my.at)), las=3, col='lightskyblue' ) 
        text( cex=.8, x=my.at,  y=y0, labels=cdata$date[my.at], srt=90, adj=1.2, xpd=TRUE, col='lightskyblue' )
        text( cex=.8, x=c(idx,nrow(cdata)),    y=y0, labels=cdata$date[c(idx,nrow(cdata))],   srt=90, adj=1.2, xpd=TRUE, col='lightgreen' )
        text( cex=.8, x=c(idx,nrow(cdata))+nrow(candle)/50, y=max(candle$high)+dd/3, labels=candle$close[c(idx,nrow(cdata))],col='lightgreen')

        xdata <- 1:nrow(candle)
        segments(xdata,candle$low,xdata,candle$high,col='grey25' )
        rect(xdata-.4,candle$candleLower,xdata+.4,candle$candleUpper,col=candle$fill )
        lines( xdata, candle$ma1W, col='orange')
        lines( xdata, candle$ma1M, col='purple')
        lines( xdata, candle$ma1Q, col='pink')
        abline( v=my.at, col='gray' )
        abline( h=c(max(candle$high),min(candle$low),candle[nrow(candle),"close"],ylines),col=c('darkred','darkgreen','darkblue'))
        if( length(idx)>0 ){  abline( v=idx, col='lightgreen' )  }
        points( c(which.max(candle$high),which.min(candle$low),
                nrow(candle)),c(max(candle$high),min(candle$low),candle[nrow(candle),"close"]),col='yellow',cex=1,lwd=2)

        legend( "topleft", lty=c(1,1,1,0), lwd=rep(1,3), col=c('orange','purple','pink'), text.col='lightskyblue', box.col='lightskyblue',
          legend=c(paste("Moving",c("WA","MA","QA")),paste("closing=",candle$close[length(candle$close)],sep=""))
        )
  } ## for k
  if (!is.null(pdfname) & show.plot) {  dev.off()  }
  par(def.par)  #- reset to default
}
##
##






##
##  S1 <- getStockData( PRICE=NULL, years=c(201401:201409), codes=c("4746","2498","2845","8083"))
##

info_fcn <- function(data=S1){
  for(k in 1:length(data[[1]])){  print(data[[1]][[k]][[1]])  }
}
##
##










if (FALSE) {

cat(" 個股日成交資訊：        getStockData( years=c(2013,2014), codes, TransformData = T )", "\n",
##   "外資及陸資買賣超彙總表： getVolumeData( Type=c(1), dates=c(201401), TransformData = T )", "\n",
##   "投信買賣超彙總表：      getVolumeData( Type=c(2), dates=c(201401), TransformData = T )", "\n",
##   "自營商買賣超彙總表：    getVolumeData( Type=c(3), dates=c(201401), TransformData = T )", "\n",
     "三大法人買賣超日報：    getVolumeData_ALL( dates=c(201401), TransformData = T )", "\n",
     "市場日成交資訊:        getMarketData( dates=c(201401:201402), TransformData = T, filename=NULL )", "\n",
     "三大法人買賣金額統計表： getAmountData( dates=c(20140120:20140124), TransformData = T, filename=NULL )", "\n",
     "各類指數日成交量值:     getTypesData( dates=c(20140120:20140124), TransformData = T, filename=NULL )", "\n",
     "個股日本益比、殖利率及股價淨值比: getRatioData( dates=c(20140120:20140124), filename=NULL )", "\n",
     "三大法人持股資訊:      ShareData <- function(Date=20140623,Sub=TRUE,code=NULL)", "\n",
     "畫圖:        MakePlots( wData=d5, stocks=NULL, nr=248, pdfname='stock_trend_plots_2006_now.pdf', WDH=20, HGT=9 )", "\n"
 )

}


















