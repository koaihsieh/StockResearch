


cost <- function(price=100,diff=1,disc=.4){
  comm=(1.425/1000)*disc
  tax=3/1000
  cost.buy  <- pmax( price*comm, 20/1000)
  cost.sell <- pmax((price+diff)*(comm+tax), 20/1000)
  cost <- price + cost.buy + cost.sell
  cost <- price + ( price*comm + (price+diff)*(comm+tax) )
  return <- diff - (cost - price)

  data.frame(price=price,diff=diff,cost=cost,return=return,earn=return*1000)
}
cost(price=c(100,70,30,15,10))







#########################################################################################################
#########################################################################################################
###                                                                                                   ###
###                                                                                                   ###
###                                                                                                   ###
###                                                                                                   ###
#########################################################################################################
#########################################################################################################



load("VolumeDataFiles_201408.RData")


today <- matrix(0,length(price[[1]]),ncol(price[[1]][[k]][[2]])+1)
for( k in 1:nrow(today)){
  today[k,] <- c( substr(price[[1]][[k]][[1]],1,4), unlist(price[[1]][[k]][[2]][nrow(price[[1]][[k]][[2]]),]) )
}
today <- data.frame(today)
names(today) <- c("布",names(price[[1]][[k]][[2]]))
today[1:10,]


today[as.double(as.character(today$"Μ絃基"))<10,]
today[as.double(as.character(today$"Μ絃基"))< 5,]

close <- as.double(as.character(today$"Μ絃基"))

CODE.05 <- as.character(today[(close< 5),1]);  CODE.05
CODE.10 <- as.character(today[(close<10)&!(close< 5),1]);  CODE.10
CODE.15 <- as.character(today[(close<15)&!(close<10),1]);  CODE.15
stock.05 <- getStockData( PRICE=NULL, years=c(2014), codes=CODE.05)
stock.10 <- getStockData( PRICE=NULL, years=c(2014), codes=CODE.10)
stock.15 <- getStockData( PRICE=NULL, years=c(2014), codes=CODE.15)


MakePlots( wData=stock.15, stocks=CODE.15, dates=c("103/05/05","103/08/06"), pdfname="Stocks_below_15.pdf", Xpos=-10, WDH=10, HGT=6 )
MakePlots( wData=stock.10, stocks=CODE.10, dates=c("103/05/05","103/08/06"), pdfname="Stocks_below_10.pdf", Xpos=-10, WDH=10, HGT=6 )
MakePlots( wData=stock.05, stocks=CODE.05, dates=c("103/05/05","103/08/06"), pdfname="Stocks_below_05.pdf", Xpos=-10, WDH=10, HGT=6 )



MakePlots( wData=stock.10, stocks="1217", dates=c("103/05/05","103/05/30","103/08/06"), pdfname=NULL)




for(k in which((close<10)&!(close<5))){
  print(price[[1]][[k]][[1]])
}

price[[1]][[1]][[1]]


#########################################################################################################
#########################################################################################################
###                                                                                                   ###
###                                                                                                   ###
###                                                                                                   ###
###                                                                                                   ###
#########################################################################################################
#########################################################################################################


## getwd()
## load("VolumeDataFiles_201408.RData")
## objects()



source("stock functions.r")




#########################################################################################################
#########################################################################################################
###                                                                                                   ###
###                                                                                                   ###




stock <- getStockData( PRICE=NULL, years=c(2006:2014), codes=c("4746","0050","4904","2104","2498","2845","8083"))


stock1455 <- getStockData( PRICE=NULL, years=c(2009:2014), codes="1455")
MakePlots( wData=stock1455, dates=c("100/09/01","100/12/02","101/09/03","101/12/03","102/09/02","102/12/03","103/07/01"), pdfname=NULL )


stock1616 <- getStockData( PRICE=NULL, years=c(2009:2014), codes="1616")
MakePlots( wData=stock1616, dates=c("100/09/01","100/12/02","101/09/03","101/12/03","102/09/02","102/12/03","103/07/01"), pdfname=NULL )


stock1617 <- getStockData( PRICE=NULL, years=c(2009:2014), codes="1617")
MakePlots( wData=stock1617, dates=c("100/09/01","100/12/02","101/09/03","101/12/03","102/09/02","102/12/03","103/07/01"), pdfname=NULL )


stock2014 <- getStockData( PRICE=NULL, years=c(2009:2014), codes="2014")
MakePlots( wData=stock2014, dates=c("100/09/01","100/12/02","101/09/03","101/12/03","102/09/02","102/12/03","103/07/01"), pdfname=NULL )


stock1218 <- getStockData( PRICE=NULL, years=c(2009:2014), codes="1218")
MakePlots( wData=stock1218, dates=c("100/09/01","100/12/02","101/09/03","101/12/03","102/09/02","102/12/03","103/07/01"), pdfname=NULL )


stock1305 <- getStockData( PRICE=NULL, years=c(2009:2014), codes="1305")
MakePlots( wData=stock1305, dates=c("100/09/01","100/12/02","101/09/03","101/12/03","102/09/02","102/12/03","103/07/01"), pdfname=NULL )


stock1310 <- getStockData( PRICE=NULL, years=c(1997:2014), codes="1310")
MakePlots( wData=stock1310, dates=c("100/09/01","100/12/02","101/09/03","101/12/03","102/09/02","102/12/03","103/07/01"), pdfname=NULL )

MakePlots( wData=stock1310, dates=c("100/09/01","100/12/02","101/09/03","101/12/03","102/09/02","102/12/03","103/07/01"), pdfname="1310.pdf", Xpos=-280, WDH=30, HGT=18 )


stock1439 <- getStockData( PRICE=NULL, years=c(2009:2014), codes="1439")
MakePlots( wData=stock1439, dates=c("100/09/01","100/12/02","101/09/03","101/12/03","102/09/02","102/12/03","103/07/01"), pdfname=NULL )
MakePlots( wData=stock1439, dates=c("100/09/01","100/12/02","101/09/03","101/12/03","102/09/02","102/12/03","103/07/01"), Range=c("100/12/03","103/09/23"), pdfname=NULL )


stock1447 <- getStockData( PRICE=NULL, years=c(2009:2014), codes="1447")
MakePlots( wData=stock1447, dates=c("100/09/01","100/12/02","101/09/03","101/12/03","102/09/02","102/12/03","103/07/01"), pdfname=NULL )


stock1714 <- getStockData( PRICE=NULL, years=c(2009:2014), codes="1714")
MakePlots( wData=stock1714, dates=c("100/09/01","100/12/02","101/09/03","101/12/03","102/09/02","102/12/03","103/07/01"), pdfname=NULL )


stock1904 <- getStockData( PRICE=NULL, years=c(2009:2014), codes="1904")
MakePlots( wData=stock1904, dates=c("100/09/01","100/12/02","101/09/03","101/12/03","102/09/02","102/12/03","103/07/01"), pdfname=NULL )


stock1907 <- getStockData( PRICE=NULL, years=c(2009:2014), codes="1907")
MakePlots( wData=stock1907, dates=c("100/09/01","100/12/02","101/09/03","101/12/03","102/09/02","102/12/03","103/07/01"), pdfname=NULL )


stock2024 <- getStockData( PRICE=NULL, years=c(2009:2014), codes="2024")
MakePlots( wData=stock2024, dates=c("100/09/01","100/12/02","101/09/03","101/12/03","102/09/02","102/12/03","103/07/01"), pdfname=NULL )


stock5438 <- getStockData( PRICE=NULL, years=c(2009:2014), codes="5438")
MakePlots( wData=stock5438, dates=c("100/09/01","100/12/02","101/09/03","101/12/03","102/09/02","102/12/03","103/07/01"), pdfname=NULL )


stock2329 <- getStockData( PRICE=NULL, years=c(2009:2014), codes="2329")
MakePlots( wData=stock2329, dates=c("100/09/01","100/12/02","101/09/03","101/12/03","102/09/02","102/12/03","103/07/01"), pdfname=NULL )


stock2610 <- getStockData( PRICE=NULL, years=c(2009:2014), codes="2610")
MakePlots( wData=stock2610, dates=c("100/09/01","100/12/02","101/09/03","101/12/03","102/09/02","102/12/03","103/07/01"), pdfname=NULL )


stock2325 <- getStockData( PRICE=NULL, years=c(2009:2014), codes="2325")
MakePlots( wData=stock2325, dates=c("100/09/01","100/12/02","101/09/03","101/12/03","102/09/02","102/12/03","103/07/01"), pdfname=NULL )
MakePlots( wData=stock2325, dates=c("100/09/01","100/12/02","101/09/03","101/12/03","102/09/02","102/12/03","103/07/01"), Range=c("102/12/03","103/09/24"), pdfname=NULL )


stock3293 <- getStockData( PRICE=NULL, years=c(2009:2014), codes="3293")
MakePlots( wData=stock3293, dates=c("100/09/01","100/12/02","101/09/03","101/12/03","102/09/02","102/12/03","103/07/01"), pdfname=NULL )


stock3443 <- getStockData( PRICE=NULL, years=c(2000:2014), codes="3443")
MakePlots( wData=stock3443, dates=c("100/09/01","100/12/02","101/09/03","101/12/03","102/09/02","102/12/03","103/07/01"), pdfname=NULL )


stock2499 <- getStockData( PRICE=NULL, years=c(2009:2014), codes="2499")
MakePlots( wData=stock2499, dates=c("100/09/01","100/12/02","101/09/03","101/12/03","102/09/02","102/12/03","103/07/01"), pdfname=NULL )


stocktour <- getStockData( PRICE=NULL, years=c(2012:2014), codes=CODE )
CODE <- c("2704","2705","2707","4104","4126","4138","6702","2610")
MakePlots( wData=stocktour, stock=CODE[8], dates=c("100/09/01","100/12/02","101/09/03","101/12/03","102/09/02","102/12/03","103/07/01"), pdfname=NULL )


stock2603 <- getStockData( PRICE=NULL, years=c(2009:2014), codes="2603")
MakePlots( wData=stock2603, dates=c("100/09/01","100/12/02","101/09/03","101/12/03","102/09/02","102/12/03","103/07/01"), pdfname=NULL )


stock3474 <- getStockData( PRICE=NULL, years=c(2000:2014), codes="3474")
MakePlots( wData=stock3474, dates=c("100/09/01","100/12/02","101/09/03","101/12/03","102/09/02","102/12/03","103/07/01"), pdfname=NULL )
MakePlots( wData=stock3474, dates=c("100/09/01","100/12/02","101/09/03","102/04/23","103/08/28","103/05/29","103/09/16"),Range=c("102/04/03","103/09/24"), pdfname=NULL )


stock8076 <- getStockData( PRICE=NULL, years=c(2009:2014), codes="8076")
MakePlots( wData=stock8076, dates=c("100/09/01","100/12/02","101/09/03","101/12/03","102/09/02","102/12/03","103/07/01"), pdfname=NULL )


stock3078 <- getStockData( PRICE=NULL, years=c(2009:2014), codes="3078")
MakePlots( wData=stock3078, dates=c("100/09/01","100/12/02","101/09/03","101/12/03","102/09/02","102/12/03","103/07/01"), pdfname=NULL )


stock6116 <- getStockData( PRICE=NULL, years=c(2009:2014), codes="6116")
MakePlots( wData=stock6116, dates=c("100/09/01","100/12/02","101/09/03","101/12/03","102/09/02","102/12/03","103/07/01"), pdfname=NULL )


stock3536 <- getStockData( PRICE=NULL, years=c(2009:2014), codes="3536")
MakePlots( wData=stock3536, dates=c("100/09/01","100/12/02","101/09/03","101/12/03","102/09/02","102/12/03","103/07/01"), pdfname=NULL )


stock3176 <- getStockData( PRICE=NULL, years=c(2013:2014), codes="3176")
MakePlots( wData=stock3176, dates=c("100/09/01","100/12/02","101/09/03","101/12/03","102/09/02","102/12/03","103/07/01"), pdfname=NULL )
MakePlots( wData=stock3176, dates=c("100/09/01","100/12/02","101/09/03","102/04/23","103/08/28","103/05/29","103/09/16"),Range=c("102/08/07","103/09/24"), pdfname=NULL )


stock8446 <- getStockData( PRICE=NULL, years=c(2014), codes="8446")
MakePlots( wData=stock8446, dates=c("100/09/01","100/12/02","101/09/03","101/12/03","102/09/02","102/12/03","103/07/01"), pdfname=NULL )


stock3059 <- getStockData( PRICE=NULL, years=c(2014), codes="3059")
MakePlots( wData=stock3059, dates=c("100/09/01","100/12/02","101/09/03","101/12/03","102/09/02","102/12/03","103/07/01"), pdfname=NULL )


stock3294 <- getStockData( PRICE=NULL, years=c(2014), codes="3294")
MakePlots( wData=stock3294, dates=c("100/09/01","100/12/02","101/09/03","101/12/03","102/09/02","102/12/03","103/07/01"), pdfname=NULL )





source("stock functions.r")





CODES.IBTS <- c("4960","2882","8050","1446","3598","2458","4162")
stock.IBTS <- getStockData( PRICE=NULL, years=c(2009:2014), codes=CODES.IBTS)
MakePlots( wData=stock.IBTS, stock=CODES.IBTS[1], dates=c("100/09/01","100/12/02","101/09/03","101/12/03","102/09/02","102/12/03","103/07/01"), pdfname=NULL )
MakePlots( wData=stock.IBTS, stock=CODES.IBTS[2], dates=c("100/09/01","100/12/02","101/09/03","101/12/03","102/09/02","102/12/03","103/07/01"), pdfname=NULL )
MakePlots( wData=stock.IBTS, stock=CODES.IBTS[3], dates=c("100/09/01","100/12/02","101/09/03","101/12/03","102/09/02","102/12/03","103/07/01"), pdfname=NULL )
MakePlots( wData=stock.IBTS, stock=CODES.IBTS[4], dates=c("100/09/01","100/12/02","101/09/03","101/12/03","102/09/02","102/12/03","103/07/01"), pdfname=NULL )
MakePlots( wData=stock.IBTS, stock=CODES.IBTS[5], dates=c("100/09/01","100/12/02","101/09/03","101/12/03","102/09/02","102/12/03","103/07/01"), pdfname=NULL )
MakePlots( wData=stock.IBTS, stock=CODES.IBTS[6], dates=c("100/09/01","100/12/02","101/09/03","101/12/03","102/09/02","102/12/03","103/07/01"), pdfname=NULL )
MakePlots( wData=stock.IBTS, stock=CODES.IBTS[7], dates=c("100/09/01","100/12/02","101/09/03","101/12/03","102/09/02","102/12/03","103/07/01"), pdfname=NULL )


stock8050 <- getStockData( PRICE=NULL, years=c(2009:2014), codes="8050")
MakePlots( wData=stock8050, dates=c("100/09/01","100/12/02","101/09/03","101/12/03","102/09/02","102/12/03","103/07/01"), pdfname=NULL )
MakePlots( wData=stock8050, dates=c("100/09/01","100/12/02","101/09/03","101/12/03","102/09/02","102/12/03","103/07/01"), pdfname="plots/8050.pdf" )


stock2634 <- getStockData( PRICE=NULL, years=c(2014:2014), codes="2634")
MakePlots( wData=stock2634, dates=c("100/09/01","100/12/02","101/09/03","101/12/03","102/09/02","102/12/03","103/07/01"), pdfname=NULL )


stock3030 <- getStockData( PRICE=NULL, years=c(2009:2014), codes="3030")
MakePlots( wData=stock3030, dates=c("100/09/01","100/12/02","101/09/03","101/12/03","102/09/02","102/12/03","103/07/01"), pdfname=NULL )


stock3474 <- getStockData( PRICE=NULL, years=c(2009:2014), codes="3474")
stock3474[[1]][[1]][[1]]
MakePlots( wData=stock3474, dates=c("100/09/01","100/12/02","101/09/03","101/12/03","102/09/02","102/12/03","103/07/01"), pdfname=NULL )


stock8042 <- getStockData( PRICE=NULL, years=c(2009:2014), codes="8042")
stock8042[[1]][[1]][[1]]
MakePlots( wData=stock8042, dates=c("100/09/01","100/12/02","101/09/03","101/12/03","102/09/02","102/12/03","103/07/01"), pdfname=NULL )


stock2384 <- getStockData( PRICE=NULL, years=c(2009:2014), codes="2384")
stock2384[[1]][[1]][[1]]
MakePlots( wData=stock2384, dates=c("100/09/01","100/12/02","101/09/03","101/12/03","102/09/02","102/12/03","103/07/01"), pdfname=NULL )


stock5490 <- getStockData( PRICE=NULL, years=c(2009:2014), codes="5490")
stock5490[[1]][[1]][[1]]
MakePlots( wData=stock5490, dates=c("100/09/01","100/12/02","101/09/03","101/12/03","102/09/02","102/12/03","103/07/01"), pdfname=NULL )


stock3285 <- getStockData( PRICE=NULL, years=c(2009:2014), codes="3285")
stock3285[[1]][[1]][[1]]
MakePlots( wData=stock3285, dates=c("100/09/01","100/12/02","101/09/03","101/12/03","102/09/02","102/12/03","103/07/01"), pdfname=NULL )


stock8021 <- getStockData( PRICE=NULL, years=c(2009:2014), codes="8021")
stock8021[[1]][[1]][[1]]
MakePlots( wData=stock8021, dates=c("100/09/01","100/12/02","101/09/03","101/12/03","102/09/02","102/12/03","103/07/01"), pdfname=NULL )


stock2317 <- getStockData( PRICE=NULL, years=c(2009:2014), codes="2317")
stock2317[[1]][[1]][[1]]
MakePlots( wData=stock2317, dates=c("100/09/01","100/12/02","101/09/03","101/12/03","102/09/02","102/12/03","103/07/01"), pdfname=NULL )


stock2330 <- getStockData( PRICE=NULL, years=c(2009:2014), codes="2330")
stock2330[[1]][[1]][[1]]
MakePlots( wData=stock2330, dates=c("100/09/01","100/12/02","101/09/03","101/12/03","102/09/02","102/12/03","103/07/01"), pdfname=NULL )


stock3141 <- getStockData( PRICE=NULL, years=c(2009:2014), codes="3141")
stock3141[[1]][[1]][[1]]
MakePlots( wData=stock3141, dates=c("100/09/01","100/12/02","101/09/03","101/12/03","102/09/02","102/12/03","103/07/01"), pdfname=NULL )


stock1423 <- getStockData( PRICE=NULL, years=c(2009:2014), codes="1423")
stock1423[[1]][[1]][[1]]
MakePlots( wData=stock1423, dates=c("100/09/01","100/12/02","101/09/03","101/12/03","102/09/02","102/12/03","103/07/01"), pdfname=NULL )


stock1423 <- getStockData( PRICE=NULL, years=c(2009:2014), codes="1423")
stock1423[[1]][[1]][[1]]
MakePlots( wData=stock1423, dates=c("100/09/01","100/12/02","101/09/03","101/12/03","102/09/02","102/12/03","103/07/01"), pdfname=NULL )


stock1423 <- getStockData( PRICE=NULL, years=c(2009:2014), codes="1423")
stock1423[[1]][[1]][[1]]
MakePlots( wData=stock1423, dates=c("100/09/01","100/12/02","101/09/03","101/12/03","102/09/02","102/12/03","103/07/01"), pdfname=NULL )


stock1423 <- getStockData( PRICE=NULL, years=c(2009:2014), codes="1423")
stock1423[[1]][[1]][[1]]
MakePlots( wData=stock1423, dates=c("100/09/01","100/12/02","101/09/03","101/12/03","102/09/02","102/12/03","103/07/01"), pdfname=NULL )


stock1423 <- getStockData( PRICE=NULL, years=c(2009:2014), codes="1423")
stock1423[[1]][[1]][[1]]
MakePlots( wData=stock1423, dates=c("100/09/01","100/12/02","101/09/03","101/12/03","102/09/02","102/12/03","103/07/01"), pdfname=NULL )


stock1423 <- getStockData( PRICE=NULL, years=c(2009:2014), codes="1423")
stock1423[[1]][[1]][[1]]
MakePlots( wData=stock1423, dates=c("100/09/01","100/12/02","101/09/03","101/12/03","102/09/02","102/12/03","103/07/01"), pdfname=NULL )


stock1423 <- getStockData( PRICE=NULL, years=c(2009:2014), codes="1423")
stock1423[[1]][[1]][[1]]
MakePlots( wData=stock1423, dates=c("100/09/01","100/12/02","101/09/03","101/12/03","102/09/02","102/12/03","103/07/01"), pdfname=NULL )


stock1423 <- getStockData( PRICE=NULL, years=c(2009:2014), codes="1423")
stock1423[[1]][[1]][[1]]
MakePlots( wData=stock1423, dates=c("100/09/01","100/12/02","101/09/03","101/12/03","102/09/02","102/12/03","103/07/01"), pdfname=NULL )


stock1423 <- getStockData( PRICE=NULL, years=c(2009:2014), codes="1423")
stock1423[[1]][[1]][[1]]
MakePlots( wData=stock1423, dates=c("100/09/01","100/12/02","101/09/03","101/12/03","102/09/02","102/12/03","103/07/01"), pdfname=NULL )


==


stock1423 <- getStockData( PRICE=NULL, years=c(2009:2014), codes="1423")
MakePlots( wData=stock1423, dates=c("100/09/01","100/12/02","101/09/03","101/12/03","102/09/02","102/12/03","103/07/01"), pdfname=NULL )


stock4746 <- getStockData( PRICE=NULL, years=c(2000:2014), codes="4746")
MakePlots( wData=stock4746, dates=c("102/06/26","102/11/08","103/04/09","103/08/06"), pdfname=NULL )
MakePlots( wData=stock4746, dates=c("102/06/26","102/11/08","103/04/09","103/08/06"), pdfname="plots/4746.pdf", WDH=20, HGT=10 )
MakePlots( wData=stock4746, dates=c("102/06/26","102/11/08","103/04/09","103/08/06"), Range=c("103/04/09"), pdfname="plots/4746_sub.pdf", WDH=12, HGT=6 )
stock4746[[1]][[1]][[2]]

stock0050 <- getStockData( PRICE=NULL, years=c(2009:2014), codes="0050")
MakePlots( wData=stock0050, dates=c("102/06/26","102/11/08","103/04/09","103/08/06"), pdfname=NULL )



















stock[[1]]
stock[[2]]
stock[[3]]

MakePlots( wData=stock, stocks="4746", dates=c("103/05/05","103/08/06"), pdfname=NULL )
MakePlots( wData=stock, stocks="0050", dates=c("103/05/05","103/08/06"), pdfname=NULL )
MakePlots( wData=stock, stocks="4904", dates=c("102/06/03","102/10/03","103/07/01","102/11/01"), pdfname=NULL, Xpos=-150, ylines=60:65 )
MakePlots( wData=stock, stocks="2104", dates=c("103/05/05","103/08/06"), pdfname=NULL, Xpos=-150 )



MakePlots( wData=stock, stocks="4746", dates=c("103/05/05","103/08/06"), pdfname=NULL )
MakePlots( wData=stock, stocks="0050", dates=c("103/05/05","103/08/06"), pdfname=NULL )
MakePlots( wData=stock, stocks=c("4746","0050"), dates=c("103/05/05","103/08/06"), range=c("103/06/07"), pdfname=NULL, dim=c(2,1) )
MakePlots( wData=stock, stocks=c("4746","0050"), dates=c("103/05/05","103/08/06"), range=c("103/01/01"), pdfname=NULL, dim=c(2,1) )
MakePlots( wData=stock, stocks=NULL, dates=c("103/05/05","103/08/06"), range=c("103/06/07"), pdfname=NULL, dim=c(3,2) )

stock[[1]][[1]][[2]][,1]

info_fcn(stock)


dev.off()



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


source("stock functions.r")

source("stock functions for real data.r")



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

































































