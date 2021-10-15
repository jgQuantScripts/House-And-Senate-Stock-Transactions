require("XML");require("xml2");require("stringr");require("jsonlite");require("lubridate")
require("RQuantLib");require("data.table");require("quantmod");require("pbapply")
# API - source
# https://housestockwatcher.com/api
# ******************************************************************************************************
#                                         files Map/Index
# ******************************************************************************************************
#  https://house-stock-watcher-data.s3-us-west-2.amazonaws.com/data/filemap.xml
tmp <- xml2::read_xml(x = paste0("https://house-stock-watcher-data.s3-us-west-2.amazonaws.com",
                                 "/data/filemap.xml"))
# extract all json file names available 1:length(xml_children(tmp)
getNames = lapply(as.list(1:10), function(ii){
  xml2::as_list(x=xml_child(tmp, ii))$Key  
})
# unlist/as.character
getNames = unique(rapply(getNames, function(x) head(x, 1)))
# ******************************************************************************************************
#                                         helper functions
# ******************************************************************************************************
# https://stackoverflow.com/questions/5060076/convert-html-character-entity-encoding-in-r
html2txt <- function(str) {
  xpathApply(htmlParse(str, asText=TRUE),
             "//body//text()", 
             xmlValue)[[1]] 
}
html2txt2 <- function(pos,TODAY) {
  do.call(c,xpathApply(htmlParse(TODAY[[6]][[1]]$asset_description[pos], asText=TRUE),
                       "//body//text()", 
                       xmlValue)) %>% paste(collapse=" ")
}
# ******************************************************************************************************
#                                         main function
# ******************************************************************************************************
# function to "Prettify" data
houseTrades = function(fileN){
  # extract file  
  url = paste0("https://house-stock-watcher-data.s3-us-west-2.amazonaws.com/",fileN)
  # read data
  TODAY = read_json(url,simplifyVector=TRUE)
  # extract contents in json 
  dta = as.data.frame(rbindlist(TODAY$transactions,use.names = TRUE,fill = TRUE))
  if(nrow(dta)>0){
  tmp = lapply(as.list(1:length(TODAY$transactions)), function(ii){
    dta = as.data.frame(TODAY$transactions[[ii]])
    dta$name = TODAY$name[ii]
    dta$reportDate = TODAY$filing_date[ii]
    dta$district = TODAY$district[ii]
    dta$source = TODAY$source_ptr_link[ii]
    dta$transcribed_by = TODAY$transcribed_by[ii]
    # formatting
    dta$amount       = gsub(" ","",gsub("\\,","",gsub("\\$","",dta$amount)))
    dta$transaction_date = as.Date(dta$transaction_date,format="%Y-%m-%d")
    dta$reportDate = as.Date(dta$reportDate,format="%m/%d/%Y")  
    dta
  })
  dta = rbindlist(tmp,use.names=TRUE,fill=TRUE)       
  }else{
    dta = NULL
  }
  # return data.frame
  dta
}
# test function
trades = houseTrades(getNames[1])
# ******************************************************************************************************
#                                         read ALL Data
# ******************************************************************************************************
getAllHouseTrades = function(){
  # extract file  
  url = paste0("https://house-stock-watcher-data.s3-us-west-2.amazonaws.com/",
               "data/all_transactions.json")
  # read data
  TODAY = read_json(url,simplifyVector=TRUE)
  # format
  TODAY$ticker = gsub("\\N/A","--",TODAY$ticker)
  TODAY$ticker = gsub("\\<NA>","--",TODAY$ticker)
  TODAY$amount = gsub(" ","",gsub("\\,","",gsub("\\$","",TODAY$amount)))
  TODAY$disclosure_date = as.Date(TODAY$disclosure_date, format="%m/%d/%Y")
  TODAY$transaction_date = as.Date(TODAY$transaction_date, format="%Y-%m-%d")
  # return data.frame
  TODAY
}
ALL = getAllHouseTrades()
#write.table(ALL,paste0("~/Desktop/SenatorTrades.csv"),sep=",")

# Stock Ranking - what are they buying:
getRanking = function(ALL,from,to){
  # only want YTD
  ALL = subset(ALL, ALL$transaction_date >= as.Date(from) &
                 ALL$transaction_date <= as.Date(to))
  
  df = as.data.frame(unique(ALL$ticker))
  colnames(df) = "ticker"
  
  # column for purchases
  df$Purchases = do.call(rbind,lapply(as.list(1:nrow(df)), function(ii){
    nrow(subset(ALL,ALL$ticker == df$ticker[ii] & ALL$type=="purchase"))
  }))
  # column for Partial Sales
  df$PartialSales = do.call(rbind,lapply(as.list(1:nrow(df)), function(ii){
    nrow(subset(ALL,ALL$ticker == df$ticker[ii] & ALL$type=="sale_partial"))
  }))
  # column for Full Sales
  df$FullSales = do.call(rbind,lapply(as.list(1:nrow(df)), function(ii){
    nrow(subset(ALL,ALL$ticker == df$ticker[ii] & ALL$type=="sale_full"))
  }))
  # column for Exchanges
  df$Exchanges = do.call(rbind,lapply(as.list(1:nrow(df)), function(ii){
    nrow(subset(ALL,ALL$ticker == df$ticker[ii] & ALL$type=="exchange"))
  }))
  # Score: 1 point for Purchases, -0.5 point for Partial Sales, -1 for Full Sales
  df$Score = (df$Purchases*1)+(df$PartialSales*-0.5)+(df$FullSales*-1)
  # order in descending order
  df = df[order(df$Score,decreasing = TRUE),]
  # return data.frame
  df
}

# test function
RANKS = getRanking(ALL=ALL,from="2021-01-01",to=Sys.Date())

# Stocks only
STKS = subset(ALL, ALL$ticker != "--")
STKS = subset(STKS,STKS$transaction_date >= as.Date("2021-01-01") & 
                   STKS$transaction_date <= Sys.Date())
# get symbols for unique stocks
# e <- new.env()
# tickers = unique(STKS$ticker)
# getSymbols(tickers,env = e, from="2020-01-01")
# CLOSE <- do.call(merge,eapply(e,Ad))
# colnames(CLOSE) = gsub(".Adjusted","",names(CLOSE))
# date formatting to get DB name
seqDATE = seq.Date(as.Date("2021-10-01"),Sys.Date(),by="1 day")
seqDATE = last(seqDATE[weekdays(seqDATE) == "Sunday"])
daytoday = format(seqDATE,"%Y%m%d")
# used in later functions -> last Trading day available in DB
maxDATE = as.Date(seqDATE - days(2))
# gets the stock price from DB
# ticker =  stock ticker ex. AAPL
# daytoday = part of the database name -> call most recent
# date = will return the stock price on this "date"
getSTK = function(ticker,daytoday,date)
{
  # establist SQLite Connection
  driver = dbDriver("SQLite")
  con = dbConnect(driver, dbname = paste0("~/OneDrive - Riverside Community College District/Dump/SQLite/",
                                          daytoday,"_getSymbols.db"))
  # query stock from database
  tmp = dbGetQuery(con,paste0("SELECT CLOSE FROM getSymbols WHERE Symbol='",ticker,"' AND Date='",
                              as.numeric(as.Date(date)),"';"))
  dbDisconnect(con) # disconnect
  as.numeric(tmp)   # return price for stock
}
# gets stock Performance - 1 day, 1 week, etc.
# ticker =  stock ticker ex. AAPL
# daytoday = part of the database name -> call most recent
# date = will return the stock price on this "date"
# maxDate = last date pulled by the database :/
getSTK_ = function(ticker,daytoday,date,maxDate=maxDATE)
{
  # date formatting
  maxDate = as.Date(maxDate)
  # sequence of dates -> focuses on a year after entry price
  DAYS = seq.Date(from=as.Date(date),to=as.Date(date)+years(1),by=1)
  # will only extract Trading days from DAYS
  DAYS = DAYS[isBusinessDay(calendar = "UnitedStates/NYSE",DAYS)]
  # will return Max available date if date falls into the future
  oneDay  = ifelse(DAYS[1+1] > maxDate, maxDate,DAYS[1+1]) %>% as.Date()
  fiveDay = ifelse(DAYS[1+5] > maxDate, maxDate,DAYS[1+5]) %>% as.Date()
  oneWk   = ifelse(DAYS[1+7] > maxDate, maxDate,DAYS[1+7]) %>% as.Date()
  twoWk   = ifelse(DAYS[1+14] > maxDate, maxDate,DAYS[1+14]) %>% as.Date()
  oneMo   = ifelse(DAYS[1+30] > maxDate, maxDate,DAYS[1+30]) %>% as.Date()
  threeMo = ifelse(DAYS[1+90] > maxDate, maxDate,DAYS[1+90]) %>% as.Date()
  sixMo   = ifelse(DAYS[1+180] > maxDate, maxDate,DAYS[1+180]) %>% as.Date()
  oneYr   = ifelse(DAYS[length(DAYS)] > maxDate, maxDate,DAYS[length(DAYS)]) %>% as.Date()
  # gets the entry price
  buyPRC = getSTK(ticker=ticker,daytoday = daytoday,date=date)
  # establish a connection with Database
  driver = dbDriver("SQLite")
  con = dbConnect(driver, dbname = paste0("~/OneDrive - Riverside Community College District/Dump/SQLite/",
                                          daytoday,"_getSymbols.db"))
  # query prices for given days
  d1 = dbGetQuery(con,paste0("SELECT CLOSE FROM getSymbols WHERE Symbol='",ticker,"' AND Date='",
                              as.numeric(oneDay),"';"))
  d5 = dbGetQuery(con,paste0("SELECT CLOSE FROM getSymbols WHERE Symbol='",ticker,"' AND Date='",
                             as.numeric(fiveDay),"';"))
  w1 = dbGetQuery(con,paste0("SELECT CLOSE FROM getSymbols WHERE Symbol='",ticker,"' AND Date='",
                             as.numeric(oneWk),"';"))
  w2 = dbGetQuery(con,paste0("SELECT CLOSE FROM getSymbols WHERE Symbol='",ticker,"' AND Date='",
                             as.numeric(twoWk),"';"))
  m1 = dbGetQuery(con,paste0("SELECT CLOSE FROM getSymbols WHERE Symbol='",ticker,"' AND Date='",
                             as.numeric(oneMo),"';"))
  m3 = dbGetQuery(con,paste0("SELECT CLOSE FROM getSymbols WHERE Symbol='",ticker,"' AND Date='",
                             as.numeric(threeMo),"';"))
  m6 = dbGetQuery(con,paste0("SELECT CLOSE FROM getSymbols WHERE Symbol='",ticker,"' AND Date='",
                             as.numeric(sixMo),"';"))
  y1 = dbGetQuery(con,paste0("SELECT CLOSE FROM getSymbols WHERE Symbol='",ticker,"' AND Date='",
                             as.numeric(oneYr),"';"))
  # disconnect from DB
  dbDisconnect(con) 
  # try to cbind prices 
  df = try(cbind(d1,d5,w1,w2,m1,m3,m6,y1))
  # if there is an error (most likely the prices couldn't be found)....
  if(inherits(df,"try-error")){
    # create a synthetic data.frame such that the entry price will be the same over the lenght of the year
    df = as.data.frame(t(rep(buyPRC,8)))
    # format column names
    colnames(df) = c("day1","day5","week1","week2","month1","month3","month6","year1")  
  }else{
    # format column names if there's no error
    colnames(df) = c("day1","day5","week1","week2","month1","month3","month6","year1")  
  }
  # find the simple return over the length of times
  pctRet = round(df/buyPRC-1,4)
  # format column names for the date frame
  colnames(pctRet) = c("pct_day1","pct_day5","pct_week1","pct_week2","pct_month1",
                       "pct_month3","pct_month6","pct_year1")
  # cbind prices & % returns
  cbind(df,pctRet)
}
# will reconcile trades / gets the performance
# STKS = Transaction data from Reresentatives
# daytoday = part of the database name -> call most recent
# maxDate = last date pulled by the database :/
reconcileBuyTrades = function(STKS,daytoday,maxDATE)
{
  # split Transaction Amounts
  AMTS = gsub("\\+","",STKS$amount)
  AMTS = as.data.frame(do.call(rbind,str_split(STKS$amount, "-")))
  colnames(AMTS) = c("minAmt","maxAmt")
  AMTS$minAmt = as.numeric(AMTS$minAmt)
  AMTS$maxAmt = as.numeric(AMTS$maxAmt)
  AMTS$maxAmt[is.na(AMTS$maxAmt)] <- 15000
  AMTS$minAmt[is.na(AMTS$minAmt)] <- 1001
  # merge with data
  STKS = cbind(STKS,AMTS)
  # unique number of representatives
  #NOMS = unique(STKS$representative)
  # subset needed columns only
  STKS = STKS[,c("transaction_date","disclosure_date",
                 "ticker","asset_description","type",
                 "representative","minAmt","maxAmt")]
  STKS = subset(STKS, STKS$type == "purchase")
  # get stock prices from DB
  prc0 = pblapply(as.list(1:nrow(STKS)), function(ii){
    getSTK(ticker = STKS$ticker[ii], daytoday = daytoday,date = STKS$transaction_date[ii])
  })
  STKS$stkPRC = do.call(rbind,prc0)

  # strip out NA stock prices
  NAs = STKS[is.na(STKS$stkPRC),]   # NA Prices    :(
  STKS = STKS[!is.na(STKS$stkPRC),] # no NA Prices :)
  # some tickers in the transactions are misspelled...
  # need to manually adjust them
  NAs$ticker = gsub("LTD","BBWI",NAs$ticker)
  NAs$ticker = gsub("AAPl","AAPL",NAs$ticker)
  NAs$ticker = gsub("APPL","AAPL",NAs$ticker)
  NAs$ticker = gsub("LCRX","LRCX",NAs$ticker)
  NAs$ticker = gsub("ALFY","ALLY",NAs$ticker)
  NAs$ticker = gsub("CCC","CLVT",NAs$ticker)
  NAs$ticker = gsub("ALb","ALB",NAs$ticker)
  NAs$ticker = gsub("ETWO.W","ETWO-WT",NAs$ticker)
  NAs$ticker = gsub("APHA","TLRY",NAs$ticker)
  NAs$ticker = gsub("DESY","DSEY",NAs$ticker)
  NAs$ticker = gsub("PLT","PLNT",NAs$ticker)
  NAs$ticker = gsub("35G.SG","G",NAs$ticker)
  NAs$ticker = gsub("BXS$A","BXS-PA",NAs$ticker)
  NAs$ticker = gsub("BOA","BAC",NAs$ticker)
  NAs$ticker = gsub("Broadcom Inc.","AVGO",NAs$ticker)
  NAs$ticker = gsub("ZOOM","ZM",NAs$ticker)
  NAs$ticker = gsub("RY.TO","RY",NAs$ticker)
  NAs$ticker = gsub("BRKB","BRK.B",NAs$ticker)
  NAs$ticker = gsub("HOn","HON",NAs$ticker)
  NAs$ticker = gsub("INTL","INTC",NAs$ticker)
  NAs$ticker = gsub("APCD","APD",NAs$ticker)
  NAs$ticker = gsub("WYND","WH",NAs$ticker)
  NAs$ticker = gsub("SEA","SE",NAs$ticker)
  NAs$ticker = gsub("SPYS","SPYG",NAs$ticker)
  NAs$ticker = gsub("BLDr","BLDR",NAs$ticker)
  NAs$ticker = gsub("VTRA","VTRS",NAs$ticker)
  # re run to get prices
  prc0 = pblapply(as.list(1:nrow(NAs)), function(ii){
    getSTK(ticker = NAs$ticker[ii], daytoday =daytoday,date = NAs$transaction_date[ii])
  })
  # rbind data
  NAs$stkPRC = do.call(rbind,prc0)
  NAs_good = NAs[!is.na(NAs$stkPRC),] # no NA Prices
  NAs = NAs[is.na(NAs$stkPRC),]       # NA Prices
  # merge good transactions with complete transaction data.frame
  STKS = rbind(STKS,NAs_good);rm(NAs_good)
  # calculates performance on the good transactions
  PERF = pblapply(as.list(1:nrow(STKS)), function(ii){
    getSTK_(ticker=STKS$ticker[ii],daytoday=daytoday,date=STKS$transaction_date[ii],maxDate=maxDATE)
  })
  # row bind performance data
  PERF = do.call(rbind,PERF)
  # cbind with stock data
  STKS = cbind(STKS,PERF)
  # return the list of Performance data.frame & NA transactions 
  list(STKS,NAs)  
}
# test function
DTA = reconcileBuyTrades(STKS=STKS,daytoday=daytoday,maxDATE=maxDATE)
# good data
BUYS = DTA[[1]]
# na data - could not find tickers
buyNAs = DTA[[2]]

# summarize by ticker
# REC = Pass in Trade Reconciliation 
byTickerBUY = function(REC)
{
  # extract unique tickers
  uTics = unique(REC$ticker)
  # lapply -> pass in tickers to extract trade Data 
  YTD = pblapply(as.list(uTics), function(x){
    tmp = subset(REC, REC$ticker == x)
    # number of transactions
    nTrans = nrow(tmp) %>% as.numeric() %>% as.data.frame()
    # min Purchase Date
    minDate = min(tmp$transaction_date) %>% as.data.frame()
    # max Purchase Date
    maxDate = max(tmp$transaction_date) %>% as.data.frame()
    # Estimated Purchase Amount
    tmp$estPurchAmt = (tmp$minAmt+tmp$maxAmt)/2
    estTotalAmt = sum(tmp$estPurchAmt) %>% as.data.frame()
    # Estimate Qty Purchased
    tmp$estQty = round(tmp$estPurchAmt/tmp$stkPRC,2)
    estTotalShrs = sum(tmp$estQty) %>% as.data.frame()
    # average price paid
    estPurchPrcShr = round(estTotalAmt/estTotalShrs,2)
    # average return by time
    avgRet = colMeans(tmp[,c("pct_day1","pct_day5","pct_week1",
                             "pct_week2","pct_month1","pct_month3",
                             "pct_month6","pct_year1")]) %>% t() %>%  as.data.frame()
    # combine data
    NOM = unique(tmp$ticker) %>% as.data.frame()
    # min price Paid
    minPRC = min(tmp$stkPRC) %>% as.data.frame()
    # max price Paid
    maxPRC = max(tmp$stkPRC) %>% as.data.frame()
    # last traded price on DB
    lastPRC = getSTK(ticker=x,daytoday=daytoday,date=maxDATE)
    # combine data
    all = cbind(NOM,minDate,maxDate,nTrans,estTotalAmt,estTotalShrs,estPurchPrcShr,minPRC,maxPRC,lastPRC,avgRet)
    colnames(all)[1:10] = c("ticker","firstPurchase","latestPurchase","nTrans",
                           "estTotalDollarAmount","estSharesOwned","vwap_paid",
                           "minPricePaid","maxPricePaid","lastPrice")
    all
  })
  # rbind results
  YTD = rbindlist(YTD,use.names = TRUE,fill = TRUE)  
  YTD
}
# get summary by ticker
allBUYS = byTickerBUY(REC=BUYS)
# ******************************************************************************************************
#                                         sells
# ******************************************************************************************************
# will reconcile sells including partial sales
reconcileSellTrades = function(STKS)
{
  # split Transaction Amounts
  AMTS = gsub("\\+","",STKS$amount)
  AMTS = as.data.frame(do.call(rbind,str_split(STKS$amount, "-")))
  colnames(AMTS) = c("minAmt","maxAmt")
  AMTS$minAmt = as.numeric(AMTS$minAmt)
  AMTS$maxAmt = as.numeric(AMTS$maxAmt)
  AMTS$maxAmt[is.na(AMTS$maxAmt)] <- 15000
  AMTS$minAmt[is.na(AMTS$minAmt)] <- 1001
  # merge with data
  STKS = cbind(STKS,AMTS)
  # unique number of representatives
  NOMS = unique(STKS$representative)
  # subset needed columns only
  STKS = STKS[,c("transaction_date","disclosure_date",
                 "ticker","asset_description","type",
                 "representative","minAmt","maxAmt")]
  STKS = subset(STKS, STKS$type == "sale_full" | STKS$type == "sale_partial")
  # get stock prices from DB
  prc0 = pblapply(as.list(1:nrow(STKS)), function(ii){
    getSTK(ticker = STKS$ticker[ii], daytoday = daytoday,date = STKS$transaction_date[ii])
  })
  STKS$stkPRC = do.call(rbind,prc0)
  
  # strip out NA stock prices
  NAs = STKS[is.na(STKS$stkPRC),]   # NA Prices
  STKS = STKS[!is.na(STKS$stkPRC),] # no NA Prices
  
  
  NAs$ticker = gsub("LTD","BBWI",NAs$ticker)
  NAs$ticker = gsub("AAPl","AAPL",NAs$ticker)
  NAs$ticker = gsub("APPL","AAPL",NAs$ticker)
  NAs$ticker = gsub("LCRX","LRCX",NAs$ticker)
  NAs$ticker = gsub("ALFY","ALLY",NAs$ticker)
  NAs$ticker = gsub("CCC","CLVT",NAs$ticker)
  NAs$ticker = gsub("ALb","ALB",NAs$ticker)
  NAs$ticker = gsub("ETWO.W","ETWO-WT",NAs$ticker)
  NAs$ticker = gsub("APHA","TLRY",NAs$ticker)
  NAs$ticker = gsub("DESY","DSEY",NAs$ticker)
  NAs$ticker = gsub("PLT","PLNT",NAs$ticker)
  NAs$ticker = gsub("35G.SG","G",NAs$ticker)
  NAs$ticker = gsub("BXS$A","BXS-PA",NAs$ticker)
  NAs$ticker = gsub("BOA","BAC",NAs$ticker)
  NAs$ticker = gsub("Broadcom Inc.","AVGO",NAs$ticker)
  NAs$ticker = gsub("ZOOM","ZM",NAs$ticker)
  NAs$ticker = gsub("RY.TO","RY",NAs$ticker)
  NAs$ticker = gsub("BRKB","BRK.B",NAs$ticker)
  NAs$ticker = gsub("HOn","HON",NAs$ticker)
  NAs$ticker = gsub("INTL","INTC",NAs$ticker)
  NAs$ticker = gsub("APCD","APD",NAs$ticker)
  NAs$ticker = gsub("WYND","WH",NAs$ticker)
  NAs$ticker = gsub("SEA","SE",NAs$ticker)
  NAs$ticker = gsub("SPYS","SPYG",NAs$ticker)
  NAs$ticker = gsub("BLDr","BLDR",NAs$ticker)
  NAs$ticker = gsub("VTRA","VTRS",NAs$ticker)
  prc0 = pblapply(as.list(1:nrow(NAs)), function(ii){
    getSTK(ticker = NAs$ticker[ii], daytoday = daytoday,date = NAs$transaction_date[ii])
  })
  NAs$stkPRC = do.call(rbind,prc0)
  NAs_good = NAs[!is.na(NAs$stkPRC),] # no NA Prices
  NAs = NAs[is.na(NAs$stkPRC),]   # NA Prices
  STKS = rbind(STKS,NAs_good);rm(NAs_good)
  
  PERF = pblapply(as.list(1:nrow(STKS)), function(ii){
    getSTK_(ticker=STKS$ticker[ii],daytoday=daytoday,date=STKS$transaction_date[ii],maxDate=maxDATE)
  })
  PERF = do.call(rbind,PERF)

  STKS = cbind(STKS,PERF)
  list(STKS,NAs)  
}
# get sell trades
DTA = reconcileSellTrades(STKS)
SELLS = DTA[[1]]
sellNAs = DTA[[2]]

# summarize by ticker
byTickerSELL = function(REC)
{
  uTics = unique(REC$ticker)
  YTD = pblapply(as.list(uTics), function(x){
    tmp = subset(REC, REC$ticker == x)
    # number of transactions
    nTrans = nrow(tmp) %>% as.numeric() %>% as.data.frame()
    # min Purchase Date
    minDate = min(tmp$transaction_date) %>% as.data.frame()
    # max Purchase Date
    maxDate = max(tmp$transaction_date) %>% as.data.frame()
    # Estimated Purchase Amount
    tmp$estPurchAmt = (tmp$minAmt+tmp$maxAmt)/2
    estTotalAmt = sum(tmp$estPurchAmt) %>% as.data.frame()
    # Estimate Qty Purchased
    tmp$estQty = round(tmp$estPurchAmt/tmp$stkPRC,2)
    estTotalShrs = sum(tmp$estQty) %>% as.data.frame()
    # average price paid
    estPurchPrcShr = round(estTotalAmt/estTotalShrs,2)
    # average return by time
    avgRet = colMeans(tmp[,c("pct_day1","pct_day5","pct_week1",
                             "pct_week2","pct_month1","pct_month3",
                             "pct_month6","pct_year1")]) %>% t() %>%  as.data.frame()
    # combine data
    NOM = unique(tmp$ticker) %>% as.data.frame()
    # min price Paid
    minPRC = min(tmp$stkPRC) %>% as.data.frame()
    # max price Paid
    maxPRC = max(tmp$stkPRC) %>% as.data.frame()
    
    all = cbind(NOM,minDate,maxDate,nTrans,estTotalAmt,estTotalShrs,estPurchPrcShr,minPRC,maxPRC,avgRet)
    colnames(all)[1:9] = c("ticker","firstPurchase","latestPurchase","nTrans",
                           "estTotalDollarAmount","estSharesOwned","vwap_paid",
                           "minPricePaid","maxPricePaid")
    all
  })
  YTD = rbindlist(YTD,use.names = TRUE,fill = TRUE)  
  YTD
}
# all sales
allSELLS= byTickerSELL(REC=SELLS)
# ******************************************************************************************************
#                                         reconcile buys/sells
# ******************************************************************************************************

# calculate PnL / net Positions
netACT = function(allBUYS,allSELLS){
  
  # these are the stocks that are both in the allBUYS and allSELLS
  allB_tickers = unique(allBUYS$ticker)
  allS_tickers = unique(allSELLS$ticker)
  
  # length(allS_tickers[allS_tickers %in% allB_tickers])
  # length(allB_tickers[allB_tickers %in% allS_tickers])
  # finds matching tickers
  SinB = allS_tickers[allS_tickers %in% allB_tickers]
  allBUYS = do.call(rbind,pblapply(as.list(SinB), function(x){
    subset(allBUYS,allBUYS$ticker == x)
  }))
  allSELLS = do.call(rbind,pblapply(as.list(SinB), function(x){
    subset(allSELLS,allSELLS$ticker == x)
  }))
  
  
  allBUYS = allBUYS[,c("ticker","nTrans","estTotalDollarAmount","estSharesOwned","vwap_paid")]
  colnames(allBUYS) = c("ticker","buyTrans","estBuyDollarVol","estBuyShares","vwap_paid") 
  allSELLS = allSELLS[,c("ticker","nTrans","estTotalDollarAmount","estSharesOwned","vwap_paid")]
  colnames(allSELLS) = c("ticker","sellTrans","estSellDollarVol","estSellShares","vwap_sold") 
  
  REC = pblapply(as.list(SinB), function(x){
    bot  = subset(allBUYS, allBUYS$ticker==x)
    sell = subset(allSELLS, allSELLS$ticker==x)
    bs = as.data.frame(cbind(bot$ticker,bot$buyTrans,sell$sellTrans,bot$estBuyDollarVol,sell$estSellDollarVol,
          bot$estBuyShares,sell$estSellShares,bot$vwap_paid,sell$vwap_sold))
    colnames(bs) = c("ticker","buyTrans","sellTrans","estBuyDollarVol","estSellDollarVol",
                     "estBuyShares","estSellShares","vwap_paid","vwap_sold")
    bs$netDollarVol = as.numeric(bs$estBuyDollarVol) - as.numeric(bs$estSellDollarVol)
    bs$netShares    = as.numeric(bs$estBuyShares) - as.numeric(bs$estSellShares)
    bs$vwap_change  = as.numeric(bs$vwap_sold) - as.numeric(bs$vwap_paid)
    bs$vwap_PCTchange  = round(as.numeric(bs$vwap_sold)/as.numeric(bs$vwap_paid)-1,4)
    bs
  })
  
  REC = rbindlist(REC,use.names = TRUE,fill = TRUE)
  
  REC
}
# return net activity
netActivity = netACT(allBUYS,allSELLS)

# calculate net Longs: 
# the following function will return a summary of those
# tickers not found in the allSELLS data.frame
noSELLS = function(allBUYS,allSELLS){
  
  # these are the stocks that are both in the allBUYS and allSELLS
  allB_tickers = unique(allBUYS$ticker)
  allS_tickers = unique(allSELLS$ticker)
  
  # length(allB_tickers)
  # length(allS_tickers)
  
  # all Buy tickers NOT in Sell tickers
  SinB = allB_tickers[!(allB_tickers %in% allS_tickers)]
  allBUYS = do.call(rbind,pblapply(as.list(SinB), function(x){
    subset(allBUYS,allBUYS$ticker == x)
  }))
  
  # format column names
  allBUYS = allBUYS[,c("ticker","nTrans","estTotalDollarAmount","estSharesOwned","vwap_paid")]
  colnames(allBUYS) = c("ticker","buyTrans","estBuyDollarVol","estBuyShares","vwap_paid") 
  
  # add last traded price on DB
  allBUYS$lastPRC = pblapply(as.list(allBUYS$ticker), function(x){
    getSTK(ticker = x, date = maxDATE,daytoday = daytoday)
  })
  # calculate estimated percentage return 
  allBUYS$pct2VWAP = round(as.numeric(allBUYS$lastPRC)/as.numeric(allBUYS$vwap_paid)-1,4)
  # return data.frame
  allBUYS
}

# these stocks have not been sold 
netLong = noSELLS(allBUYS,allSELLS)
# ******************************************************************************************************
#                                         best investor - solely on stocks
# ******************************************************************************************************
# will return the best investor(s)/representative(s)
bestInvestorRank = function(STKS)
{
  # extract the unique names of representatives
  NOMS = unique(STKS$representative)
  # gets total return for SPY ETF YTD
  bmSTART = getSTK(ticker = "SPY",daytoday = daytoday,date="2021-01-04")
  bmEND   = getSTK(ticker = "SPY",daytoday = daytoday,date=maxDATE)
  bmRET = round(bmEND/bmSTART-1,4)
  # calculate portfolio returns YTD
  TBL = pblapply(as.list(NOMS), function(x){
    # subset to rep. data
    tmp = subset(STKS, STKS$representative == x)
    STATE = str_sub(unique(tmp$district),start = 1,end = 2)
    # split Transaction Amounts
    AMTS = gsub("\\+","",tmp$amount)
    AMTS = as.data.frame(do.call(rbind,str_split(tmp$amount, "-")))
    colnames(AMTS) = c("minAmt","maxAmt")
    AMTS$minAmt = as.numeric(AMTS$minAmt)
    AMTS$maxAmt = as.numeric(AMTS$maxAmt)
    AMTS$maxAmt[is.na(AMTS$maxAmt)] <- 15000
    AMTS$minAmt[is.na(AMTS$minAmt)] <- 1001
    AMTS$midAmt = round((AMTS$minAmt+AMTS$maxAmt)/2,2)
    tmp = cbind(tmp,AMTS)
    # subset only 
    tmp = tmp[,c("disclosure_date","transaction_date","ticker","type","representative","minAmt","midAmt","maxAmt")]
    # get the stock price at the time of the transaction
    tmp$transPRC <- lapply(as.list(1:nrow(tmp)), function(ii){
      getSTK(ticker = tmp$ticker[ii], daytoday =daytoday,date = tmp$transaction_date[ii])
    })
    # get the latest available price
    tmp$lastPRC <- lapply(as.list(1:nrow(tmp)), function(ii){
      getSTK(ticker = tmp$ticker[ii], daytoday =daytoday,date = maxDATE)
    })
    # add percentage return
    tmp$pctRET = round(as.numeric(tmp$lastPRC)/as.numeric(tmp$transPRC)-1,4)
    tmp$pctRET[is.na(tmp$pctRET)]<-0
    # BUYS 
    BOT = subset(tmp,tmp$type == "purchase")
    # SELLS
    SLD = subset(tmp,tmp$type == "sale_full" | tmp$type == "sale_partial")
    # add weights
    BOT$wts = round(as.numeric(BOT$midAmt)/sum(as.numeric(BOT$midAmt)),4)
    estDollarBuys = sum(as.numeric(BOT$midAmt))
    estDollarSells = sum(as.numeric(SLD$midAmt))
    netDollarAmt = estDollarBuys - estDollarSells
    # portfolio Return
    BOT$portRET = round(BOT$wts*BOT$pctRET,4)
    BOT$portRET[is.na(BOT$portRET)]<-0
    # TOTAL PORTFOLIO RETURN
    repRET = sum(BOT$portRET)
    repVSspy = repRET - bmRET
    OUT = as.data.frame(cbind(paste(x), repRET, bmRET, repVSspy,STATE,estDollarBuys,estDollarSells,netDollarAmt,
                              list(BOT),list(SLD)))
    colnames(OUT) = c("representative","portfolioRet","spyRet","repVSbm","State",
                      "estDollarBuys","estDollarSells","netDollarAmt","BUYS","SELLS")
    # convert to numeric to compare 
    OUT$portfolioRet = OUT$portfolioRet %>% as.numeric()
    OUT$spyRet = OUT$spyRet %>% as.numeric()
    OUT$repVSbm = OUT$repVSbm %>% as.numeric()
    OUT$estDollarBuys = OUT$estDollarBuys %>% as.numeric()
    OUT$estDollarSells = OUT$estDollarSells %>% as.numeric()
    OUT$netDollarAmt = OUT$netDollarAmt %>% as.numeric()
    
    OUT
  })
  # rbind results
  TBL = rbindlist(TBL,use.names = TRUE,fill = TRUE)
  # rank by SPY outperformance
  TBL=TBL[order(TBL$repVSbm, decreasing = TRUE),]
  # return TABLE
  TBL
}

# get Rank of best traders
TBL = bestInvestorRank(STKS = STKS)


