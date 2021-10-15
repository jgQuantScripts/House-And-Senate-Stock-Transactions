require("XML");require("xml2");require("stringr");require("jsonlite");require("lubridate")
require("RQuantLib");require("data.table");require("quantmod");require("pbapply")
# API - source
# https://senatestockwatcher.com/api
# ******************************************************************************************************
#                                         files Map/Index
# ******************************************************************************************************
#  https://senate-stock-watcher-data.s3-us-west-2.amazonaws.com/aggregate/filemap.xml
tmp <- xml2::read_xml(x = paste0("https://senate-stock-watcher-data.s3-us-west-2.amazonaws.com/",
                                 "aggregate/filemap.xml"))
# extract all json file names available 
getNames = lapply(as.list(1:length(xml_children(tmp))), function(ii){
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
senateTrades = function(fileN){
  # extract file  
  url = paste0("https://senate-stock-watcher-data.s3-us-west-2.amazonaws.com/",fileN)
  # read data
  TODAY = read_json(url,simplifyVector=TRUE)
  # extract contents in json 
  tDate = TODAY[[6]][[1]]$transaction_date
  owner = TODAY[[6]][[1]]$owner
  ticker = do.call(rbind,lapply(as.list(1:length(tDate)),
                                function(ii) html2txt(TODAY[[6]][[1]]$ticker[ii])))
  DESC = do.call(rbind,lapply(as.list(1:length(tDate)),
                              function(ii) html2txt2(ii,TODAY=TODAY)))
  assetType = TODAY[[6]][[1]]$asset_type
  botSold   = TODAY[[6]][[1]]$type
  AMT       = gsub(" ","",gsub("\\,","",gsub("\\$","",TODAY[[6]][[1]]$amount)))
  COMMENT   = TODAY[[6]][[1]]$comment
  SENATORn  = TODAY[[3]]
  reportDate= TODAY[[5]]
  Source    = TODAY[[4]]
  # put everything together
  df = as.data.frame(cbind(tDate,owner,ticker,DESC,assetType,
                           botSold,AMT,COMMENT,SENATORn,reportDate,Source))
  # format column names
  colnames(df) = c("transactionDate","owner","ticker","assetDescription",
                   "assetType","buyORsell","amount","comment","name","reportDate","source")
  # return data.frame
  df
}
# test function
trades = senateTrades(getNames[1])
# ******************************************************************************************************
#                                         read ALL Data
# ******************************************************************************************************
getAllSenateTrades = function(){
  # extract file  
  url = paste0("https://senate-stock-watcher-data.s3-us-west-2.amazonaws.com/aggregate/",
               "all_transactions.json")
  # read data
  TODAY = read_json(url,simplifyVector=TRUE)
  # extract contents in json 
  tDate  = TODAY$transaction_date
  owner  = TODAY$owner
  ticker = gsub("\\N/A","--",TODAY$ticker)
  DESC = do.call(rbind,lapply(as.list(1:nrow(TODAY)),
                              function(ii) html2txt(TODAY$asset_description[ii])))
  assetType = TODAY$asset_type
  botSold   = TODAY$type
  AMT       = gsub(" ","",gsub("\\,","",gsub("\\$","",TODAY$amount)))
  COMMENT   = TODAY$comment
  SENATORn  = TODAY$senator
  reportDate= TODAY$disclosure_date
  Source    = TODAY$ptr_link
  # put everything together
  df = as.data.frame(cbind(tDate,owner,ticker,DESC,assetType,
                           botSold,AMT,COMMENT,SENATORn,reportDate,Source))
  # format column names
  colnames(df) = c("transactionDate","owner","ticker","assetDescription",
                   "assetType","buyORsell","amount","comment","name","reportDate","source")
  # return data.frame
  df
}
ALL = getAllSenateTrades()
#write.table(ALL,paste0("~/Desktop/SenatorTrades.csv"),sep=",")

# Stock Ranking - what are they buying:
getRanking = function(ALL,from,to){
# only want YTD
ALL$transactionDate = as.Date(ALL$transactionDate,format="%m/%d/%Y")
ALL = subset(ALL, ALL$transactionDate >= as.Date(from) &
             ALL$transactionDate <= as.Date(to))

df = as.data.frame(unique(ALL$ticker))
colnames(df) = "ticker"

# column for purchases
df$Purchases = do.call(rbind,lapply(as.list(1:nrow(df)), function(ii){
  nrow(subset(ALL,ALL$ticker == df$ticker[ii] & ALL$buyORsell=="Purchase"))
}))
# column for Partial Sales
df$PartialSales = do.call(rbind,lapply(as.list(1:nrow(df)), function(ii){
  nrow(subset(ALL,ALL$ticker == df$ticker[ii] & ALL$buyORsell=="Sale (Partial)"))
}))
# column for Full Sales
df$FullSales = do.call(rbind,lapply(as.list(1:nrow(df)), function(ii){
  nrow(subset(ALL,ALL$ticker == df$ticker[ii] & ALL$buyORsell=="Sale (Full)"))
}))
# column for Exchanges
df$Exchanges = do.call(rbind,lapply(as.list(1:nrow(df)), function(ii){
  nrow(subset(ALL,ALL$ticker == df$ticker[ii] & ALL$buyORsell=="Exchange"))
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
