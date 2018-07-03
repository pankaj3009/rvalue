timer.start=Sys.time()
library(zoo)
library(TTR)
library(rredis)
library(log4r)
library(bizdays)
library(RTrade)
library(PerformanceAnalytics)
library(lubridate)


# Parallel version of SMC-LO.1.0.R

#### SETUP ####
options(scipen = 999)

args.commandline = commandArgs(trailingOnly = TRUE)
if (length(args.commandline) > 0) {
        args <- args.commandline
}

redisConnect()
redisSelect(1)
if (length(args) > 1) {
        static <- redisHGetAll(toupper(args[2]))
} else{
        static <- redisHGetAll("SMC-LO")
}

newargs <- unlist(strsplit(static$args, ","))
if (length(args) <= 1 && length(newargs > 1)) {
        args <- newargs
}
redisClose()
backtest=FALSE
kWriteToRedis <- as.logical(static$WriteToRedis)
kGetMarketData <- as.logical(static$GetMarketData)
kBackTestStartDate <- static$BackTestStartDate
kBackTestEndDate <- static$BackTestEndDate
kBackTestCloseAllDate <- static$BackTestCloseAllDate
kNiftyDataFolder <- static$NiftyDataFolder
kTimeZone <- static$TimeZone
kBrokerage <- as.numeric(static$BrokerageOnValue) / 100
kHomeDirectory = static$HomeDirectory
kCommittedCapital = as.numeric(static$CommittedCapital)
kLogFile = static$LogFile
kHolidayFile = static$HolidayFile
kStrategy = args[2]
kExclusionList=static$ExclusionList
kRequireOverSold = as.logical(static$RequireOversold)
kRedisDatabase = as.numeric(args[3])
kExclusionList<-strsplit(kExclusionList[1],",")[[1]]
kSimulation = FALSE

logger <- create.logger()
logfile(logger) <- kLogFile
level(logger) <- 'INFO'
levellog(logger, "INFO", "Starting EOD Scan")


setwd(kHomeDirectory)
holidays <- read.csv(kHolidayFile)
holidays <- as.Date(as.character(holidays[, 1]), "%Y%m%d")
create.calendar(
        holidays = holidays,
        name = "india",
        weekdays = c("saturday", "sunday")
)

kDeployMonths = as.numeric(static$DeployMonths)
kCapitalGrowth = as.numeric(static$CapitalGrowth) / 100
kMinMarketCap = as.numeric(static$MinMarketCap)
kMinOrderValue = as.numeric(static$MinOrderValue)
kUpside = as.numeric(static$Upside)
kThresholdROCE = as.numeric(static$ThresholdROCE)
kThresholdDividedPayoutPercent = as.numeric(static$ThresholdDividedPayoutPercent)
kRSIEntry = as.numeric(static$RSIEntry)
kRSIExit = as.numeric(static$RSIExit)
kR2Fit = as.numeric(static$R2Fit)
kSlope = as.numeric(static$Slope)
kTradesPerMonth = as.numeric(static$TradesPerMonth)
kExitDays = as.numeric(static$ExitDays)
kWorkingDaysForSlope = as.numeric(static$WorkingDaysForSlope)
kBackTestEndDate = strftime(adjust("India", as.Date(kBackTestEndDate, tz = kTimeZone), bdc = 2), "%Y-%m-%d")
kBackTestStartDate = strftime(adjust("India", as.Date(kBackTestStartDate, tz = kTimeZone), bdc = 0), "%Y-%m-%d")
kBackTestCloseAllDate = strftime(adjust("India", as.Date(kBackTestCloseAllDate, tz = kTimeZone), bdc = 0), "%Y-%m-%d")

#### FUNCTIONS ####

MonthsSinceStart <- function(endDate, startDate) {
        #startDate and endDate are to be given in Date format using as.Date()
        length(seq(
                from = startDate,
                to = endDate,
                by = 'month'
        )) - 1
}

CalculateNPV <- function(portfolio, date, path) {
        npv = 0
        realizedprofit = 0
        unrealizedprofit = 0
        for (l in seq_len(nrow(portfolio))) {
                name = portfolio[l, 'symbol']
                if (is.na(portfolio[l, 'exittime'])) {
                        load(paste(path, name, ".Rdata", sep = ""))
                        md<-unique(md)
                        price = md[md$date == date, 'asettle']
                        #print(paste("Date:",as.character(date),sep=""))
                        if (length(price) == 1) {
                                portfolio[l, 'mtm'] = price
                                buyindex = which(md$date == portfolio[l, 'entrytime'])
                                mtmindex = which(md$date== date)
                                portfolio[l, 'mv'] = portfolio[l, 'size'] * price
                                npv = npv + portfolio[l, 'mv']
                                unrealizedprofit = unrealizedprofit + portfolio[l, 'size'] * portfolio[l, 'mtm'] - portfolio[l, 'size'] *
                                        portfolio[l, 'entryprice'] - kBrokerage * portfolio[l, 'size'] * portfolio[l, 'mtm'] - kBrokerage * portfolio[l, 'size'] * portfolio[l, 'entryprice']
                        } else{
                                npv = npv + ifelse(is.na(portfolio[l, 'mv']),0,portfolio[l, 'mv'])
                        }
                } else{
                        load(paste(path, name, ".Rdata", sep = ""))
                        md<-unique(md)
                        buyindex = which(as.Date(md$date, tz = "Asia/Kolkata") == portfolio[l, 'entrytime'])
                        sellindex = which(as.Date(md$date, tz = "Asia/Kolkata") == portfolio[l, "exittime"])
                        realizedprofit = realizedprofit + portfolio[l, 'size'] * portfolio[l, 'exitprice'] - portfolio[l, 'size'] * portfolio[l, 'entryprice'] - kBrokerage * portfolio[l, 'size'] * portfolio[l, 'exitprice'] -
                                kBrokerage * portfolio[l, 'size'] * portfolio[l, 'entryprice']
                }
        }
        list(npv, portfolio, realizedprofit, unrealizedprofit)
}

#### ALGORITHM ####
StatementDate = seq.Date(from = as.Date(kBackTestStartDate),to = min(Sys.Date(),as.Date(kBackTestCloseAllDate)),1)
TargetPortfolioValue = data.frame(date=as.POSIXct(as.character(StatementDate),tz=kTimeZone),value=rep(kCommittedCapital,length(StatementDate)))
MonthsElapsed = sapply(StatementDate, MonthsSinceStart,as.Date(kBackTestStartDate)) +1
TargetPortfolioValue$value = pmin(TargetPortfolioValue$value * MonthsElapsed / kDeployMonths,TargetPortfolioValue$value)
Interest = TargetPortfolioValue$value * kCapitalGrowth / 365
Interest = cumsum(Interest)
TargetPortfolioValue$value = TargetPortfolioValue$value + Interest
# Generate list of symbols
df4files=c("2011_df4.Rdata","2012_df4.Rdata","2013_df4.Rdata","2014_df4.Rdata","2015_df4.Rdata","2016_df4.Rdata","2017_df4.Rdata")
df4files=c("2016_df4.Rdata","2017_df4.Rdata")
symbolchange=getSymbolChange()
symbols=data.frame()
cutoff=as.character(as.Date(kBackTestStartDate)-kWorkingDaysForSlope*2)
buycounter=1
for (f in df4files){
        startdate=paste(strsplit(f,"_")[[1]][1],"-06-30",sep="")
        startdate=as.POSIXct(startdate,tz="Asia/Kolkata")
        enddate=startdate+365*24*60*60
        load(f)
        df4 = df4[df4$UPSIDE > (kUpside - 25) & df4$ROCE>kThresholdROCE & df4$DIVIDENDPAYOUTPERC>kThresholdDividedPayoutPercent &  df4$MCAP > kMinMarketCap, ]
        df4$NEWTICKER=sapply(df4$TICKER,function(x) last(linkedsymbols(symbolchange,x)$symbol))
        symbols=rbind(symbols,data.frame(symbol=df4$NEWTICKER,startdate=rep(startdate,length(df4$NEWTICKER)),enddate=rep(enddate,length(df4$NEWTICKER)),value=df4$THEORETICALVALUE, sharesoutstandingon=as.POSIXct(as.character(df4$sharesOutstandingDate),tz="Asia/Kolkata"),financialdate=as.POSIXct(as.character(df4$FINDATE),tz=kTimeZone), stringsAsFactors = FALSE))
}
print(paste("Number of symbols:",nrow(symbols)))
signals=data.frame()
buycounter=0
for (i in 1:nrow(symbols)){
        print(paste("Processing",i,symbols$symbol[i],sep=":"))
        md=loadSymbol(symbols$symbol[i])
        md=md[md$date>=cutoff,]
        md$eligible=ifelse(md$date>=symbols$startdate[i] & md$date<=symbols$enddate[i],1,0 )
        md$daysinhistory=as.numeric(rownames(md))
        md$month=strftime(md$date,format="%y%m")
        md$r2=RTrade::r2(md$asettle,TRUE,252)
        md$slope=RTrade::slope(md$asettle,TRUE,252)
        if(first(md$date)<=symbols$sharesoutstandingon[i]){
                md$referencesplitadjust=last(md[md$date<symbols$sharesoutstandingon[i],c("splitadjust")])
        }else{
                md$referencesplitadjust=1
        }
       
        md$referencevalue=symbols$value[i]/md$referencesplitadjust # referencevalue is value of share adjusted to asettle...
        md$rsi=RSI(md$asettle, 2)
        if(!kRequireOverSold){
                md$oversold=TRUE
        }else{
                md$oversold=runSum(RSI(md$asettle, 2) < kRSIEntry, 2) == 2
                
        }
        md$buy=ifelse(md$eligible==1 & md$r2>kR2Fit/100 & ((1+md$slope)^kWorkingDaysForSlope-1) >kSlope/100 & md$date > symbols$financialdate[i] + 90*24*60*60 & md$oversold & md$rsi<kRSIEntry & (md$referencevalue-md$asettle)*100/md$asettle>kUpside,1,0)
        md$buycount<-ave(md$buy, md$month, FUN = cumsum)
        md$buy=ifelse(md$buycount==1 & Ref(md$buycount,-1)==0,1,0) # this gives unique  value to buy signal for md
#        md$buy=ifelse(md$buycount>0 & Ref(md$buycount,-1)!=md$buycount,md$buycount,0) # this gives unique  value to buy signal for md
#        md$buy=ifelse(md$buy>0,md$buy+buycounter,0) # this gives unique value for buy signals across signals
        md$sell=ifelse(md$rsi>kRSIExit,1,0)
        md$sellprice=md$asettle
        md$buyprice=md$asettle
        md$positionscore=md$rsi
        signals=rbind(signals,md)
}

signals.filtered=signals[signals$date>=kBackTestStartDate & (signals$buy>0 |signals$sell >0),]
signals.filtered=signals.filtered[order(signals.filtered$date,signals.filtered$positionscore),]
trades=data.frame()
currentmonth=first(signals.filtered$month)
positionsThisMonth=0
for(i in 1:nrow(signals.filtered)){
        s=signals.filtered[i,]
        if(s$month!=currentmonth){
                currentmonth=s$month
                positionsThisMonth=0
        }
        
        if(s$buy>0 && positionsThisMonth<kTradesPerMonth){
                out = CalculateNPV(trades,s$date, kNiftyDataFolder)
                npv=out[[1]]
                gap=TargetPortfolioValue[TargetPortfolioValue$date==s$date,c("value")]-npv
                if(gap>0){
                        sizevalue=gap/(kTradesPerMonth-positionsThisMonth)
                        size=round(sizevalue/s$asettle)
                        trade=data.frame(symbol=s$symbol,trade="BUY",size=size,entrytime=s$date,entryprice=s$asettle,exittime=as.POSIXct(NA_real_,origin="1970-01-01"),exitprice=NA_real_,exitreason="Open",bars=0,stringsAsFactors = FALSE)
                        trades=rbind(trades,trade)
                        positionsThisMonth=positionsThisMonth+1
                }
        }else if (s$sell>0 & nrow(trades)>0){
                sell.opportunties=grep(s$symbol,trades[,c("symbol")])
                if(length(sell.opportunties)>0){
                        for(j in 1:length(sell.opportunties)){
                                sellindex=sell.opportunties[j]
                                if(trades[sellindex,c("exitreason")]=="Open"){
                                        #print(paste("sellindex:",sellindex))
                                        #print(s)
                                        daysheld=difftime(s$date,trades[sellindex,c("entrytime")],"days")
                                        if(daysheld>kExitDays){
                                                trades[sellindex,c("exittime")]=s$date
                                                trades[sellindex,c("exitprice")]=s$asettle
                                                trades[sellindex,c("exitreason")]="RegularExit"
                                        }                                        
                                }
                        }
                }
        }
}

# update mtm
trades.open.index=which(trades$exitreason=="Open")
# update open position mtm price
if(length(trades.open.index)>0){
        for(i in 1:length(trades.open.index)){
                symbol=trades$symbol[trades.open.index[i]]
                md=loadSymbol(symbol,realtime = FALSE)
                md=md[which(as.Date(md$date,tz=kTimeZone)<=min(as.Date(kBackTestEndDate),Sys.Date())),]
                trades$exitprice[trades.open.index[i]]=tail(md$asettle,1)
        }
}

if(!backtest & kWriteToRedis){
        saveRDS(trades,file=paste(args[2],"_trades_",strftime(Sys.time()),sep=""))
        saveRDS(signals,file=paste(args[2],"_signals_",strftime(Sys.time()),sep=""))
        referencetime=adjust("India",Sys.Date()-1,bdc=2)
        referencetime=strftime(referencetime,format="%Y-%m-%d")
        referencetime=as.POSIXct(referencetime,tz=kTimeZone)
        order=data.frame( OrderType="LMT",
                          OrderStage="INIT",
                          TriggerPrice="0",
                          Scale="TRUE",
                          TIF="GTC",
                          OrderReference=tolower(args[2]),
                          stringsAsFactors = FALSE)
        trades$symbol=paste(trades$symbol,"_STK___",sep="")
        placeRedisOrder(trades,referencetime,order,args[3],setLimitPrice=TRUE)
}

timer.end=Sys.time()
runtime=timer.end-timer.start
print(runtime)