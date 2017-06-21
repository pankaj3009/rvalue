library(zoo)
library(TTR)
library(rredis)
library(log4r)
library(bizdays)
library(RTrade)
library(PerformanceAnalytics)
library(lubridate)

options(scipen=999)

args.commandline=commandArgs(trailingOnly=TRUE)
if(length(args.commandline)>0){
        args<-args.commandline
}

redisConnect()
redisSelect(1)
if(length(args)>1){
        static<-redisHGetAll(toupper(args[2]))
}else{
        static<-redisHGetAll("SMC-LO")
}

newargs<-unlist(strsplit(static$args,","))
if(length(args)<=1 && length(newargs>1)){
        args<-newargs
}
redisClose()

kWriteToRedis <- as.logical(static$WriteToRedis)
kGetMarketData<-as.logical(static$GetMarketData)
kBackTestStartDate<-static$BackTestStartDate
kBackTestEndDate<-static$BackTestEndDate
kBackTestCloseAllDate<-static$BackTestCloseAllDate
kNiftyDataFolder <- static$NiftyDataFolder
kTimeZone <- static$TimeZone
kBrokerage<-as.numeric(static$BrokerageOnValue)/100
kHomeDirectory=static$HomeDirectory
kCommittedCapital=as.numeric(static$CommittedCapital)
kLogFile=static$LogFile
kHolidayFile=static$HolidayFile
kStrategy=args[2]
kRedisDatabase=as.numeric(args[3])



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

kDeployMonths=as.numeric(static$DeployMonths)
kCapitalGrowth=as.numeric(static$CapitalGrowth)/100
kMinMarketCap=as.numeric(static$MinMarketCap)
kMinOrderValue=as.numeric(static$MinOrderValue)
kUpside=as.numeric(static$Upside)
kThresholdROCE = as.numeric(static$ThresholdROCE)
kThresholdDividedPayoutPercent=as.numeric(static$ThresholdDividedPayoutPercent)
kRSIEntry=as.numeric(static$RSIEntry)
kRSIExit=as.numeric(static$RSIExit)
kR2Fit=as.numeric(static$R2Fit)
kSlope=as.numeric(static$Slope)
kTradesPerMonth=as.numeric(static$TradesPerMonth)
kExitDays=as.numeric(static$ExitDays)
kWorkingDaysForSlope=as.numeric(static$WorkingDaysForSlope)
kBackTestEndDate=strftime(adjust("India",as.Date(kBackTestEndDate, tz = kTimeZone),bdc=2),"%Y-%m-%d")
kBackTestStartDate=strftime(adjust("India",as.Date(kBackTestStartDate, tz = kTimeZone),bdc=0),"%Y-%m-%d")
kBackTestCloseAllDate=strftime(adjust("India",as.Date(kBackTestCloseAllDate, tz = kTimeZone),bdc=0),"%Y-%m-%d")

#### FUNCTIONS ####

xirr <- function(cf, dates) {
  # Secant method.
  secant <-
    function(par,
             fn,
             tol = 1.e-07,
             itmax = 100,
             trace = FALSE,
             ...) {
      # par = a starting vector with 2 starting values
      # fn = a function whose first argument is the variable of interest
      if (length(par) != 2)
        stop("You must specify a starting parameter vector of length 2")
      p.2 <- par[1]
      p.1 <- par[2]
      f <- rep(NA, length(par))
      f[1] <- fn(p.1, ...)
      f[2] <- fn(p.2, ...)
      iter <- 1
      pchg <- abs(p.2 - p.1)
      fval <- f[2]
      if (trace)
        cat("par: ", par, "fval: ", f, "\n")
      while (pchg >= tol & abs(fval) > tol & iter <= itmax) {
        p.new <- p.2 - (p.2 - p.1) * f[2] / (f[2] - f[1])
        pchg <- abs(p.new - p.2)
        fval <- ifelse(is.na(fn(p.new, ...)), 1, fn(p.new, ...))
        p.1 <- p.2
        p.2 <- p.new
        f[1] <- f[2]
        f[2] <- fval
        iter <- iter + 1
        if (trace)
          cat("par: ", p.new, "fval: ", fval, "\n")
      }
      list(par = p.new,
           value = fval,
           iter = iter)
    }
  
  # Net present value.
  npv <-
    function(irr, cashflow, times)
      sum(cashflow / (1 + irr) ^ times)
  
  times <-
    as.numeric(difftime(dates, dates[1], units = "days")) / 365.24
  
  r <- secant(
    par = c(0, 0.1),
    fn = npv,
    cashflow = cf,
    times = times
  )
  
  return(r$par)
}


slope <- function (x) {
  res <- (lm(log(x) ~ seq(1:length(x))))
  res$coefficients[2]
}

r2 <- function(x) {
  res <- (lm(log(x) ~ seq(1:length(x))))
  summary(res)$r.squared
}

MonthsSinceStart <- function(endDate, startDate) {
  #startDate and endDate are to be given in Date format using as.Date()
  length(seq(from = startDate, to = endDate, by = 'month')) - 1
}

CalculateNPV <- function(portfolio, date, path) {
  npv = 0
  realizedprofit = 0
  unrealizedprofit = 0
  if (nrow(portfolio) > 0) {
    for (l in 1:nrow(portfolio)) {
      name = portfolio[l, 'symbol']
      if (is.na(portfolio[l, 'exittime'])) {
        load(paste(path, name, ".Rdata", sep = ""))
        price = md[as.Date(md$date, tz = "Asia/Kolkata") == date, 'settle']
        #print(paste("Date:",as.character(date),sep=""))
        if (length(price) == 1) {
          portfolio[l, 'mtm'] = price
          buyindex= which(as.Date(md$date, tz = "Asia/Kolkata") == portfolio[l,'entrytime'])
          mtmindex= which(as.Date(md$date, tz = "Asia/Kolkata") == date)
          splitadjustment=md$splitadjust[buyindex]/md$splitadjust[mtmindex]
          portfolio[l, 'mv'] = portfolio[l, 'size'] * price * splitadjustment
          npv = npv + portfolio[l, 'mv']
          unrealizedprofit = unrealizedprofit + portfolio[l, 'size'] * splitadjustment* portfolio[l, 'mtm'] - portfolio[l, 'size']*portfolio[l, 'entryprice'] - kBrokerage * portfolio[l, 'size'] * splitadjustment * portfolio[l, 'mtm'] - kBrokerage * portfolio[l, 'size'] * portfolio[l, 'entryprice']
        } else{
          npv = npv + ifelse(is.na(portfolio[l, 'mv']), 0, portfolio[l, 'mv'])
        }
      } else{
              load(paste(path, name, ".Rdata", sep = ""))
              buyindex= which(as.Date(md$date, tz = "Asia/Kolkata") == portfolio[l,'entrytime'])
              sellindex= which(as.Date(md$date, tz = "Asia/Kolkata") == portfolio[l,"exittime"])
              splitadjustment=md$splitadjust[buyindex]/md$splitadjust[sellindex]
        realizedprofit = realizedprofit + portfolio[l, 'size'] * splitadjustment* portfolio[l, 'exitprice'] - portfolio[l, 'size']* portfolio[l, 'entryprice'] - kBrokerage * portfolio[l, 'size'] * splitadjustment * portfolio[l, 'exitprice'] -kBrokerage * portfolio[l, 'size']*portfolio[l, 'entryprice']
      }
    }
  }
  list(npv, portfolio, realizedprofit, unrealizedprofit)
}

GetDF4FileName <- function(date) {
  fileName = ""
  year = substring(date, 1, 4)
  month = substr(date, 6, 7)
  if (as.numeric(month) <= 6) {
    year = as.numeric(year) - 1
  }
  fileName = paste(year, "_df4.Rdata", sep = "")
  fileName
}

UpdateDF4Upside <- function(df4, settledate,datafolder,WorkingDaysForSlope=252) {
        redisConnect()
        redisSelect(2)
        #update splits
        a<-unlist(redisSMembers("splits")) # get values from redis in a vector
        tmp <- (strsplit(a, split="_")) # convert vector to list
        k<-lengths(tmp) # expansion size for each list element
        allvalues<-unlist(tmp) # convert list to vector
        splits <- data.frame(date=1:length(a), symbol=1:length(a),oldshares=1:length(a),newshares=1:length(a),reason=rep("",length(a)),stringsAsFactors = FALSE)
        for(i in 1:length(a)) {
                for(j in 1:k[i]){
                        runsum=cumsum(k)[i]
                        splits[i, j] <- allvalues[runsum-k[i]+j]
                }
        }
        splits$date=as.POSIXct(splits$date,format="%Y%m%d",tz="Asia/Kolkata")
        splits$oldshares<-as.numeric(splits$oldshares)
        splits$newshares<-as.numeric(splits$newshares)
        
        #update symbol change
        a<-unlist(redisSMembers("symbolchange")) # get values from redis in a vector
        tmp <- (strsplit(a, split="_")) # convert vector to list
        k<-lengths(tmp) # expansion size for each list element
        allvalues<-unlist(tmp) # convert list to vector
        symbolchange <- data.frame(date=rep("",length(a)), key=rep("",length(a)),newsymbol=rep("",length(a)),stringsAsFactors = FALSE)
        for(i in 1:length(a)) {
                for(j in 1:k[i]){
                        runsum=cumsum(k)[i]
                        symbolchange[i, j] <- allvalues[runsum-k[i]+j]
                }
        }
        symbolchange$date=as.POSIXct(symbolchange$date,format="%Y%m%d",tz="Asia/Kolkata")
        symbolchange$key = gsub("[^0-9A-Za-z/-]", "", symbolchange$key)
        symbolchange$newsymbol = gsub("[^0-9A-Za-z/-]", "", symbolchange$newsymbol)
        redisClose()
        symbolchange<-symbolchange[symbolchange$date<=as.POSIXct(date,format="%Y%m%d",tz="Asia/Kolkata"),]
        #update symbols with current symbol
        matches<-match(df4$TICKER,symbolchange$key)
        df4indicestochange<-which(!is.na(matches))
        newsymbolindices=matches[which(!is.na(matches))]
        df4[df4indicestochange,]$TICKER<-symbolchange$newsymbol[newsymbolindices]
        
        for (i in 1:nrow(df4)) {
                symbol = df4[i, 'TICKER']
                if (file.exists(paste(
                        datafolder,
                        symbol,
                        ".Rdata",
                        sep = ""
                ))) {
                        load(paste(
                                datafolder,
                                symbol,
                                ".Rdata",
                                sep = ""
                        ))
                        endlength = which(md$date == settledate)
                        if (length(endlength) == 1 && endlength > (WorkingDaysForSlope-1)) {
                                #print(md$symbol)
                                df4$CurrentRSI[i] = RSI(md$asettle, 2)[endlength]
                                startlength = endlength - (WorkingDaysForSlope-1)
                                OverSold = runSum(RSI(md$asettle, 2) < 20, 2) == 2
                                df4$OverSold[i] = OverSold[endlength]
                                df4$AnnualizedSlope[i] = exp(slope(md$asettle[startlength:endlength])) ^
                                        WorkingDaysForSlope - 1
                                df4$r[i] = r2(md$asettle[startlength:endlength])
                                df4$sumproduct[i] = df4$AnnualizedSlope[i] * df4$r[i]
                                lastprice = md$asettle[endlength]
                                shareoutstandingindex=tail(which(as.Date(md$date,tz="Asia/Kolkata")<=df4$sharesOutstandingDate[i]),1)
                                if(length(shareoutstandingindex)==1){
                                        if(!is.na(df4$sharesOutstandingDate) && as.Date(settledate,tz="Asia/Kolkata")>df4$sharesOutstandingDate){
                                                df4$THEORETICALVALUE[i]=df4$THEORETICALVALUE[i]/md[shareoutstandingindex,'splitadjust']
                                                #print(paste(symbol,df4$sharesOutstandingDate,settledate,sep=","))
                                        }
                                        df4$UPSIDE[i] = (df4$THEORETICALVALUE[i] - lastprice) * 100 / lastprice
                                        df4$UPSIDE[i] = trunc(df4$UPSIDE[i], 0)
                                        df4$UPSIDE[i] = as.numeric(df4$UPSIDE[i])
                                        df4$LATEST_SHARE_PRICE[i] = lastprice
                                        df4$MCAP[i] = as.numeric(df4$COMMON_SHARE_OUTSTANDING_IN_MILLIONS[i]) *
                                                lastprice* md[shareoutstandingindex,'splitadjust'] / 1000
                                        
                                }
                        }else{
                                # data does not exist for a year...
                                df4$UPSIDE[i] = -10000
                        }
                }
        }
        df4
}

CashFlow <- function(portfolio, settledate,brokerage) {
        vcash = rep(0, length(settledate))
        for (p in 1:nrow(portfolio)) {
                index = which(settledate == as.Date(portfolio[p, 'entrytime'], tz = "Asia/Kolkata"))
                vcash[index] = vcash[index] - portfolio[p, 'size'] * portfolio[p, 'entryprice']- portfolio[p, 'size'] * portfolio[p, 'entryprice']*brokerage
        }
        for (p in 1:nrow(portfolio)) {
                if (!is.na(portfolio[p, 'exittime'])) {
                        index = which(settledate == as.Date(portfolio[p, 'exittime'], tz = "Asia/Kolkata"))
                        vcash[index] = vcash[index] + portfolio[p, 'size'] *
                                portfolio[p, 'exitprice'] -portfolio[p, 'size'] *portfolio[p, 'exitprice']*brokerage
                }
        }
        vcash
        
}

UpdatePortfolioBuy <-
  function(portfolio,
           shortlist,
           date,
           value,
           month,
           path) {
    if (nrow(shortlist) > 0) {
      for (s in 1:nrow(shortlist)) {
        symbol = shortlist[s, ]$TICKER
        load(paste(path, symbol, ".Rdata", sep = ""))
        price = md[as.Date(md$date, tz = "Asia/Kolkata") == date, "settle"]
        if (length(price) > 0) {
          size = as.integer(value / price)
          if (size > 0) {
            print(paste("Entry Date:", date, " ,symbol:", symbol, sep = ""))
            portfolio <- rbind(
              portfolio,
              data.frame(
                symbol = symbol,
                trade="BUY",
                size = size,
                entrytime = date,
                entryprice = price,
                exittime = NA_character_,
                exitprice = NA_real_,
                mtm = NA_real_,
                mv = NA_real_,
                month = month,
                profit= NA_real_,
                stringsAsFactors = FALSE
              )
            )
          }
        }
      }
    }
    portfolio
  }

# GetCurrentPosition <- function(symbol, portfolio) {
#   position <- 0
#   if(nrow(portfolio)>0){
#     for (row in 1:nrow(portfolio)) {
#       if (is.na(portfolio[row, 'exittime']) &&
#           portfolio[row, 'symbol'] == symbol) {
#         position = position + portfolio[row, 'size']
#       }
#     }   
#   }
#   position
# }





#### ALGORITHM ####
#DaysSinceStart = as.numeric(Sys.Date() - as.Date(StrategyStartDate)) + 1
Summary = data.frame(
        irr = as.numeric(),
        profit = as.numeric(),
        winratio = as.numeric(),
        stringsAsFactors = FALSE
)

StatementDate = seq.Date(from = as.Date(kBackTestStartDate), to = min(Sys.Date(),as.Date(kBackTestCloseAllDate)), 1)
TargetPortfolioValue = numeric(length(StatementDate))
ActualPortfolioValue = rep(NA_real_, length(StatementDate))
Gap = rep(NA_real_, length(StatementDate))
RealizedProfit = rep(NA_real_, length(StatementDate))
UnRealizedProfit = rep(NA_real_, length(StatementDate))

TargetPortfolioValue = rep(kCommittedCapital, length(StatementDate))
Cash = rep(0, length(StatementDate))
MonthsElapsed = sapply(StatementDate, MonthsSinceStart, as.Date(kBackTestStartDate)) +
        1
TargetPortfolioValue = pmin(TargetPortfolioValue * MonthsElapsed / kDeployMonths,
                            TargetPortfolioValue)
# allow buildup of portfolio with interest
Interest = TargetPortfolioValue * kCapitalGrowth / 365
Interest = cumsum(Interest)
TargetPortfolioValue = TargetPortfolioValue + Interest
indexOfSystematicSellStart = which(StatementDate == min(Sys.Date(),as.Date(kBackTestEndDate) +
                                           1))
indexOfSystematicSellEnd = which(StatementDate == min(Sys.Date(), as.Date(kBackTestCloseAllDate)))
TargetPortfolioValue[indexOfSystematicSellStart:indexOfSystematicSellEnd] =
        seq(
                from = TargetPortfolioValue[indexOfSystematicSellStart],
                to = 0,
                length.out = (indexOfSystematicSellEnd - indexOfSystematicSellStart + 1)
        )
if (indexOfSystematicSellEnd < length(TargetPortfolioValue)) {
        TargetPortfolioValue[indexOfSystematicSellEnd + 1:length(TargetPortfolioValue)] =
                0
}

if (args[1]==2 || !file.exists(paste("Portfolio_",args[2],".Rdata",sep=""))) {
        Portfolio = data.frame(
                symbol = as.character(),
                size = as.numeric(),
                trade=as.character(),
                entrytime = as.character(),
                entryprice = as.numeric(),
                exittime = as.character(),
                exitprice = as.numeric(),
                mtm = as.numeric(),
                mv = as.numeric(),
                month = as.numeric(),
                profit=as.numeric(),
                stringsAsFactors = FALSE
        )
}else{
        load(paste("Portfolio_",args[2],".Rdata",sep=""))
}

StartingDate = as.Date(kBackTestStartDate)
if (nrow(Portfolio) > 0) {
        for (row in 1:nrow(Portfolio)) {
                if (!is.na(Portfolio[row, 'exittime'])) {
                        tempDate = as.Date(Portfolio[row, 'entrytime'])
                        StartingDate = ifelse(tempDate > StartingDate, tempDate, StartingDate)
                } else{
                        tempDate = as.Date(Portfolio[row, 'entrytime'])
                        StartingDate = ifelse(tempDate > StartingDate, tempDate, StartingDate)
                }
        }
        StartingDate = adjust.next(as.Date(StartingDate) + 1, "india")
}


StartingIndex = which(StatementDate == StartingDate)

#for (d in StartingIndex:399) {
for (d in StartingIndex:length(StatementDate)) {
        date = StatementDate[d]
        if (length(grep("S(at|un)", weekdays(date, abbr = TRUE))) == 0) {
                print(paste("Processing date:", date, sep = ""))
                #weekday
                out = CalculateNPV(Portfolio, date, kNiftyDataFolder)
                npv = out[[1]]
                Portfolio = out[[2]]
                RealizedProfit[d] = out[[3]]
                UnRealizedProfit[d] = out[[4]]
                ActualPortfolioValue[d] = npv
                Gap[d] = TargetPortfolioValue[d] - npv
                CurrentMonth = MonthsElapsed[d]
                
                # Sell Portfolio
                if (nrow(Portfolio) > 0) {
                        for (p in 1:nrow(Portfolio)) {
                                DaysSincePurchase = as.numeric(date - as.Date(Portfolio[p, 'entrytime']))
                                if (is.na(Portfolio[p, 'exittime']) &&
                                    DaysSincePurchase > kExitDays) {
                                        symbol = Portfolio[p, 'symbol']
                                        load(paste(
                                                kNiftyDataFolder,
                                                symbol,
                                                ".Rdata",
                                                sep = ""
                                        ))
                                        OverBought = runSum(RSI(md$asettle, 2) > kRSIExit,
                                                            2) == 2
                                        enddate = which(
                                                as.Date(md$date, tz = "Asia/Kolkata") == date
                                        )
                                        if ((length(enddate) > 0 &&
                                             OverBought[enddate] == TRUE) ||
                                            (
                                                    length(enddate) > 0 &&
                                                    as.Date(md$date[enddate]) >= as.Date(
                                                            kBackTestCloseAllDate
                                                    )
                                            )) {
                                                print(paste(
                                                        "exit d:",
                                                        d,
                                                        sep = ""
                                                ))
                                                origposition<-GetCurrentPosition(symbol,Portfolio,path=kNiftyDataFolder,position.on = date)
                                                Portfolio[p, 'exittime'] = as.character(date)
                                                Portfolio[p, 'exitprice'] = md$settle[enddate]
                                                if(kWriteToRedis){
                                                        redisConnect()
                                                        redisSelect(kRedisDatabase)
                                                        buyindex= which(as.Date(md$date, tz = "Asia/Kolkata") == Portfolio[p,'entrytime'])
                                                        sellindex= which(as.Date(md$date, tz = "Asia/Kolkata") == date)
                                                        size = Portfolio[p, 'size']*md$splitadjust[buyindex]/md$splitadjust[sellindex]
                                                        size=floor(size)
                                                        longname = paste(symbol, "_STK___", sep = "")
                                                        if (size > 0) {
                                                                redisRPush(paste("trades", kStrategy, sep = ":"),
                                                                           charToRaw(
                                                                                   paste(longname, size, "SELL", 0, origposition, sep = ":")
                                                                           ))
                                                        }
                                                        
                                                        levellog(logger,
                                                                 "INFO",
                                                                 paste(longname, size, "SELL", 0, position, sep = ":"))
                                                        redisClose()
                                                }
                                        }
                                }
                        }
                }
                
                #Now Scan for Buys
                if (nrow(Portfolio) > 0) {
                        DistinctPurchasesThisMonth = length(unique(Portfolio[Portfolio$month == CurrentMonth, c("symbol")]))
                } else{
                        DistinctPurchasesThisMonth = 0
                }
                #print(paste("Processing Buy. Gap:", Gap,",MinOrderValue:",MinOrderValue,",DistinctPurchasesThisMonth:",DistinctPurchasesThisMonth, ",date:",date,sep = ""))
                if (Gap[d] > kMinOrderValue &&
                    DistinctPurchasesThisMonth < kTradesPerMonth &&
                    date < as.Date(kBackTestEndDate)) {
                        load(GetDF4FileName(date))
                        df4 = df4[df4$UPSIDE > (kUpside -50),] # get a smaller list of df4 that has a positive upside
                        df4 <-
                                UpdateDF4Upside(df4, as.character(date),kNiftyDataFolder)
                        # print(paste("Processing Buy for d2:", d,",date:",date, sep = ""))
                        shortlist <-
                                df4[df4$UPSIDE > kUpside &
                                            df4$DIVIDENDPAYOUTPERC > kThresholdDividedPayoutPercent &
                                            df4$ROCE > kThresholdROCE &
                                            df4$AnnualizedSlope > kSlope /
                                            100  &
                                            df4$r > kR2Fit / 100 &
                                            df4$CurrentRSI < kRSIEntry &
                                            df4$FINDATE + 90 < date &
                                            df4$MCAP > kMinMarketCap , ]
                        #df4$FINDATE+90 < date covers scenarios where the FINANCIALS are forward looking
                        existingSymbols <-
                                unique(Portfolio[Portfolio$month == CurrentMonth, c("symbol")])
                        dupes = match(existingSymbols, shortlist$TICKER)
                        dupes <- dupes[!is.na(dupes)]
                        if (length(dupes > 0)) {
                                shortlist <- shortlist[-dupes, ]
                        }
                        if (DistinctPurchasesThisMonth < kTradesPerMonth &&
                            nrow(shortlist) > kTradesPerMonth - DistinctPurchasesThisMonth) {
                                shortlist <- shortlist[1:(kTradesPerMonth - DistinctPurchasesThisMonth), ]
                        }
                        
                        if (nrow(shortlist) > 0 &&
                            DistinctPurchasesThisMonth < kTradesPerMonth) {
                                InvestmentValue = Gap[d] / (kTradesPerMonth - DistinctPurchasesThisMonth)
                                # write to redis
                                if(kWriteToRedis){
                                        redisConnect()
                                        redisSelect(kRedisDatabase)
                                        for (row in 1:nrow(shortlist)) {
                                                scrip = shortlist[row, 'TICKER']
                                                load(paste(kNiftyDataFolder, scrip, ".Rdata", sep = ""))
                                                price = md[as.Date(md$date, tz = "Asia/Kolkata") == date, 'settle']
                                                size = as.integer(InvestmentValue / price)
                                                longname = paste(scrip, "_STK___", sep = "")
                                                if (size > 0) {
                                                        position=GetCurrentPosition(scrip,Portfolio,path=kNiftyDataFolder,position.on = date)
                                                        #position = GetCurrentPosition(scrip, Portfolio)
                                                        redisRPush(paste("trades", kStrategy, sep = ":"),
                                                                   charToRaw(
                                                                           paste(longname, size, "BUY", 0, position, sep = ":")
                                                                   ))
                                                }
                                        }
                                        levellog(logger,
                                                 "INFO",
                                                 paste(longname, size, "BUY", 0, position, sep = ":"))
                                        redisClose()
                                }
                                # update portfolio
                                print(
                                        paste(
                                                "entry d:",
                                                d,
                                                "InvestmentValue:",
                                                InvestmentValue,
                                                sep = ""
                                        )
                                )
                                Portfolio <-
                                        UpdatePortfolioBuy(
                                                Portfolio,
                                                shortlist,
                                                date,
                                                InvestmentValue,
                                                CurrentMonth,
                                                kNiftyDataFolder
                                        )
                        }
                }
                
                
        }
}
#Cleanup values before reporting
out = CalculateNPV(Portfolio, date, kNiftyDataFolder)
npv = out[[1]]
Portfolio = out[[2]]
RealizedProfit[d] = out[[3]]
UnRealizedProfit[d] = out[[4]]

#stopCluster(cl)
UnRealizedProfit <- na.locf(UnRealizedProfit, na.rm = FALSE)
UnRealizedProfit <- na.locf(UnRealizedProfit, fromLast = TRUE)

RealizedProfit <- na.locf(RealizedProfit, na.rm = FALSE)
RealizedProfit <- na.locf(RealizedProfit, fromLast  = TRUE)

Gap <- na.locf(Gap, na.rm = FALSE)
Gap <- na.locf(Gap, fromLast = TRUE)

maxdddate = which(RealizedProfit + UnRealizedProfit == min(RealizedProfit +
                                                                   UnRealizedProfit))

cashflow <- CashFlow(Portfolio, StatementDate,kBrokerage)
cashflow[length(cashflow)] <- cashflow[length(cashflow)] + npv
if(sum(cashflow)>0){
        irr <- xirr(cashflow, StatementDate) * 100
        print(paste("xirr:", irr, sep = ""))
        
}else{
        irr=0
}

naindices=which(is.na(Portfolio$mtm))
if(length(naindices)>0){
        Portfolio[naindices,]$mtm<-Portfolio[naindices,]$entryprice
        Portfolio[naindices,]$profit<-0       
}
Portfolio$exitprice<-ifelse(is.na(Portfolio$exittime),Portfolio$mtm,Portfolio$exitprice)

for(p in 1:nrow(Portfolio)){
        load(paste(kNiftyDataFolder, Portfolio[p,'symbol'], ".Rdata", sep = ""))
        buyindex= which(as.Date(md$date, tz = "Asia/Kolkata") == Portfolio[p,'entrytime'])
        sellindex= ifelse(is.na(Portfolio$exittime[p]),nrow(md),which(as.Date(md$date, tz = "Asia/Kolkata") == Portfolio[p,'exittime']))
        splitadjust=md$splitadjust[buyindex]/md$splitadjust[sellindex]
        Portfolio$profit[p]=Portfolio$size[p] *(splitadjust * Portfolio$exitprice[p] - Portfolio$entryprice[p])- kBrokerage * Portfolio$size[p] * (Portfolio$exitprice[p]*splitadjust + Portfolio$entryprice[p])
     }



if(args[1]==1){
        save(Portfolio,file=paste("Portfolio_",args[2],".Rdata",sep=""))
}

if(args[1]==2){ # BackTest
        winratio <-
                sum((
                        ifelse(is.na(Portfolio$mtm), Portfolio$entryprice, Portfolio$mtm) - Portfolio$entryprice
                ) >= 0) / nrow(Portfolio)
        profit = sum(Portfolio$profit[complete.cases(Portfolio$profit)])
        
        Summary <-
                rbind(Summary,
                      data.frame(
                              irr = irr,
                              winratio = winratio,
                              profit = profit
                      ))

        ActualPortfolioValue <-
                ifelse(ActualPortfolioValue == 0, NA_real_, ActualPortfolioValue)
        ActualPortfolioValue <-
                na.locf(ActualPortfolioValue, na.rm = FALSE)
        ActualPortfolioValue <-
                ifelse(is.na(ActualPortfolioValue), 0, ActualPortfolioValue)
        # DailyPNL<-RealizedProfit+UnRealizedProfit
        # workingdays<-which(DailyPNL!=Ref(DailyPNL,-1))
        # DailyPNLWorking<-DailyPNL[workingdays]-Ref(DailyPNL[workingdays],-1)
        # DailyPNLWorking <- ifelse(is.na(DailyPNLWorking), 0, DailyPNLWorking)
        # StatementDateWorking<-StatementDate[workingdays]
        # DailyReturnWorking<-ifelse(ActualPortfolioValue[workingdays] == 0, 0, DailyPNLWorking / ActualPortfolioValue[workingdays])
        # df <- data.frame(time = StatementDate[workingdays], return = DailyReturnWorking)
        # df <- read.zoo(df)
        # sharpe <-
        #         SharpeRatio((df[df != 0][, 1, drop = FALSE]), Rf = .07 / 365, FUN = "StdDev") *
        #         sqrt(kWorkingDaysForSlope)
        
        pnl<-data.frame(bizdays=as.Date(StatementDate,tz=kTimeZone),realized=0,unrealized=0,brokerage=0)
        cumpnl<-CalculateDailyPNL(Portfolio,pnl,kNiftyDataFolder,kBrokerage,per.contract.brokerage = FALSE,deriv=FALSE)
        DailyPNL<-cumpnl$realized+cumpnl$unrealized-cumpnl$brokerage
        workingdays<-which(DailyPNL!=Ref(DailyPNL,-1))
        DailyPNLWorking<-DailyPNL[workingdays]-Ref(DailyPNL[workingdays],-1)
        DailyPNLWorking <- ifelse(is.na(DailyPNLWorking), 0, DailyPNLWorking)
        StatementDateWorking<-StatementDate[workingdays]
        DailyReturnWorking<-ifelse(ActualPortfolioValue[workingdays] == 0, 0, DailyPNLWorking / ActualPortfolioValue[workingdays])
        sharpe<-sharpe(DailyReturnWorking)
        
        my.range <- range(ActualPortfolioValue)
        plot(x = StatementDate,
             y = ActualPortfolioValue,
             type = 'l', main="Portfolio Buildup",ylab="INR",xlab="Date",axes=FALSE,ylim=my.range,lty=1)
        my.legend.size <-legend("topright",c("Actual Portfolio Value","Target Portfolio Value","Gap"),plot = FALSE)
        my.range[2] <- 1.04*(my.range[2]+my.legend.size$rect$h)
        axis.Date(1,StatementDate,at=seq(min(StatementDate), max(StatementDate)+90, by="3 mon"),, format="%m-%Y")
        axis(2,at=seq(-5000000,50000000,5000000),labels=paste(seq(-5,50,5),"M",sep=""),las=1)
        lines(x = StatementDate,
              y = TargetPortfolioValue,
              ylab="",xlab="",lty=2)
        lines(x = StatementDate,
              y = (TargetPortfolioValue-ActualPortfolioValue),
              ,xlab="",ylab="",lty=3)
        legend("topleft",lty=c(1,2,3),
               legend=c("Actual Portfolio Value","Target Portfolio Value","Gap"), cex=0.85, bty="n", xjust=1)
        
        # Profit and Loss Graph
        plot(x = StatementDate,
             y = (cumpnl$realized + cumpnl$unrealized),
             type = 'l',main="Profit & Loss",xlab="Date",ylab="Profit",axes=FALSE)
        axis.Date(1,StatementDate,at=seq(min(StatementDate), max(StatementDate)+90, by="3 mon"),, format="%m-%Y")
        minProfit=min(cumpnl$realized+cumpnl$unrealized)
        maxProfit=max(cumpnl$realized+cumpnl$unrealized)
        points=pretty(seq(minProfit,maxProfit,by=(maxProfit-minProfit)/5))
        axis(2,at=points,labels=paste(points/1000000,"M",sep=""),las=1)
        
        # DailyReturn
        plot(x = StatementDateWorking,
             y = DailyReturnWorking,
             type = 'l',main="DailyReturn",xlab="Date",ylab="Return",axes=FALSE)
        axis.Date(1,StatementDate,at=seq(min(StatementDateWorking), max(StatementDateWorking)+90, by="3 mon"),, format="%m-%Y")
        minProfit=min( DailyReturnWorking)
        maxProfit=max( DailyReturnWorking)
        points=pretty(seq(minProfit,maxProfit,by=(maxProfit-minProfit)/5))
        axis(2,at=points,labels=paste(points*100,"%",sep=""),las=1)
        
        #Cash Buildup
        plot(x = StatementDate,
             y = ifelse(cumsum(cashflow)<0,cumsum(cashflow),0),
             type = 'l',main="Deployed Cash",xlab="Date",ylab="Deployed Cash",axes=FALSE)
        axis.Date(1,StatementDate,at=seq(min(StatementDate), max(StatementDate)+90, by="3 mon"),, format="%m-%Y")
        minProfit=min(cumsum(cashflow))
        maxProfit=0
        points=pretty(seq(minProfit,maxProfit,by=(maxProfit-minProfit)/5))
        axis(2,at=points,labels=paste(points/1000000,"M",sep=""),las=1)
}
