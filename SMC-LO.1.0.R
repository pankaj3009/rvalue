library(zoo)
library(TTR)
library(rredis)
library(log4r)
library(bizdays)
library(RTrade)
library(PerformanceAnalytics)
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
        static<-redisHGetAll("VALUE01")
}

newargs<-unlist(strsplit(static$args,","))
if(length(args)<=1 && length(newargs>1)){
        args<-newargs
}
redisClose()

kWriteToRedis <- as.logical(static$WriteToRedis)
kGetMarketData<-as.logical(static$GetMarketData)
kDataCutOffBefore<-static$DataCutOffBefore
kBackTestStartDate<-static$BackTestStartDate
kBackTestEndDate<-static$BackTestEndDate
kFNODataFolder <- static$FNODataFolder
kNiftyDataFolder <- static$NiftyDataFolder
kTimeZone <- static$TimeZone
kBrokerage<-as.numeric(static$SingleLegBrokerageAsPercentOfValue)/100
kPerContractBrokerage=as.numeric(static$SingleLegBrokerageAsValuePerContract)
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
kCapitalGrowth=as.numeric(static$CapitalGrowth)
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
      name = portfolio[l, 'scrip']
      if (is.na(portfolio[l, 'sellprice'])) {
        load(paste(path, name, ".Rdata", sep = ""))
        price = md[as.Date(md$date, tz = "Asia/Kolkata") == date, 'asettle']
        #print(paste("Date:",as.character(date),sep=""))
        if (length(price) == 1) {
          portfolio[l, 'mtm'] = price
          portfolio[l, 'mv'] = portfolio[l, 'size'] * price
          npv = npv + portfolio[l, 'mv']
          unrealizedprofit = unrealizedprofit + portfolio[l, 'size'] * (portfolio[l, 'mtm'] - portfolio[l, 'buyprice']) - kBrokerage /
            100 * portfolio[l, 'size'] *
            (portfolio[l, 'mtm'] + portfolio[l, 'buyprice'])
        } else{
          npv = npv + ifelse(is.na(portfolio[l, 'mv']), 0, portfolio[l, 'mv'])
        }
      } else{
        realizedprofit = realizedprofit + portfolio[l, 'size'] * (portfolio[l, 'sellprice'] - portfolio[l, 'buyprice']) - kBrokerage /
          100 * portfolio[l, 'size'] *
          (portfolio[l, 'sellprice'] + portfolio[l, 'buyprice'])
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

UpdateDF4Upside <- function(df4, settledate,datafolder) {
        #update symbols with current symbol
        splits <-
                read.csv(
                        paste(datafolder, "/", "splits.csv", sep = ""),
                        header = TRUE,
                        stringsAsFactors = FALSE
                )
        symbolchange <-
                read.csv(
                        paste(datafolder, "/", "symbolchange.csv", sep = ""),
                        header = TRUE,
                        stringsAsFactors = FALSE
                )
        symbolchange <-
                data.frame(key = symbolchange$SM_KEY_SYMBOL,
                           newsymbol = symbolchange$SM_NEW_SYMBOL)
        symbolchange$key = gsub("[^0-9A-Za-z/-]", "", symbolchange$key)
        symbolchange$newsymbol = gsub("[^0-9A-Za-z/-]", "", symbolchange$newsymbol)
        
        splits <-
                data.frame(
                        date = as.POSIXct(splits$date, tz = "Asia/Kolkata"),
                        symbol = splits$symbol,
                        oldshares = splits$oldshares,
                        newshares = splits$newshares
                )
        matches<-match(df4$TICKER,symbolchange$key)
        df4indicestochange<-which(!is.na(matches))
        newsymbolindices=matches[which(!is.na(matches))]
        df4[df4indicestochange,]$TICKER<-symbolchange$newsymbol[newsymbolindices]
        
        for (i in 1:nrow(df4)) {
                symbol = df4[i, 'TICKER']
                if (file.exists(paste(
                        "/home/psharma/Dropbox/rfiles/daily/",
                        symbol,
                        ".Rdata",
                        sep = ""
                ))) {
                        load(paste(
                                "/home/psharma/Dropbox/rfiles/daily/",
                                symbol,
                                ".Rdata",
                                sep = ""
                        ))
                        endlength = which(md$date == settledate)
                        if (length(endlength) == 1 && endlength > 251) {
                                #print(md$symbol)
                                df4$CurrentRSI[i] = RSI(md$asettle, 2)[endlength]
                                startlength = endlength - 251
                                OverSold = runSum(RSI(md$asettle, 2) < 20, 2) == 2
                                df4$OverSold[i] = OverSold[endlength]
                                df4$AnnualizedSlope[i] = exp(slope(md$asettle[startlength:endlength])) ^
                                        252 - 1
                                df4$r[i] = r2(md$asettle[startlength:endlength])
                                df4$sumproduct[i] = df4$AnnualizedSlope[i] * df4$r[i]
                                lastprice = md$asettle[endlength]
                                shareoutstandingindex=tail(which(as.Date(md$date,tz="Asia/Kolkata")<=df4$sharesOutstandingDate[i]),1)
                                df4$THEORETICALVALUE[i]=df4$THEORETICALVALUE[i]/md[shareoutstandingindex,'splitadjust']
                                df4$UPSIDE[i] = (df4$THEORETICALVALUE[i] - lastprice) * 100 / lastprice
                                df4$UPSIDE[i] = trunc(df4$UPSIDE[i], 0)
                                df4$UPSIDE[i] = as.numeric(df4$UPSIDE[i])
                                df4$LATEST_SHARE_PRICE[i] = lastprice
                                df4$MCAP[i] = as.numeric(df4$COMMON_SHARE_OUTSTANDING_IN_MILLIONS[i]) *
                                        lastprice* md[shareoutstandingindex,'splitadjust'] / 1000
                        }
                }
        }
        df4
}

CashFlow <- function(portfolio, settledate) {
  vcash = rep(0, length(settledate))
  if (nrow(portfolio) > 0) {
    for (p in 1:nrow(portfolio)) {
      index = which(settledate == as.Date(portfolio[p, 'buydate'], tz = "Asia/Kolkata"))
      vcash[index] = vcash[index] - portfolio[p, 'size'] * portfolio[p, 'buyprice']
    }
    for (p in 1:nrow(portfolio)) {
      if (!is.na(portfolio[p, 'selldate'])) {
        index = which(settledate == as.Date(portfolio[p, 'selldate'], tz = "Asia/Kolkata"))
        vcash[index] = vcash[index] + portfolio[p, 'size'] * portfolio[p, 'sellprice']
      }
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
        scrip = shortlist[s, ]$TICKER
        load(paste(path, scrip, ".Rdata", sep = ""))
        price = md[as.Date(md$date, tz = "Asia/Kolkata") == date, "asettle"]
        if (length(price) > 0) {
          size = as.integer(value / price)
          if (size > 0) {
            print(paste("Entry Date:", date, " ,Scrip:", scrip, sep = ""))
            portfolio <- rbind(
              portfolio,
              data.frame(
                scrip = scrip,
                size = size,
                buydate = date,
                buyprice = price,
                selldate = NA_character_,
                sellprice = NA_real_,
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

GetCurrentPosition <- function(scrip, portfolio) {
  position <- 0
  if(nrow(portfolio)>0){
    for (row in 1:nrow(portfolio)) {
      if (is.na(portfolio[row, 'selldate']) &&
          portfolio[row, 'scrip'] == scrip) {
        position = position + portfolio[row, 'size']
      }
    }   
  }
  position
}

#### ALGORITHM ####
Summary = data.frame(
  irr = as.numeric(),
  profit = as.numeric(),
  winratio = as.numeric(),
  stringsAsFactors = FALSE
)

today <- adjust.previous(Sys.Date() - 1, "india")
DaysSinceStart = as.numeric(today - as.Date(kBackTestStartDate)) + 1
StatementDate = seq.Date(from = as.Date(kBackTestStartDate), to = today, 1)
TargetPortfolioValue = numeric(DaysSinceStart)
ActualPortfolioValue = numeric(DaysSinceStart)
RealizedProfit = rep(NA_real_, DaysSinceStart)
UnRealizedProfit = rep(NA_real_, DaysSinceStart)

TargetPortfolioValue = rep(kCommittedCapital, DaysSinceStart)
Cash = rep(0, DaysSinceStart)
MonthsElapsed = sapply(StatementDate, MonthsSinceStart, as.Date(kBackTestStartDate)) +
  1
TargetPortfolioValue = pmin(TargetPortfolioValue * MonthsElapsed / kDeployMonths, TargetPortfolioValue)
# allow buildup of portfolio with interest
#Interest = TargetPortfolioValue * Return / 365
#Interest = cumsum(Interest)
#TargetPortfolioValue = TargetPortfolioValue + Interest
TargetPortfolioValue = TargetPortfolioValue * exp((kCapitalGrowth / 100) * (seq_along(TargetPortfolioValue) / 365))
#today = as.POSIXct(format(today), tz = "Asia/Kolkata")
if (!file.exists(paste("Portfolio_",args[2],".Rdata",sep=""))) {
  Portfolio = data.frame(
    scrip = as.character(),
    size = as.numeric(),
    buydate = as.character(),
    buyprice = as.numeric(),
    selldate = as.character(),
    sellprice = as.numeric(),
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
    if (!is.na(Portfolio[row, 'selldate'])) {
      tempDate = as.Date(Portfolio[row, 'buydate'])
      StartingDate = ifelse(tempDate > StartingDate, tempDate, StartingDate)
    } else{
      tempDate = as.Date(Portfolio[row, 'buydate'])
      StartingDate = ifelse(tempDate > StartingDate, tempDate, StartingDate)
    }
  }
  StartingDate = adjust.next(as.Date(StartingDate) + 1, "india")
}


StartingIndex = which(StatementDate == StartingDate)

for (d in StartingIndex:length(StatementDate)) {
  date = StatementDate[d]
  print(paste("Processing Date:", date, sep = ""))
#  if (length(grep("S(at|un)", weekdays(date, abbr = TRUE))) == 0) {
    if(!isHoliday(dates=StatementDate[d],calendar="India")){
    #print(paste("Processing d:", d, sep = ""))
    #weekday
    out = CalculateNPV(Portfolio, date, kNiftyDataFolder)
    npv = out[[1]]
    Portfolio = out[[2]]
    RealizedProfit[d] = out[[3]]
    UnRealizedProfit[d] = out[[4]]
    ActualPortfolioValue[d] = npv
    Gap = TargetPortfolioValue[d] - npv
    CurrentMonth = MonthsElapsed[d]
    
    # Sell Portfolio
    if (nrow(Portfolio) > 0) {
      for (p in 1:nrow(Portfolio)) {
        DaysSincePurchase = as.numeric(date - as.Date(Portfolio[p, 'buydate']))
        if (is.na(Portfolio[p, 'sellprice']) &&
            DaysSincePurchase > 370) {
          scrip = Portfolio[p, 'scrip']
          load(paste(kNiftyDataFolder, scrip, ".Rdata", sep = ""))
          OverBought = runSum(RSI(md$asettle, 2) > kRSIExit, 2) == 2
          enddate = which(as.Date(md$date, tz = "Asia/Kolkata") == date)
          
          if (length(enddate) > 0 &&
              OverBought[enddate] == TRUE) {
            print(paste("Exit Date:", date, " ,Scrip:", scrip, sep = ""))
            # write SELL to redis
            #if (today == date) {
              redisConnect()
              redisSelect(kRedisDatabase)
              size = portfolio[p, 'size']
              longname = paste(scrip, "_STK___", sep = "")
              if (size > 0) {
                position = GetCurrentPosition(scrip, Portfolio)
                redisRPush(paste("trades", kStrategy, sep = ":"),
                           charToRaw(
                             paste(longname, size, "SELL", 0, position, sep = ":")
                           ))
              }
              
              levellog(logger,
                       "INFO",
                       paste(longname, size, "SELL", 0, position, sep = ":"))
              redisClose()
            #}
            Portfolio[p, 'selldate'] = as.character(date)
            Portfolio[p, 'sellprice'] = md$asettle[enddate]
            
          }
        }
      }
    }
    
    #Now Scan for Buys
    
    if (nrow(Portfolio) > 0) {
      DistinctPurchasesThisMonth = length(unique(Portfolio[Portfolio$month == CurrentMonth, c("scrip")]))
    } else{
      DistinctPurchasesThisMonth = 0
    }
    if (Gap > kMinOrderValue &
        DistinctPurchasesThisMonth < kTradesPerMonth) {
      load(GetDF4FileName(date))
      df4 = df4[df4$UPSIDE > kUpside / 2,] # get a smaller list of df4 that has a positive upside
      df4 <- UpdateDF4Upside(df4, as.character(date),"/home/psharma/Dropbox/rfiles/daily")
      shortlist <-
        df4[df4$UPSIDE > kUpside &
              df4$DIVIDENDPAYOUTPERC > kThresholdDividedPayoutPercent &
              df4$ROCE > kThresholdROCE &
              df4$AnnualizedSlope > kSlope / 100  &
              df4$r > kR2Fit / 100 & df4$CurrentRSI < kRSIEntry &
              df4$FINDATE + 90 < date &
              df4$MCAP > kMinMarketCap , ]
      #df4$FINDATE+90 < date covers scenarios where the FINANCIALS are forward looking
      existingSymbols <-
        unique(Portfolio[Portfolio$month == CurrentMonth, c("scrip")])
      dupes = match(existingSymbols, shortlist$TICKER)
      dupes <- dupes[!is.na(dupes)]
      if (length(dupes > 0)) {
        shortlist <- shortlist[-dupes, ]
      }
      if (DistinctPurchasesThisMonth < kTradesPerMonth &&
          nrow(shortlist) > kTradesPerMonth - DistinctPurchasesThisMonth) {
        shortlist <- shortlist[order(-shortlist$UPSIDE), ]
        #shortlist<-shortlist[sample(nrow(shortlist),(kTradesPerMonth - DistinctPurchasesThisMonth)),]
        shortlist <-
          shortlist[1:(kTradesPerMonth - DistinctPurchasesThisMonth), ]
      }
      if (nrow(shortlist) > 0 &&
          DistinctPurchasesThisMonth < kTradesPerMonth) {
        InvestmentValue = Gap / (kTradesPerMonth - DistinctPurchasesThisMonth)
        # write BUY to redis
        #if (today == date) {
        redisConnect()
          redisSelect(kRedisDatabase)
          for (row in 1:nrow(shortlist)) {
            scrip = shortlist[row, 'TICKER']
            load(paste(kNiftyDataFolder, scrip, ".Rdata", sep = ""))
            price = md[as.Date(md$date, tz = "Asia/Kolkata") == date, 'asettle']
            size = as.integer(InvestmentValue / price)
            longname = paste(scrip, "_STK___", sep = "")
            if (size > 0) {
              position = GetCurrentPosition(scrip, Portfolio)
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
        #}
        
        # update portfolio
        Portfolio <-
          UpdatePortfolioBuy(Portfolio,
                             shortlist,
                             date,
                             InvestmentValue,
                             CurrentMonth,
                             kNiftyDataFolder)
      }
    }
  }
}

#stopCluster(cl)

Portfolio$profit = ifelse(
  !is.na(Portfolio$sellprice),
  Portfolio$size * (Portfolio$sellprice - Portfolio$buyprice) - kBrokerage /
    100 * Portfolio$size *
    (Portfolio$sellprice + Portfolio$buyprice),
  Portfolio$size * (Portfolio$mtm - Portfolio$buyprice) - kBrokerage /
    100 * Portfolio$size *
    (Portfolio$mtm + Portfolio$buyprice)
)

UnRealizedProfit <-
  ifelse(UnRealizedProfit == 0, NA_real_, UnRealizedProfit)
UnRealizedProfit <- na.locf(UnRealizedProfit, na.rm = FALSE)
UnRealizedProfit <-
  ifelse(is.na(UnRealizedProfit), 0, UnRealizedProfit)

RealizedProfit <-
  ifelse(RealizedProfit == 0, NA_real_, RealizedProfit)
RealizedProfit <- na.locf(RealizedProfit, na.rm = FALSE)
RealizedProfit <- ifelse(is.na(RealizedProfit), 0, RealizedProfit)
maxdddate = which(RealizedProfit + UnRealizedProfit == min(RealizedProfit +
                                                             UnRealizedProfit))
if (nrow(Portfolio) > 0) {
  cashflow <- CashFlow(Portfolio, StatementDate)
  cashflow[length(cashflow)] <- cashflow[length(cashflow)] + npv
  
  print(StatementDate[maxdddate][1])
  print(paste("Max Loss:", (RealizedProfit[maxdddate][1] + UnRealizedProfit[maxdddate][1])), sep =
          "")
  print(paste(
    "Prior Peak Equity:",
    max(RealizedProfit[1:maxdddate[1]] + UnRealizedProfit[1:maxdddate[1]])
  ), sep =
    "")
  # plot(x = StatementDate,
  #      y = RealizedProfit + UnRealizedProfit,
  #      type = 'l')
  if(sum(cashflow)>0){
    irr <- xirr(cashflow, StatementDate) * 100
    print(paste("xirr:", irr, sep = ""))
    
  }else{
    irr=0
  }
  winratio <-
    sum((
      ifelse(is.na(Portfolio$mtm), Portfolio$buyprice, Portfolio$mtm) - Portfolio$buyprice
    ) >= 0) / nrow(Portfolio)
  print(paste("winratio:", winratio, sep = ""))
  profit = sum(Portfolio$profit[complete.cases(Portfolio$profit)])
  Summary <-
    rbind(Summary,
          data.frame(
            irr = irr,
            winratio = winratio,
            profit = profit
          ))
  save(Portfolio,file=paste("Portfolio_",args[2],".Rdata",sep=""))
  ActualPortfolioValue <-
    ifelse(ActualPortfolioValue == 0, NA_real_, ActualPortfolioValue)
  ActualPortfolioValue <-
    na.locf(ActualPortfolioValue, na.rm = FALSE)
  ActualPortfolioValue <-
    ifelse(is.na(ActualPortfolioValue), 0, ActualPortfolioValue)
  DailyPNL <-
    (RealizedProfit + UnRealizedProfit) - Ref(RealizedProfit + UnRealizedProfit, -1)
  DailyPNL <- ifelse(is.na(DailyPNL), 0, DailyPNL)
  DailyReturn <-
    ifelse(ActualPortfolioValue == 0, 0, DailyPNL / ActualPortfolioValue)
  df <- data.frame(time = StatementDate, return = DailyReturn)
  df <- read.zoo(df)
  sharpe <-
    SharpeRatio((df[df != 0][, 1, drop = FALSE]), Rf = .07 / 365, FUN = "StdDev") *
    sqrt(252)
  print(paste("sharpe:", sharpe, sep = ""))
}


# Buy Logic
# For Each StatementDate
# Calculate NPV
# if Gap >10000 & stocksbought this month <5
#   Load corresponding DF4
#   Update Upside
#   Create Shortlist
#   for each shortlist & while #investmentsInMonth <5
#     InvestmentValue=Gap/(5-#InvestmentsInMonth)
#     Write purchase to REDIS
#     update portfolio
#   End For
# End For
