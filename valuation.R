library(zoo)

#### PARAMETERS ####
InitialCapital = 1000000
StrategyStartDate = "2011-07-01"
DeployMonths = 12
Return = 0.2
MinOrderValue = 10000
path = "/home/psharma/Seafile/rfiles/daily/"
Upside = 50
RSIEntry=30
RSIExit=80
SingleLegTransactionCost=0.25 # In Percent

#### FUNCTIONS ####
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
  realizedprofit=0
  unrealizedprofit=0
  if (nrow(portfolio) > 0) {
    for (l in 1:nrow(portfolio)) {
      name = portfolio[l, 'scrip']
      if (is.na(portfolio[l, 'sellprice'])) {
        load(paste(path, name, ".Rdata", sep = ""))
        price = md[as.Date(md$date, tz = "Asia/Kolkata") == date, 'settle']
        #print(paste("Date:",as.character(date),sep=""))
        if (length(price) == 1) {
          portfolio[l, 'mtm'] = price
          portfolio[l, 'mv'] = portfolio[l, 'size'] * price
          npv = npv + portfolio[l, 'mv']
          unrealizedprofit=unrealizedprofit+portfolio[l, 'size'] * (portfolio[l, 'mtm'] - portfolio[l, 'buyprice']) - SingleLegTransactionCost /
            100 * portfolio[l, 'size'] *
            (portfolio[l, 'mtm'] + portfolio[l, 'buyprice'])
        } else{
          npv = npv + ifelse(is.na(portfolio[l, 'mv']), 0, portfolio[l, 'mv'])
        }
      }else{
        realizedprofit=realizedprofit+portfolio[l, 'size'] * (portfolio[l, 'sellprice'] - portfolio[l, 'buyprice']) - SingleLegTransactionCost /
          100 * portfolio[l, 'size'] *
          (portfolio[l, 'sellprice'] + portfolio[l, 'buyprice'])
      }
    }
  }
  list(npv, portfolio,realizedprofit,unrealizedprofit)
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

UpdateDF4Upside <- function(df4, settledate) {
  for (i in 1:nrow(df4)) {
    symbol = df4[i, 'TICKER']
    if (file.exists(paste(
      "/home/psharma/Seafile/rfiles/daily/",
      symbol,
      ".Rdata",
      sep = ""
    ))) {
      load(paste(
        "/home/psharma/Seafile/rfiles/daily/",
        symbol,
        ".Rdata",
        sep = ""
      ))
      endlength = which(md$date == settledate)
      if (length(endlength) == 1 && endlength > 251) {
        df4$CurrentRSI[i] = RSI(md$settle, 2)[endlength]
        startlength = endlength - 251
        OverSold = runSum(RSI(md$settle, 2) < 20, 2) == 2
        df4$OverSold[i] = OverSold[endlength]
        df4$AnnualizedSlope[i] = exp(slope(md$settle[startlength:endlength])) ^
          252 - 1
        df4$r[i] = r2(md$settle[startlength:endlength])
        df4$sumproduct[i] = df4$AnnualizedSlope[i] * df4$r[i]
        lastprice = md$settle[endlength]
        df4$UPSIDE[i] = (df4$THEORETICALVALUE[i] - lastprice) * 100 / lastprice
        df4$UPSIDE[i] = trunc(df4$UPSIDE[i], 0)
        df4$UPSIDE[i] = as.numeric(df4$UPSIDE[i])
        df4$LATEST_SHARE_PRICE[i] = lastprice
      }
    }
  }
  df4
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
        price = md[as.Date(md$date, tz = "Asia/Kolkata") == date, "settle"]
        if (length(price) > 0) {
          size = as.integer(value / price)
          if (size > 0) {
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
                stringsAsFactors = FALSE
              )
            )
          }
        }
      }
      
    }
    portfolio
  }

#### ALGORITHM ####
DaysSinceStart = as.numeric(Sys.Date() - as.Date(StrategyStartDate)) + 1
StatementDate = seq.Date(from = as.Date(StrategyStartDate), to = Sys.Date(), 1)
TargetPortfolioValue = numeric(DaysSinceStart)
ActualPortfolioValue = numeric(DaysSinceStart)
RealizedProfit = rep(NA_real_,DaysSinceStart)
UnRealizedProfit = rep(NA_real_,DaysSinceStart)

TargetPortfolioValue = rep(InitialCapital, DaysSinceStart)
MonthsElapsed = sapply(StatementDate, MonthsSinceStart, as.Date(StrategyStartDate)) +
  1
TargetPortfolioValue = pmin(TargetPortfolioValue * MonthsElapsed / 12, TargetPortfolioValue)
# allow buildup of portfolio with interest
Interest = TargetPortfolioValue * Return / 365
Interest = cumsum(Interest)
TargetPortfolioValue = TargetPortfolioValue + Interest
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
  stringsAsFactors = FALSE
)


for (d in 1:length(StatementDate)) {
  date = StatementDate[d]
  if (length(grep("S(at|un)", weekdays(date, abbr = TRUE))) == 0) {
    print(paste("Processing d:", d, sep = ""))
    #weekday
    # Sell Portfolio
    if (nrow(Portfolio) > 0) {
      for (p in 1:nrow(Portfolio)) {
        DaysSincePurchase = as.numeric(date - as.Date(Portfolio[p, 'buydate']))
        if (is.na(Portfolio[p, 'sellprice']) &&
            DaysSincePurchase > 370) {
          scrip = Portfolio[p, 'scrip']
          load(paste(path, scrip, ".Rdata", sep = ""))
          OverBought = runSum(RSI(md$settle, 2) > RSIExit, 2) == 2
          enddate = which(as.Date(md$date, tz = "Asia/Kolkata") == date)
          if (length(enddate) > 0 && OverBought[enddate] == TRUE) {
            print(paste("exit d:", d, sep = ""))
            Portfolio[p, 'selldate'] = as.character(date)
            Portfolio[p, 'sellprice'] = md$settle[enddate]
          }
        }
      }
    }
    
    
    #Now Scan for Buys
    
    out = CalculateNPV(Portfolio, date, path)
    npv = out[[1]]
    Portfolio = out[[2]]
    RealizedProfit[d]=out[[3]]
    UnRealizedProfit[d]=out[[4]]
    ActualPortfolioValue[d] = npv
    Gap = TargetPortfolioValue[d] - npv
    CurrentMonth = MonthsElapsed[d]
    if (nrow(Portfolio) > 0) {
      DistinctPurchasesThisMonth = length(unique(Portfolio[Portfolio$month == CurrentMonth, c("scrip")]))
    } else{
      DistinctPurchasesThisMonth = 0
    }
    if (Gap > MinOrderValue & DistinctPurchasesThisMonth < 5) {
      load(GetDF4FileName(date))
      df4 = df4[df4$UPSIDE > Upside / 2,] # get a smaller list of df4 that has a positive upside
      df4 <- UpdateDF4Upside(df4, as.character(date))
      shortlist <-
        df4[df4$UPSIDE > Upside &
              df4$DIVIDENDPAYOUTPERC > 10 &
              df4$ROCE > 20 &
              df4$AnnualizedSlope > 0 &
              df4$r > 0.5 & df4$CurrentRSI < RSIEntry &
              df4$FINDATE+90 <date,]
      #df4$FINDATE+90 < date covers scenarios where the FINANCIALS are forward looking
      existingSymbols <-
        unique(Portfolio[Portfolio$month == CurrentMonth, c("scrip")])
      dupes = match(existingSymbols, shortlist$TICKER)
      dupes <- dupes[!is.na(dupes)]
      if (length(dupes > 0)) {
        shortlist <- shortlist[-dupes, ]
      }
      if (DistinctPurchasesThisMonth < 5 &&
          nrow(shortlist) > 5 - DistinctPurchasesThisMonth) {
        shortlist <- shortlist[1:(5 - DistinctPurchasesThisMonth), ]
      }
      
      if (nrow(shortlist) > 0 && DistinctPurchasesThisMonth < 5) {
        InvestmentValue = Gap / (5 - DistinctPurchasesThisMonth)
        # write to redis
        # update portfolio
        print(paste("entry d:", d, sep = ""))
        Portfolio <-
          UpdatePortfolioBuy(Portfolio,
                             shortlist,
                             date,
                             InvestmentValue,
                             CurrentMonth,
                             path)
      }
    }
    
    
  }
}

Portfolio$profit = ifelse(
  !is.na(Portfolio$sellprice),
  Portfolio$size * (Portfolio$sellprice - Portfolio$buyprice) - SingleLegTransactionCost/100 * Portfolio$size *
    (Portfolio$sellprice + Portfolio$buyprice),
  Portfolio$size * (Portfolio$mtm - Portfolio$buyprice) - SingleLegTransactionCost/100 * Portfolio$size *
    (Portfolio$mtm + Portfolio$buyprice)
)

UnRealizedProfit<-ifelse(UnRealizedProfit==0,NA_real_,UnRealizedProfit)
UnRealizedProfit<-na.locf(UnRealizedProfit,na.rm = FALSE)
UnRealizedProfit<-ifelse(is.na(UnRealizedProfit),0,UnRealizedProfit)

RealizedProfit<-ifelse(RealizedProfit==0,NA_real_,RealizedProfit)
RealizedProfit<-na.locf(RealizedProfit,na.rm = FALSE)
RealizedProfit<-ifelse(is.na(RealizedProfit),0,RealizedProfit)

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
