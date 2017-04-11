library(zoo)
library(TTR)
#library(doParallel)
options(scipen=999) #disable scientific notation
#options(scipen=0) #enable scientific notation

#### PARAMETERS ####
folder="20160923"
Strategy="value01"
setwd(paste("/home/psharma/Seafile/servers/FundamentalData/",folder,sep=""))
InitialCapital = 20000000
#StrategyStartDate = "2011-07-01"
StrategyStartDate = "2012-01-01"
#StrategyEndDate = "2015-06-30"
StrategyEndDate = "2015-12-31"
#StrategyCloseAllDate = "2016-06-30"
StrategyCloseAllDate = "2016-12-31"

DeployMonths = 12
Return = 0.1 
MinMarketCap=0
MinOrderValue = 10000
path = "/home/psharma/Seafile/rfiles/daily/"
Upside = 25 # In Percent
RSIEntry=20
RSIExit=80
R2Fit=70 # In Percent
Slope=10 # In Percent
SingleLegTransactionCost=0.20 # In Percent

#### FUNCTIONS ####

xirr <- function(cf, dates) {
  
  # Secant method.
  secant <- function(par, fn, tol=1.e-07, itmax = 100, trace=FALSE, ...) { 
    # par = a starting vector with 2 starting values 
    # fn = a function whose first argument is the variable of interest 
    if (length(par) != 2) stop("You must specify a starting parameter vector of length 2") 
    p.2 <- par[1] 
    p.1 <- par[2] 
    f <- rep(NA, length(par)) 
    f[1] <- fn(p.1, ...) 
    f[2] <- fn(p.2, ...) 
    iter <- 1 
    pchg <- abs(p.2 - p.1) 
    fval <- f[2] 
    if (trace) cat("par: ", par, "fval: ", f, "\n") 
    while (pchg >= tol & abs(fval) > tol & iter <= itmax) { 
      p.new <- p.2 - (p.2 - p.1) * f[2] / (f[2] - f[1]) 
      pchg <- abs(p.new - p.2) 
      fval <- ifelse(is.na(fn(p.new, ...)),1,fn(p.new, ...))
      #      fval <- fn(p.new, ...) 
      p.1 <- p.2 
      p.2 <- p.new 
      f[1] <- f[2] 
      f[2] <- fval 
      iter <- iter + 1 
      if (trace) cat("par: ", p.new, "fval: ", fval, "\n") 
    } 
    list(par = p.new, value = fval, iter=iter) 
  } 
  
  # Net present value.
  npv <- function(irr, cashflow, times) sum(cashflow / (1 + irr)^times) 
  
  times <- as.numeric(difftime(dates, dates[1], units="days"))/365.24 
  
  r <- secant(par=c(0,0.1), fn=npv, cashflow=cf, times=times) 
  
  return(r$par)
}


NPV<-function(paym,pdates,IRR){
  ptimes<-as.Date(pdates)-min(as.Date(pdates))
  ptimes<-as.numeric(ptimes,units="days")/365.25
  NPV<-sum(paym*(1+IRR)^{-ptimes})
  NPV
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
        df4$MCAP[i]=as.numeric(df4$COMMON_SHARE_OUTSTANDING_IN_MILLIONS[i])*lastprice/1000
      }
    }
  }
  df4
}

CashFlow <-function(portfolio,settledate){
  vcash=rep(0,length(settledate))
  for(p in 1:nrow(portfolio)){
    index=which(settledate==as.Date(portfolio[p,'buydate'],tz="Asia/Kolkata"))
    vcash[index]=vcash[index]-portfolio[p,'size']*portfolio[p,'buyprice']
  }
  for(p in 1:nrow(portfolio)){
    if(!is.na(portfolio[p,'selldate'])){
      index=which(settledate==as.Date(portfolio[p,'selldate'],tz="Asia/Kolkata"))
      vcash[index]=vcash[index]+portfolio[p,'size']*portfolio[p,'sellprice']
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
Cash = rep(0,DaysSinceStart)
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

#cl <- makeCluster(detectCores())
#registerDoParallel(cl)
for (d in 1:length(StatementDate)) {
  date = StatementDate[d]
  if (length(grep("S(at|un)", weekdays(date, abbr = TRUE))) == 0 ) {
    print(paste("Processing d:", d, sep = ""))
    #weekday
    out = CalculateNPV(Portfolio, date, path)
    npv = out[[1]]
    Portfolio = out[[2]]
    RealizedProfit[d]=out[[3]]
    UnRealizedProfit[d]=out[[4]]
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
          load(paste(path, scrip, ".Rdata", sep = ""))
          OverBought = runSum(RSI(md$settle, 2) > RSIExit, 2) == 2
          enddate = which(as.Date(md$date, tz = "Asia/Kolkata") == date)
          if ((length(enddate) > 0 && OverBought[enddate] == TRUE)||(length(enddate)>0 && as.Date(md$date[enddate])>as.Date(StrategyCloseAllDate))) {
            print(paste("exit d:", d, sep = ""))
            Portfolio[p, 'selldate'] = as.character(date)
            Portfolio[p, 'sellprice'] = md$settle[enddate]
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
    print(paste("Processing Buy. Gap:", Gap,",MinOrderValue:",MinOrderValue,",DistinctPurchasesThisMonth:",DistinctPurchasesThisMonth, ",date:",date,sep = ""))
    if (Gap > MinOrderValue && DistinctPurchasesThisMonth < 5 && date< as.Date(StrategyEndDate)) {
      load(GetDF4FileName(date))
      df4 = df4[df4$UPSIDE > Upside / 2,] # get a smaller list of df4 that has a positive upside
      df4 <- UpdateDF4Upside(df4, as.character(date))
      print(paste("Processing Buy for d2:", d,",date:",date, sep = ""))
      shortlist <-
        df4[df4$UPSIDE > Upside &
              df4$DIVIDENDPAYOUTPERC > 10 &
              df4$ROCE > 15 &
              df4$AnnualizedSlope > Slope/100  &
              df4$r > R2Fit/100 & df4$CurrentRSI < RSIEntry &
              df4$FINDATE+90 < date &
              df4$MCAP >MinMarketCap , ]
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
#stopCluster(cl)
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
maxdddate=which(RealizedProfit+UnRealizedProfit==min(RealizedProfit+UnRealizedProfit))

cashflow<-CashFlow(Portfolio,StatementDate)
cashflow[length(cashflow)]<-cashflow[length(cashflow)]+npv
irr<-nlm(function(p){NPV(cashflow,StatementDate,p)^2},p=0.1)

print(StatementDate[maxdddate])
print(paste("Max Loss:",(RealizedProfit[maxdddate]+UnRealizedProfit[maxdddate])),sep="")
print(paste("Prior Peak Equity:",max(RealizedProfit[1:maxdddate]+UnRealizedProfit[1:maxdddate])),sep="")
plot(x=StatementDate,y=RealizedProfit+UnRealizedProfit,type='l')
print(paste("xirr:",xirr(cashflow,StatementDate)*100,sep=""))
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
# png(file="mag_feb.png", units="in", width=11, height=8.5, res=300)
