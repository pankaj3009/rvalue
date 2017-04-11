setwd(paste(
        "/home/psharma/Seafile/rfiles/valuation/",
        "",
        sep = ""
))
library(zoo)
library(TTR)
library(RTrade)
library(PerformanceAnalytics)
source("valuationfunctions.R")
options(scipen = 999) #disable scientific notation
#options(scipen=0) #enable scientific notation

#### PARAMETERS ####
folder = "20160923"
Strategy = "value01"
setwd(paste(
        "/home/psharma/Seafile/servers/FundamentalData/",
        folder,
        sep = ""
))
InitialCapital = 20000000
StrategyStartDate = "2013-01-01"
StrategyEndDate = "2016-12-31"
StrategyCloseAllDate = "2017-12-31"
ROCE=15
DividendPayOut=10
ExitDays = +370
WorkingDaysForSlope = 252
DeployMonths = 12
Return = 0.1
MinMarketCap = 0
MinOrderValue = 10000
path = "/home/psharma/Seafile/rfiles/daily/"
Upside = 25 # In Percent
RSIEntry = 20
RSIExit = 80
R2Fit = 70 # In Percent
Slope = 10 # In Percent
SingleLegTransactionCost = 0.20 # In Percent

#### ALGORITHM ####
#DaysSinceStart = as.numeric(Sys.Date() - as.Date(StrategyStartDate)) + 1
StatementDate = seq.Date(from = as.Date(StrategyStartDate), to = as.Date(StrategyEndDate), 1)
TargetPortfolioValue = numeric(length(StatementDate))
ActualPortfolioValue = rep(NA_real_, length(StatementDate))
Gap = rep(NA_real_, length(StatementDate))
RealizedProfit = rep(NA_real_, length(StatementDate))
UnRealizedProfit = rep(NA_real_, length(StatementDate))

TargetPortfolioValue = rep(InitialCapital, length(StatementDate))
Cash = rep(0, length(StatementDate))
MonthsElapsed = sapply(StatementDate, MonthsSinceStart, as.Date(StrategyStartDate)) +
        1
TargetPortfolioValue = pmin(TargetPortfolioValue * MonthsElapsed / DeployMonths,
                            TargetPortfolioValue)
# allow buildup of portfolio with interest
Interest = TargetPortfolioValue * Return / 365
Interest = cumsum(Interest)
TargetPortfolioValue = TargetPortfolioValue + Interest
indexOfSystematicSellStart = ifelse(length(which(StatementDate == as.Date(StrategyEndDate)))>0, which(StatementDate == as.Date(StrategyEndDate))+
                                           1,length(StatementDate)+1)
indexOfSystematicSellEnd = ifelse(length(which(StatementDate == as.Date(StrategyCloseAllDate)))>0, which(StatementDate == as.Date(StrategyCloseAllDate))+
                                                                 1,length(StatementDate))
if(indexOfSystematicSellEnd>=indexOfSystematicSellStart){
        TargetPortfolioValue[indexOfSystematicSellStart:indexOfSystematicSellEnd] =
                seq(
                        from = TargetPortfolioValue[indexOfSystematicSellStart],
                        to = 0,
                        length.out = (indexOfSystematicSellEnd - indexOfSystematicSellStart + 1)
                )
        
}
if (indexOfSystematicSellEnd < length(TargetPortfolioValue)) {
        TargetPortfolioValue[indexOfSystematicSellEnd + 1:length(TargetPortfolioValue)] =
                0
}

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
        if (length(grep("S(at|un)", weekdays(date, abbr = TRUE))) == 0) {
                print(paste("Processing d:", d, sep = ""))
                #weekday
                out = CalculateNPV(Portfolio, date, path)
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
                                DaysSincePurchase = as.numeric(date - as.Date(Portfolio[p, 'buydate']))
                                if (is.na(Portfolio[p, 'sellprice']) &&
                                    DaysSincePurchase > ExitDays) {
                                        scrip = Portfolio[p, 'scrip']
                                        load(paste(
                                                path,
                                                scrip,
                                                ".Rdata",
                                                sep = ""
                                        ))
                                        OverBought = runSum(RSI(md$settle, 2) > RSIExit,
                                                            2) == 2
                                        enddate = which(
                                                as.Date(md$date, tz = "Asia/Kolkata") == date
                                        )
                                        if ((length(enddate) > 0 &&
                                             OverBought[enddate] == TRUE) ||
                                            (
                                                    length(enddate) > 0 &&
                                                    as.Date(md$date[enddate]) >= as.Date(
                                                            StrategyCloseAllDate
                                                    )
                                            )) {
                                                print(paste(
                                                        "exit d:",
                                                        d,
                                                        sep = ""
                                                ))
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
                #print(paste("Processing Buy. Gap:", Gap,",MinOrderValue:",MinOrderValue,",DistinctPurchasesThisMonth:",DistinctPurchasesThisMonth, ",date:",date,sep = ""))
                if (Gap[d] > MinOrderValue &&
                    DistinctPurchasesThisMonth < 5 &&
                    date < as.Date(StrategyEndDate)) {
                        load(GetDF4FileName(date))
                        df4 = df4[df4$UPSIDE > Upside / 2,] # get a smaller list of df4 that has a positive upside
                        df4 <-
                                UpdateDF4Upside(df4, as.character(date))
                        # print(paste("Processing Buy for d2:", d,",date:",date, sep = ""))
                        shortlist <-
                                df4[df4$UPSIDE > Upside &
                                            df4$DIVIDENDPAYOUTPERC > DividendPayOut &
                                            df4$ROCE > ROCE &
                                            df4$AnnualizedSlope > Slope /
                                            100  &
                                            df4$r > R2Fit / 100 &
                                            df4$CurrentRSI < RSIEntry &
                                            df4$FINDATE + 90 < date &
                                            df4$MCAP > MinMarketCap , ]
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
                        
                        if (nrow(shortlist) > 0 &&
                            DistinctPurchasesThisMonth < 5) {
                                InvestmentValue = Gap[d] / (5 - DistinctPurchasesThisMonth)
                                # write to redis
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
                                                path
                                        )
                        }
                }
                
                
        }
}
#Cleanup values before reporting
out = CalculateNPV(Portfolio, date, path)
npv = out[[1]]
Portfolio = out[[2]]
RealizedProfit[d] = out[[3]]
UnRealizedProfit[d] = out[[4]]

#stopCluster(cl)
Portfolio$profit = ifelse(
        !is.na(Portfolio$sellprice),
        Portfolio$size * (Portfolio$sellprice - Portfolio$buyprice) - SingleLegTransactionCost /
                100 * Portfolio$size *
                (Portfolio$sellprice + Portfolio$buyprice),
        Portfolio$size * (Portfolio$mtm - Portfolio$buyprice) - SingleLegTransactionCost /
                100 * Portfolio$size *
                (Portfolio$mtm + Portfolio$buyprice)
)

UnRealizedProfit <- na.locf(UnRealizedProfit, na.rm = FALSE)
UnRealizedProfit <- na.locf(UnRealizedProfit, fromLast = TRUE)

RealizedProfit <- na.locf(RealizedProfit, na.rm = FALSE)
RealizedProfit <- na.locf(RealizedProfit, fromLast  = TRUE)

Gap <- na.locf(Gap, na.rm = FALSE)
Gap <- na.locf(Gap, fromLast = TRUE)

maxdddate = which(RealizedProfit + UnRealizedProfit == min(RealizedProfit +
                                                                   UnRealizedProfit))

cashflow <- CashFlow(Portfolio, StatementDate,SingleLegTransactionCost/100)
cashflow[length(cashflow)] <- cashflow[length(cashflow)] + npv
irr <- nlm(function(p) {
        NPV(cashflow, StatementDate, p) ^ 2
}, p = 0.1)
winratio <-
        sum((
                ifelse(is.na(Portfolio$mtm), Portfolio$buyprice, Portfolio$mtm) - Portfolio$buyprice
        ) >= 0) / nrow(Portfolio)

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

mfdata<-read.csv("niftysmallmid400.csv",header=FALSE,stringsAsFactors = FALSE)
colnames(mfdata)<-c("date","price")
mfdata$date<-as.Date(mfdata$date)
mfdata<-mfdata[order(mfdata$date),]

PortfolioMF = data.frame(
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
mfnpv=0
for(p in 1:nrow(Portfolio)){
        #print(paste("number of rows:",nrow(PortfolioMF),sep=""))
        entrydate=Portfolio[p,"buydate"]
        entryvalue=Portfolio[p,"size"]*Portfolio[p,"buyprice"]
        mfbuyprice=tail(mfdata[mfdata$date<=entrydate,c("price")],1)
        mfbuysize=entryvalue/mfbuyprice
        selldate=Portfolio[p,"selldate"]
        if(!is.na(selldate)){
                mfsellprice=tail(mfdata[mfdata$date<=selldate,c("price")],1)
        }else{
                mfsellprice=mfdata$price[nrow(mfdata)]
                mfnpv=mfnpv+mfsellprice*mfbuysize
        }
        #print(paste("number of rows:",nrow(PortfolioMF),sep=""))
        profit=(mfsellprice-mfbuyprice)*mfbuysize
        PortfolioMF=rbind(PortfolioMF,data.frame(scrip="Franklin Templeton MF",size=mfbuysize,buydate=entrydate,buyprice=mfbuyprice,selldate=selldate,
                                                 sellprice=mfsellprice,mtm=NA_real_,mv=NA_real_,month=NA_real_,profit=profit,stringsAsFactors = FALSE))
        
}

mfcashflow <- CashFlow(PortfolioMF, StatementDate,0)
mfcashflow[length(cashflow)] <- mfcashflow[length(cashflow)] + mfnpv
mfxirr=xirr(mfcashflow, StatementDate) * 100

mfbuyprice=head(mfdata$price,1)
buysize=20000000/mfbuyprice
mfsellprice=tail(mfdata$price,1)
profit=(mfsellprice-mfbuyprice)*buysize
