timer.start=Sys.time()
library(TTR)
library(log4r)
library(RTrade)
library(tableHTML)
library(gmailr)

# Parallel version of SMC-LO.1.0.R

#### PARAMETERS ####
options(scipen = 999)

args.commandline = commandArgs(trailingOnly = TRUE)
if (length(args.commandline) > 0) {
        args <- args.commandline
}

### Read Parameters ###
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
kBackTest <- as.logical(static$BackTest)
kWriteToRedis <- as.logical(static$WriteToRedis)
kBackTestStartDate <- static$BackTestStartDate
kBackTestEndDate <- static$BackTestEndDate
kBackTestCloseAllDate <- static$BackTestCloseAllDate
kTimeZone <- static$TimeZone
kValueBrokerage <- as.numeric(static$BrokerageOnValue) / 100
kPerContractBrokerage=0
kHomeDirectory = static$HomeDirectory
kCommittedCapital = as.numeric(static$CommittedCapital)
kLogFile = static$LogFile
kStrategy = args[2]
kExclusionList=static$ExclusionList
kRequireOverSold = as.logical(static$RequireOversold)
kRedisDatabase = as.numeric(args[3])
kExclusionList<-strsplit(kExclusionList[1],",")[[1]]
kDF4Files=static$DF4Files
df4files=strsplit(kDF4Files[1],",")[[1]]
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
kInvestmentReturn = as.numeric(static$InvestmentReturn)
kOverdraftPenalty=as.numeric(static$OverdraftPenalty)
kMargin=1
kBrokerage=fromJSON(static$Brokerage)
kSubscribers=fromJSON(static$Subscribers)
kWorkingDaysForSlope = as.numeric(static$WorkingDaysForSlope)
kBackTestEndDate = strftime(adjust("India", as.Date(kBackTestEndDate, tz = kTimeZone), bdc = 2), "%Y-%m-%d")
kBackTestStartDate = strftime(adjust("India", as.Date(kBackTestStartDate, tz = kTimeZone), bdc = 0), "%Y-%m-%d")
kBackTestCloseAllDate = strftime(adjust("India", as.Date(kBackTestCloseAllDate, tz = kTimeZone), bdc = 0), "%Y-%m-%d")


logger <- create.logger()
logfile(logger) <- kLogFile
level(logger) <- 'INFO'
levellog(logger, "INFO", "Starting EOD Scan")

holidays=readRDS(paste(datafolder,"static/holidays.rds",sep=""))
RQuantLib::addHolidays("India",holidays)

if(!is.null(kHomeDirectory)){
        setwd(kHomeDirectory)
}

#### FUNCTIONS ####

MonthsSinceStart <- function(endDate, startDate) {
        #startDate and endDate are to be given in Date format using as.Date()
        length(seq(
                from = startDate,
                to = endDate,
                by = 'month'
        )) - 1
}

CalculateNPV <-function(portfolio,date){
        portfolio=portfolio[portfolio$entrytime<date & (portfolio$exittime>date|portfolio$exitreason=="Open"),]
        for(l in seq_len(nrow(portfolio))){
                # if(!exists(portfolio$symbol[l])){
                #         assign(portfolio$symbol[l],loadSymbol(portfolio$symbol[l]))
                # }
                # portfolio$mtm[l]=get(portfolio$symbol[l])%>%filter(date<=date)%>%select(asettle)%>%filter(row_number==1)
                portfolio$mtm[l]=getmtm(portfolio$symbol[l],date)$value
        }
        portfolio$mv=portfolio$size*portfolio$mtm
        sum(portfolio$mv)
}

#### GENERATE SYMBOLS  ####
symbolchange=getSymbolChange()
symbols=data.frame()
for (f in df4files){
        startdate=paste(strsplit(f,"_")[[1]][1],"-06-30",sep="")
        startdate=as.POSIXct(startdate,tz="Asia/Kolkata")
        enddate=startdate+365*24*60*60
        load(f)
        df4 = df4[df4$UPSIDE > (kUpside - 25) & df4$ROCE>kThresholdROCE & df4$DIVIDENDPAYOUTPERC>kThresholdDividedPayoutPercent &  df4$MCAP > kMinMarketCap, ]
        df4$NEWTICKER=sapply(df4$TICKER,function(x) last(linkedsymbols(symbolchange,x)$symbol))
        symbols=rbind(symbols,data.frame(symbol=df4$NEWTICKER,startdate=rep(startdate,length(df4$NEWTICKER)),enddate=rep(enddate,length(df4$NEWTICKER)),value=df4$THEORETICALVALUE, sharesoutstandingon=as.POSIXct(as.character(df4$sharesOutstandingDate),tz="Asia/Kolkata"),financialdate=as.POSIXct(as.character(df4$FINDATE),tz=kTimeZone), stringsAsFactors = FALSE))
}
symbolslist=list()
for(i in 1:nrow(symbols)){
        symbol=symbols$symbol[i]
        if(length(grep(symbol,symbolslist))==1){
                # append to list
                symbolslist[[symbol]]=rbind(symbolslist[[symbol]],symbols[i,])
               
        }else{
                symbolslist[[symbol]]=symbols[i,]
        }
}
names(symbolslist)=paste(names(symbolslist),"_STK___",sep="")
print(paste("Number of symbols:",length(symbolslist)))

#### GENERATE SIGNALS ####
StatementDate = seq.Date(from = as.Date(kBackTestStartDate),to = min(Sys.Date(),as.Date(kBackTestCloseAllDate)),1)
TargetPortfolioValue = data.frame(date=as.POSIXct(as.character(StatementDate),tz=kTimeZone),value=rep(kCommittedCapital,length(StatementDate)))
MonthsElapsed = sapply(StatementDate, MonthsSinceStart,as.Date(kBackTestStartDate)) +1
TargetPortfolioValue$value = pmin(TargetPortfolioValue$value * MonthsElapsed / kDeployMonths,TargetPortfolioValue$value)
Interest = TargetPortfolioValue$value * kCapitalGrowth / 365
Interest = cumsum(Interest)
TargetPortfolioValue$value = TargetPortfolioValue$value + Interest
cutoff=as.character(as.Date(kBackTestStartDate)-kWorkingDaysForSlope*1.5)
buycounter=1
signals=data.frame()

for (i in 1:length(symbolslist)){
        print(paste("Processing",i,names(symbolslist)[i],sep=":"))
        symbol=names(symbolslist)[i]
        md=loadSymbol(symbol,days=1000000)
        md=md[md$date>=cutoff,]
        if(nrow(md)>0){
                md$daysinhistory=as.numeric(rownames(md))
                md$month=strftime(md$date,format="%y%m")
                regress=regress(md$asettle,TRUE,kWorkingDaysForSlope)
                md$slope=regress[,1]
                md$r2=regress[,2]
                md$predict=lmprediction(md$asettle,period=kWorkingDaysForSlope)
                md$rsi=RSI(md$asettle, 2)
                if(!kRequireOverSold){
                        md$oversold=TRUE
                }else{
                        md$oversold=runSum(RSI(md$asettle, 2) < kRSIEntry, 2) == 2
                        
                }
                md$hhv=hhv(md$asettle,-kWorkingDaysForSlope)
                md$drawdown= (md$hhv-md$asettle)/md$hhv
                md$slopereturn.refperiod= ((1+md$slope)^kWorkingDaysForSlope-1) 
                md$return.refperiod=(md$asettle-Ref(md$asettle,-kWorkingDaysForSlope))/Ref(md$asettle,-kWorkingDaysForSlope)
                shortlists=symbolslist[[symbol]]
                md$referencevalue=0
                md$referencesplitadjust=1
                md$eligible=0
                for(j in 1:nrow(shortlists)){
                        shortlist=shortlists[j,]
                        md[md$date>=shortlist$startdate & md$date<=shortlist$enddate, 'eligible']=1
                        if(first(md$date)<=shortlist$sharesoutstandingon[1]){
                                md[md$date>=shortlist$startdate & md$date<=shortlist$enddate, 'referencesplitadjust']=last(md[md$date<shortlist$sharesoutstandingon[1],c("splitadjust")])
                        }
                        # referencevalue is value of share adjusted to asettle...
                        md[md$date>=shortlist$startdate & md$date<=shortlist$enddate, 'referencevalue']=shortlist$value[1]/md[md$date>=shortlist$startdate & md$date<=shortlist$enddate, 'referencesplitadjust'] 
                }
                #md$buy=ifelse(md$eligible==1 & md$date<kBackTestEndDate & md$r2>kR2Fit/100 & md$slopereturn.refperiod >kSlope/100 & md$return.refperiod >kSlope/100 & md$asettle<md$predict & md$asettle>0.7*md$predict & md$date > symbols$financialdate[i] + 90*24*60*60 & md$oversold & md$rsi<kRSIEntry & (md$referencevalue-md$asettle)*100/md$asettle>kUpside & md$drawdown<1,1,0)
                md$buy=ifelse(md$eligible==1 & md$date<kBackTestEndDate & md$r2>kR2Fit/100 & md$slopereturn.refperiod >kSlope/100 & md$return.refperiod >kSlope/100 & md$date > symbols$financialdate[i] + 90*24*60*60 & md$oversold & md$rsi<kRSIEntry & (md$referencevalue-md$asettle)*100/md$asettle>kUpside & md$drawdown<1,1,0)
                md$buycount<-ave(md$buy, md$month, FUN = cumsum)
                md$buy=ifelse(md$buycount==1 & Ref(md$buycount,-1)==0,1,0) # this gives unique  value to buy signal for md
                md$sell=ifelse(md$rsi>kRSIExit,1,0)
                md$sellprice=md$asettle
                md$buyprice=md$asettle
                md$positionscore=md$rsi
                signals=rbind(signals,md)
        }else{
                print(paste("Market Data not found for",symbols$symbol[i]))
        }
}

saveRDS(signals,file="signals.rds")

signals.filtered=na.exclude(signals)
signals.filtered=signals.filtered[signals.filtered$date>=as.POSIXct(kBackTestStartDate,tz=kTimeZone) & signals.filtered$date<=as.POSIXct(kBackTestCloseAllDate,tz=kTimeZone) & (signals.filtered$buy>0 |signals.filtered$sell >0),]
signals.filtered=signals.filtered[order(signals.filtered$date,signals.filtered$positionscore),]

#### GENERATE TRADES ####
trades=data.frame()
trades=data.frame(symbol=signals.filtered$symbol,trade="BUY",size=0,entrytime=signals.filtered$date,entryprice=signals.filtered$asettle,exitreason="Open",bars=0,buy=signals.filtered$buy,stringsAsFactors = FALSE)
tradesbuy=trades[trades$buy==1,]
tradessell=trades[trades$buy==0,]
tradessell=dplyr::select(tradessell,symbol=symbol,exittime=entrytime,exitprice=entryprice)
tradesbuy$exitdatepotential=tradesbuy$entrytime+kExitDays*24*60*60
tradesbuy.sell=dplyr::inner_join(tradesbuy,tradessell,by=c("symbol"))
tradesbuy.sell.closed=dplyr::filter(tradesbuy.sell,exittime>=exitdatepotential)
tradesbuy.sell.closed=dplyr::group_by(tradesbuy.sell.closed,.dots=c("symbol","entrytime"))%>%arrange(exittime)
tradesbuy.sell.closed=dplyr::top_n(tradesbuy.sell.closed,-1,wt=exittime)
if(nrow(tradesbuy.sell.closed)>0){
        tradesbuy.sell.closed$exitreason="RegularExit"
}
trades.open=anti_join(tradesbuy,tradesbuy.sell.closed,by=c("symbol","entrytime"))
trades=dplyr::bind_rows(tradesbuy.sell.closed,trades.open)
trades=dplyr::arrange(trades,entrytime)
trades=dplyr::select(trades,symbol,trade,size,entryprice,entrytime,exitprice,exittime,bars,exitreason)
trades=as.data.frame(trades)
if(nrow(trades)>0){
        trades$size=0
        currentmonth=""
        positionsThisMonth=0
        #update size
        for( i in seq_len(nrow(trades))){
                print(paste("Potential Trade Row #",i,sep=""))
                if(strftime(trades$entrytime[i],format="%y%m")!=currentmonth){
                        currentmonth=strftime(trades$entrytime[i],format="%y%m")
                        positionsThisMonth=0
                }
                gap=0
                if(positionsThisMonth<kTradesPerMonth){
                        out = CalculateNPV(trades,trades$entrytime[i])
                        npv=out[[1]]
                        gap=TargetPortfolioValue[TargetPortfolioValue$date==trades$entrytime[i],c("value")]-npv
                }        
                if(gap>0){
                        sizevalue=gap/(kTradesPerMonth-positionsThisMonth)
                        size=round(sizevalue/trades$entryprice[i])
                        trades$size[i]=size
                        positionsThisMonth=positionsThisMonth+1
                }
        }
        saveRDS(trades,file="trades.rds")
        trades=dplyr::filter(trades,size>0)
        trades=revalPortfolio(trades,kBrokerage,realtime=FALSE)
}

#### MAP TO DERIVATIES ####
#### WRITE TO REDIS ####
if(!kBackTest & kWriteToRedis){
        saveRDS(trades,file=paste(args[2],"_trades_",strftime(Sys.time(),format="%Y-%m-%d %H-%M-%S"),".rds",sep=""))
        saveRDS(signals,file=paste(args[2],"_signals_",strftime(Sys.time(),format="%Y-%m-%d %H-%M-%S"),".rds",sep=""))
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
        placeRedisOrder(trades,referencetime,order,args[3],setLimitPrice=TRUE)
}

#### BACKTEST ####
if(kBackTest){
        bizdays=unique(signals$date)
        if(kBackTestCloseAllDate<Sys.Date()){
                trades[which(is.na(trades$exittime)),'exittime']=as.POSIXct(kBackTestCloseAllDate,tz=kTimeZone)
        }
        bizdays=bizdays[bizdays>=as.POSIXct(kBackTestStartDate,tz=kTimeZone) & bizdays<=as.POSIXct(kBackTestCloseAllDate,tz=kTimeZone)]
        pnl<-data.frame(bizdays,realized=0,unrealized=0,brokerage=0)
        cumpnl<-CalculateDailyPNL(trades,pnl,kBrokerage)
        
        # calculate sharpe
        CumPNL <-  cumpnl$realized + cumpnl$unrealized - cumpnl$brokerage
        DailyPNLWorking <-  CumPNL - Ref(CumPNL, -1)
        DailyPNLWorking <-  ifelse(is.na(DailyPNLWorking),0,DailyPNLWorking)
        DailyReturnWorking <-  ifelse(cumpnl$longnpv == 0, 0,DailyPNLWorking / cumpnl$longnpv)
        sharpe <- sharpe(DailyReturnWorking)
        
        # calculate IRR
        cumpnl$cashflow[nrow(cumpnl)]=cumpnl$cashflow[nrow(cumpnl)]+(cumpnl$longnpv+cumpnl$shortnpv)[nrow(cumpnl)]
        xirr=xirr(cumpnl$cashflow,cumpnl$bizdays,trace = TRUE)
        
        # print stats
        print(paste("# Trades:",nrow(trades)))
        print(paste("Profit as Sum of Equity Trades:",specify_decimal(sum(DailyPNLWorking),0)))
        print(paste("Win Ratio:",sum(trades$abspnl>0)*100/nrow(trades)))
        print(paste("Avg % Gain Per Winning Trade:",specify_decimal(mean(trades$netpercentprofit[trades$netpercentprofit>0])*100,1)))
        print(paste("Avg % Loss Per Losing Trade:",specify_decimal(mean(trades$netpercentprofit[trades$netpercentprofit<0])*100,1)))
        print(paste("Avg % Profit Per Trade:",specify_decimal(mean(trades$netpercentprofit)*100,1)))
        print(paste("Avg Holding Days:",sum(trades$bars)/nrow(trades)))
        
        # graph portfolio npv, target and gap
        TargetPortfolioValue.bizdays=TargetPortfolioValue[match(cumpnl$bizdays,TargetPortfolioValue$date),]
        my.range <- range(cumpnl$longnpv+cumpnl$shortnpv)
        plot(
                x = cumpnl$bizdays,
                y = cumpnl$longnpv+cumpnl$shortnpv,
                type = 'l',
                main = "Portfolio Buildup",
                ylab = "INR",
                xlab = "Date",
                axes = FALSE,
                ylim = my.range,
                lty = 1
        )
        
        axis.POSIXct(
                1,
                cumpnl$bizdays,
                at = seq(
                        min(cumpnl$bizdays),
                        max(cumpnl$bizdays) + 90,
                        by = "3 mon"
                ),
                format = "%m-%Y",
                labels=TRUE
        )
        
        minValue = min(cumpnl$longnpv+cumpnl$shortnpv)
        maxValue = max(cumpnl$longnpv+cumpnl$shortnpv)
        points = pretty(seq(
                minValue,
                maxValue,
                by = (maxValue - minValue) / 5
        ))
        
        axis(
                2,
                at = points,
                labels = paste(points / 1000000,
                               "M",
                               sep = ""),
                las = 1
        )
        
        lines(
                x = cumpnl$bizdays,
                y = TargetPortfolioValue.bizdays$value,
                ylab = "",
                xlab = "",
                lty = 2
        )
        lines(
                x = cumpnl$bizdays,
                y = (TargetPortfolioValue.bizdays$value - cumpnl$longnpv+cumpnl$shortnpv),
                xlab = "",
                ylab = "",
                lty = 3
        )
        legend(
                "topleft",
                lty = c(1, 2, 3),
                legend = c(
                        "Actual Portfolio Value",
                        "Target Portfolio Value",
                        "Gap"
                ),
                cex = 0.85,
                bty = "n",
                xjust = 1
        )
        
        # graph p&l
        plot(
                x = cumpnl$bizdays,
                y = (cumpnl$realized + cumpnl$unrealized),
                type = 'l',
                main = "Profit & Loss",
                xlab = "Date",
                ylab = "Profit",
                axes = FALSE
        )
        axis.POSIXct(
                1,
                cumpnl$bizdays,
                at = seq(
                        min(cumpnl$bizdays),
                        max(cumpnl$bizdays) + 90,
                        by = "3 mon"
                ),
                format = "%m-%Y"
        )
        minProfit = min(cumpnl$realized + cumpnl$unrealized)
        maxProfit = max(cumpnl$realized + cumpnl$unrealized)
        points = pretty(seq(
                minProfit,
                maxProfit,
                by = (maxProfit - minProfit) / 5
        ))
        axis(
                2,
                at = points,
                labels = paste(points / 1000000,
                               "M",
                               sep = ""),
                las = 1
        )
        legend(
                "topleft",
                legend= c(paste("Sharpe:",RTrade::specify_decimal(sharpe,2),sep=" "),
                          paste("XIRR:",RTrade::specify_decimal(xirr,2),sep=" ")),
                bty="n"
        )
        
        # benchmark vs strategy
        # Cumul                 Year 1 Year 2 Year 3 ....
        # IRR
        # Sharpe
        # YE Equity
}

#### EXECUTION SUMMARY ####
if(!kBackTest){
        generateExecutionSummary(trades,unique(c(signals$date,as.POSIXct(strftime(Sys.Date(),tz=kTimeZone)))),kBackTestStartDate,kBackTestCloseAllDate,args[2],args[3],kSubscribers,kBrokerage,kCommittedCapital,kMarginOnUnrealized = FALSE,realtime=FALSE)
}
#### PRINT RUN TIME ####
timer.end=Sys.time()
runtime=timer.end-timer.start
print(runtime)