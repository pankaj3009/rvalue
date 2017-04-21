library(XML)
library(gdata)
library(TTR)
library(zoo)

generatedf4=TRUE

folder="20160923"
#setwd("C:/Users/Pankaj/Documents/Dropbox/servers/FundamentalData/20160730")
setwd(paste("/home/psharma/Dropbox/servers/FundamentalData/",folder,sep=""))

slope <- function (x) {
        res <- (lm(log(x) ~ seq(1:length(x))))
        res$coefficients[2]
}

r2 <- function(x) {
        res <- (lm(log(x) ~ seq(1:length(x))))
        summary(res)$r.squared
}

args = commandArgs(trailingOnly = TRUE)
if (length(args) > 0)
{
  finyear = args[1]
  taxRate = args[2]
  floorInterestRate = args[3]
  growthRate = args[4]
  riskFreeRate = args[5]
  beta = args[6]
  equityMarketReturn = args[7]
  seedWACC <- args[8]
  
} else{
  finyear = 2016
  taxRate = 0.30
  floorInterestRate = 0.09
  growthRate = 0.05
  riskFreeRate = 0.07
  beta = 1.0
  equityMarketReturn = 0.12
  seedWACC <- 52 / 100
}

if(generatedf4){
symbols <- c(unique(df2$LONGNAME), unique(df3$CompanyName))
symbols <- unique(symbols)

fileName <- list.files(pattern = "_FINSTAT.xml")
estimatesFile <- list.files(pattern = "_ESTIMATES.xml")

df4 <-
  data.frame(
    LONGNAME = character(0),
    TICKER=character(0),
    EBITMEAN = integer(0),
    AACR = integer(0),
    AITL = integer(0),
    LAPB = integer(0),
    STLD = character(),
    SCSI = integer(0),
    CMIN = integer(0),
    SINN = integer(0),
    COMMON_SHARE_OUTSTANDING_IN_MILLIONS = integer(0),
    sharesOutstandingDate=as.Date(character()),
    LATEST_SHARE_PRICE = integer(0),
    THEORETICALVALUE = integer(0),
    UPSIDE = integer(0),
    ROCE=integer(0),
    DIVIDENDPAYOUTPERC=integer(0),
    MCAP=integer(0),
    FINDATE=integer(0),
    stringsAsFactors = FALSE
  )

df2$ENDDATE <- as.Date(df2$ENDDATE, '%Y-%m-%d')
df2$EndYear <- as.numeric(format(df2$ENDDATE, '%Y'))
#nameComp <- 0
#ebitValue <- 0

  search <- unique(df2$LONGNAME)

#which(df2$ESTIMATE=="EBITMEAN", arr.ind=TRUE)

for (i in 1:length(symbols))
  #for (i in 459:459)
    
{
  skip = FALSE
  nameComp <- symbols[i]
  print(paste(nameComp,i,sep=":"))
  # ebitmean <- df2[df2$ESTIMATE %in% c("EBITMEAN") &
  #                   df2$LONGNAME %in% c(nameComp), c("VALUE")]
  # if (length(ebitmean == 1)) {
  #   valueSOPI = df2[df2$LONGNAME == nameComp &
  #                df2$COA == "SOPI" & !is.na(df2$COA) &
  #                df2$EndYear == finyear &
  #                df2$PERIODINMONTH == 12 &
  #                !is.na(df2$PERIODINMONTH), c("VALUE")]
  #   if (length(valueSOPI == 1)) {
  #     symbolFinYear = finyear
  #     symbolDate = df2[df2$LONGNAME == nameComp &
  #                        df2$COA == "SOPI" & !is.na(df2$COA) &
  #                        df2$EndYear == finyear &
  #                        df2$PERIODINMONTH == 12, c("ENDDATE")]
  #     
  #   } else{
  #     valueSOPI = df2[df2$LONGNAME == nameComp &
  #                  df2$COA == "SOPI" & !is.na(df2$COA) &
  #                  (df2$EndYear == finyear - 1) &
  #                  df2$PERIODINMONTH == 12, c("VALUE")]
  #     if (length(valueSOPI == 1)) {
  #       symbolFinYear = finyear - 1
  #       symbolDate = df2[df2$LONGNAME == nameComp &
  #                          df2$COA == "SOPI" & !is.na(df2$COA) &
  #                          (df2$EndYear == finyear - 1) &
  #                          df2$PERIODINMONTH == 12, c("ENDDATE")]
  #     } else{
  #       skip = TRUE
  #     }
  #     
  #   }
  # }
  # if (length(ebitmean) == 0) 
    {
    # no estimates. use the latest finyear as proxy
    ebitmean=integer(0)
      ebitmean = df2[df2$LONGNAME == nameComp &
                     df2$COA == "SOPI" & !is.na(df2$COA) &
                     df2$EndYear == finyear &
                     df2$PERIODINMONTH == 12, c("VALUE")] 
    symbolFinYear = finyear
    symbolDate = df2[df2$LONGNAME == nameComp &
                       df2$COA == "SOPI" & !is.na(df2$COA) &
                       df2$EndYear == finyear &
                       df2$PERIODINMONTH == 12, c("ENDDATE")]
    valueSOPI=ebitmean
  }
  if (length(ebitmean) == 0) {
    # else use the prior fin year
    ebitmean = df2[df2$LONGNAME == nameComp &
                     df2$COA == "SOPI" & !is.na(df2$COA) &
                     (df2$EndYear == finyear - 1) &
                     df2$PERIODINMONTH == 12, c("VALUE")]
    symbolFinYear = finyear - 1
    symbolDate = df2[df2$LONGNAME == nameComp &
                       df2$COA == "SOPI" & !is.na(df2$COA) &
                       (df2$EndYear == finyear - 1) &
                       df2$PERIODINMONTH == 12, c("ENDDATE")]
    valueSOPI=ebitmean
  }
  
  if (length(ebitmean) == 0) {
    #exit this symbol processing as we dont appear to have EBIT for estimates.
    skip = TRUE
  }
  ebitmean <- as.numeric(ebitmean)
  valueSOPI<-as.numeric(valueSOPI)
  ticker=df2[df2$LONGNAME == nameComp,c("TICKER")][1]
  

  if (!skip) {
    ebitmean<-ebitmean[1]
    symbolDate<-symbolDate[1]
    data<-df2[df2$LONGNAME == nameComp &
                !is.na(df2$COA) &
                df2$ENDDATE == symbolDate &
                df2$PERIODINMONTH == 12 &
                !is.na(df2$ENDDATE), c("COA","VALUE")]
    
    valueAACR <-as.numeric(data[data$COA == "AACR", c("VALUE")])
    valueAACR<-ifelse (length(valueAACR) == 0,0,valueAACR[1])
    
    valueAITL <-  as.numeric(data[data$COA == "AITL", c("VALUE")])
    valueAITL<-ifelse (length(valueAITL) == 0 ,0,valueAITL[1])
    
    valueLAPB <- as.numeric(data[data$COA == "LAPB", c("VALUE")])
    valueLAPB<-ifelse (length(valueLAPB) == 0, 0,valueLAPB[1])

    valueSTLD <-as.numeric(data[data$COA == "STLD", c("VALUE")])
    valueSTLD<-ifelse (length(valueSTLD) == 0, 0,valueSTLD[1])
    
    valueLSTD <- as.numeric(data[data$COA == "LSTD", c("VALUE")])
    valueLSTD<-ifelse (length(valueLSTD) == 0,0,valueLSTD[1])

    valueSCSI <- as.numeric(data[data$COA == "SCSI", c("VALUE")])
    valueSCSI<-ifelse (length(valueSCSI) == 0, 0,valueSCSI[1])
    
    valueCMIN <- as.numeric(data[data$COA == "CMIN", c("VALUE")])
    valueCMIN=ifelse (length(valueCMIN) == 0,0,valueCMIN)
    
    valueSINN <-as.numeric(data[data$COA == "SINN", c("VALUE")])
    valueSINN=ifelse (length(valueSINN) == 0,0,valueSINN[1])
    commonSharesOutstanding <-  df3[df3$CompanyName == nameComp, 'SharesOutstanding']
    commonSharesOutstanding <- as.numeric(commonSharesOutstanding)
    commonSharesOutstandingInMillions <- commonSharesOutstanding / 1000000
    shareoutstandingdate<-(df3[df3$CompanyName == nameComp, 'SharesOutstandingDate'])
    if(length(commonSharesOutstanding)>0){
      
    lastPrice <-as.numeric(df3[df3$CompanyName == nameComp, 'LastPrice'])
    
    costOfBorrowing <- valueSINN / (valueSTLD + valueLSTD)
    if (is.infinite(costOfBorrowing))   costOfBorrowing <- 0
    if (is.nan(costOfBorrowing))  costOfBorrowing <- 0
    costOfDebt <- max(floorInterestRate, costOfBorrowing)
    
    #Calculation of FCFF
    valueFCFF <-ebitmean * (1 - taxRate) - (valueAACR - valueLAPB + valueAITL) * floorInterestRate
    
    
    #WACC Calculation
    valueQTEL <-as.numeric(data[data$COA == "QTEL", c("VALUE")])
    valueQTEL<-ifelse (length(valueQTEL) == 0,0,valueQTEL[1])
    
    valueATOT <-as.numeric(data[data$COA == "ATOT", c("VALUE")])
    valueATOT<-ifelse (length(valueATOT) == 0,0,valueATOT[1])
    
    valueLTCL <-as.numeric(data[data$COA == "LTCL", c("VALUE")])
    valueLTCL<-ifelse (length(valueLTCL) == 0,0,valueLTCL[1])
    
    ROCE<-ifelse(valueATOT-valueLTCL>0,round(valueSOPI*100/(valueATOT-valueLTCL),2),0)
    
    valueDDPS1<- as.numeric(data[data$COA == "DDPS1", c("VALUE")])
    valueDPPS1=ifelse(length(valueDDPS1) == 0,0,valueDDPS1[1])
    
    valueCIAC<- as.numeric(data[data$COA == "CIAC", c("VALUE")])
    valueCIAC<-ifelse(length(valueCIAC) == 0,0,valueCIAC[1])
    
    # TODO : Need to adjust commonSharesOutstanding on account of splits/bonus.
    DIVIDENDPAYOUT<-round(ifelse(valueCIAC>0,valueDDPS1*commonSharesOutstandingInMillions*100/valueCIAC,0),2)
    
    netDebt <-valueSTLD - valueSCSI 
    costOfEquity <- riskFreeRate + beta * equityMarketReturn
    waccSeed <- growthRate+0.05
    wacc <- 0
    diff <- 1
    done = FALSE
    while (!done) {
      valueEV <- valueFCFF / (waccSeed - growthRate)
      CFForEquity <- valueEV - netDebt - valueCMIN
      EquityShareOfEV <-
        ifelse(
          CFForEquity / (CFForEquity + netDebt + valueCMIN) < 0.5,
          0.5,
          ifelse(
            CFForEquity / (CFForEquity + netDebt + valueCMIN) > 1,
            1,
            CFForEquity / (CFForEquity + netDebt + valueCMIN)
          )
        )
      wacc <-
        EquityShareOfEV * (equityMarketReturn * beta) + (1 - EquityShareOfEV) *
      costOfDebt
      # print(paste(nameComp,"waccSeed",length(waccSeed),"wacc",length(wacc),"growthrate",length(growthRate),sep=":"))
      if(length(abs(wacc-waccSeed))>1){
        print(paste("wacc-waccSeed length >1",nameComp,i,sep=":"))
      }
      if (abs(wacc - waccSeed) < 0.0001) {
        done = TRUE
      } else{
        waccSeed = wacc
      }
      if(waccSeed-growthRate<0){ # we set wacc == growthrate, because the company appears to have a really low cost of capital
        done=TRUE
        wacc=growthRate
      }
      
      #print(wacc)
    }
    
    cfEquity = valueEV - netDebt - valueCMIN      #EV - Net Debt - Minority Interest
    theoreticalShareValue <-
      cfEquity / commonSharesOutstandingInMillions
    
    value <-
      (theoreticalShareValue - lastPrice) * 100 / lastPrice
  
    MCAP= ifelse(lastPrice>0,commonSharesOutstanding*lastPrice/(10^9),0)
    n.row <-
      c(
        nameComp,
        ticker,
        ebitmean ,
        valueAACR,
        valueAITL,
        valueLAPB,
        valueSTLD,
        valueSCSI,
        valueCMIN,
        valueSINN,
        commonSharesOutstandingInMillions,
        shareoutstandingdate,
        lastPrice,
        trunc(theoreticalShareValue, 0),
        trunc(value, 0),
        ROCE,
        DIVIDENDPAYOUT,
        MCAP,
        symbolDate
      )
    df4[nrow(df4) + 1,] <- n.row
    df4[is.na(df4)] <- 0
  }
  }
  
}
}

if(!generatedf4){
  load("df4.Rdata")
  
}

df4$FINDATE=as.Date(as.numeric(df4$FINDATE))
RSIPeriod=2
df4$MCAP=as.numeric(df4$MCAP)
df4$DIVIDENDPAYOUTPERC=as.numeric(df4$DIVIDENDPAYOUTPERC)
df4$ROCE=as.numeric(df4$ROCE)
df4$UPSIDE=as.numeric(df4$UPSIDE)
df4$THEORETICALVALUE=as.numeric(df4$THEORETICALVALUE)
df4$CurrentRSI=-100
df4$AnnualizedSlope=-1
df4$r=-1
df4$sumproduct=-1
df4$OverSold=FALSE

startdate=as.Date(strptime(paste(finyear-1,"03","31",sep="-"),format="%Y-%m-%d"),tz="Asia/Kolkata")
enddate=as.Date(strptime(paste(finyear,"03","31",sep="-"),format="%Y-%m-%d"),tz="Asia/Kolkata")
startdate=adjust.next(startdate)
enddate=adjust.previous(enddate)

for (i in 1:nrow(df4)){
  symbol=df4[i,'TICKER']
  print(paste("Processing Daily Data:",symbol,sep=""))
  if (file.exists(paste("/home/psharma/Dropbox/rfiles/daily/", symbol, ".Rdata", sep = ""))){
    load(paste("/home/psharma/Dropbox/rfiles/daily/", symbol, ".Rdata", sep = ""))
    startindex=which(as.Date(md$date,tz="Asia/Kolkata")==startdate)
    endindex=which(as.Date(md$date,tz="Asia/Kolkata")==enddate)
    if(length(startindex)==1 && startindex>251){
      df4$CurrentRSI[i]=RSI(md$asettle, RSIPeriod)[startindex]
      df4$OverSold[i] = (runSum(RSI(md$asettle, 2) < 20, 2) ==2)[startindex]
      df4$AnnualizedSlope[i] = exp(slope(md$asettle[(startindex-251):startindex]))^252-1
      df4$r[i]=r2(md$asettle[(startindex-251):startindex])
      df4$sumproduct[i]=df4$AnnualizedSlope[i]*df4$r[i]
      lastprice=md$asettle[startindex]
      df4$UPSIDE[i]=(df4$THEORETICALVALUE[i]-lastprice)*100/lastprice
      df4$UPSIDE[i]=trunc(df4$UPSIDE[i],0)
      df4$UPSIDE[i]=as.numeric(df4$UPSIDE[i])
      df4$LATEST_SHARE_PRICE[i]=lastprice
    }
  }
}

shortlist<-df4[df4$UPSIDE>50 & df4$DIVIDENDPAYOUTPERC>10 & df4$ROCE>20 & df4$AnnualizedSlope>0 & df4$r>0.5,]
