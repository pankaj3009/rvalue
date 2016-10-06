#library(doParallel)
library(XML)
assign("last.warning", NULL, envir = baseenv())
folder="20160923"
#setwd("C:/Users/Pankaj/Documents/Seafile/servers/FundamentalData/20160730")
setwd(paste("/home/psharma/Seafile/servers/FundamentalData/",folder,sep=""))

GetLastDay <- function(year, month)
{
  if (month == 1)
    return (paste(as.character(year), as.character(month), "31", sep = "-"))
  else if (month == 2)
    return (paste(as.character(year), as.character(month), "28", sep = "-"))
  else if (month == 3)
    return (paste(as.character(year), as.character(month), "31", sep = "-"))
  else if (month == 4)
    return (paste(as.character(year), as.character(month), "30", sep = "-"))
  else if (month == 5)
    return (paste(as.character(year), as.character(month), "31", sep = "-"))
  else if (month == 6)
    return (paste(as.character(year), as.character(month), "30", sep = "-"))
  else if (month == 7)
    return (paste(as.character(year), as.character(month), "31", sep = "-"))
  else if (month == 8)
    return (paste(as.character(year), as.character(month), "31", sep = "-"))
  else if (month == 9)
    return (paste(as.character(year), as.character(month), "30", sep = "-"))
  else if (month == 10)
    return (paste(as.character(year), as.character(month), "31", sep = "-"))
  else if (month == 11)
    return (paste(as.character(year), as.character(month), "30", sep = "-"))
  else if (month == 12)
    return (paste(as.character(year), as.character(month), "31", sep = "-"))
  else if (month == 0)
    return(NA_character_)
}

#This code is for creating the data frame in R for the file <Symbol_FINSTAT.xml>

args = commandArgs(trailingOnly = TRUE)
if (length(args) > 0)
{
  finyear = args[1]


} else{
  finyear = "2015"

}

fileName <- list.files(pattern = "_FINSTAT.xml")
estimatesFile <- list.files(pattern = "_ESTIMATES.xml")

# generating empty DF
#df2 <- data.frame(SYMBOL = character(0), ESTIMATE = character(0), DATE = integer(0), PERIODINMONTH = integer(0),  VALUE = integer(0), COA = character(), ENDDATE = integer(0), stringsAsFactors = FALSE)
FY <- as.integer(finyear)
Counter <- 0
Result <- list(NULL)
Size <- 1

AddItemDoubling <- function(item)
{
  if (.GlobalEnv$Counter == .GlobalEnv$Size)
  {
    length(.GlobalEnv$Result) <- .GlobalEnv$Size <- .GlobalEnv$Size * 2
  }

  .GlobalEnv$Counter <- .GlobalEnv$Counter + 1

  .GlobalEnv$Result[[.GlobalEnv$Counter]] <- item
}


#maximum <- max(length(fileName),length(estimatesFile))
#cl <- makeCluster(detectCores())
#registerDoParallel(cl)
# Opening files in R
for (k in 1:length(fileName))
{
  ptm <- proc.time()
  xmlfileFinstat <- xmlParse(fileName[k])

  xpath <-
    "/ReportFinancialStatements/CoIDs/CoID[@Type ='CompanyName']"
  cName <- sapply(getNodeSet(xmlfileFinstat, xpath), xmlValue)
  
  xpath<-    "/ReportFinancialStatements/Issues/Issue/IssueID[@Type ='Ticker']"
  cSymbol <- sapply(getNodeSet(xmlfileFinstat, xpath), xmlValue)
  
  xpath <-
    "/ReportFinancialStatements/FinancialStatements/AnnualPeriods/FiscalPeriod[@Type='Annual'and @EndDate]"
  endDateNode <- getNodeSet(xmlfileFinstat, xpath)
  endDate <- sapply(endDateNode, xmlGetAttr, "EndDate")
  if (length(endDateNode) > 0)
  {
    for (l in 1:length(endDateNode))
    {
      endDate.Final <- endDate[l]
      nthNode <- endDateNode[[l]]
      xpath <- "Statement"
      stNode <- getNodeSet(nthNode, xpath)


      #str1 <- "/ReportFinancialStatements/FinancialStatements/AnnualPeriods/FiscalPeriod[@Type='Annual']/Statement "
      # mainNodes <- getNodeSet(xmlfileFinstat, str1)    # Reaching all the 18 nodes together

      totalNodes <- length(stNode) # Statement Nodes
      for (i in 1:totalNodes)
      {
        nthNode <- stNode[[i]]
        xpath <- "FPHeader/PeriodLength"
        plNodeList <- getNodeSet(nthNode, xpath)
        periodLength <- 12
        if (length(plNodeList) > 0)
          periodLength <- sapply(plNodeList, xmlValue)
        estimate <- NA_character_
        xpath <- "FPHeader/StatementDate"
        statementDate <- sapply(getNodeSet(nthNode, xpath), xmlValue)
        xpath <- "lineItem"
        lineItems <- getNodeSet(nthNode, xpath)
        totalSubNodes <- length(lineItems)

        for (j in 1:totalSubNodes)
        {
          li <- lineItems[j]

          liValue <- sapply(li, xmlValue)

          coaCode <- sapply(li, xmlGetAttr, "coaCode")
          n.row  <-
            c(
              cName,
              cSymbol,              
              estimate,
              statementDate,
              periodLength,
              liValue,
              coaCode,
              endDate.Final
            )
          AddItemDoubling(n.row)
          #samplelist[[l+1]]<-n.row
          #df2[nrow(df2)+1,] <- n.row
        }
      }
    }
  }

  # for the Interim period

  xpath <-
    "/ReportFinancialStatements/FinancialStatements/InterimPeriods/FiscalPeriod[@Type='Interim'and @EndDate]"
  endDateNode <- getNodeSet(xmlfileFinstat, xpath)
  endDate <- sapply(endDateNode, xmlGetAttr, "EndDate")

  if (length(endDateNode) > 0)
    for (l in 1:length(endDateNode))
    {
      nthNode <- endDateNode[[l]]
      xpath <- "Statement"
      stNode <- getNodeSet(nthNode, xpath)
      endDate.Final <- endDate[l]

      #str2 <- "/ReportFinancialStatements/FinancialStatements/InterimPeriods/FiscalPeriod[@Type = 'Interim' ]/Statement"
      #interimNodes <- getNodeSet(xmlfileFinstat, str2)
      lengthInterimNodes <- length(stNode)

      for (i in 1:lengthInterimNodes)
      {
        nthNode <- stNode[[i]]
        xpath <- "FPHeader/PeriodLength"
        plNodeList <- getNodeSet(nthNode, xpath)

        periodLength <- 3
        if (length(plNodeList) > 0)
          periodLength <- sapply(plNodeList, xmlValue)

        #estimste <- "NA"

        xpath <- "FPHeader/StatementDate"
        statementDate <- sapply(getNodeSet(nthNode, xpath), xmlValue)

        xpath <- "lineItem"
        lineitems <- getNodeSet(nthNode, xpath)
        nodeslineitems <- length(lineitems)

        for (j in 1:nodeslineitems)
        {
          lineitemvalue <- sapply(lineitems[j], xmlValue)
          coaCode <- sapply(lineitems[j], xmlGetAttr, "coaCode")
          n.row  <-
            c(
              cName,
              cSymbol,
              NA_character_,
              statementDate,
              periodLength,
              lineitemvalue,
              coaCode,
              endDate.Final
            )
          l = l + 1

          AddItemDoubling(n.row)

        }
      }

    }


  #print(proc.time() - ptm)
  print(paste0("completed financials import:", fileName[k]))
}
# FINSTAT is working fine
#stopCluster(cl)

#Estimates
for (k in 1:length(estimatesFile))
{
  xmlfilestimates <- xmlParse(estimatesFile[k])

  xpath <- "/REarnEstCons/Company/CoName/Name"
  cName <- sapply(getNodeSet(xmlfilestimates, xpath), xmlValue)

  xpath<-    "/ReportFinancialStatements/Issues/Issue/IssueID[@Type ='Ticker']"
  
  xpath <- "/REarnEstCons/Company/SecurityInfo/Security/SecIds/SecId[@type ='TICKER' and @set ='LOCAL']"
  cSymbol <- sapply(getNodeSet(xmlfilestimates, xpath), xmlValue)
  
  xpath <-
    paste(
      "//REarnEstCons/ConsEstimates/FYEstimates/FYEstimate[@type = 'EBIT']/FYPeriod[@periodType='A' and @fYear ='",
      as.character(FY),
      "']",
      "/ConsEstimate[@type ='Mean']/ConsValue[@dateType ='CURR']",
      sep = ""
    )
  meanNode <- getNodeSet(xmlfilestimates, xpath)
  meanValue <- sapply(meanNode, xmlValue)
  if (length(meanValue) == 0)
    meanValue <- NA_character_

  xpath <-
    paste(
      "//REarnEstCons/ConsEstimates/FYEstimates/FYEstimate[@type = 'EBIT']/FYPeriod[@periodType='A' and @fYear ='",
      as.character(FY),
      "']",
      "/ConsEstimate[@type ='StdDev']/ConsValue[@dateType ='CURR']",
      sep = ""
    )
  sdNode <- getNodeSet(xmlfilestimates, xpath)
  sdValue <- sapply(sdNode, xmlValue)
  if (length(sdValue) == 0)
    sdValue <- NA_character_

  xpath <-
    paste(
      "//REarnEstCons/ConsEstimates/FYEstimates/FYEstimate[@type = 'EBIT']/FYPeriod[@periodType='A' and @fYear ='",
      as.character(FY),
      "']",
      "/ConsEstimate[@type ='NumOfEst']/ConsValue[@dateType ='CURR']",
      sep = ""
    )
  numofEstNode <- getNodeSet(xmlfilestimates, xpath)
  numofEstValue <- sapply(numofEstNode, xmlValue)
  if (length(numofEstValue) == 0)
    numofEstValue <- NA_character_

  est.name <- c("EBITMEAN", "EBITStdDev", "EBITNumofEst")
  est.value <- c(meanValue, sdValue, numofEstValue)
  #estvalue <- as.list(estvalue)

  xpath <-
    paste(
      "//REarnEstCons/ConsEstimates/FYEstimates/FYEstimate[@type = 'EBIT']/FYPeriod[@periodType='A' and @fYear ='",
      as.character(FY),
      "']",
      sep = ""
    )
  endmonthNode <- getNodeSet(xmlfilestimates, xpath)
  endmonthValue <- sapply(endmonthNode, xmlGetAttr, "endMonth")

  pim <- "12"
  if (length(endmonthValue) == 0)
    pim <- NA_character_

  if (length(endmonthValue) == 0) {
    endmonthValue <- 0
  }

  fy <- FY
  fm <- as.numeric(endmonthValue)
  if(length(fm)>1){
    fm=fm[1]
  }
  
  if(length(fy)>1 | length(fm)>1){
  print(paste("Symbol:",estimatesFile[k], "size of fy:",length(fy),"size of fm:",length(fm),sep=""))
  }
  for (i in 1:3)
  {
    n.row  <-
      c(cName,
        cSymbol,
        est.name[i],
        GetLastDay(fy, fm),
        pim ,
        est.value[i],
        NA_character_,
        NA_character_)
   if(!is.na(GetLastDay(fy,fm))){
     AddItemDoubling(n.row)
     
   }
    #df2[nrow(df2)+1,] <- n.row
  }

  #Reading BUY SELL HOLD etc. From Estimates File

  buy  <-
    "//REarnEstCons/ConsEstimates/Recommendations/STOpinion/ConsOpinion[@desc = 'BUY']/ConsOpValue/ConsValue[@dateType = 'CURR']"
  sell <-
    "//REarnEstCons/ConsEstimates/Recommendations/STOpinion/ConsOpinion[@desc = 'SELL']/ConsOpValue/ConsValue[@dateType = 'CURR']"
  underperform <-
    "//REarnEstCons/ConsEstimates/Recommendations/STOpinion/ConsOpinion[@desc = 'UNDERPERFORM']/ConsOpValue/ConsValue[@dateType = 'CURR']"
  outperform <-
    "//REarnEstCons/ConsEstimates/Recommendations/STOpinion/ConsOpinion[@desc = 'OUTPERFORM']/ConsOpValue/ConsValue[@dateType = 'CURR']"
  hold <-
    "//REarnEstCons/ConsEstimates/Recommendations/STOpinion/ConsOpinion[@desc = 'HOLD']/ConsOpValue/ConsValue[@dateType = 'CURR']"

  buyNode <- getNodeSet(xmlfilestimates, buy)
  sellNode <- getNodeSet(xmlfilestimates, sell)
  underperformNode <- getNodeSet(xmlfilestimates, underperform)
  outperformNode <-  getNodeSet(xmlfilestimates, outperform)
  holdNode <- getNodeSet(xmlfilestimates, hold)

  grp <- c("BUY" , "SELL", "UNDERPERFORM", "OUTPERFORM", "HOLD")

  buy.value <- sapply(buyNode, xmlValue)
  sell.value <- sapply(sellNode, xmlValue)
  underperform.value <- sapply(underperformNode, xmlValue)
  outperform.value <-  sapply(outperformNode, xmlValue)
  hold.value <- sapply(holdNode, xmlValue)

  if (length(buy.value) == 0)
    buy.value <- NA_character_
  if (length(sell.value) == 0)
    sell.value <- NA_character_
  if (length(underperform.value) == 0)
    underperform.value <- NA_character_
  if (length(outperform.value) == 0)
    outperform.value <- NA_character_
  if (length(hold.value) == 0)
    hold.value <- NA_character_

  result <-
    c(buy.value ,
      sell.value,
      underperform.value,
      outperform.value,
      hold.value)
  datevalue <- GetLastDay(fy, fm)
  if(length(datevalue)>1){
    print(paste("Symbol:",estimatesFile[k], "size of datevalue:",length(datevalue),"k:",k,sep=""))
    
  }
  #pim <- "12"

  for (i in 1:5)
  {
    n.row  <- c(cName, cSymbol,grp[i], datevalue, pim, result[i], NA_character_, NA_character_)
    if(!is.na(datevalue)){
      AddItemDoubling(n.row)
    }
    if(length(n.row)>8){
      paste("Symbol:",estimatesFile[k],"k:",k)
    }
    #df2[nrow(df2)+1,] <- n.row
  }
  #print(paste0("completed estimates import:", estimatesFile[k]))
}
Result <-
  Result[!sapply(Result, is.null)] # Remove null values from Result
df2 <-
  data.frame(matrix(unlist(Result),
                    nrow = length(Result),
                    byrow = T), stringsAsFactors = FALSE)
colnames(df2) <-
  c("LONGNAME",
    "TICKER",
    "ESTIMATE",
    "DATE",
    "PERIODINMONTH",
    "VALUE",
    "COA",
    "ENDDATE")
#df2 <- df2[order(df2$SYMBOL), ]
df2=unique(df2)
Infy<-df2[grep("Infosys",df2$SYMBOL),]
#write.csv(df2,file = paste("DF2_",as.character(FY),".csv", sep=""))
for(i in 1:length(Result)){
  if(length(Result[[i]])!=8){
    print(paste(Result[[i]][2],Result[[i]][6],length(Result[[i]])))
  }
}
