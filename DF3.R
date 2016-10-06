#For the Files of the type snapshot

library(XML);
folder="20160923"
#setwd("C:/Users/Pankaj/Documents/Seafile/servers/FundamentalData/20160730")
setwd(paste("/home/psharma/Seafile/servers/FundamentalData/",folder,sep=""))

# Opening files in R

#path <- "C:\\Users\\admin\\Desktop\\Ingovern\\sample directory"
files <- list.files(pattern = "_SNAPSHOT.xml")

# generating empty DF 
df3 <- data.frame(CompanyName = character(0),RecentSplitDate = integer(0),
                    RecentSplitRatio = integer(0), Employees = integer(0), SharesOutstandingDate =integer(0), 
                    SharesOutstanding = integer(0), TotalFloat = integer(0), CommonShareholders =integer(0),
                    TRBCCode = integer(0), PriceCurrency = character(0), ReportingCurrency = character(0), 
                    ExchangeRate = integer(0), Pdate = integer(0), LastPrice = integer(0),
                    High =integer(0),	Low =integer(0), 	AvgVol = integer(0),	EV =integer(0), stringsAsFactors = FALSE)
for (j in 1:length(files))
{
  xmlfile <- xmlParse(files[j])
  
  xpath <- "//ReportSnapshot/CoIDs/CoID[@Type='CompanyName']"
  companyName <- sapply((getNodeSet(xmlfile,xpath)),xmlValue)
  
  xpath <- "//ReportSnapshot/Issues/Issue/MostRecentSplit"
  recentSplitNode <- getNodeSet(xmlfile,xpath)
  recentSplitDate <- NA_character_
  recentSplitRatio <-NA_character_
  if (length(recentSplitNode) > 0 ){
    recentSplitDate <-  sapply(recentSplitNode,xmlGetAttr,"Date")
    recentSplitRatio <- sapply(recentSplitNode,xmlValue)
  }
    
  
  xpath <- "//ReportSnapshot/CoGeneralInfo/Employees"
  employeesNode <- getNodeSet(xmlfile,xpath)
  employees <-NA_character_
  if (length(employeesNode)>0)
  employees <- sapply(employeesNode,xmlValue)
  
  xpath <- "//ReportSnapshot/CoGeneralInfo/SharesOut"
  sharesOutNode <- getNodeSet(xmlfile,xpath)
  sharesOutstandingDate <- NA_character_
  sharesOutstanding <- NA_character_
  totalfloat <- NA_character_
  if (length(sharesOutNode) >0 ){
  sharesOutstandingDate <- sapply(sharesOutNode,xmlGetAttr,"Date")
  sharesOutstanding <- sapply(sharesOutNode,xmlValue)
  totalfloat <- sapply(sharesOutNode,xmlGetAttr,"TotalFloat")
  }
  
  xpath <- "//ReportSnapshot/CoGeneralInfo/CommonShareholders" 
  commonShareHoldersNode <- getNodeSet(xmlfile,xpath)
  commonShareHolders <- NA_character_
  if (length(commonShareHoldersNode)>0){
    commonShareHolders <- sapply(commonShareHoldersNode,xmlValue)
  }
  
  xpath <- "//ReportSnapshot/peerInfo/IndustryInfo/Industry[@type = 'TRBC']"
  codeTRBCNode <- getNodeSet(xmlfile,xpath)
  codeTRBC <- NA_character_
  if (length(codeTRBCNode)>0 )
  codeTRBC<- sapply(codeTRBCNode,xmlGetAttr,"code")
  
  xpath <- "//ReportSnapshot/Ratios" 
  ratioNode <- getNodeSet(xmlfile,xpath)
  reportingCurrency <- NA_character_
  priceCurrency <- NA_character_
  exchangeRate <- NA_character_
  if (length(ratioNode)>0)  {
  reportingCurrency <- sapply(ratioNode,xmlGetAttr,"ReportingCurrency")
  priceCurrency <- sapply(ratioNode,xmlGetAttr,"PriceCurrency")
  exchangeRate <- sapply(ratioNode,xmlGetAttr,"ExchangeRate")
  }
    
  xpath <- "//ReportSnapshot/Ratios/Group[@ID = 'Price and Volume']/Ratio[@FieldName = 'NPRICE']"
  lastPriceNode <- getNodeSet(xmlfile,xpath)
  lastPrice <- NA_character_
  if (length(lastPriceNode)>0)
  lastPrice <- sapply(lastPriceNode,xmlValue)
  
  xpath <-"//ReportSnapshot/Ratios/Group[@ID = 'Price and Volume']/Ratio[@FieldName = 'NHIG']"
  highNode <- getNodeSet(xmlfile,xpath)
  high <- NA_character_
  if (length(highNode)>0)
    high <- sapply(highNode,xmlValue)
  
  xpath <- "//ReportSnapshot/Ratios/Group[@ID = 'Price and Volume']/Ratio[@FieldName = 'NLOW']"
  lowNode <- getNodeSet(xmlfile,xpath)
  low <- NA_character_
  if (length(lowNode)>0)
    low<- sapply(lowNode,xmlValue)
  
  xpath <- "//ReportSnapshot/Ratios/Group[@ID = 'Price and Volume']/Ratio[@FieldName = 'VOL10DAVG']"
  avgVolNode <- getNodeSet(xmlfile,xpath)
  avgVol <- NA_character_
  if (length(avgVolNode)>0)
    avgVol<- sapply(lowNode,xmlValue)
  
  xpath <- "//ReportSnapshot/Ratios/Group[@ID = 'Price and Volume']/Ratio[@FieldName = 'EV']"
  eVNode <- getNodeSet(xmlfile,xpath)
  eV <- NA_character_
  if (length(eVNode)>0)
    eV <- sapply(eVNode,xmlValue)
  
  xpath <- "//ReportSnapshot/Ratios/Group[@ID = 'Price and Volume']/Ratio[@FieldName = 'PDATE']"
  pDateNode <- getNodeSet(xmlfile,xpath)
  pDate <- NA_character_
  if (length(pDateNode)>0){
  pDate <- sapply(pDateNode,xmlValue)
  pDate <- substring(pDate,1,10)
  }
  
  n.row <- c(companyName,recentSplitDate,recentSplitRatio,employees,sharesOutstandingDate,sharesOutstanding,totalfloat,
             commonShareHolders,codeTRBC,priceCurrency,reportingCurrency,exchangeRate,pDate,lastPrice,high,low,avgVol,eV)
  df3[nrow(df3)+1,] <- n.row   
  }