# Creating Dataframe with a given XML file with parsing completed
library(XML)
folder="20160923"
#setwd("C:/Users/Pankaj/Documents/Seafile/servers/FundamentalData/20160730")
setwd(paste("/home/psharma/Seafile/servers/FundamentalData/",folder,sep=""))

files <- list.files(pattern = "_FINSTAT.xml")

# generating empty DF
df1 <-
  data.frame(
    COA = character(0),
    Description = character(0),
    Type = character(0),
    stringsAsFactors = FALSE
  )
samplelist <- list()

for (j in 1:length(files))
{
  print(files[j])
  xmlfile <- xmlParse(files[j])


  xpath <-
    "/ReportFinancialStatements/FinancialStatements/COAMap/mapItem"
  listCOA <- getNodeSet(xmlfile, xpath)

  # Adding new rows
  if (length(listCOA) > 0) {
    for (i in 1:length(listCOA))
    {
      newrow <-
        c(
          sapply(listCOA[i], xmlGetAttr, "coaItem"),
          sapply(listCOA[i], xmlValue),
          sapply(listCOA[i], xmlGetAttr, "statementType")
        )
      #df1[nrow(df1)+1,] <- newrow
      samplelist[[i]] <- newrow
    }
  }
}

df1 <-
  data.frame(matrix(
    unlist(samplelist),
    nrow = length(samplelist),
    byrow = T
  ), stringsAsFactors = FALSE)
colnames(df1) <- c("COA", "Description", "Type")
df1 <- unique(df1, by = "COA")
