cassandrasocket=make.socket(host="91.121.168.138",port=4242)
for(l in 1:nrow(df2)){
        if(!is.na(df2[l,c("Type")])){
                millisecs=as.numeric(strptime(df2[l,c("ENDDATE")],tz="Asia/Kolkata","%Y-%m-%d"))*1000
                out=""
                switch (df2[l,c("Type")],
                        "BAL" = {out=paste("put ","india.nse.equity.s1.bal ",millisecs," ",df2[l,c("VALUE")]," coa=",df2[l,c("COA")]," symbol=",tolower(df2[l,c("TICKER")]),"\n",sep="")},
                        "INC" = {out=paste("put ","india.nse.equity.s1.inc ",millisecs," ",df2[l,c("VALUE")]," coa=",df2[l,c("COA")]," month=",df2[l,c("PERIODINMONTH")]," symbol=",tolower(df2[l,c("TICKER")]),"\n",sep="")},
                        "CAS" = {out=paste("put ","india.nse.equity.s1.cas ",millisecs," ",df2[l,c("VALUE")]," coa=",df2[l,c("COA")]," month=",df2[l,c("PERIODINMONTH")]," symbol=",tolower(df2[l,c("TICKER")]),"\n",sep="")},
                        {out=""}
                )
                if(out!=""){
                        write.socket(cassandrasocket,out)        
                }
                print(out)
        }
}
on.exit(close.socket(casssandrasocket))