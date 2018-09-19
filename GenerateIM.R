strategies=c("VALUE01","VALUE02","VALUE03","VALUE04","VALUE05")
for(s in strategies){
        filename=paste(strftime(Sys.Date(),format="%Y%m%d"),"_",s,".pdf",sep="")
        rmarkdown::render("IM.SMCLO.V1.1.Rmd",params=list(algorithm=s),output_file=filename)
}