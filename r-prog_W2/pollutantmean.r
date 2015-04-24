pollutantmean=function(directory,pollutant,id=1:332){
  coltot=c()
  for(i in id){
    filename=paste(formatC(i,width=3,flag="0"),".csv",sep="")
    dirname=paste(directory,filename,sep="/")
    a=read.csv(dirname)
    if(pollutant=="sulfate"){
      acol=a[,2]
      acol=acol[!is.na(acol)]
    }
    if(pollutant=="nitrate"){
      acol=a[,3]
      acol=acol[!is.na(acol)]
    }
    coltot=c(coltot,acol)
  }
  return(mean(coltot))
}