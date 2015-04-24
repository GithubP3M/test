corr=function(directory,threshold=0){
  nobs=complete(directory)
  #loop to find which file respect the threshold
  fileid=c()
  for (obs in 1:dim(nobs)[1]){
    if (nobs[obs,2]>threshold){
      fileid=rbind(fileid,nobs[obs,1])
    }
  }
  #loop to read the necessary files
  tot=list()
  if (length(fileid)==0){
    corcol=numeric(0)
    return(corcol)
  }else{
    for(i in 1:length(fileid)){
      filename=paste(formatC(fileid[i],width=3,flag="0"),".csv",sep="")
      dirname=paste(directory,filename,sep="/")
      a=read.csv(dirname)
      tot[[i]]=a
    }
  }
  
  
  comp=list()
  for (i in 1:length(tot)){
    #linetot=list()
    linetot=c()
    iter=1
    list_it=1
    size=dim(tot[[i]])[1]
    while (iter<=size){
      if (!any(is.na(tot[[i]][iter,]))){
        #linetot[[list_it]]=tot[[i]][iter,]
        linetot=rbind(linetot,tot[[i]][iter,])
        list_it=list_it+1
      } else {
        list_it=list_it
      }
      iter=iter+1
    }
    comp[[i]]=linetot
  }
  
  #calculate corr
  corcol=c()
  for (ob in 1:length(comp)){
    co=cor(comp[[ob]][,2],comp[[ob]][,3])
    corcol=c(corcol,co)
  }
  return(corcol)
}