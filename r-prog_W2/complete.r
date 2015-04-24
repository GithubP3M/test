complete=function(directory,id=1:332){
  tot=list()
  for(i in 1:length(id)){
    filename=paste(formatC(id[i],width=3,flag="0"),".csv",sep="")
    dirname=paste(directory,filename,sep="/")
    a=read.csv(dirname)
    tot[[i]]=a
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
  
  l=length(comp)
  nobs=c()
  id=c()
  for (i in 1:l){
    id=rbind(id,comp[[i]][1,4])
    nobs=rbind(nobs,dim(comp[[i]])[1])
  }
  matr=cbind(id,nobs)
  colnames(matr)=c("id","nobs")
  rownames(matr)=c(1:dim(matr)[1])
  return(data.frame(matr))
  #return(comp)
}