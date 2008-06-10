`likelihood` <-
function(mito.lm.obj,meth.lm.obj){

m=nrow(mito.lm.obj$resid1)
n=ncol(mito.lm.obj$resid1)
ratio1<-ratio2<-matrix(0,m,n)


for(i in 1:n){

ratio1[,i]<-(mito.lm.obj$sd.resid1[,i]^2)-(meth.lm.obj$sd.resid1[,i]^2)
ratio2[,i]<-(mito.lm.obj$sd.resid2[,i]^2)-(meth.lm.obj$sd.resid2[,i]^2)

}

result<-list(ratio1=ratio1,ratio2=ratio2)
result

}

