`cutoff.plot` <-
function(mlda.data,cutoff.obj,IR=0.01,CR=1.4){

size<-ncol(mlda.data$R)
par(mfrow=c(2,2))
m<-1
if(!is.null(mlda.data$sample.names)){
sample.names<-mlda.data$sample.names
}
else{
sample.names<-paste("sample",size)
}
for(i in 1:size){
plot.new()
plot.window(range(1,1.8),range(0,0.4))
axis(1)
axis(2)
for(j in 1:ncol(cutoff.obj$rate[[i]]$pm.min)){
tmp1<-cutoff.obj$rate[[i]]$pm.min[,j]
tmp2<-cutoff.obj$rate[[i]]$mm.max[,j]
points(tmp1,tmp2)
}
title(sample.names[i],xlab="CR",ylab="IR")
abline(v=CR,h=IR,lty=2,col=2)
if(m%%4==0&m!=size){
dev.new()
par(mfrow=c(2,2))
}
m<-m+1

}


}

