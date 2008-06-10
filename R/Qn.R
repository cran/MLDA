`Qn` <-
function(re,myr=range(0.2,0.5)){

n<-nrow(re)
q1<-quantile(re,c(0.2,0.8))
lower1<-mean(re[re<q1[1]])
upper1<-mean(re[re>q1[2]])

q2<-quantile(re,0.5)

lower2<-mean(re[re<q2])
upper2<-mean(re[re>q2])

Qn<-(lower1-upper1)/(lower2-upper2)

Qn

}

