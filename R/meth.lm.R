`meth.lm` <-
function(middle.lm.obj,mito.lm.obj){
                                
         ds<-mito.lm.obj$ds
         dataset=mito.lm.obj$dataset
         low1=middle.lm.obj$low1
         low2=middle.lm.obj$low2
         list=mito.lm.obj$list
         result=mito.lm.obj
         seq_flag=mito.lm.obj$mito.flag
         nr<-nrow(dataset$R)
         nc<-ncol(dataset$R)
                               
         lm1<-lm2<-vector("list",nc)
         fit1<-fit2<-resid1<-resid2<-sd.resid1<-sd.resid2<-matrix(0,nr,nc)
         r2<-r2.ds<-lamda1<-lamda2<-vector(length=nc)
                                
         size<-nc                  
         for(i in 1:size){
                                
               lm1[[i]]<-line(dataset$R[low1[,i],i],dataset$G[low1[,i],i])
               
               if(ds){
               
                      lm2[[i]]<-line(dataset$Gds[low2[,i],i],dataset$Rds[low2[,i],i])
               }
               else{
                                        
                      lm2[[i]]<-line(dataset$Rds[low2[,i],i],dataset$Gds[low2[,i],i])
                                
               }
            fit1[,i]<-dataset$R[,i]*coef(lm1[[i]])[2]+coef(lm1[[i]])[1]
            resid1[,i]<-dataset$G[,i]-fit1[,i]
            if(ds){
            
                  fit2[,i]<-dataset$Gds[,i]*coef(lm2[[i]])[2]+coef(lm2[[i]])[1]
                  resid2[,i]<-dataset$Rds[,i]-fit2[,i]
                  
           }
            else{
            
                  fit2[,i]<-dataset$Rds[,i]*coef(lm2[[i]])[2]+coef(lm2[[i]])[1]
                  resid2[,i]<-dataset$Gds[,i]-fit2[,i]
                  
           }
                                                                                
        }
                                
          for(i in 1:size){
                                
                if(length(dataset$flag)!=0){
                
                     tmp1<-resid1[resid1[,i]<0&dataset$flag[,i]!=(-50),i]
                      tmp2<-resid2[resid2[,i]<0&dataset$flagds[,i]!=(-50),i]
                
                }
                else{
                
                      tmp1<-resid1[resid1[,i]<0,i]
                      tmp2<-resid2[resid2[,i]<0,i]
                      
               }
                myr<-quantile(tmp1,0.05)
                tmp1<-tmp1[tmp1>myr]
                myr<-quantile(tmp2,0.05)
                tmp2<-tmp2[tmp2>myr]                        
                lamda1[i]<-sqrt(var(tmp1))
lamda2[i]<-sqrt(var(tmp2))
 
                sd.resid1[,i]<-resid1[,i]/lamda1[i]
                sd.resid2[,i]<-resid2[,i]/lamda2[i]
                data<-list(x=dataset$R[low1[,i],i],y=dataset$G[low1[,i],i])
                model<-lm1[[i]]
                r2[i]<-R2(data,model,type="robust")
                if(ds){
                      data<-list(y=dataset$Rds[low1[,i],i],x=dataset$Gds[low1[,i],i])
                }
               else{
                      data<-list(x=dataset$Rds[low1[,i],i],y=dataset$Gds[low1[,i],i])
                }
                model<-lm2[[i]]
                r2.ds[i]<-R2(data,model,type="robust")

          }
          result<-list(dataset=dataset,list=list,lm1=lm1,lm2=lm2,resid1=resid1,resid2=resid2,fit1=fit1,fit2=fit2,lamda1=lamda1,lamda2=lamda2,sd.resid1=sd.resid1,sd.resid2=sd.resid2,r2=r2,r2.ds=r2.ds)
          result
}

