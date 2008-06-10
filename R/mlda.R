`mlda` <-
function(dataset,trim = 0.05,norm = TRUE){

list <- dataset$sample.names
mito.flag <- dataset$mito.flag
mito.obj <- mito.lm(dataset=dataset,list=list,mito.flag=mito.flag,norm=norm,ds=TRUE,trim=trim)
middle.obj <- middle.lm(mito.obj)
meth.obj <- meth.lm(middle.obj,mito.obj)
datalr <- likelihood(mito.obj,meth.obj)
result <- datalr
result

}

