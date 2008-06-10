\name{MLDA-package}
\alias{MLDA-package}
\alias{MLDA}
\docType{package}
\title{
Methylation Linear Discriminant Analysis (MLDA)
}
\description{

	Three linear models were constructed by mlda(). 
  One is the unmethylation model constructed using mitochondrial sequences. 
  One is the intermediate model used to select the loci that can be used as the methylated reference. 
  The other one is the methylation model constructed using the loci identified by the intermediate model. 
  We estimated the distance of each locus to the methylated and unmethylated lines using standardised 
  residuals. The likelihood of the locus being methylated was estimated by the difference of the squared
  standardised residual from the methylated line to that from the unmethylated line. 
  Consequently, we had a measure of methylation status for each locus based on the log likelihood 
  ratio of methylation rather than log ratio of cy3 (undigested) and cy5(digested) intensities. The cut-offs for unmethylation
  and methylation are determined by cutoff(). Each feature is assigned a score (+1 methylated, -1 unmethylated, 0 uncertain) by score.cal().
  The robust regression modesl is constructed using the averaged scores for two classes. The outliers of the robust regression models are listed by mlda.sig().
  
  
}
\details{
\tabular{ll}{
Package: \tab MLDA\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2008-06-10\cr
License: \tab GPL (version 2 or later) \cr
Depends: \tab R (>= 2.7.0)\cr
LazyLoad: \tab yes\cr
}

}
\author{
Wei Dai, Jim Paul, Robert Brown

Maintainer: Wei Dai <w.dai@imperial.ac.uk>

}

\keyword{ package }

\examples{
		
data(hcgi12k)
i<-c(1,2,5,6)

##construct dataset
dataset<-mlda.data(dig1=hcgi12k$G[,i],undig1=hcgi12k$R[,i],dig2=hcgi12k$Rds[,i],undig2=hcgi12k$Gds[,i],unmeth.flag=hcgi12k$mito.flag,cl=hcgi12k$cl[i],sample.names=hcgi12k$sample.names[i],flag1=hcgi12k$flag[,i],flag2=hcgi12k$flagds[,i])

##Transform the log ratios of features to log likelihood ratios
mlda.result<-mlda(dataset,norm=FALSE,trim=0.05)

##Determie the cut-offs of log likelihood ratios for methylation and unmethylation on dye-swapped/duplicate arrays
mytest<-cutoff(dataset,mlda.result,ncut=10)

##Select the optimal IR and CR             
cutoff.plot(dataset,mytest)

##Score the features and construct the robust regression model usig the averaged scores in two classes of samples
##Determied the outliers (positive and negative) of the model    
myscore<-score.cal(mlda.result,mytest,dataset,perm=50)

##Report the analysis result
result<-mlda.sig(score.obj=myscore,dataset,pvalue=0.05,loci.ID=rownames(dataset$R))

##Check the iterestig loci on the plot of log likeihood ratios
lr.plot(dataset,mlda.result,mytest,loci=c("119_A_6","66_G_6"),legend=TRUE)

##Visualise the analysis result
mlda.sig.plot(myscore,control1=dataset$mito.flag)
}
