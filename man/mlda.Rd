\name{mlda}
\alias{mlda}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Methylation Linear Discriminant Analysis }
\description{
   A function to transform the log ratios of signal intensities into log likelihood ratios using three linear models on DMH array
}
\usage{
mlda(dataset, trim = 0.05, norm = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dataset}{ data object from mlda.data() }
  \item{trim}{ the trim degree when estimate the variance of the residuals to the linear model }
  \item{norm}{ normalisation data using unmethylated model as reference by default. The intercept of the unmethylation linear model is adjusted around 0 }
}
\details{

  Three linear models were constructed in this function. 
  One is the unmethylation model constructed using mitochondrial sequences or other unmethylated controls. 
  One is the intermediate model used to select the loci that can be used as the methylated reference. 
  The other one is the methylation model constructed using the loci identified by the intermediate model. 
  We estimated the distance of each locus to the methylated and unmethylated lines using standardised 
  residuals. The likelihood of the locus being methylated was estimated by the difference of the squared
  standardised residual from the methylated line to that from the unmethylated line. 
  Consequently, we had a measure of methylation status for each locus based on the log likelihood 
  ratio of methylation rather than log ratio of cy3 (undigested) and cy5(digested) intensities.
}
\value{
  a list containing:
  
  \item{ratio1 }{A matrix of log likelihood ratios}
  \item{ratio2 }{A matrix of log likelihood ratios on dye-swapped/duplicated array}
  
}

\author{ Wei Dai \email{w.dai@imperial.ac.uk} }

\examples{
	
data(hcgi12k)
mlda.result<-mlda(hcgi12k,norm=FALSE,trim=0.05)

}
