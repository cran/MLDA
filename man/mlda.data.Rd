\name{mlda.data}
\alias{mlda.data}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Generate a data set in mlda }
\description{
  A function to generate a dataset object in mlda
}
\usage{
mlda.data(dig1, undig1, dig2, undig2, unmeth.flag, cl, sample.names = NULL, flag1 = NULL, flag2 = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dig1}{ log-transformed signal intensities for digested samples in DMH }
  \item{undig1}{ log-transformed signal intensities for undigested samples in DMH }
  \item{dig2}{ log-transformed signal intensities for digested samples in DMH on dye-swapped/duplicated array }
  \item{undig2}{ log-transformed signal intensities for undigested samples in DMH on dye-swapped/duplicated array }
  \item{unmeth.flag}{ flag for unmethylated controls }
  \item{cl}{ class of samples (class 1 and class 2) }
  \item{sample.names}{ sample labels }
  \item{flag1}{ flag for the probes with low quality }
  \item{flag2}{ flag for the probes with low quality on dye-swapped/duplicated arrays }
}

\value{
  a list containing:
  
  \item{R }{A matrix of log-transformed signal intensities for undigested samples in DMH}
  \item{G }{A matrix of log-transformed signal intensities for digested samples in DMH}
  \item{Rds }{A matrix of log-transformed signal intensities for digested samples in DMH on dye-swapped/duplicated array}
  \item{Gds }{A matrix of log-transformed signal intensities for undigested samples in DMH on dye-swapped/duplicated array}
  \item{mito.flag }{A vector of flag for unmethylated controls}
  \item{cl }{A vector of class labels for samples}
  \item{sample.names }{sample labels}
  \item{flag }{A matrix of flag for the low quality probes}
  \item{flagds }{A matrix of flag for the low quality probes on dye-swapped/duplicated arrays}
}

\author{ Wei Dai   \email{w.dai@imperial.ac.uk} }

\examples{

		data(hcgi12k)
		i<-c(1,5)
		dataset<-mlda.data(dig1=hcgi12k$G[,i],undig1=hcgi12k$R[,i],dig2=hcgi12k$Rds[,i],undig2=hcgi12k$Gds[,i],unmeth.flag=hcgi12k$mito.flag,cl=hcgi12k$cl[i],sample.names=hcgi12k$sample.names[i],flag1=hcgi12k$flag[,i],flag2=hcgi12k$flagds[,i])
}
