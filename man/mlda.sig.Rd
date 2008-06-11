\name{mlda.sig}
\alias{mlda.sig}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Identification of differentially methylated loci }
\description{
  
  This function is to identify the differentially methylated loci between two classes of samples.
 
}
\usage{
mlda.sig(score.obj, dataset, loci.ID = NULL, pvalue = 0.01, filter = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{score.obj}{ An object from score.cal() }
  \item{dataset}{ A dataset object from mlda.data()  }
  \item{loci.ID}{ loci ID on DMH array }
  \item{pvalue}{ p value for positive and negative outliers }
  \item{filter}{ The low quality probes are excluded by default}
}
\value{
  A list containing:
  
  \item{loci.up }{hypermethylated loci in class 2, sensitive and resistant scores}
  \item{loci.down }{hypomethylated loci in class 2, sensitive and resistnat scores}
  \item{pos.p }{p value for positive outlier}
  \item{neg.p }{p value for negative outlier}
}

\author{ Wei Dai \email{w.dai@imperial.ac.uk} }
\seealso{  \code{\link{MLDA}} Examples are included }
