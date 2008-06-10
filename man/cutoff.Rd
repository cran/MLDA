\name{cutoff}
\alias{cutoff}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Determination of log likelihood ratio cut-offs for methylation and unmethylation }
\description{
  A function to determine the log likelihood ratio cut-offs for methylation and unmethylation in each sample
}

\usage{
cutoff(dataset, mlda.obj, IR = 0.01, CR = 1.4, ncut = 30, unmeth.start = 1, meth.start = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{dataset}{ A dataset object from mlda.data() }
  \item{mlda.obj}{ An object from mlda() }
  \item{IR}{ Inconsistent rate for dye-swapped/duplicated arrays. Default is 0.01. }
  \item{CR}{ Consistent rate for dye-swapped/duplicated arrays. Default is 1.4. }
  \item{ncut}{ The number of threshold intervals of log likelihood ratio for unmethylation and methylation }
  \item{unmeth.start}{ The start value of threshold of log likelihood ratio for unmethylation}
  \item{meth.start}{ The start value of threshold of log likelihood ratio for methylation }
}
\details{

  Two inconsistency rates (IRmeth and IRunmeth) and two consistency rate (CRmeth and CRunmeth) between
  dye-swapped/duplicated arrays were used to determine the log likelihood ratio cut-offs. IRmeth represents
  the rate of the microarray features identified as methylated in one array but as unmethylated in the other
  one, while IRunmeth is the rate of the microarray features identified as unmethylated in one array but as methylated
  in the other one. CRmeth and CRunmeth are the rates for the spots identified as methylated and unmethylated in both
  dye-swapped/duplicate arrays.
  
  The log likelihood ratio thresholds for methylated and unmethyalted features, which kept the IR low (0.01) and CR high (1.4)
  were used as the cut-offs for the methylated and unmethylated loci. Empirically, IR tends to rise with the increase of CR slowly,
  but starts to increase dramatically when the CR goes above 1.4, at which point the inconsistency rate is generally about 0.01. Therefore,
  we use IR=0.01 and CR=1.4 by default. 
  
}
\value{
  a list containing
  \item{lr.thres }{A matrix including cut-offs for unmethylation and methylation for each sample}
  \item{rate }{A list of IRs and CRs calculated at various thresholds}
}

\author{ Wei Dai \email{w.dai@imperial.ac.uk} }

\seealso{  \code{\link{MLDA}} Examples are included }