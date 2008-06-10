\name{lr.plot}
\alias{lr.plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Plot of log likelihood ratios on dye-swapped/duplicated arrays }
\description{
  A function to generate a plot of log likelihood ratios on dye-swapped/duplicated arrays.
  The unmethylation and methylation cut-offs are shown on the plot and the interesting loci
  can be presented on the plot.
}
\usage{
lr.plot(mlda.data, mlda.obj, cutoff.obj, loci = NULL, legend = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mlda.data}{ A dataset object from mlda.data()}
  \item{mlda.obj}{ An object from mlda() }
  \item{cutoff.obj}{ An object from cutoff() }
  \item{loci}{ A vector of interesting loci }
  \item{legend}{ Show legend on the plot }
}

\value{
  
  No value returned
  
}

\author{ Wei Dai \email{w.dai@imperial.ac.uk} }

\seealso{  \code{\link{MLDA}} Examples are included }