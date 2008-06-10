\name{cutoff.plot}
\alias{cutoff.plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Plot of CR against IR }
\description{
  A function to generate a plot of CR against IR. This plot helps users to determine the optimal
  IR and CR of dye-swapped/duplicated arrays. 
}
\usage{
cutoff.plot(mlda.data, cutoff.obj, IR = 0.01, CR = 1.4)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mlda.data}{ A dataset object from mlda.data() }
  \item{cutoff.obj}{ An object from cutoff() }
  \item{IR}{ Inconsistency rate for dye-swapped/duplicated arrays }
  \item{CR}{ Consistency rate for dye-swapped/duplicated arrays }
}
\details{
  Empirically, IR is about 0.01 and CR is about 1.4 on hcgi12k array. But we observed higher IR and lower CR on
  another CpG island array using DMH. This is mainly resulted from the large variance introduced by dye-swap design.
  This function can visualise the IRs and CRs on dye-swapped/duplicated arrays and help users to select the optimal
  IR and CR cut-offs. 
}
\value{No value returned}
\author{ Wei Dai \email{w.dai@imperial.ac.uk} }
\seealso{ \code{\link{cutoff}} A function to calculate the IRs and CRs at various thresholds\cr 
					\code{\link{MLDA}} Examples are included 
					
}
