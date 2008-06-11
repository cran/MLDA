\name{list.compare}
\alias{list.compare}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ List comparison }
\description{
  
 		A function to get the overlap of two lists
 	
}
\usage{
list.compare(list1, list2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{list1}{ The first list }
  \item{list2}{ The second list }
}

\value{

	A vector of logical values for the overlap in the second list
	
}

\author{ Wei Dai \email{w.dai@imperial.ac.uk} }

\examples{

list1<-paste("gene",1:8)
list2<-paste("gene",6:12)
result<-list.compare(list1,list2)
overlap<-list2[result]

}