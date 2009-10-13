\name{mlda.sig.plot}
\alias{mlda.sig.plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Plot of MLDA result }
\description{
  A function to report the analysis result from MLDA.
}
\usage{
mlda.sig.plot(score.obj, pvalue = 0.05, control1 = NULL, 
							control2 = NULL, control3 = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{score.obj}{ An object from \code{score.cal} }
  \item{pvalue}{ A significant level }
  \item{control1}{ Controls in DMH, such as SA1 on hcgi12k }
  \item{control2}{ Controls in DMH, such as SA2 on hcgi12k }
  \item{control3}{ Controls in DMH, such as mitochondrial features on hcgi12k }
}

\value{
  
  No value returned
}

\author{ Wei Dai }

\seealso{ \code{\link{MLDA}} Examples are included}
