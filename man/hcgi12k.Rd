\name{hcgi12k}
\alias{hcgi12k}
\docType{data}
\title{ DMH dataset for A2780 series sensitive and resistant cell lines}
\description{
  A DMH dataset for A2780 series sensitive and resistant cell lines on HCGI12K array (\url{http://data.microarrays.ca/cpg/}).
  Cell lines include A2780, A2780p3, A2780p5, A2780p6, A2780cp70, MCP1, MCP3, MCP6
}
\usage{data(hcgi12k)}
\format{
  The format is:
\itemize{

 \item{R}{A matrix of log-transformed signal intensities for red channel}
 \item{G}{A matrix of log-transformed signal intensities for green channel}
 \item{Rds}{A matrix of log-transformed signal intensities for red channel on dye-swapped array}
 \item{Gds}{A matrix of log-transformed signal intensities for geen channel on dye-swapped array}
 \item{mito.flag}{A vector of flag for mitochondrial features}
 \item{cl}{A vector of class of cell lines. 1 = sensitive cell line 2 = resistant cell line}
 \item{sample.names}{A vector of sample names}
 \item{flag}{A matrix of flag of low quality probes. The low quality probes are flagged as -50, otherwise are flagged as 0}
 \item{flagds}{A matrix of flag of low quality probes on dye-swapped arrays. The low quality probes are flagged as -50, otherwise are flagged as 0}
}
}

\source{
  \url{http://www.ebi.ac.uk/microarray-as/aer/#ae-browse/q=E-TABM-210[2]}
}
\details{
	
	The background of dataset have been corrected, the controls (SA1, SA2, Blank and Arabidopsis) on hcgi12k array have been excluded.\cr
	
	A2780: A human ovarian adenocarcinoma cell line derived from an untreated patient\cr
	A2780p3: A clonal derivative of A2780\cr
	A2780p5: A clonal derivative of A2780\cr
	A2780p6: A clonal derivative of A2780\cr
	A2780cp70: A resistant derivative of A2780 isolated after exposure to cisplatin\cr
	MCP1, MCP3 and MCP6: In vitro selected resistant derivatives of A2780, isolated after a multiple
	explosures to increasing cisplatin concentration\cr

}

\examples{
data(hcgi12k)

}
\keyword{datasets}