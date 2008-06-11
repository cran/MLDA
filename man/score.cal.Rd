\name{score.cal}
\alias{score.cal}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Weighted Score Scheme }
\description{
  A function to assign each feature a weighted score based on the cut-offs for methylation and unmethylation.The averaged scores for two calsses are calculated.
  A robust regresion model is constructed using the averaged scores for two classes. The positive and negative outliers of regression models are identified.
}
\usage{
score.cal(mlda.obj, cutoff.obj, mlda.data, perm = 200, control = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{mlda.obj}{ An object from mlda() }
  \item{cutoff.obj}{ An object from cutoff() }
  \item{mlda.data}{ A dataset object frmo mlda.data() }
  \item{perm}{ The times of sampling in the normal distribution estimated from the standerdised residuals of robust regression model construced by averaged sensitive and resistant scores}
  \item{control}{ The mean and variance of normal distribution are estimated using controls of which methylations status are not changed in two classes}

}

\value{
  A list containing:
  
  \item{sr.obj }{A list including standardised residuals, p value for positive and negative outliers at various thresholds}
  \item{score }{A score matrix. Each feature is assigned a score.}

}
\details{
	
	Each feature was scored based on the cut-offs of log likelihood ratios for methylation and unmethylation on dye-swapped/duplicate arrays
	using a weighted methylation scoring scheme. The features consistently identified as methylated candidates on dye-swapped/duplicate arrays
	are scored of 1; similarly unmethylated features were scored of -1. The rest of the feature were assigned a weighted score based on their location
	on the plot (see \code{\link{lr.plot}}).
	
	Score scheme:\cr
	
	f ( i, j ) = 1  i > a and j > a\cr
	f ( i, j ) = -1 i > b and j > b\cr
	f ( i, j ) = 0.45 ( i > a and 0 < j < a ) or ( 0 < i < a and j > a )\cr
	f ( i, j ) = -0.45 ( i < b and b < j < 0 ) or ( b < i < 0 and j < b )\cr
	f ( i, j ) = 0.2 ( 0 < i < a and 0 < j < a ) or ( b < i < 0 and b < j < 0 )\cr
	f ( i, j ) = 0.0001 ( 0 < i < a and b < j < 0 ) or ( b < i < 0 and 0 < j < a )\cr
	
	otherwise,  f( i, j ) = 0\cr\cr
	
	a: methylation cut-off\cr
	b: unmethylation cut-off\cr
	
}
\author{ Wei Dai \email{w.dai@imperial.ac.uk} }

\seealso{  \code{\link{MLDA}} Examples are included }