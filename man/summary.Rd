\name{tSummaryStats}
\alias{tSummaryStats}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate network summary statistics at multiple time points
}
\description{
Applies a ergm-style formula of network statistics to cross-sectional networks collapsed from a networkDynamic at multiple time points to construct a matrix of values describing the change in statistics over time. 
}
\usage{
tSummaryStats(nd, f, start, end, time.interval = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nd}{
\code{\link{networkDynamic}} object to be evaluated
}
  \item{f}{
a character string providing the 'right hand side' of an ergm formula. For example \code{'~ edges + concurrent'}
}
  \item{start}{
optional numeric time value at which evaluation should start (default is first observed time)
}
  \item{end}{
optional numeric time value at which evaluation should end (default is last observed time)
}
  \item{time.interval}{
optional numeric value giving time interval between evaluations (default is 1)
}
}
\details{
 Constructs a set of times to evaluate based on \code{start},\code{end} and \code{time.interval}. Extracts a static network at each time point and uses it to construct a formula with \code{f}. The formula is passed to ergm's \code{\link{summary.statistics.formula}} function to calculate the net value of the change statistics for each term in the formula.  The values of the statistics are arranged into a matrix so that each row is a term and each column is the time point at which the statistics were evaluated. See \code{\link{ergm-terms}} for a list of available term statistics.
 
Be aware that if the network's vertex activity dynamics imply cross-sectional networks of different sizes, the interpretation of the statistic at each time point may not be the same.  
}
\value{
A numeric matrix of values of term statistics in which each row corresponds to a statistic and each column is the time point at which the statistic was evaluated
}

\author{
skyebend@uw.edu
}



\seealso{
See also \code{\link{summary.statistics.formula}} and \code{\link{ergm-terms}}.
}
\examples{
 library(networkDynamicData)
 data(concurrencyComparisonNets)
 tSummaryStats(base,'~edges+concurrent',start=0,end=100,time.interval = 10)
 
 data(windsurfers)
 tSummaryStats(windsurfers,'~edges+degree(c(1,2))')
}
