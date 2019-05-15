\name{tErgmStats}
\alias{tErgmStats}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate network summary statistics at multiple time points
}
\description{
Applies a ergm-style formula of network statistics to cross-sectional networks collapsed from a networkDynamic at multiple time points to construct a matrix of values describing the change in statistics over time. 
}
\usage{
tErgmStats(nd, formula, start, end, time.interval = 1, aggregate.dur, rule)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nd}{
\code{\link{networkDynamic}} object to be evaluated
}
  \item{formula}{
a character string providing an ergm term name or the 'right hand side' of an ergm formula. For example \code{'~ edges + concurrent'}
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
 \item{aggregate.dur}{
optional numeric value giving the duration of time bin to aggregate over for each evaluation (default 0). See \code{\link{network.collapse}}
 }
 \item{rule}{
 character vector describing rule to be used if multiple attribute values are encountred when using non-zero \code{aggregate.dur}. Default is \code{latest}. See \code{\link{network.collapse}} for details
 }
}
\details{
 Constructs a set of times to evaluate based on \code{start},\code{end} and \code{time.interval}. Extracts a static network at each time point and uses it to construct a formula with \code{f}. The formula is passed to ergm's \code{\link[ergm:summary_formula]{summary_formula}} function to calculate the net value of the change statistics for each term in the formula.  The values of the statistics are grouped into a time-series object (class \code{\link{ts}}). The ts object can be thought of as a matrix such that each column is a formula term and each row is the time point at which the statistics were evaluated. See \code{\link[ergm]{ergm-terms}} for a list of available term statistics.  The \code{aggregate.dur} can be used to specifiy the duration of the aggregation bin, especially useful when working with continuous time networks.  Usually the \code{time.interval} would be set to the same value to ensure non-overlapping bins. 
 
Be aware that if the network's vertex activity dynamics imply cross-sectional networks of different sizes, the interpretation of the statistic at each time point may not be the same.  
}
\value{
A time-series (\code{\link{ts}}) object containing term statistics in which each column corresponds to a statistic and each row is the time point at which the statistic was evaluated
}

\author{
skyebend@uw.edu
}



\seealso{
See also \code{\link[ergm]{summary_formula}} and \code{\link[ergm]{ergm-terms}}. For more information about time-series objects, see \code{\link{ts}} and \code{\link{plot.ts}} for plotting quickly plotting timelines for multiple statistics.  The \code{summary_formula.networkDynamic} function in the \code{tergm} package offers very similar functionality.
}
\examples{
 \dontrun{
 data(windsurfers)
 tErgmStats(windsurfers,'~edges+degree(c(1,2))')
 library(networkDynamicData)
 data(concurrencyComparisonNets)
 tErgmStats(base,'~edges+concurrent',
               start=0,end=100,time.interval = 10)
 # show as multiple plots
 plot(
   tErgmStats(base,'~edges+concurrent',
                start=0,end=100,time.interval = 10),
                
                )
  }

 
}
