\name{tDegree}
\alias{tDegree}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Report momentary degree of a networkDynamic object at multiple timepoints
}
\description{
Calculates the degree of vertices at a sequence of time points over a network's temporal evolution
}
\usage{
tDegree(nd, start, end, time.interval = 1, cmode = c("freeman", "indegree", "outdegree"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nd}{
the \code{\link[networkDynamic]{networkDynamic}} object to be evaluated
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
  \item{cmode}{
   mode for evaluating degree.  one of  \code{"freeman", "indegree", "outdegree"}
}
}
\details{
  Evaluates the momentary degrees of a network at multiple time points and returns results in a form suitable for sumarizing the distributions. If a vertex is not active at a time point, its degree will be recorded as \code{NA}.
}
\value{
A \code{\link{ts}} (time series) object, a numeric matrix with giving the momentary degree of each vertex at each time point. Columns coresponding to each vertex in the input network and row corresponding to each time point at which degree was evaluated.
}

\author{
skyebend
}

\seealso{
See also \code{tSnaStats(nd,'degree')} and \code{tErgmStats(nd,'sociality')} for alternate ways to compute degree using external packages. 
}


\examples{
data(McFarland_cls33_10_16_96)
tDegree(cls33_10_16_96)
# compute mean temporal degree
mean(tDegree(cls33_10_16_96),na.rm=TRUE)
\dontrun{
library(networkDynamicData)
data(concurrencyComparisonNets)
# compute mean for each network, sampled at 11 time points
mean(colMeans(tDegree(base,start = 0,end=102,time.interval = 10)))
mean(colMeans(tDegree(middle,start = 0,end=102,time.interval = 10)))
mean(colMeans(tDegree(monog,start = 0,end=102,time.interval = 10)))

# plot distribution of vertices' mean momentary degree
hist(rowMeans(tDegree(base,start = 0,end=102,time.interval = 10)))

# plot distribution of momentary degrees of vertices
hist(tDegree(base,start = 0,end=102,time.interval = 10))
}

}