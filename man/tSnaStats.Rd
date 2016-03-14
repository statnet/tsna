\name{tSnaStats}
\alias{tSnaStats}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Apply the sna package's static graph- and vertex-level network descriptive statistics at multiple time points
}
\description{
Samples collapsed static networks at regular intervals along a network dynamic object, applies the named static \code{\link[sna]{sna}} descriptive statistic function to each network, and returns the result as a time series. Additional arguments to the function can be included via \code{...} . Set the sna function's directedness and self-loops flags appropriately by default. 
}
\usage{
tSnaStats(nd, snafun, start, end, time.interval = 1, aggregate.dur=0, rule='latest', ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nd}{
a \code{networkDynamic} object to be evaluated
}
  \item{snafun}{
character string giving the name of the \code{\link[sna]{sna}} package function to be applied. i.e 'mutuality'
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
optional numeric value giving the duration of time bin to aggregate over for each evaluation (default 0). See \code{\link{collapse.network}}
 }
 \item{rule}{
 character vector describing rule to be used if multiple attribute values are encountred when using non-zero \code{aggregate.dur}. Default is \code{latest}. See \code{\link{collapse.network}} for details
 }
  \item{\dots}{
additional arguments to be passed on to the sna function. See docs for each function for possible arguments.
}
}
\details{
This wrapper directly calls functions in the \code{\link[sna]{sna}} package, so it will only work if that package is installed. Below is a list of supported functions:

Graph-Level statistics:
\itemize{
  \item \code{\link[sna]{components}} Number of (Maximal) Components Within a Given Graph
  \item \code{\link[sna]{triad.census}} Davis and Leinhardt Triad Census
  \item \code{\link[sna]{connectedness}} Graph Connectedness Scores
  \item \code{\link[sna]{dyad.census}} Holland and Leinhardt MAN Dyad Census
  \item \code{\link[sna]{efficiency}} Graph Efficiency Scores
  \item \code{\link[sna]{gden}} Graph Density
  \item \code{\link[sna]{grecip}} Graph Reciprocity
  \item \code{\link[sna]{gtrans}} Graph Transitivity
  \item \code{\link[sna]{hierarchy}} Graph Hierarchy Scores
  \item \code{\link[sna]{lubness}} Graph LUBness Scores
  \item \code{\link[sna]{mutuality}} Graph Mutuality
  \item \code{\link[sna]{centralization}} Graph Centralization (must provide centrality measure)
}

Vertex-level statistics:
\itemize{
  \item \code{\link[sna]{closeness}} Vertex Closeness Centrality Scores
  \item \code{\link[sna]{betweenness}} Vertex Betweenness Centrality Scores
  \item \code{\link[sna]{bonpow}} Vertex Bonacich Power Centrality Scores
  \item \code{\link[sna]{degree}} Vertex Degree Centrality Scores
  \item \code{\link[sna]{evcent}} Vertex Eigenvector Centrality Scores
  \item \code{\link[sna]{flowbet}} Vertex Flow Betweenness Scores
  \item \code{\link[sna]{graphcent}} Vertex (Harary) Graph Centrality Scores
  \item \code{\link[sna]{infocent}} Vertex Information Centrality Scores 
  \item \code{\link[sna]{loadcent}} Vertex Load Centrality Scores
  \item \code{\link[sna]{prestige}} Vertex Prestige Scores
}

Most of the sna functions involve converting the network to a matrix and can be quite expensive to calculate for a single time point, so use care when applying to large or long-duration networks.

Some of the sna functions are undefined or produce numerical errors when applied to networks with certain configurations (such as zero edges).

The sna functions generally cannot handle networks with no vertices, so stats will be replaced with NA when they are encountered. 

There may be some overlap with ergm terms available through \code{\link{tErgmStats}} and the ergm version will generally be faster
}
\value{
a \code{\link{ts}} (time series) object. A matrix in which rows correspond to the time points evaluated and columns correspond to values of statistics produced.  In the case of vertex-level indices, there will be one column per vertex. For the census measures, each column will correspond to a census element.
}
\references{
 Carter T. Butts (2014). sna: Tools for Social Network Analysis. R package version
  2.3-2. http://CRAN.R-project.org/package=sna
}
\author{
skyebend@uw.edu
}
\note{
Note that this is an early DRAFT implementation.  Does not yet include binning options needed for non-discrete time networks, and has not been tested with networks that have changing vertex activity. 
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
See also \code{\link{tErgmStats}},
}
\examples{
library(networkDynamicData)
data(harry_potter_support)

# compute triad census scores for each time point
tSnaStats(harry_potter_support,snafun='triad.census')

# compute graph transitivities
tSnaStats(harry_potter_support,snafun='gtrans')
\dontrun{
data(concurrencyComparisonNets)
# since thes are big nets, with lots of timepoints, 
# set time.interval to avoid evaluating every step
tSnaStats(base,'prestige',time.interval=25,rescale=TRUE)

# since it is time series, easy to plot
plot(tSnaStats(base,'components',time.interval=10))
}
}

