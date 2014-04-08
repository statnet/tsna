\name{reachable_set_sizes}
\alias{reachable_set_sizes}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
computes sizes of temporally reachable sets in a dynamicNetwork
}
\description{
computes sizes of temporally reachable sets in a dynamicNetwork, using either a full census or sample of starting vertices of the specified size
}
\usage{
reachable_set_sizes(nd, direction = "fwd", sample = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nd}{
 a networkDynamic object
}
  \item{direction}{
 currently only \code{'fwd'} works, to calculate the sizes forward-reachable sets (as opposed to backwards-reachable)
}
  \item{sample}{
 default is \code{FALSE}, if numeric, indicates the size of the sample of vertices to use to calculate the reachable sets
}
}
\details{
Calls \code{link{paths.fwd.earliest}} on with each starting vertex to determine the sizes of the sets of vertices that are reachable. If sample is set to a numeric value, it will only sample that many vertices instead of doing the (expensive) full census
}
\value{
a vector giving the sizes of the set of vertices reachable from each seed vertex. 
}

\author{
skyebend
}
\note{
Needs additional options for time range specification, implementation of backwards sets
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
See also as \code{\link{paths.fwd.earliest}}
}
\examples{
data(moodyContactSim)
reachable_set_sizes(moodyContactSim)

# only sample 3 paths
reachable_set_sizes(moodyContactSim,sample=3)
}