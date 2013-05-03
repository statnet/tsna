\name{reachable}
\alias{forward.reachable}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Find the set of vertices reachable from a given set using only paths moving forward in time
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
forward.reachable(nd, v, start = NULL, end = NULL, per.step.depth = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nd}{
a \code{network} (usually a \code{networkDynamic}) object
}
  \item{v}{
numeric vector giving the set of initial vertex.ids to start from
}
  \item{start}{
 The beginning of the time range to start from 
}
  \item{end}{
End of the time range to search to
}
  \item{per.step.depth}{
 How many steps (default=1) to search per unit of time.
}
}
\details{
The default value of \code{per.step.depth=Inf} is equivilent to assuming that the `process' takes no time to travel along vertices
}
\value{
A numeric vector of vertex.ids reachable from the initial set of vertex.id by `traveling' forward in time along active vertices and edges subject to bounding paramters.
}

\author{
skyebend
}
\note{
This is a pure R implementation, probably very slow.

This function could be ill-defined when using non-Inf \code{per.step.depth} with networks with instantaneous (onset=terminus) spells as it will treat elements active at time t as active until the next change in the network. 

TODO:  should be able to specifiy edge weight attribute to be included in time calcualtions. 
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (nd, v, start = NULL, end = NULL, per.step.depth = 1) 
{
    if (!is.network(nd)) {
        stop("the first argument to forward.reachble must be a network object")
    }
    if (!is.numeric(v)) {
        stop("v argument must be vector of valid numeric vertex ids")
    }
    if (max(v) > network.size(nd) | min(v) < 1) {
        stop("v argument must be vector of valid numeric vertex ids")
    }
    times <- get.change.times(nd, vertex.attribute.activity = FALSE, 
        edge.attribute.activity = FALSE, network.attribute.activity = FALSE)
    if (length(times) == 0) {
        times <- 0
    }
    if (is.null(start)) {
        start <- min(times)
    }
    if (is.null(end)) {
        end <- max(times)
    }
    times <- times[times >= start]
    times <- times[times <= end]
    reached <- v
    for (t in times) {
        ngs <- get.neighborhood.active(nd, v = v, at = t, type = "out")
        new <- setdiff(ngs, reached)
        reached <- c(reached, new)
        if (per.step.depth > 1 & length(new) > 0) {
            d <- 1
            while (d <= per.step.depth) {
                ngs <- get.neighborhood.active(nd, v = new, at = t, 
                  type = "out")
                new <- setdiff(ngs, reached)
                if (length(new) == 0) {
                  break
                }
                reached <- c(reached, new)
                d <- d + 1
            }
        }
    }
    return(v)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
