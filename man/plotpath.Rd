\name{plotpath}
\alias{plotpath}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plots a hilighted path on top of a network plot.
}
\description{
A wrapper for \code{plot.network} that also plots a hilighted path. 
}
\usage{
plotpath(nd, path.results, path.col = "#FF000055", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nd}{
a \code{networkDynamic} object
}
  \item{path.results}{
a two-item list of distances and previous vertices: the results of a path calulation on \code{nd}
}
  \item{path.col}{
a valid color (possibly transparent) to be used for the path
}
  \item{\dots}{
additional arguments to be passed to \code{plot.network}.
}
}
\details{
Plots the \code{networkDynamic} object using the normal \code{plot.network} function and \code{...} areguments.  Then uses \code{\link{create_tree}} to extract a tree using the \code{path.results}, and overplots the edges of the tree onto the network plot using the \code{path.col} color.  
}
\value{
Generates a network plot with a hilighted path, invisibly returns the plot coordinates. 
}

\author{
skyebend
}

\seealso{
See also \code{\link{paths.fwd.earliest}}
}
\examples{
data(moodyContactSim)
v10path<-paths.fwd.earliest(moodyContactSim,v=10,start=0)
plotpath(moodyContactSim,v10path,displaylabels=TRUE)
}

