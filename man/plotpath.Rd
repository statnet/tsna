%  Part of the statnet package, http://statnetproject.org
%
%  This software is distributed under the GPL-3 license.  It is free,
%  open source, and has the attribution requirements (GPL Section 7) in
%    http://statnetproject.org/attribution
%
%  Copyright 2014 the statnet development team
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{plotPaths}
\alias{plotPaths}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plots a hilighted path on top of a network plot.
}
\description{
A wrapper for \code{plot.network} that also plots a hilighted path. 
}
\usage{
plotPaths(nd, path.results, path.col = "#FF000055", ...)
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
Plots the \code{networkDynamic} object using the normal \code{plot.network} function and \code{...} areguments.  Then uses \code{\link{createPaths}} to extract a tree using the \code{path.results}, and overplots the edges of the tree onto the network plot using the \code{path.col} color.  
}
\value{
Generates a network plot with a hilighted path, invisibly returns the plot coordinates. 
}

\author{
skyebend
}

\seealso{
See also \code{\link{tPathDistance}}
}
\examples{
data(moodyContactSim)
v10path<-tPathDistance(moodyContactSim,v=10,start=0)
plotPaths(moodyContactSim,v10path,displaylabels=TRUE)
}

