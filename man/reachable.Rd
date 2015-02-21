%  Part of the statnet package, http://statnetproject.org
%
%  This software is distributed under the GPL-3 license.  It is free,
%  open source, and has the attribution requirements (GPL Section 7) in
%    http://statnetproject.org/attribution
%
%  Copyright 2014 the statnet development team
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\name{reachable}
\alias{forward.reachable}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Find the set of vertices reachable from a given set using only paths moving forward in time
}
\description{
Does a breadth-first search from the specified set of vertices, respecting the direction and timing of edges. TODO: vertex activity.
}
\usage{
forward.reachable(nd, v, start = NULL, end = NULL, per.step.depth = Inf)
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
See also \code{\link{tPath}} for a dramatically faster implementation
}

