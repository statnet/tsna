%  Part of the statnet package, http://statnetproject.org
%
%  This software is distributed under the GPL-3 license.  It is free,
%  open source, and has the attribution requirements (GPL Section 7) in
%    http://statnetproject.org/attribution
%
%  Copyright 2014 the statnet development team
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{as.network.tPath}
\alias{as.network.tPath}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create a network object from a \code{tPath} object -- the results of a path search
}
\description{
Takes the results of a temporal path search (a \code{\link{tPath}}) as input and creates a new network object representing the path tree which can be plotted for debugging, etc. 
}
\usage{
  \method{as.network}{tPath}(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A \code{tPath} list with several elements, the output of a path search.
}
  \item{...}{possible additional arguments}
}
\details{
Attributes of original network are not copied
}
\value{
a directed \code{networkDynamic} object representing the path information from the input. Each edge has the onset time indicated by its distance in the path. 
}

\author{
skyebend
}
\note{
this is probably not the final form of this function
}



\seealso{
See also \code{\link{paths}}.
}
\examples{
  data(moodyContactSim)
  v1path<-tPath(moodyContactSim,v=1)
  v1tree<-as.network(v1path)
  plot(v1tree,displaylabels=TRUE)
}

