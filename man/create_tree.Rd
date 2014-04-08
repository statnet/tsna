\name{createPaths}
\alias{createPaths}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create a network object from the results of a path search
}
\description{
Takes the results of a path search (\code{\link{paths}}) as input and creates a new network object representing the path tree which can be plotted for debugging, etc. 
}
\usage{
createPaths(path.results)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{path.results}{
A \code{list} with two elements, the output of a path search. The first, named 'distance' is a vector with length corresponding to the size of the network, each element of which is the distance for each vertex, the second, 'previous', is a vector of the ids of the previous vertex on the path. 
}
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
  v1path<-tPathDistance(moodyContactSim,v=1)
  v1tree<-createPaths(v1path)
  plot(v1tree,displaylabels=TRUE)
}

