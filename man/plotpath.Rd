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
\alias{plot.tPath}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Network Plots a Highlighting Temporal Path(s) .
}
\description{
Wrapper for \code{\link[network]{plot.network}} with appropriate defaults to plot a highlighted path, or over-plot highlighted paths on a on top of a static aggregate network plot. 
}
\usage{
plotPaths(nd, paths,  path.col = rainbow(length(paths), alpha = 0.5), 
           displaylabels = TRUE, coord=NULL, ...)
           
\method{plot}{tPath}(x, edge.col = "red", 
              edge.label.col = edge.col, 
              edge.lwd = 10, 
              edge.label.cex = 0.7, 
              displaylabels = TRUE, 
              displayisolates = FALSE, 
              jitter = FALSE, 
              vertex.lwd = (x$gsteps == 0) * 4 + 1, 
              vertex.cex = (x$gsteps == 0) * 1.5, 
              vertex.col = NA, ...) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nd}{
a \code{networkDynamic} object to be plotted. 
}
  \item{paths}{
 a \code{tPath} object containing temporal path (presumably extracted from \code{nd}) to be plotted over the network, or a list of paths to be plotted together on the same network.
}
  \item{path.col}{
vector of valid colors (possibly transparent) to be used for each path. Default will created semi-transparent colors from the rainbow palette.
} 
 \item{x}{object (assumed to be \code{\link{tPath}}) to be plotted on top of a static aggregate network plot}
  \item{\dots}{additional arguments to be passed to \code{\link[network]{plot.network}} and \code{\link{plot.tPath}}.
}
\item{coord}{optional numeric matrix of coordinates for positioning vertices. See \code{\link{plot.network}}}
\item{edge.col}{color for drawing edges (paths). See \code{\link[network]{plot.network}}}
\item{edge.label.col}{color for edge labels. Default to same color as edges. See \code{\link[network]{plot.network}}} 
\item{edge.lwd}{numeric expansion factor for edge line widths. See \code{\link[network]{plot.network}}}
\item{edge.label.cex }{numeric expansion factor for edge labels. See \code{\link[network]{plot.network}}} 
\item{displaylabels}{logical, should vertex labels be included on the plot? See \code{\link[network]{plot.network}}} 
\item{displayisolates}{logical, should isolated vertices be included in the plot? See \code{\link[network]{plot.network}}} 
\item{jitter}{adds random noise to positions (disabled by default) See \code{\link[network]{plot.network}}}
\item{vertex.lwd}{Vertex border line with. See \code{\link[network]{plot.network}}} 
\item{vertex.cex}{Vertex expansion factor. Default is to scale up the origin vertex for the path, and not draw the other vertices. See \code{\link[network]{plot.network}}} 
\item{vertex.col}{Color for vertices. Default is to leave them un-colored. See \code{\link[network]{plot.network}}}

}
\details{
\code{plotPaths} plots the \code{networkDynamic} object using the normal \code{plot.network} function and \code{...} arguments.  Then calls \code{\link[network]{plot.tPath}} for each \code{\link{tPath}} object in \code{paths} to over-plot the edges of path onto the network plot using the corresponding \code{path.col} color.  Use of semi-transparent colors can help (somewhat) improve readability when paths overlap on the same edges.  

\code{plot.tPath} plots the path information encoded in a single \code{\link{tPath}} object.  It first creates a network using \code{\link{as.network.tPath}} and then calls \code{\link[network]{plot.network}}  with suitable defaults for drawing (or over-drawing) the path (doesn't display isolated vertices, draws times as edge labels, draws a color around the source vertex, etc. )
}
\value{
Generates a network plot with a highlighted path, invisibly returns the plot coordinates. 
}

\author{
skyebend
}

\seealso{
See also \code{\link{tPath}}
}
\examples{
data(moodyContactSim)
v10path<-tPath(moodyContactSim,v=10,start=0)
# plot just the path from v10
plot(v10path)

# plot the path from v10 on top of the network
plotPaths(moodyContactSim,v10path)

# plot the paths from both v10 and v1
v1path<-tPath(moodyContactSim,v=1,start=0)
plotPaths(moodyContactSim,list(v10path,v1path))

# if ndtv package is installed, along with Graphviz system library,
# nice hierarchical trees can be drawn
\dontrun{
plot(v10path,
     coord=network.layout.animate.Graphviz(
                   as.network(v10path),
                   layout.par = list(gv.engine='dot')
            ),
            jitter=FALSE
    )
}

}

