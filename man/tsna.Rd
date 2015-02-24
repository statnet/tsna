\name{tsna}
\alias{tsna}
\docType{package}
\title{
Tools for Temporal Social Network Analysis
}
\description{
Temporal SNA tools for continuous- and discrete-time longitudinal networks. having vertex, edge, and attribute dynamics stored in the networkDynamic format. This work was supported by grant R01HD68395 from the National Institute of Health.
}

\details{
This package provides tools for working with longitudinal network data in \code{\link[networkDynamic]{networkDynamic-package}} format. This data structure is essentially a list in the \code{\link[network]{network}} format in which elements also have an attached \code{\link[networkDynamic]{activity.attribute}}, a matrix of spells indicating when vertex or edge is active.  The networkDynamic package provides tools (\code{\link[networkDynamic]{networkDynamic}}) for translating longitudinal data from various formats (timed edge lists, lists of toggles, sets of matrices, etc).

Currently the package consists of several groups of functions
\itemize{
  \item{wrappers for 'static' social network analysis metrics and apply them at multiple time points}
  \item{functions that construct or use temporal paths through networks} 
  \item{basic tools for measuring durations of ties, rates of change, etc} 
  \item{utility functions for plotting, etc} 
}
The sections below provide some additional details

It is the intention that, like the \code{networkDynamic} package, tsna should support both continuous time and discrete time representations of networks.  However, we are prioritizing development of discrete time measures suitable for use with simulation data (i.e. \code{stergm} models) so many of the functions are still missing the necessary arguments to facilitate binning. 

}

\section{Standard 'static' Sna metrics}{
 These functions operate by collapsing the dynamic network into a static network at a series of regular intervals and returning the results as a time series \code{\link{ts}} object. They can provide general description of trends in a network dataset.  Generally assumes that vertex set is not substantially changing. 
  \itemize{
    \item \code{\link{tErgmStats}} -- descriptive stats (ergm terms) from the \code{ergm} package
    \item \code{\link{tSnaStats}} -- descriptive stats from the \code{sna} package. Both graph- and vertex-level measures.  centralities, components, reciprocity, betweenness, triad-census, etc. 
  }
}

\section{Temporal path based metrics}{
These functions compute and use temporal paths (network geodesics that are constrained by the activity times of edges) through a network. 
  \itemize{
    \item \code{\link{tPath}}
    \item \code{\link{tReach}}
  }
}

\section{Rates and Duration}{
These functions can be used to compute distributions of (observed) activity durations in a data structure.  Note that due to censoring (edges that begin before or end after the time observation window for the network) the observed durations may be biased away from the 'real' values (or model parameters).  The duration estimate functions use various types of survival analysis to return estimates of these values.  
  \itemize{
    \item \code{\link{edgeDuration}}
    \item \code{\link{vertexDuration}}
    \item \code{\link{connectedDuration}}
    \item \code{\link{tEdgeDissolution}}
    \item \code{\link{tEdgeFormation}}
  }
}

\section{tsna Utilities}{
  \itemize{
    \item \code{\link{as.network.tPath}}
    \item \code{\link{plotPaths}}
  }
}

\section{Data Sets}{
  \itemize{
    \item \code{\link{moodyContactSim}}
    \item additional useful datasets provided by the \code{networkDynamicData} and \code{networkDynamic} packages
  }
}


\source{
This package is part of the statnet suite of packages \url{http://statnet.org}.  For citation information see \code{citation('tsna')}.
}

