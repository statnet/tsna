\name{edgeDurationEst}
\alias{edgeDurationEst}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Compute Estimates of spell Durations using Survival Analysis
}
\description{
Uses the \code{\link{survival}} package to compute estimates of the 'true' mean and median edge durations for networks in which not all of the edge spells are fully observed.
}
\usage{
edgeDurationEst(nd, mode = c("right.censoring", "left.truncation", "both"), 
                   subject = c("edges", "spells", "dyads"), e = seq_along(nd$mel), 
                   start = NULL, end = NULL, active.default = TRUE, 
                   rmean = c("common", "individual", "extant"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nd}{
\code{\link{networkDynamic}} for which estimates should be computed
}
  \item{mode}{
 character, the mode of spell censoring/truncation to calculate. 
 \itemize{
  \item \code{'right.censoring'} assumes some edges may have un-observed / unknown ending times. (i.e. the observation of the network ended before the edges' spell terminates, spell ends marked with \code{Inf})
  \item \code{'left.truncation'} assumes some edges may have un-observed / unknown onset times. (i.e. the observation of the network began after some edges' spells had already started, spell onsets marked with \code{-Inf})
  \item \code{'both'} not yet implemented
 }
 
}
  \item{subject}{
 not yet used (TODO: how does it handle multiple spells per edge or multiplex edges)
}
  \item{e}{
numeric vector of edge ids to consider in the calculation (default is all eges)
}
  \item{start}{
 (optional) numeric value providing the start time which the network should be observed from
}
  \item{end}{
(optional) numeric value providing the end time which the network should be observed until
}
  \item{active.default}{
  logical, should edges with no explict activity spell be considered active by default?
}
  \item{rmean}{
  See \code{\link[survival]{print.survfit}}
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Prints summary numbers for the survival curves and (invisibly) returns them as a numeric vector of length 9. 
\itemize{
 \item \code{records} number of records considered???
 \item \code{n.max}
 \item \code{n.start}
 \item \code{events} number of events occured at time t???
 \item \code{*rmean} estimated mean edge duration
 \item \code{*se(rmean)} standard error of estimate of mean edge duration
 \item \code{median}  estimated median edge duration
 \item \code{0.95LCL} lower confidence limit for the survival curve
 \item \code{0.95UCL} upper confidence limit for the survival curve
}
}
\references{
\url{https://statnet.csde.washington.edu/trac/wiki/Vital\%20Dynamic}
\url{https://statnet.csde.washington.edu/trac/raw-attachment/wiki/Vital\%20Dynamic/vital.pdf}
\url{https://statnet.csde.washington.edu/trac/wiki/DurationEstimate}
}
\author{
Kirk Li kirkli@uw.edu
}
\note{
this is an early draft of the function
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
See also \code{\link{edgeDuration}} for computing the observed/extant durations
}
\examples{
library(networkDynamicData)
data(concurrencyComparisonNets)
edgeDurationEst(base) # 'true' value (model input) is 25
}

