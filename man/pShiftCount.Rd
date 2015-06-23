\name{pShiftCount}
\alias{pShiftCount}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Compute Counts of Gibson's Participation Shifts
}
\description{
Uses the \code{relevent} package to compute counts of dyadic turn-taking events using a typology outlined by Gibson (2003)
}
\usage{
pShiftCount(nd, start = NULL, end = NULL, output = c("final", "full"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nd}{
\code{networkDynamic} object to be evaluated
}
  \item{start}{
   numeric initial time point to start evaluation from
}
  \item{end}{
   numeric ending time point to finish evaluation
}
  \item{output}{
  chracter value indicating if only the \code{'final'} counts should be reported, or the \code{'full'} matrix with updated counts for each event. 
  }
}
\details{
Uses the \code{accum.ps} function in the package \code{relevent} to build counts of accumulated dyad participation shifts (turn-taking changes) using the dynamic information on tie changes represented in a directed \code{networkDynamic} object.   The P-shifts are given in the order used in Gibson's 2003 Social Forces paper, namely:
\itemize{
  \item \itemize{
  Turn Receiving:
  \item [1] AB->BA  (Alex talks to Brett, then Brett replies)
  \item [2] AB->B0  (Alex talks to Brett, then Brett addresses the group)
  \item [3] AB->BY  (Alex talks to Brett, then Brett talks to Yuki)
  }
  \item \itemize{
  Turn Claiming:
  \item [4] A0->X0  (Alex talks to the group, then Xuan talks to the group)
  \item [5] A0->XA  (Alex talks to the group, then Xuan talks to Alex)
  \item [6] A0->XY  (Alex talks to the group, then Xuan talks to Yuki)
  }
  \item \itemize{
  Turn Usurping:
  \item [7] AB->X0  (Alex talks to Brett, then Xuan talks to the group)
  \item [8] AB->XA  (Alex talks to Brett, then Xuan talks to Alex)
  \item [9] AB->XB  (Alex talks to Brett, then Xuan talks to Brett)
  \item [10] AB->XY (Alex talks to Brett, then Xuan talks to Yuki)
  }
  \item \itemize{
  Turn Continuing:
  \item [11] A0->AY (Alex talks to the group, then addresses Yuki)
  \item [12] AB->A0 (Alex talks to Brett, then makes remark to the group)
  \item [13] AB->AY (Alex talks to Brett, then to Yuki)
  }
  }

This uses Gibson's notation, in which A is the initial source, B is the
initial target, X is a new (shifted) speaker, Y is a new (shifted) target,
and 0 is used where no well-defined speaker or target is present.  (Here, this
would occurs when NA is given for source or destination, not currently supported)

It is worth noting that not all adjacent event pairs induce P-shifts, and hence
the shift counts will not increment with every event.  In particular, the first
event does not induce a shift (since there is no prior event), and neither does
a repetition of a previous event (e.g., AB->AB or A0->A0).  The full set is
thus affinely independent in general, although they will have a near (or 
even full) dimension of affine dependence on most data sets.

Event order is determined by sorting the network's edge spells by on the onset time of edge and then by the terminus.  Gibson's typology assumes that edges/ties directed 'at the group' are distinguishable those directed at individuals, and has a strong assumption of sequential non-simultaneous events. Because the \code{networkDynamic} object does not explicitly clode for 'group' utterences, simultaneous edges originating from a speaker (same onset,terminus, and tail vertex) are assumed to be directed at the group, even if not all group members are reached by the ties.  
}
\value{
For \code{output='final'} (the default), the output is a matrix with one row containing counts for each of the 13 P-shift types accumulated over the time period requested. For \code{ouptut='full'}, the output is a data.frame with rows corresponding to each edge spell event. The first 13 rows are the counts of P-shift types, and the remaining four rows are the 'onset', 'terminus', 'tail', 'head', and a 'group' column indicating if the event was considered as a group-directed. 
}
\references{
Gibson, D.R. (2003) 'Participation Shifts: Order and Differentiation in Group Conversation'  
Social Forces  81 (4): 1335-1380 \url{http://sf.oxfordjournals.org/content/81/4/1335.short}

Carter T. Butts (2008). A Relational Event Framework for Social Action. Sociological Methodology, 38(1), 155--200.


}
\author{
Carter Butts buttsc@uci.edu, Skye Bender-deMoll skyebend@uw.edu
}


\examples{
data(McFarland_cls33_10_16_96)
pShiftCount(cls33_10_16_96)

}

