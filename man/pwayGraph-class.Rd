\name{pwayGraph-class}
\docType{class}
\alias{pwayGraph-class}
\alias{plot,pwayGraph-method}

\title{Class "pwayGraph" -- extension to graphNEL for pathway rendering}

\description{ extension to graphNEL for pathway rendering }

\section{Objects from the Class}{
Objects can be created by calls of the form 
\code{new("pwayGraph", nodes, edgeL, edgemode)}.

There is a plot method that will work reasonably well if the plotting
surface is big enough.
}

\section{Slots}{
  \describe{
    \item{\code{pwaySource}:}{Object of class \code{"character"} KEGG or BIOCARTA }
    \item{\code{nodes}:}{Object of class \code{"vector"} pathway constituents in the native vocabulary }
    \item{\code{edgeL}:}{Object of class \code{"list"} constituent
relations in the native vocabulary }
    \item{\code{edgemode}:}{Object of class \code{"character"} directed,
typically }
    \item{\code{edgeData}:}{Object of class \code{"attrData"} relationship
attributes }
    \item{\code{nodeData}:}{Object of class \code{"attrData"} node
attributes }
    \item{\code{graphData}:}{Object of class \code{"list"} this holds
the special rendering attributes for edges and nodes, for nodes it
seems particularly important to have fixedsize = FALSE}
  }
}

\section{Extends}{
Class \code{\link[graph]{graphNEL-class}}, directly.
Class \code{\link[graph]{graph-class}}, by class "graphNEL", distance 2.
}

\section{Methods}{
  \describe{
    \item{plot}{\code{signature(x = "pwayGraph")}: renders the pathway }
    }
}

%\references{ ~put references to the literature/web site here ~ }

\author{ Vince Carey <stvjc@channing.harvard.edu> }

%\note{ ~~further notes~~ }
% ~Make other sections like Warning with \section{Warning }{....} ~
%
%\seealso{
%	~~objects to See Also as \code{\link{~~fun~~}}, ~~~
%	 \code{\linkS4class{CLASSNAME}} for links to other classes
%}

\examples{
showClass("pwayGraph")
G1 = graphcMAP("stresspathway")
G1@graphData$nAttrs$labels[1:10]
}

\keyword{classes}
