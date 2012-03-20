\name{coloredGraph-class}
\docType{class}
\alias{coloredGraph-class}
\alias{pwayRendAttrs}
\alias{plot,coloredGraph,ANY-method}

\title{Class "coloredGraph" }
\description{	 a graph to which color attributes have been attached }
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("coloredGraph", nodes, edgeL, edgemode)}.
these are graphNEL instances with some additional graphData
}
\section{Slots}{
	 \describe{
    \item{\code{nodes}:}{Object of class \code{"vector"} ~~ }
    \item{\code{edgeL}:}{Object of class \code{"list"} ~~ }
    \item{\code{edgeData}:}{Object of class \code{"attrData"} ~~ }
    \item{\code{nodeData}:}{Object of class \code{"attrData"} ~~ }
    \item{\code{renderInfo}:}{Object of class \code{"renderInfo"} ~~ }
    \item{\code{graphData}:}{Object of class \code{"list"} ~~ }
  }
}
\section{Extends}{
Class \code{"\link[graph]{graphNEL-class}"}, directly.
Class \code{"\link[graph]{graph-class}"}, by class "graphNEL", distance 2.
}
\section{Methods}{
  \describe{
    \item{plot}{\code{signature(x = "coloredGraph")}: ... }
	 }
}
\examples{
showClass("coloredGraph")
example(randomGraph)
nn = nodes(g1)
x = runif(length(nn))
names(x) = nn
h1 = colorNodes(g1, x, colorRampPalette(brewer.pal(9, "Blues"))(length(nn)),
 pwayRendAttrs)
h1
plot(h1)
}
\keyword{classes}
