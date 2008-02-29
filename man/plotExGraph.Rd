\name{plotExGraph}
\alias{plotExGraph}
\title{plot a gene network, coloring nodes according
to relative expression values  }
\description{plot a gene network, coloring nodes according
to relative expression values  }
\usage{
plotExGraph(g, es, sampind=1, pal=colorRampPalette(brewer.pal(9, "Blues"))(length(nodes(g))), attgen=pwayRendAttrs)
}
\arguments{
  \item{g}{ graph representing a gene network}
  \item{es}{ an ExpressionSet instance }
  \item{sampind}{ sample to be used  to obtain relative expression values }
  \item{pal}{ palette for coloring the nodes}
  \item{attgen}{ attribute generating function }
}
\details{
plots a colored network on the current graphics display
}
\value{
as returned by Rgraphviz plot method for graphNEL instances
}
%\references{  }
\author{Vince Carey <stvjc@channing.harvard.edu>}
%\note{  }


%\seealso{  }

\examples{
library(graph)
data(pancrCaIni)
library(ALL)
data(ALL)
library(hgu95av2.db)
collap1 = reduceES( ALL, nodes(pancrCaIni), revmap(hgu95av2SYMBOL), "symbol", mean )
library(RColorBrewer)
plotExGraph( pancrCaIni, collap1, 1 )
}
\keyword{ models }
