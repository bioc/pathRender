\name{colorNodes}
\alias{colorNodes}
\alias{colorNodes,graphNEL,numeric,character,function-method}

\title{ attach node coloring information to a graphNEL instance }
\description{ attach node coloring information to a graphNEL instance }
\usage{
colorNodes(g, nodeAss, pal, attgen)
}
\arguments{
  \item{g}{ graphNEL instance}
  \item{nodeAss}{ color map for nodes: vector with elements
evaluating to colors and nodes as element names}
  \item{pal}{ a palette (use colorRampPalette for color interpolation) }
  \item{attgen}{ attribute generating function -- pwayRendAttrs is 
prototype}
}
%\details{
%}
\value{
a graphNEL instance with additional rendering data
}
%\references{  }
\author{Vince Carey <stvjc@channing.harvard.edu>}
%\note{  }


%\seealso{  }

\examples{
example(randomGraph)
nn = nodes(g1)
x = runif(length(nn))
names(x) = nn
h1 = colorNodes(g1, x, colorRampPalette(brewer.pal(9, "Blues"))(length(nn)),
 pwayRendAttrs)
h1
}
\keyword{ models }
