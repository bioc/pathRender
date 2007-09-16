\name{graphcMAP}
\alias{graphcMAP}
\title{obtain a graph object corresponding to a
cMAP pathway
}
\description{obtain a graph object corresponding to a
cMAP pathway
}
\usage{
graphcMAP(pname)
}
\arguments{
  \item{pname}{ character token identifying a KEGG or cMAP
pathway}
}
\details{
reuses code from pathRender but emits a 
\code{\link[graph]{graphNEL-class}} instance with some
additional information for rendering
}
\value{an instance of pwayGraph, which extends graphNEL
}
%\references{  }
\author{Vince Carey <stvjc@channing.harvard.edu>}
%\note{  }


%\seealso{  }

\examples{
G1 = graphcMAP("p53pathway")
G1
nodes(G1)
if (require(Rgraphviz)) plot(G1)
}
\keyword{ models }
