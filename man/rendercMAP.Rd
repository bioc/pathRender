\name{rendercMAPPathway}
\alias{rendercMAPPathway}
\title{ Render pathways from cMAP }
\description{
Build graphs based on pathway or interaction data from cMAP database, 
render them using Rgraphviz.
}
\usage{
rendercMAPPathway(pname, ino=0) 
}
\arguments{
  \item{pname}{ name of the pathway to render}
  \item{ino}{ index of the interaction in the given pathway to render}
}
\details{
For a given pathway in cMAP database, we build a subgraph for each interaction 
in the pathway, join them together to form the graph for the complete pathway. 
The subgraphs for interactions and the graph for the pathway include info for 
rendering, such as labels/shapes/fillcolors for nodes, colors/styles/weights 
for edges.  If user specifies an index of interaction, only the interaction is
rendered.  Otherwise, the complete pathway is rendered. 
}
\value{
None.  A graphical output is presented.
}
\author{ Li Long <li.long@isb-sib.ch> }

\examples{
   rendercMAPPathway("plateletapppathway")
   rendercMAPPathway("plateletapppathway", 5)
   rendercMAPPathway("hsa00601")
   rendercMAPPathway("hsa00601", 10)
}
\keyword{ graphs }% at least one, from doc/KEYWORDS

