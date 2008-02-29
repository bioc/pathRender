\name{reduceES}
\alias{reduceES}
\title{collapse the assay values in an ExpressionSet to a
set of specified genes, using a statistic when multiple
probes map to a given gene}
\description{collapse the assay values in an ExpressionSet to a
set of specified genes, using a statistic when multiple
probes map to a given gene}
\usage{
reduceES(es, annovec, ann2featMap, pdvname="symbol", collapseFun=NULL)
}
\arguments{
  \item{es}{ ExpressionSet instance}
  \item{annovec}{ genes to retain }
  \item{ann2featMap}{ either an AnnDbBimap from AnnotationDbi (typically
constructed with revmap(), or a named vector mapping from symbols
to probe set IDs}
  \item{pdvname}{ featureData variable name to be used to hold the 
annotations of variables kept  }
  \item{collapseFun}{ statistical function for collapsing data across
probes mapping to the same gene }
}
%\details{
%}
\value{
An ExpressionSet instance limited to genes in annovec, condensed
if necessary using collapseFun to get one number per gene
from multiple probes
}
%\references{  }
\author{Vince Carey <stvjc@channing.harvard.edu>}
%\note{  }


%\seealso{  }

\examples{
library(ALL)
data(ALL)
library(hgu95av2.db)
rr = revmap(hgu95av2SYMBOL)
exprs(reduceES(ALL[,1:3], c("DDR1", "CPNE1"), rr, "sym", mean))
}
\keyword{ models }
