setClass("coloredGraph", contains="graphNEL")

setGeneric("colorNodes", function(g, nodeAss, pal, attgen) standardGeneric("colorNodes"))
setMethod("colorNodes", c("graphNEL", "numeric", "character", "function"), function(g, nodeAss, pal, attgen) {
  nn = nodes(g)
  an = names(nodeAss)
  if (!all(an %in% nn)) stop("nodeAss must have names a subset of nodes(g)")
  ncol = length(pal)
  bounds = seq(min(nodeAss)-.01, max(nodeAss)+.01, length=ncol+1)
  asscut = cut( nodeAss, breaks = bounds )
  colcodes = as.numeric(asscut)
  names(colcodes) = names(nodeAss)
  fullco = rep(0, length(nodes(g)))
  names(fullco) = nodes(g)
  fullco[ names(colcodes) ] = as.numeric(colcodes)
  natt = attgen(g, fillcolor=fullco)
  g = new("coloredGraph", nodes=nodes(g), edgeL=edgeL(g), edgemode=edgemode(g))
  g@graphData$nodeRendAttr = natt
  g@graphData$pal = pal
  g
})
