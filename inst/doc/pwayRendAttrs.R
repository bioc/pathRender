pwayRendAttrs = function(g, AllBorder="transparent",
    AllFixedsize=FALSE, AllFontsize=22.1, AllShape="rectangle",
    fillcolor="transparent", ...) {
   nn = nodes(g)
   numn = length(nn)
   color = rep(AllBorder, numn)
   names(color)=nn
   fixedsize = rep(AllFixedsize, numn)
   names(fixedsize) = nn
   if (length(fillcolor)==1) {
	fillcolvec = rep(fillcolor, numn)
   	names(fillcolvec) = nn
   }
   else if (!identical(names(fillcolor), nodes(g))) 
  	stop("names on vector fillcolor must match nodes(g) exactly")
   else fillcolvec = fillcolor
   shape = rep(AllShape, numn)
   names(shape) = nn
   fontsize = rep(AllFontsize, numn)
   names(fontsize) = nn
   list(color=color, fixedsize=fixedsize, fillcolor=fillcolvec, shape=shape,
      fontsize=fontsize )
}

aggFeatures = function( fvec, splitvar, aggfun ) {
  if (!is.vector(fvec)) stop("fvec must satisfy is.vector()")
  if (!(length(fvec) == length(splitvar))) stop("fvec and splitvar must have equal lengths")
  sapply(split(fvec, splitvar), aggfun)
}

revAnnoEnv = function(env) {
  li = as.list(env)
  na = names(li)
  lens = sapply(li,length)
  rna = rep(na,lens)
  newbas = as.character(unlist(li))
  split(rna, newbas)
}

featNamesFromAnno = function( annovec, ann2featMap ) {
  if (is(ann2featMap, "AtomicAnnDbBimap")) anslist =  mget( annovec, ann2featMap, ifnotfound=NA ) 
  else if (is(ann2featMap, "list")) 
           anslist = ann2featMap[ match(annovec, names(ann2featMap)) ]
  lens = sapply(anslist,length)
  nn = rep(names(anslist), lens)
  ans = unlist(anslist)
  names(ans) = nn
  ans
}

reduceES = function( es, annovec, ann2featMap, pdvname="symbol" ) {
  n1 = na.omit(featNamesFromAnno( annovec, ann2featMap ))
  es2 = es[ n1, ]
  newdf = data.frame(names(n1))
  names(newdf) = pdvname
  rownames(newdf) = n1
  featureData(es2) = new("AnnotatedDataFrame", newdf)
  es2
}
  
if (!("package:ALL" %in% search())) library(ALL)
if (!exists("ALL")) data(ALL)
if (!("package:graph" %in% search())) library(graph)
data(MAPKsig)
library(hgu95av2.db)
X = reduceES( ALL, nodes(MAPKsig), revmap(hgu95av2SYMBOL) )
 

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

setMethod("plot", "coloredGraph", function(x, y,  ...) {
  oldp = palette(x@graphData$pal)
  plot(as(x, "graphNEL"), nodeAttrs = x@graphData$nodeRendAttr )
  palette(oldp)
})
