###################################################
### chunk number 1: code
###################################################
#require(graph)
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
  if (is(ann2featMap, "AnnDbBimap")) 
           anslist =  AnnotationDbi::mget( annovec, ann2featMap, ifnotfound=NA ) 
  else if (is(ann2featMap, "list")) 
           anslist = ann2featMap[ match(annovec, names(ann2featMap)) ]
  lens = sapply(anslist,length)
  nn = rep(names(anslist), lens)
  ans = unlist(anslist)
  names(ans) = nn
  ans
}

reduceES = function( es, annovec, ann2featMap, pdvname="symbol", collapseFun=NULL ) {
#
# es is an ExpressionSet with featureNames f, say, and
# annovec is a collection of annotations a.  ann2featMap
# is either an AtomicAnnDbBimap with keys in a and range in f, or
# a list with element names in a and element values in f
#
# a new ExpressionSet is computed with features restricted to those
# mapped from annovec
#
  n1 = na.omit(featNamesFromAnno( annovec, ann2featMap ))
  es2 = es[ n1, ]
  newdf = data.frame(names(n1))
  names(newdf) = pdvname
  rownames(newdf) = n1
  featureData(es2) = new("AnnotatedDataFrame", newdf)
  if (!is.null(collapseFun)) {
     splv = as.character(newdf[,1])
     s1 = aggFeatures( exprs(es2)[,1], splv, collapseFun )
     ans = apply( exprs(es2), 2, aggFeatures, splv, collapseFun )
     rownames(ans) = names(s1)
     newdf = data.frame(names(s1))
     names(newdf) = pdvname
     rownames(newdf) = names(s1)
     es2 = new("ExpressionSet", exprs=ans, phenoData=phenoData(es))
     featureData(es2) = new("AnnotatedDataFrame", newdf)
  }
  es2
}
  
#if (!("package:ALL" %in% search())) library(ALL)
#if (!exists("ALL")) data(ALL)
#if (!("package:graph" %in% search())) library(graph)
#data(MAPKsig)
#library(hgu95av2.db)
#X = reduceES( ALL, nodes(MAPKsig), revmap(hgu95av2SYMBOL) )
 


setMethod("plot", "coloredGraph", function(x, y,  ...) {
  oldp = palette(x@graphData$pal)
  plot(as(x, "graphNEL"), nodeAttrs = x@graphData$nodeRendAttr )
  palette(oldp)
})

quantizeByRow = function(es, stratFun = function(x) cut(x, quantile(x, c(0,.25,.5,.75,1)) )){
 fn = featureNames(es) # lost by apply
 exprs(es) = t(apply(exprs(es), 1, function(x) as.numeric(stratFun(x))))
 featureNames(es) = fn
 es
}
 
#require(RColorBrewer)
plotExGraph = function(g, es, sampind = 1, pal=colorRampPalette(brewer.pal(9,"Blues"))(length(nodes(g))),
   attgen=pwayRendAttrs) {
 h = colorNodes(g, exprs(es)[,sampind], pal, attgen)
 plot(h)
}



