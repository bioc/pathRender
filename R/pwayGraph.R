
library("graph")

setClass("pwayGraph", 
         representation(pwaySource="character"), 
         contains="graphNEL")

setMethod("plot", "pwayGraph", function(x, y, ...) {
   plot(as(x, "graphNEL"), 
        nodeAttrs=x@graphData$nAttrs, 
        edgeAttrs=x@graphData$eAttrs,
        recipEdges="distinct", ...)
})

