setClass("pwayGraph", representation(pwaySource="character"), contains="graphNEL")

graphcMAP = function (pname) 
{
    kp <- as.list(cMAPKEGGPATHWAY)
    bp <- as.list(cMAPCARTAPATHWAY)
    if (any(names(kp) == pname)) {
    #    print("KEGG")
        type = "KEGG"
        ki <- as.list(cMAPKEGGINTERACTION)
        km <- as.list(cMAPKEGGMOLECULE)
        }
    else if (any(names(bp) == pname)) {
    #    print("BIOCARTA")
        type = "BIOCARTA"
	kp = as.list(cMAPCARTAPATHWAY)
        ki <- as.list(cMAPCARTAINTERACTION)
        km <- as.list(cMAPCARTAMOLECULE)
        }
    else stop("pname not found in cMAPKEGGPATHWAY")
    i_mem <- apply(as.matrix(kp[[pname]]$component), 2, paste)
    i_cnt <- length(i_mem)
    G <- new("graphNEL", edgemode = "directed")
    nAttrs <- list()
    eAttrs <- list()
    for (i in 1:i_cnt) {
            ig <- infoOnecMAPInteraction(i, i_mem, ki, km)
            G <- join(G, ig$graph)
            nAttrs$label <- c(nAttrs$label, ig$nAttrs$label)
            nAttrs$shape <- c(nAttrs$shape, ig$nAttrs$shape)
            nAttrs$fillcolor <- c(nAttrs$fillcolor, ig$nAttrs$fillcolor)
            eAttrs$color <- c(eAttrs$color, ig$eAttrs$color)
            eAttrs$style <- c(eAttrs$style, ig$eAttrs$style)
            eAttrs$weight <- c(eAttrs$weight, ig$eAttrs$weight)
        }
    G = new("pwayGraph", nodes=nodes(G), edgeL=edgeL(G), edgemode=edgemode(G))
    G@pwaySource = type
    #G@graphData = list()
    G@graphData$eAttrs=eAttrs
    nnode = length(nAttrs$label)
    nnames = names(nAttrs$label)
    fixup = which(is.na(nAttrs$label))
    if (length(fixup)>0) nAttrs$label[fixup] = nnames[fixup]
    nAttrs$fixedsize = rep(FALSE, nnode)
    names(nAttrs$fixedsize) = nnames
    G@graphData$nAttrs=nAttrs
    G
}

setMethod("plot", "pwayGraph", function(x, y, ...) {
   plot(as(x, "graphNEL"), nodeAttrs=x@graphData$nAttrs, edgeAttrs=x@graphData$eAttrs,
    recipEdges="distinct", ...)
})

