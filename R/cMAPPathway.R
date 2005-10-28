
infoOnecMAPInteraction <- function(i, i_mem, bi, bm) {

  m_data <- as.matrix(bi[[i_mem[i]]]$component)
  m_cnt <- length(m_data)

  nodeVec <- c(i_mem[i])

  if ( m_cnt > 0 ) {
     m_mem <- sapply(m_data, function(x) { x$id } )
     e_data <- bm[m_mem]

     # nodes in the subgraph: molecules + interaction
     nodeVec <- unique(c(m_mem, nodeVec))     # molecules are unique now
  }
  

  # edges in the subgraph: directed based on molecule's role in interaction
  edL <- vector("list", length=length(nodeVec))
  names(edL) <- nodeVec
  for ( j in 1:length(edL) ) {  edL[[j]] <- list(edges=c()) }

  if ( m_cnt > 0 ) {
    for ( j in 1:m_cnt ) {
       switch(m_data[[j]]$edge,
       "input"=,
       "agent"= edL[[m_data[[j]]$id]] <- list(edges = c(edL[[m_data[[j]]$id]]$edges, i_mem[i])),
       "inhibitor"=,
       "output"= edL[[i_mem[i]]] <- list(edges = c(edL[[i_mem[i]]]$edges, m_data[[j]]$id)),
       stop(paste("add new edge type for: ", m_data[[j]]$edge)))
    }
  }

  # subgraph
  gg <- new("graphNEL", nodes=nodeVec, edgeL=edL, edgemode="directed")

  # node attributes for rendering
  nlabels <- c()
  nshapes <- c()
  nfillcolors <- c()

  nlabels[nodeVec] <- c("")
  nshapes[nodeVec] <- c("plaintext")
  nfillcolors[nodeVec] <- c("transparent")

  # molecule nodes
  if ( m_cnt > 0 ) {
    for ( j in 1:(length(nodeVec)-1) ) {

       nlabels[nodeVec[j]] <- e_data[[j]]$extid["AS"]

       nshapes[nodeVec[j]] <- switch ( e_data[[j]]$type,
          			"complex"  = c("box"),
          			"compound" = c("box"),
          			"rna"      = c("plaintext"),
          			"protein"  = c("plaintext"),
          			c("plaintext")
          			)
    }
  }

  # interaction node
  nlabels[i_mem[i]] <- bi[[i_mem[i]]]$process
  nshapes[i_mem[i]] <- switch ( bi[[i_mem[i]]]$process,
     			"modification"  = c("box"),
     			"binding"       = c("box"),
     			"transcription" = c("ellipse"),
     			"reaction"      = c("ellipse"),
     			"translocation" = c("ellipse"),
     			c("plaintext")
  			)

  nfillcolors[i_mem[i]] <- switch ( bi[[i_mem[i]]]$process,
     			"modification"  = c("red"),
     			"binding"       = c("red"),
     			"transcription" = c("yellow"),
     			"reaction"      = c("green"),
     			"translocation" = c("blue"),
     			c("red")
  			)

  nAttrs <- list(label=nlabels, shape=nshapes, fillcolor=nfillcolors)

  # edge attributes for rendering
  ecolors <- c()
  estyles <- c()
  eweights <- c()

  ecolors[edgeNames(gg, recipEdges="distinct")] <- c("")
  estyles[edgeNames(gg, recipEdges="distinct")] <- c("solid")
  eweights[edgeNames(gg, recipEdges="distinct")] <- c("1")

  if ( m_cnt > 0 ) {
    for ( j in 1:m_cnt ) {
       ecolors[j] <- switch(m_data[[j]]$edge,
       			"input"     = "black",
       			"output"    = "black",
       			"agent"     = "green",
       			"inhibitor" = "red",
       			stop(paste("add new edge color for: ", m_data[[j]]$edge)))
       estyles[j] <- switch(m_data[[j]]$edge,
       			"input"     = "solid",
       			"output"    = "solid",
       			"agent"     = "solid",
       			"inhibitor" = "solid",
       			stop(paste("add new edge style for: ", m_data[[j]]$edge)))
       eweights[j] <- switch(m_data[[j]]$edge,
       			"input"     = "1",
       			"output"    = "1",
       			"agent"     = "1",
       			"inhibitor" = "1",
       			stop(paste("add new edge weight for: ", m_data[[j]]$edge)))
    }
  }

  eAttrs <- list(color=ecolors, style=estyles, weight=eweights)

  list(graph=gg, nAttrs=nAttrs, eAttrs=eAttrs)

}

renderOnecMAPInteraction <- function(i, i_mem, bi, bm) {
  
  i_cnt <- length(i_mem)

  if ( 0 < i && i <= i_cnt ) {
     ii <- infoOnecMAPInteraction(i, i_mem, bi, bm)
     plot(ii$graph, nodeAttrs=ii$nAttrs, edgeAttrs = ii$eAttrs, 
          recipEdges="distinct")
  }
  else {
     stop("cannot find this interaction for given pathway in cMAP")
  }
}

renderOnecMAPPathway <- function(i_mem, bi, bm) {
  
  i_cnt <- length(i_mem)

  G <- new("graphNEL", edgemode="directed")
  nAttrs <- list()
  eAttrs <- list()

  for ( i in 1:i_cnt ) {

      ig <- infoOnecMAPInteraction(i, i_mem, bi, bm)

      G <- join(G, ig$graph) 

      nAttrs$label <- c(nAttrs$label, ig$nAttrs$label)
      nAttrs$shape <- c(nAttrs$shape, ig$nAttrs$shape)
      nAttrs$fillcolor <- c(nAttrs$fillcolor, ig$nAttrs$fillcolor)

      eAttrs$color <- c(eAttrs$color, ig$eAttrs$color)
      eAttrs$style <- c(eAttrs$style, ig$eAttrs$style)
      eAttrs$weight <- c(eAttrs$weight, ig$eAttrs$weight)
  }

  plot(G, nodeAttrs=nAttrs, edgeAttrs = eAttrs, recipEdges="distinct")
}

