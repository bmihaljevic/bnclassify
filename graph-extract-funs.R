# only add nodes without edges  
# what is this adj()?

# what do i need is directed for? 
# recall: for my kruskal i also need to cover undirected graphs 
# i also need weights for the kruskal. the kruskal could also been an adjacency matrix; it is not sparse. yet directed is only triangular. 

("isAdjacent",signature(object="graph", from="character",
                                 to="character"),
          function(object, from, to) {
              eSpec <- .normalizeEdges(from, to)
              from <- eSpec$from
              to <- eSpec$to
              fromIdx <- match(from, nodes(object), nomatch=0)
              toIdx <- match(to, nodes(object), nomatch=0)
              if (any(fromIdx == 0))
                stop("unknown nodes in 'from': ",
                     pasteq(from[fromIdx == 0]))
              if (any(toIdx == 0))
                stop("unknown nodes in 'to': ",
                     pasteq(to[toIdx == 0]))
              fromEdges <- edges(object)[from]
              # WTF is this?? 
              .Call(graph_is_adjacent, fromEdges, to)
          })


("subGraph", signature(snodes="character", graph="graphNEL"),
          function(snodes, graph) {
              origNodes <- nodes(graph)
              snodesIdx <- match(snodes, origNodes)
              if (any(is.na(snodesIdx))) {
                  bad <- snodes[which(is.na(snodesIdx))]
                  stop("'snodes' contains nodes not in graph: ",
                       pasteq(bad))

              }
              killedNodes <- origNodes[-snodesIdx]
              newEdges <- lapply(edges(graph)[snodes],
                                 function(x) {
                                     whD <- match(killedNodes, x, nomatch=0)
                                     if (any(whD))
                                       x[-whD]
                                     else
                                       x
                                 })
              ans <- graphNEL(nodes=snodes, edgeL=newEdges,
                         edgemode=edgemode(graph))
              ## FIXME: need to clean the attributes, right now we are passing
              ##        too much.
              nodeIdx <- match(snodes, names(graph@nodeData), 0)
              ans@nodeData@defaults <- graph@nodeData@defaults
              ans@nodeData@data <- graph@nodeData@data[nodeIdx]
              ee <- .getAllEdges(ans)
              if (length(ee$from) && length(ee$to)) {
                  kk <- .makeEdgeKeys(ee$from, ee$to)
                  whkk <- match(kk, names(graph@edgeData), 0)
                  ans@edgeData@defaults <- graph@edgeData@defaults
                  ans@edgeData@data <- graph@edgeData@data[whkk]
              }
              ans
          })

("ugraph", "graph",
          function(graph) {
              if (!isDirected(graph))
                return(graph)
              eMat <- edgeMatrix(graph)
              ## add recip edges
              eMat <- cbind(eMat, eMat[c(2, 1), ])
              ## put into graphNEL edgeL format
              eL <- lapply(split(as.vector(eMat[2, ]), as.vector(eMat[1, ])),
                           function(x) list(edges=unique(x)))
              theNodes <- nodes(graph)
              ## some nodes may be missing
              names(eL) <- theNodes[as.integer(names(eL))]
              ## add empty edge list for nodes with no edges
              noEdgeNodes <- theNodes[!(theNodes %in% names(eL))]
              noEdges <- lapply(noEdgeNodes,
                                function(x) list(edges=numeric(0)))
              names(noEdges) <- noEdgeNodes
              ## FIXME: should we skip standard initialize for speed?
              ## need to copy over at least the nodeData...
              graphNEL(nodes=theNodes, edgeL=c(eL, noEdges),
                  edgemode="undirected")
          })


("edgeMatrix", c("graphNEL", "ANY"),
           function(object, duplicates=FALSE) {
                   ## Return a 2 row numeric matrix (from, to, weight)
               ed <- object@edgeL
               ##reorder to the same order as nodes
               ed <- ed[nodes(object)]
               nN <- length(ed)
               eds<-lapply(ed, function(x) x$edges)
               elem <- listLen(eds)
               from <- rep(seq_len(nN), elem)
               to <- unlist(eds, use.names=FALSE)
               ans <- rbind(from, to)
               ##we duplicate edges in undirected graphNEL
               ##so here we remove them
               if( edgemode(object) == "undirected"  && !duplicates) {
                   swap <- from>to
                   ans[1,swap]<-to[swap]
                   ans[2,swap]<-from[swap]
                   t1 <- paste(ans[1,], ans[2,], sep="+")
                   ans <- ans[ ,!duplicated(t1), drop=FALSE]
               }
               ans
           })


("addNode", signature(node="character", object="graphNEL",
                               edges="missing"),
          function(node, object, edges) {
              gN = nodes(object)
              already <- match(node, gN)
              if( any(!is.na(already)) )
                stop("node(s) already in graph: ", pasteq(gN[already]))
              checkValidNodeName(node)
              ## add them on the end so we don't renumber
              gN = c(gN, node)
              edgeL <-  object@edgeL
              nEd <- vector("list", length=length(node))
              names(nEd) <- node
              for(i in seq(along=nEd))
                nEd[[i]] <- list(edges=numeric(0))
              edgeL <- c(edgeL, nEd)
              object@nodes <- gN
              object@edgeL <- edgeL
              object
          })


("adj", c("graphNEL", "ANY"), function(object, index) {
    initI <- as.character(index)
    nd <- nodes(object)
    if( is.character(index) )
      index <- match(index, nd)
    if( is.na(index) || index < 0 || index > length(nd) )
      stop("vertex is not in graph: ", sQuote(initI))
    edges(object)[index]})


("complement", c("graph"), function(x) {
    if( edgemode(x) != "undirected" )
        stop("'edgemode' not supported: ", sQuote(edgemode(x)))

    xN <- nodes(x)
    xE <- edges(x)
    rval <- vector("list", length=length(xE))
    names(rval) <- xN
    for( i in xN ) {
        ans <-xN[ !(xN %in% c(i, xE[[i]])) ]
        lena <- length(ans)
        if( lena > 0 )
            rval[[i]] <- list(edges=match(ans, xN),
                              weights=rep(1, lena))
        else
            rval[[i]] <- list(edges=numeric(0), weights=numeric(0))
    }
    graphNEL(nodes=xN, edgeL=rval, edgemode=edgemode(x))
})

("removeNode", c("character", "graphNEL"),
          function(node, object) {
              ##first clear the node -- does the checking too
              object <- clearNode(node, object)
              nN <- nodes(object)
              wh <- match(node, nN)
              gN <- nN[-wh]
              nE <- object@edgeL[-wh]
              ## Now renumber the nodes as stored in the edgelist
              nE2 <- lapply(nE, function(el) {
                  oldN <- nN[el$edges]
                  el$edges <- match(oldN, gN)
                  el
              })
              object@nodes <- gN
              object@edgeL <- nE2
              object
          })

("removeEdge",
          signature(from="character", to="character", graph="graphNEL"),
          function(from, to, graph) {
              gN <- nodes(graph)
              wh <- match(c(from, to), gN)
              if( any(is.na(wh)) )
                stop("'from' or 'to' not in graph: ",
                     pasteq(unique(wh[is.na[wh]])))
              if (length(to) == 1)
                to <- rep(to, length(from))
              if (length(from) == 1)
                from <- rep(from, length(to))
              if (!isDirected(graph)) {
                  fromOrig <- from
                  from <- c(fromOrig, to)
                  to <- c(to, fromOrig)
                  remove(fromOrig)
              }
              graph <- clearEdgeData(graph, from, to)
              remEL <- split(to, from)
              fromU <- names(remEL)
              nE <- edges(graph, fromU)
              whD <- mapply(function(x, y) match(x, y), remEL, nE,
                            SIMPLIFY=FALSE)
              graph@edgeL <- edgeKiller(graph@edgeL, fromU, whD)
              graph
          })

("isDirected", "graph",
	  function(object){
            edgemode(object) == "directed"
          }) 

("ugraph", "graph",
          function(graph) {
              if (!isDirected(graph))
                return(graph)
              eMat <- edgeMatrix(graph)
              ## add recip edges
              eMat <- cbind(eMat, eMat[c(2, 1), ])
              ## put into graphNEL edgeL format
              eL <- lapply(split(as.vector(eMat[2, ]), as.vector(eMat[1, ])),
                           function(x) list(edges=unique(x)))
              theNodes <- nodes(graph)
              ## some nodes may be missing
              names(eL) <- theNodes[as.integer(names(eL))]
              ## add empty edge list for nodes with no edges
              noEdgeNodes <- theNodes[!(theNodes %in% names(eL))]
              noEdges <- lapply(noEdgeNodes,
                                function(x) list(edges=numeric(0)))
              names(noEdges) <- noEdgeNodes
              ## FIXME: should we skip standard initialize for speed?
              ## need to copy over at least the nodeData...
              graphNEL(nodes=theNodes, edgeL=c(eL, noEdges),
                  edgemode="undirected")
          })