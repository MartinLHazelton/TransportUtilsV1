#' Add Boundary Nodes
#'
#' This function adds boundary nodes to a refined OD data frame based on the
#' given going-inside nodes and going-outside nodes of the sub-region.
#'
#' @param ODrefined An OD data frame with node IDs and location IDs.
#' @param in2outNodes The nodes that are going outside the sub-region.
#' @param out2inNodes The nodes that are going inside the sub-region.
#'
#' @return The output is a new OD data frame with the added boundary nodes of
#' the sub-region.
#'
#' @keywords addBoundaryOD
#'
#' @examples
#' location <- c(osmdata::getbb("oxford uk"))
#' wholenet <- getnet("oxford uk")
#' oxford.od <- ODdata("oxford uk")
#' regn <- c(-1.24, 51.71, -1.19, 51.79) # Selected a sub-region
#' subbox <- vec2bbox(regn)
#' mainnet <- mainnet(wholenet)
#' cntrnet <- contractNet(mainnet)
#' ODlist <- getODnodes(cntrnet, oxford.od)
#' oxford.od.nodes <- ODlist[[1]]
#' oxford.main.nodes <- ODlist[[2]]
#' oxford.od <- addODnodeID(oxford.main.nodes, oxford.od)
#' oxford.od.matrix <- ODwithdemand(oxford.od)
#' OxfordOD <- renameODdata(oxford.od.matrix)
#' odlist2 <- od2network(OxfordOD, oxford.od.nodes, cntrnet)
#' OxfordODre <- odlist2[[1]]
#'
#' in2outFrom <- tailor(subbox, OxfordODre, od="from")
#' in2outTo <- tailor(subbox, OxfordODre, od="to", "outside")
#'
#' fromIDs <- getFromToID(in2outFrom, "from")
#' toIDs <- getFromToID(in2outTo, "to")
#' in2outNodes <- getboundaryNodes(cntrnet, subbox, OxfordODre,
#'                                 fromIDs, toIDs, "in2out")
#'
#' out2inFrom <- tailor(subbox, OxfordODre, od="from", "outside")
#' out2inTo <- tailor(subbox, OxfordODre, od="to")
#'
#' fromIDs <- getFromToID(out2inFrom, "from")
#' toIDs <- getFromToID(out2inTo, "to")
#' out2inNodes <- getboundaryNodes(cntrnet, subbox, OxfordODre,
#'                                 fromIDs, toIDs, "out2in")
#'
#' newODdata <- addBoundaryOD(OxfordODre, in2outNodes, out2inNodes)
#' head(newODdata)
#'
#' @export

addBoundaryOD <- function(ODrefined, in2outNodes, out2inNodes){
  allIDS <- c(ODrefined$from_id, ODrefined$to_id)
  maxOldID <- max(as.numeric(unique(allIDS)))

  allNodes <- c(ODrefined$fromNode, ODrefined$toNode)
  maxOldNode <- max(as.numeric(unique(allNodes)))

  df1 <- in2outNodes[, c("in2out_lon", "in2out_lat")]
  names(df1) <- c("x", "y")
  df2 <- out2inNodes[, c("out2in_lon", "out2in_lat")]
  names(df2) <- c("x", "y")
  tmpdf <- rbind(df1, df2)
  newNodesAndIDs <- unique(tmpdf)
  n <- nrow(unique(newNodesAndIDs))
  newNodesAndIDs$ODnodeID <- maxOldNode + 1:n
  newNodesAndIDs$NetNodeID <- maxOldID + 1:n

  # Prepare "In2out" Data for adding to the refined OD-data.
  preIn2out <- ODrefined[NULL, ]

  nr <- 0
  for (i in 1:nrow(in2outNodes)) {
    nr <- nr+1
    preIn2out[nr, c("from_id", "from_lon", "from_lat", "fromNode")] <-
      in2outNodes[i, c("from_id", "from_lon", "from_lat", "fromNode")]
    preIn2out[nr, c("demand")] <- in2outNodes$demand[i]

    tmpid <- which(newNodesAndIDs$x==in2outNodes$in2out_lon[i] &
                     newNodesAndIDs$y==in2outNodes$in2out_lat[i])

    preIn2out[nr, c("to_id", "toNode")] <-
      newNodesAndIDs[tmpid, c("NetNodeID", "ODnodeID")]
    preIn2out[nr, c("to_lon", "to_lat")] <-
      newNodesAndIDs[tmpid, c("x", "y")]

    nr <- nr+1
    preIn2out[nr, c("from_id", "fromNode")] <-
      newNodesAndIDs[tmpid, c("NetNodeID", "ODnodeID")]
    preIn2out[nr, c("from_lon", "from_lat")] <-
      newNodesAndIDs[tmpid, c("x", "y")]

    preIn2out[nr, c("to_id", "to_lon", "to_lat", "toNode")] <-
      in2outNodes[i, c("to_id", "to_lon", "to_lat", "toNode")]

    sharedNodes <- which((in2outNodes$in2out_lon==in2outNodes$in2out_lon[i] &
                            in2outNodes$in2out_lat==in2outNodes$in2out_lat[i]) &
                           (in2outNodes$to_id==in2outNodes$to_id[i]))
    preIn2out[nr, c("demand")] <- sum(in2outNodes$demand[sharedNodes])
  }
  preIn2out <- unique(preIn2out)

  # Prepare "Out2in" Data for adding to the refined OD-data.
  preOut2in <- ODrefined[NULL, ]

  nr <- 0
  for (i in 1:nrow(out2inNodes)) {
    nr <- nr+1
    preOut2in[nr, c("from_id", "from_lon", "from_lat", "fromNode")] <-
      out2inNodes[i, c("from_id", "from_lon", "from_lat", "fromNode")]
    preOut2in[nr, c("demand")] <- out2inNodes$demand[i]

    tmpid <- which(newNodesAndIDs$x==out2inNodes$out2in_lon[i] &
                     newNodesAndIDs$y==out2inNodes$out2in_lat[i])

    preOut2in[nr, c("to_id", "toNode")] <-
      newNodesAndIDs[tmpid, c("NetNodeID", "ODnodeID")]
    preOut2in[nr, c("to_lon", "to_lat")] <- newNodesAndIDs[tmpid, c("x", "y")]

    nr <- nr+1
    preOut2in[nr, c("from_id", "fromNode")] <-
      newNodesAndIDs[tmpid, c("NetNodeID", "ODnodeID")]
    preOut2in[nr, c("from_lon", "from_lat")] <- newNodesAndIDs[tmpid, c("x", "y")]

    preOut2in[nr, c("to_id", "to_lon", "to_lat", "toNode")] <-
      out2inNodes[i, c("to_id", "to_lon", "to_lat", "toNode")]

    sharedNodes <- which((out2inNodes$out2in_lon==out2inNodes$out2in_lon[i] &
                            out2inNodes$out2in_lat==out2inNodes$out2in_lat[i]) &
                           (out2inNodes$to_id==out2inNodes$to_id[i]))
    preOut2in[nr, c("demand")] <- sum(out2inNodes$demand[sharedNodes])
  }
  preOut2in <- unique(preOut2in)
  newODdata <- rbind(ODrefined, preIn2out, preOut2in)
  return(newODdata)
}
