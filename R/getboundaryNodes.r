#' Get Boundary Nodes
#'
#' This function gets boundary nodes based on the boundary-crossed routes.
#'
#' @param wholenet Reference network with node IDs and lon-lat coordinates.
#' @param subbox A sub-region of the reference network.
#' @param infoData The reference data frame.
#' @param fromIDs The starting node IDs of routes.
#' @param toIDs The ending node IDs of routes.
#' @param status Specified situation on "going-inside" (out2in) or "going
#' outside" (in2out).
#'
#' @return The output is the boundary nodes, which is identified by shortest
#' paths.
#'
#' @keywords getboundaryNodes
#'
#' @examples
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
#' shownetwork(osmdata::getbb("oxford uk"), wholenet, cntrnet)
#' addbox(regn, "blue")
#' with(
#'   in2outNodes,
#'   points(in2out_lon, in2out_lat, pch = 4, col= "green")
#' )
#'
#' @import igraph
#'
#' @export

getboundaryNodes <- function(wholenet, subbox, infoData, fromIDs, toIDs, status){
  if(status=="in2out"){
    boundaryNodes <- data.frame(from_id = character(), to_id = character(),
                                in2out_lon = double(), in2out_lat = double(),
                                from_lon = double(), from_lat = double(),
                                to_lon = double(), to_lat = double(),
                                fromNode = character(), toNode = character(),
                                demand = double()
    )
  }
  if(status=="out2in"){
    boundaryNodes <- data.frame(from_id = character(), to_id = character(),
                                out2in_lon = double(), out2in_lat = double(),
                                from_lon = double(), from_lat = double(),
                                to_lon = double(), to_lat = double(),
                                fromNode = character(), toNode = character(),
                                demand = double()
    )
  }
  netG <- NULL
  netW <- NULL
  id <- tmpIDs <- frIDs <- tmplist <- frWTs <- NULL
  allIDs <- unique(c(wholenet$from_id, wholenet$to_id))
  for (id in allIDs) {
    if(id%in%wholenet$to_id){

      tmpIDs <- wholenet$to_id==id

      frIDs <- wholenet[tmpIDs, "from_id"]
      tmplist <- list(temp = frIDs)
      names(tmplist)[1] <- id
      netG <- append(netG, tmplist)

      frWTs <- wholenet[tmpIDs, "d"]
      tmplist <- list(temp = frWTs)
      names(tmplist)[1] <- id
      netW <- append(netW, tmplist)
    }
  }

  netE <- as.matrix(stack(netG))
  gNet <- graph_from_edgelist(netE)
  E(gNet)$weight <- stack(netW)[[1]]

  n <- 0
  for (fromID in fromIDs) {
    for (toID in toIDs) {
      n <- n + 1
      nodesOD <- all_shortest_paths(gNet, fromID, toID)[[1]][[1]]
      nlist <- names(nodesOD)

      # shortest path:
      p1to2 <- which((wholenet$from_id%in%nlist[1:(length(nlist)-1)]) &
                       (wholenet$to_id%in%nlist[2:(length(nlist))])
      )

      shortPath <- wholenet[p1to2, ]

      if(status=="in2out"){
        # "from" is inside and "to" is outside:
        crossID <- with(
          shortPath,
          which(
            (
              (to_lon < subbox[1] | to_lon > subbox[3]) |
                (to_lat < subbox[2] | to_lat > subbox[4])
            ) &
              (
                (from_lon >= subbox[1] & from_lon <= subbox[3]) &
                  (from_lat >= subbox[2] & from_lat <= subbox[4])
              )
          )
        )
      }
      if(status=="out2in"){
        # "to" is inside and "from" is outside:
        crossID <- with(
          shortPath,
          which(
            (
              (from_lon < subbox[1] | from_lon > subbox[3]) |
                (from_lat < subbox[2] | from_lat > subbox[4])
            ) &
              (
                (to_lon >= subbox[1] & to_lon <= subbox[3]) &
                  (to_lat >= subbox[2] & to_lat <= subbox[4])
              )
          )
        )
      }

      crossPart <- shortPath[crossID, ]
      tmplist <- addCrossedPnts(crossPart[1,], c(subbox), more=FALSE)
      boundaryNodes[n, "from_id"] <- fromID
      boundaryNodes[n, "to_id"] <- toID
      if(status=="in2out"){
        boundaryNodes[n, "in2out_lon"] <- tmplist$xyFrame$x
        boundaryNodes[n, "in2out_lat"] <- tmplist$xyFrame$y
      }
      if(status=="out2in"){
        boundaryNodes[n, "out2in_lon"] <- tmplist$xyFrame$x
        boundaryNodes[n, "out2in_lat"] <- tmplist$xyFrame$y
      }
      tmpidx <- which(infoData$from_id==fromID & infoData$to_id==toID)
      boundaryNodes[n, "demand"] <- infoData[tmpidx, "demand"]
      boundaryNodes[n, "from_lon"] <- infoData[tmpidx, "from_lon"]
      boundaryNodes[n, "from_lat"] <- infoData[tmpidx, "from_lat"]
      boundaryNodes[n, "to_lon"] <- infoData[tmpidx, "to_lon"]
      boundaryNodes[n, "to_lat"] <- infoData[tmpidx, "to_lat"]
      boundaryNodes[n, "fromNode"] <- infoData[tmpidx, "fromNode"]
      boundaryNodes[n, "toNode"] <- infoData[tmpidx, "toNode"]
    }
  }
  return(boundaryNodes)
}
