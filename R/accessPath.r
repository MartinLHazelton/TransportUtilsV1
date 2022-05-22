#' Path Accessibility
#'
#' This function creates new boundary nodes for a sub-region using the method of
#' detecting the shortest path between two nodes.
#'
#' @param wholenet Reference network with node IDs and lon-lat coordinates.
#' @param subbox Sub-region of the reference network. The case subbox = "NA" is prepared
#' for the shortest path.
#' @param more If TRUE, will return more info including the distance "d", the weighted
#' distance "d_weighted", the "time", and the weighted time "time_weighted".
#' @param fromID The starting node ID as expected.
#' @param toID The ending node ID as expected.
#' @param status Two possible status for this function. If status = NULL, as the
#' default, a full list will be returned. If status = "ShortPath", only the
#' shortest path will be returned.
#'
#' @return The output is a list of a new network with the created nodes
#' "addedNet", a boundary-crossed segment "crossSeg", and a segemnt with the
#' added new node "newCrossSeg". It also includes the shortest path "shortPath" and
#' the coordinates of the created boundary nodes "xyFrame". If the status is
#' "ShortPath", only the thortest path will be returned.
#'
#' @keywords accessPath
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
#' fromIDs <- getFromToID(in2outFrom, "from")
#' toIDs <- getFromToID(in2outTo, "to")
#'
#' listp <- accessPath(cntrnet, subbox, more=FALSE,
#'                     fromID = fromIDs[4], toID = toIDs[5])
#' shortPath <- listp$shortPath
#' xyFrame <- listp$xyFrame
#' newCrossSeg <- listp$newCrossSeg
#'
#' shownetwork(vec2bbox(location), wholenet, cntrnet)
#' addbox(regn, "blue")
#' addSegments(shortPath, "red", lwd = 2, showEnds = TRUE)
#' with(
#'   xyFrame,
#'   points(x, y, pch = 4, col= "green")
#' )
#' addSegments(newCrossSeg, "yellow", showEnds = FALSE)
#'
#' @import igraph
#'
#' @export

accessPath <- function(wholenet, subbox = "NA", more = FALSE,
                       fromID = NULL, toID = NULL, status = NULL){
  if(is.null(status)){
    subRegion <- tailor(subbox, wholenet)
    crossPart <- NULL
  }
  shortPath <- NULL
  if((!is.null(fromID)) & (!is.null(toID))){
    if(is.null(status)){
      if(!((fromID%in%subRegion$from_id & !(toID%in%subRegion$to_id)) |
         (toID%in%subRegion$to_id & !(fromID%in%subRegion$from_id)))){
        stop("The 'from-to-route' doesn't cross the sub-box boundary.
             Please check fromID and toID.")
      }
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

    nodes12 <- all_shortest_paths(gNet, fromID, toID)[[1]][[1]]
    nlist <- names(nodes12)

    nn <- length(nlist)
    rowId <- c()
    for (i in 1:(nn-1)) {
      rowId <- c(rowId, which(wholenet$from_id==nlist[i] &
                                wholenet$to_id==nlist[i+1])[1])
    }
    shortPath <- wholenet[rowId, ]
  }

  if(is.null(status)){
    ## crossed-segments
    # "to" is inside and "from" is outside:
    crossSegID1 <- with(
      wholenet,
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
    # "from" is inside and "to" is outside:
    crossSegID2 <- with(
      wholenet,
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

    crossSeg <- wholenet[sort(c(crossSegID1, crossSegID2)), ]
    tempNet <- wholenet[-sort(c(crossSegID1, crossSegID2)), ]
    templist <- addCrossedPnts(crossSeg, c(subbox), more)
    newCrossSeg <- templist$crossedAdded
    xyFrame <- templist$xyFrame
    addedNet <- rbind(tempNet, newCrossSeg)
    return(list(addedNet=addedNet, newCrossSeg=newCrossSeg, crossSeg=crossSeg,
                shortPath=shortPath, xyFrame=xyFrame))
  }
  if(status=="ShortPath"){
    return(shortPath)
  }
}
