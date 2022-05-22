#' Add Coordinates to OD Data
#'
#' This function adds lon-lat coorniates to the OD data according to the given
#' OD nodes.
#'
#' @param odData The OD data frame, which needs lon-lat coordinates.
#' @param odNode A data frame with nodes info.
#' @param wholenet The reference network with lon-lat coordinates.
#'
#' @return The output is a list of an OD data frame with lon-lat coordinates and
#' a data frame contains nodes out of the network.
#'
#' @keywords od2network
#'
#' @examples
#' location <- c(osmdata::getbb("oxford uk"))
#' wholenet <- getnet("oxford uk")
#' oxford.od <- ODdata("oxford uk")
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
#' outNet <- odlist2[[2]]
#'
#' head(OxfordODre)
#' head(outNet)
#'
#' @export

od2network <- function(odData, odNode, wholenet){
  nr <- nrow(odData)
  matchedOD <- odData
  matchedOD <- merge(matchedOD, odNode[,2:4], by.x = "fromNode", by.y = "nodeID")
  names(matchedOD)[4:5] <- c("from_lon", "from_lat")
  # matchedOD <- unique(matchedOD)
  matchedOD <- merge(matchedOD, odNode[,2:4], by.x = "toNode", by.y = "nodeID")
  names(matchedOD)[6:7] <- c("to_lon", "to_lat")
  matchedOD <- unique(matchedOD)
  if(nrow(matchedOD)==nr){
    odData <- matchedOD
  }
  matchedOD <- merge(matchedOD, unique(wholenet[,3:5]),
                      by.x = c("from_lon", "from_lat"),
                      by.y = c("from_lon", "from_lat")
  )
  # matchedOD <- unique(matchedOD)
  matchedOD <- merge(unique(wholenet[,6:8]), matchedOD,
                      by.x = c("to_lon", "to_lat"),
                      by.y = c("to_lon", "to_lat")
  )
  matchedOD <- unique(matchedOD)

  if(nrow(matchedOD)==nr){
    odData <- matchedOD
    outNet <- NULL
  }else{
    outNet <- odData[which((!odData$fromNode%in%matchedOD$fromNode) |
                             (!odData$toNode%in%matchedOD$toNode)
    ), ]
  }
  return(list(matchedOD, outNet))
}
