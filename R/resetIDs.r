#' Reset Node IDs
#'
#' This function resets node IDs with increasing positive integers.
#'
#' @param network A network data frame with node IDs, location IDs and the
#' lon-lat coordinates.
#'
#' @return The output is a network with the reset node IDs.
#'
#' @keywords resetIDs
#'
#' @examples
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
#' newODdata <- resetIDs(OxfordODre)
#' head(newODdata)
#'
#' @export

resetIDs <- function(network){
  oldNodes <- unique(c(network$toNode, network$fromNode))
  tmpdf <- data.frame(toNode = oldNodes,
                      newNode = 1:length(oldNodes)
  )
  nrow(network)
  df <- merge(network, tmpdf, by="toNode")
  nrow(df)
  df <- df[,c("fromNode", "newNode", "from_id", "to_id",
                  "from_lon", "from_lat", "to_lon", "to_lat",
                  "demand")]
  names(df) <- c("fromNode", "toNode", "from_id", "to_id",
                   "from_lon", "from_lat", "to_lon", "to_lat",
                   "demand")
  tmpdf <- data.frame(fromNode = oldNodes,
                      newNode = 1:length(oldNodes)
  )
  df <- merge(df, tmpdf, by="fromNode")
  df <- df[,c("newNode", "toNode", "from_id", "to_id",
                  "from_lon", "from_lat", "to_lon", "to_lat",
                  "demand")]
  names(df) <- c("fromNode", "toNode", "from_id", "to_id",
                   "from_lon", "from_lat", "to_lon", "to_lat",
                   "demand")
  return(df)
}
