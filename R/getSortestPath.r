#' Get Shortest Path
#'
#' This function gets the shortest path for the provided from_id and to_id
#' within a given network.
#'
#' @param network A given network. In general, it is a contracted network.
#' @param fromID The starting location represented by a from_id.
#' @param toID The ending location represented by a to_id.
#'
#' @return The output is a data frame of the shortest path within the given
#' network.
#'
#' @keywords getSortestPath
#'
#' @examples
#' location <- c(osmdata::getbb("oxford uk"))
#' wholenet <- getnet("oxford uk")
#' mainnet <- mainnet(wholenet)
#' cntrnet <- contractNet(mainnet)
#'
#' set.seed(1)
#' fromID <- cntrnet[sample(1:nrow(cntrnet), 1), "from_id"]
#' set.seed(2)
#' toID <- cntrnet[sample(1:nrow(cntrnet), 1), "to_id"]
#' ShortPath <- getSortestPath(cntrnet, fromID = fromID, toID = toID)
#'
#' shownetwork(vec2bbox(location), wholenet, cntrnet)
#' addSegments(ShortPath, "red", lwd = 2, showEnds = TRUE)
#'
#' @export

getSortestPath <- function(network, fromID = fromID, toID = toID){
  return(accessPath(network, subbox="NA", more=FALSE,
             fromID = fromID, toID = toID, status = "ShortPath"))
}

