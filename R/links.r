#' Link to Node IDs
#'
#' This function links the "from_id" and "to_id" to node IDs based on the given two
#' data frames.
#'
#' @param mainnodes A data frame with main node IDs.
#' @param network A network with "from_id" and "to_id." In general, it is a
#' ontracted network.
#'
#' @return The out put is a network data frame with added node IDs.
#'
#' @keywords links
#'
#' @examples
#' wholenet <- getnet("oxford uk")
#' oxford.od <- ODdata("oxford uk")
#' mainnet <- mainnet(wholenet)
#' cntrnet <- contractNet(mainnet)
#' ODlist <- getODnodes(cntrnet, oxford.od)
#' oxford.main.nodes <- ODlist[[2]]
#' oxford.links.df <- links(oxford.main.nodes, cntrnet)
#' head(oxford.links.df)
#'
#' @export

links <- function(mainnodes, network){
  id2nodeID <- function(x){
    output <- numeric(length(x))
    for (i in 1:length(x)){
      output[i] <- mainnodes$nodeID[mainnodes$id == x[i]]
    }
    output
  }
  linksdata <- network[, c(3, 4, 5, 6, 7, 8, 9, 10, 11, 13, 14)]
  linksdata$from_nodeID <- id2nodeID(linksdata$from_id)
  linksdata$to_nodeID <- id2nodeID(linksdata$to_id)
  return(linksdata)
}

