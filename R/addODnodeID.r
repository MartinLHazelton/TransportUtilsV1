#' Add Node IDs
#'
#' This function adds node IDs to the OD data.
#'
#' @param mainnodes Provided node IDs.
#' @param ODdata A data frame that needs node IDs.
#'
#' @return The output is a new data frame with node IDs, "from_nodeID" and
#' "to_nodeID".
#'
#' @keywords addODnodeID
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
#'
#' c("from_nodeID", "to_nodeID") %in% names(oxford.od)
#'
#' @export

addODnodeID <- function(mainnodes, ODdata){
  coords2nodeID <- function(x,y){
    output <- numeric(length(x))
    for (i in 1:length(x)){
      output[i] <- mainnodes$nodeID[which.min( (x[i]-mainnodes$x)^2
                                                       + (y[i]-mainnodes$y)^2)]
    }
    output
  }
  ODdata$from_nodeID <- coords2nodeID(ODdata$fx,ODdata$fy)
  ODdata$to_nodeID <- coords2nodeID(ODdata$tx,ODdata$ty)
  return(ODdata)
}
