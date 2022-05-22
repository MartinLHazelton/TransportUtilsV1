#' Set Node IDs for OD Data
#'
#' This function adds node IDs to the OD data by using the
#' "match_points_to_graph" function (in the "dodgr" package) and referring to a
#' given network with node IDs.
#'
#' @param network The referred network with node IDs. In general, it is a
#' contracted network.
#' @param ODdata An OD data that needs node IDs.
#'
#' @return The output is a list with a new OD data, where node IDs are added,
#' and a data frame with main nodes.
#'
#' @keywords getODnodes
#'
#' @examples
#' wholenet <- getnet("oxford uk")
#' oxford.od <- ODdata("oxford uk")
#' mainnet <- mainnet(wholenet)
#' cntrnet <- contractNet(mainnet)
#' ODlist <- getODnodes(cntrnet, oxford.od)
#' oxford.od.nodes <- ODlist[[1]]
#' oxford.main.nodes <- ODlist[[2]]
#' head(oxford.od.nodes)
#' head(oxford.main.nodes)
#'
#' @import dodgr
#'
#' @export

getODnodes <- function(network, ODdata){
  oxford.o <- unique(cbind(ODdata$fx, ODdata$fy))
  oxford.d <- unique(cbind(ODdata$tx, ODdata$ty))

  mainnodes <- dodgr_vertices(network)
  mainnodes$nodeID <- mainnodes$n  # NB numbering starts at zero
  mainnodes <- mainnodes[, c(1, 2, 3, 6)]
  tmp <- match_points_to_graph(mainnodes,
                               data.frame(x = oxford.o[, 1], y = oxford.o[, 2]),
                               connected=FALSE)

  ODnodes <- mainnodes[tmp, ]
  ODnodes$true.x <- oxford.o[, 1]
  ODnodes$true.y <- oxford.o[, 2]
  return(list(ODnodes, mainnodes))
}
