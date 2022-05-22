#' Get the Contracted Network
#'
#' This function introduces dodgr_contract_graph functions to get the major
#' contracted network with "component = 1".
#'
#' @param network A network data frame in the dodgr class (refer to the "dodgr"
#' package).
#'
#' @return The output is a contracted network.
#'
#' @keywords contractNet
#'
#' @examples
#' location <- c(osmdata::getbb("oxford uk"))
#' wholenet <- getnet("oxford uk")
#' mainnet <- mainnet(wholenet)
#' cntrnet <- contractNet(mainnet)
#' shownetwork(vec2bbox(location), wholenet, cntrnet)
#'
#' @import dodgr
#'
#' @export

contractNet <- function(network){
  contracted <- dodgr_contract_graph(network)
  contracted <- contracted[contracted$component==1, ]
  return(contracted)
}

