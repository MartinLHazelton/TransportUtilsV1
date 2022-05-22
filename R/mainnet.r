#' Get the Main Network
#'
#' This function extracts the main network from a given network based on the
#' provided road types.
#'
#' @param network A given network with the road type of "highway".
#' @param mainset A provided vector of road type, e.g. c("trunk", "trunk_link").
#' If NULL, will use the default setting, c("trunk", "trunk_link", "primary",
#' "primary_link", "secondary", "secondary_link", "tertiary", "tertiary_link").
#'
#' @return The output is a main network with the preferred road type.
#'
#' @keywords mainnet
#'
#' @examples
#' bbox <- osmdata::getbb("oxford uk")
#' wholenet <- getnet("oxford uk")
#' mainnet <- mainnet(wholenet)
#' shownetwork(bbox, wholenet, mainnet)
#'
#' @export

mainnet <- function(network, mainset = NULL){
  if(is.null(mainset)){
    mainset <- c("trunk", "trunk_link", "primary", "primary_link", "secondary",
              "secondary_link", "tertiary", "tertiary_link")
  }
  network_main <- network[network$highway %in% mainset, ]
  network_main <- network_main[network_main$component == 1, ]
  return(network_main)
}
