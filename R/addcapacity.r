#' Add Capacities to a Network
#'
#' This function adds numerical capacities to the network data frame.
#'
#' @param linksdata A network with the "highway" column factorized by "trunk",
#' "primary", "secondary", "tertiary", "trunk_link", "primary_link",
#' "secondary_link" and "tertiary_link".
#' @param specified Prepared for further development on specified numerical
#' capacities.
#'
#' @return The output is a network data frame with the added capacities.
#'
#' @keywords addcapacity
#'
#' @examples
#' location <- c(osmdata::getbb("oxford uk"))
#' wholenet <- getnet("oxford uk")
#' oxford.od <- ODdata("oxford uk")
#' mainnet <- mainnet(wholenet)
#' cntrnet <- contractNet(mainnet)
#' ODlist <- getODnodes(cntrnet, oxford.od)
#' oxford.main.nodes <- ODlist[[2]]
#' oxford.links.df <- links(oxford.main.nodes, cntrnet)
#' OxfordLinks <- addcapacity(oxford.links.df)
#' head(OxfordLinks)
#'
#' @export

addcapacity <- function(linksdata, specified = NULL){
  linkID <- 1:nrow(linksdata)
  linksdata$linkID <- linkID
  capacity <- numeric(nrow(linksdata))
  capacity[linksdata$highway == "trunk"] <- 3600
  capacity[linksdata$highway == "primary"] <- 1470
  capacity[linksdata$highway == "secondary"] <- 1300
  capacity[linksdata$highway == "tertiary"] <- 1140
  capacity[linksdata$highway == "trunk_link"] <- 3600
  capacity[linksdata$highway == "primary_link"] <- 1470
  capacity[linksdata$highway == "secondary_link"] <- 1300
  capacity[linksdata$highway == "tertiary_link"] <- 1140
  newlinksdata <- data.frame(linkID = linkID,
                            fromNode = linksdata$from_nodeID,
                            toNode = linksdata$to_nodeID,
                            cost0 = linksdata$time_weighted,
                            capacity = capacity)
  newlinksdata$cost0 <- round(newlinksdata$cost0, 1)
  return(newlinksdata)
}
