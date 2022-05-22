#' Wrap Link-Data and OD-Data
#'
#' This function wraps the link-data and the OD data into a list according to a
#' given location.
#' 
#' @param location The location of a preferred network.
#' @param wt_for The specified profile for the network. The default setting is
#' "motorcar".
#' @param mainset A provided vector of road type, e.g. c("trunk", "trunk_link").
#' If NULL, will use the default setting, c("trunk", "trunk_link", "primary",
#' "primary_link", "secondary", "secondary_link", "tertiary", "tertiary_link").
#' 
#' @return The output is a list contains the original link-data and the refined
#' OD data.
#' 
#' @keywords wrapper
#' 
#' @examples 
#' 
#' location <- "oxford uk"
#' twoDataFrame <- wrapper(location)
#' originalLinkData <- twoDataFrame$linksData
#' refinedODdata <- twoDataFrame$refinedODdata
#' head(originalLinkData)
#' head(refinedODdata)
#' 
#' @export

wrapper <- function(location, wt_for = "motorcar", mainset = NULL){
  wholenet <- getnet(location, wt_for)
  ODdataframe <- ODdata(location)
  mainnet <- mainnet(wholenet)
  cntrnet <- contractNet(mainnet)
  ODlist <- getODnodes(cntrnet, ODdataframe)
  odNodes <- ODlist[[1]]
  ODmainNodes <- ODlist[[2]]
  
  ODmainNodes <- ODlist[[2]]
  linksData <- links(ODmainNodes, cntrnet)

  ODdataframe <- addODnodeID(ODmainNodes, ODdataframe)
  ODmatrix <- ODwithdemand(ODdataframe)
  OxfordOD <- renameODdata(ODmatrix)
  odlist2 <- od2network(OxfordOD, odNodes, cntrnet)
  OxfordODre <- odlist2[[1]]
  refinedODdata <- resetIDs(OxfordODre)
  return(list(linksData = linksData, refinedODdata = refinedODdata))
}
