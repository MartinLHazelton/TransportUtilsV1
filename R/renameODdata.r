#' Rename the OD Data
#'
#' This function renames the OD data by using column names: "demand", "fromNode"
#' and "toNode".
#'
#' @param ODdata The original OD data.
#'
#' @return The output is the OD data with names, "demand", "fromNode"
#' and "toNode".
#'
#' @keywords renameODdata
#'
#' @examples
#' wholenet <- getnet("oxford uk")
#' oxford.od <- ODdata("oxford uk")
#' mainnet <- mainnet(wholenet)
#' cntrnet <- contractNet(mainnet)
#' ODlist <- getODnodes(cntrnet, oxford.od)
#' oxford.main.nodes <- ODlist[[2]]
#' oxford.od <- addODnodeID(oxford.main.nodes, oxford.od)
#' oxford.od.matrix <- ODwithdemand(oxford.od)
#' OxfordOD <- renameODdata(oxford.od.matrix)
#' head(OxfordOD)
#'
#' @export

renameODdata <- function(ODdata){
  names(ODdata) <- c("demand", "fromNode", "toNode")
  return(ODdata)
}
