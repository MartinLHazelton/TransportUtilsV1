#' Merge Demands for Boundary Nodes
#'
#' This function sums all possible demands for boundary nodes. It can be used to
#' exam the maximum work-load on each added boundary node.
#'
#' @param boundaryData A data frame with boundary nodes.
#' @param status A description of going-inside ("out2in") or going-outside
#' ("in2out") the region.
#'
#' @return The output is a data frame with the grouped nodes and the total
#' demands for each node.
#'
#' @keywords mergeDemands
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
#' oxford.od.matrix <- ODwithdemand(oxford.od)
#' OxfordOD <- renameODdata(oxford.od.matrix)
#' odlist2 <- od2network(OxfordOD, oxford.od.nodes, cntrnet)
#' OxfordODre <- odlist2[[1]]
#'
#' in2outFrom <- tailor(subbox, OxfordODre, od="from")
#' in2outTo <- tailor(subbox, OxfordODre, od="to", "outside")
#' fromIDs <- getFromToID(in2outFrom, "from")
#' toIDs <- getFromToID(in2outTo, "to")
#' in2outNodes <- getboundaryNodes(cntrnet, subbox, OxfordODre, fromIDs, toIDs, "in2out")
#' in2outDemands <- mergeDemands(in2outNodes[,c("in2out_lon", "in2out_lat", "demand")], "in2out")
#' head(in2outDemands)
#'
#' out2inFrom <- tailor(subbox, OxfordODre, od="from", "outside")
#' out2inTo <- tailor(subbox, OxfordODre, od="to")
#' fromIDs <- getFromToID(out2inFrom, "from")
#' toIDs <- getFromToID(out2inTo, "to")
#' out2inNodes <- getboundaryNodes(cntrnet, subbox, OxfordODre, fromIDs, toIDs, "out2in")
#' out2inDemands <- mergeDemands(out2inNodes[,c("out2in_lon", "out2in_lat", "demand")], "out2in")
#' head(out2inDemands)
#'
#' @export

mergeDemands <- function(boundaryData, status){
  require("dplyr")
  if(status == "in2out"){
    return(as.data.frame(boundaryData %>% group_by(in2out_lon, in2out_lat) %>%
             summarise_all(sum))
    )
  }
  if(status == "out2in"){
    return(as.data.frame(boundaryData %>% group_by(out2in_lon, out2in_lat) %>%
             summarise_all(sum)))
  }
}
