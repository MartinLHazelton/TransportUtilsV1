#' Merge Neighbour Nodes
#'
#' This function merges some close nodes into one single node.
#'
#' @param network A network with nodes and their distances.
#' @param threshold If NULL, will merge nodes based on the mean of the distances.
#' Otherwise, will merge nodes with the given threshold (as a quantile).
#'
#' @return The output is a list of a data frame with shorter-routes (shorters),
#' a data frame without shorter-routes (NOshorters), a data frame with repeated
#' merged-centres (tailorNR), and the merged network without shorter-routes
#' (merged).
#'
#' @keywords mergeNodes
#'
#' @examples
#' location <- c(osmdata::getbb("oxford uk"))
#' wholenet <- getnet("oxford uk")
#' mainnet <- mainnet(wholenet)
#' cntrnet <- contractNet(mainnet)
#' shownetwork(vec2bbox(location), wholenet, cntrnet)
#' addbox(location, "blue")
#' listAll <- mergeNodes(cntrnet, threshold = 0.98)
#' shorters <- listAll$shorters
#' merged <- listAll$merged
#' with(shorters, segments(from_lon, from_lat, to_lon, to_lat, col = "green"))
#' with(merged, segments(from_lon, from_lat, to_lon, to_lat, col = "red"))
#'
#' @export

mergeNodes <- function(network, threshold = NULL){
  df <- sort(unique(network$d))
  dd <- diff(diff(df))
  dd <- abs(dd)

  idx <- which(dd < mean(dd))
  if(!is.null(threshold)){
    idx <- which(dd < quantile(dd, threshold))
  }

  if(length(which(diff(idx)>1))!=0){
    idx2 <- min(which(diff(idx)>1))
    idx <- idx[1:idx2]
  }

  roundIDs <- which(network$d <= max(df[1:(max(idx) + 2)]))
  shorters <- network[roundIDs, ]
  NOshorters <-  network[-roundIDs, ]

  shorters$groupID <- 0
  shorters$noPts <- 0
  shorters$from_lon_centre <- 0
  shorters$from_lat_centre <- 0
  shorters$to_lon_centre <- 0
  shorters$to_lat_centre <- 0

  grp <- 0
  while(length(which(shorters$groupID == 0)) != 0){
    grp <- grp + 1
    orig <- shorters[which(shorters$groupID == 0)[1], c("from_id")]
    while(orig %in% c(shorters[which(shorters$groupID == 0), c("from_id")],
                      shorters[which(shorters$groupID == 0), c("to_id")])){

      idList <- shorters[which(shorters$from_id == orig & shorters$groupID == 0), "to_id"]
      idList <- unique(c(idList, orig))
      while(length(which(shorters$groupID == 0 & (shorters$from_id %in% idList |
                                                shorters$to_id %in% idList))
      ) !=0 ){
        rowId <- which(shorters$groupID == 0 & (shorters$from_id %in% idList |
                                                shorters$to_id %in% idList))
        shorters[rowId, "groupID"] <- grp
        idList <- c(idList, shorters[rowId, "from_id"], shorters[rowId, "to_id"])
        idList <- unique(idList)
      }
    }
    rowIDs <- which(shorters$groupID == grp)
    shorters[rowIDs, "noPts"] <- length(rowIDs)
    shorters[rowIDs, c("from_lon_centre", "to_lon_centre")] <-
      mean(c(as.matrix(shorters[rowIDs, c("from_lon", "to_lon")])))
    shorters[rowIDs, c("from_lat_centre", "to_lat_centre")] <-
      mean(c(as.matrix(shorters[rowIDs, c("from_lat", "to_lat")])))
  }

  tailorNR <- NOshorters
  for (i in 1:grp) {
    f_lon <- unique(shorters[which(shorters$groupID == i), "from_lon"])
    f_lat <- unique(shorters[which(shorters$groupID == i), "from_lat"])
    f_lon_centre <- shorters[which(shorters$groupID == i), "from_lon_centre"][1]
    f_lat_centre <- shorters[which(shorters$groupID == i), "from_lat_centre"][1]

    t_lon <- unique(shorters[which(shorters$groupID == i), "to_lon"])
    t_lat <- unique(shorters[which(shorters$groupID == i), "to_lat"])
    t_lon_centre <- shorters[which(shorters$groupID == i), "to_lon_centre"][1]
    t_lat_centre <- shorters[which(shorters$groupID == i), "to_lat_centre"][1]

    IDS <- which((tailorNR$from_lon %in% f_lon) & (tailorNR$from_lat %in% f_lat)
                 | (tailorNR$from_lon %in% t_lon) & (tailorNR$from_lat %in% t_lat))

    tailorNR[IDS, "from_lon"] <- f_lon_centre
    tailorNR[IDS, "from_lat"] <- f_lat_centre

    IDS <- which((tailorNR$to_lon %in% t_lon) & (tailorNR$to_lat %in% t_lat)
                 | (tailorNR$to_lon %in% f_lon) & (tailorNR$to_lat %in% f_lat))

    tailorNR[IDS, "to_lon"] <- t_lon_centre
    tailorNR[IDS, "to_lat"] <- t_lat_centre
  }

  IDS <- which(tailorNR$from_lon == tailorNR$to_lon &
                 tailorNR$from_lat == tailorNR$to_lat)
  if(length(IDS) > 0){
    merged <- tailorNR[-IDS, ]
  }else{
    merged <- tailorNR
  }

  return(list(shorters = shorters, NOshorters = NOshorters,
              tailorNR = tailorNR, merged = merged))
}
