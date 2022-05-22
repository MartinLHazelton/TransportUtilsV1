#' Show Networks
#'
#' This function shows the networks with a given region.
#'
#' @param bbox A given region with the range of longitudes and latitudes.
#' @param network A network as the background network.
#' @param contracted A network on top of the background network. In general, it
#' is a contracted network.
#' @param netnodes The node coordinates. If it is NOT NULL, nodes will be shown
#' on the networks.
#' @param nodeID If TRUE, node IDs will be shown on the network.
#' @param capacity If the capacities are given, the routes thickness will change
#' according to the capacities.
#' @param extended A ratio on the extended boundary of the current plot.
#'
#' @return The output is a map of the networks.
#'
#' @keywords shownetwork
#'
#' @examples
#' location <- c(osmdata::getbb("oxford uk"))
#' wholenet <- getnet("oxford uk")
#' mainnet <- mainnet(wholenet)
#' cntrnet <- contractNet(mainnet)
#' shownetwork(vec2bbox(location), wholenet, mainnet, tittle = "Main Network On The Background")
#' shownetwork(vec2bbox(location), wholenet, cntrnet)
#'
#' @export

shownetwork <- function(bbox, network, contracted = NULL,
                        netnodes = NULL, tittle = NULL,
                        nodeID = TRUE, capacity = NULL, extended = 0.02){
  # Generate the tittle
  if(is.null(tittle)){
    if(!is.null(contracted)){
      tittle <- "Contracted Network"
      if(!is.null(netnodes)){
        tittle <- "Contracted Network with Nodes"
      }
    }else{
      tittle <- "Network"
    }
  }
  if(is.numeric(bbox) & is.vector(bbox)){
    xrange <- c(bbox[1], bbox[3])
    yrange <- c(bbox[2], bbox[4])
  }else if(is.numeric(bbox) & is.matrix(bbox)){
    xrange <- bbox[1, ]
    yrange <- bbox[2, ]
  }else{
    stop("The range/bbox of the coordinates is not suitable.")
  }
  plot(0, 0, main = tittle,
       xlim = xrange + diff(xrange) * extended * c(-1,1),
       ylim = yrange + diff(yrange) * extended * c(-1,1),
       pch=19, col=2,
       xlab="Longitude", ylab="Latitude")
  with(network, segments(from_lon, from_lat, to_lon, to_lat,
                               col = grey(0.8)))
  if(!is.null(contracted)){
    if(!is.null(capacity)){
      rate <- sort(unique(capacity))
      m <- length(rate)
      n <- 1.5
      # if(m< n){n = m}
      setlwd <- (n - .5) / (rate[m] - rate[1]) * (capacity - rate[1]) + .5
      with(contracted, segments(from_lon, from_lat, to_lon, to_lat,
                                lwd = setlwd))
    }else{
      with(contracted, segments(from_lon, from_lat, to_lon, to_lat))
    }
  }
  if(!is.null(netnodes)){
    points(netnodes$x, netnodes$y, col=2, pch=19)
    if(nodeID == TRUE){
      text(netnodes$x,
           netnodes$y,
           labels = as.character(netnodes$nodeID), col=4)
    }
  }
}
