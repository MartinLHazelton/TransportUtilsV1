#' Tailor the Network
#'
#' This function tailors a network with the given boundary box.
#'
#' @param bbox A given boundary box.
#' @param network A network that needs to be tailored.
#' @param od The OD info on "from" or "to" to clarify going outside the box or
#' going inside the box respectively.
#' @param out If NULL, the inside-to-outside data will be returned.
#' If "outside", the outside-to-inside data will be returned.
#'
#' @return The output is the tailored network.
#'
#' @keywords tailor
#'
#' @examples
#' location <- c(osmdata::getbb("oxford uk"))
#' wholenet <- getnet("oxford uk")
#' mainnet <- mainnet(wholenet)
#' shownetwork(vec2bbox(location), wholenet, tittle = "Whole Network")
#' regn <- c(-1.24, 51.71, -1.19, 51.79) # Selected a sub-region
#' subbox <- vec2bbox(regn)
#' subregion <- tailor(subbox, wholenet)
#' shownetwork(vec2bbox(location), subregion, tittle = "Tailored Network")
#'
#' @export

tailor <- function(bbox, network, od = NULL, out = NULL){
  xrange <- bbox[1, ]
  yrange <- bbox[2, ]
  if(is.null(od)){
    inside <- which(network$from_lon>=xrange[1] & network$from_lon<=xrange[2] &
                      network$to_lon>=xrange[1] & network$to_lon<=xrange[2] &
                      network$from_lat>=yrange[1] & network$from_lat<=yrange[2] &
                      network$to_lat>=yrange[1] & network$to_lat<=yrange[2]
    )
  }else{
    if(od=="from"){
      inside <- which(network$from_lon>=xrange[1] & network$from_lon<=xrange[2] &
                        network$from_lat>=yrange[1] & network$from_lat<=yrange[2]
      )
    }
    if(od=="to"){
      inside <- which(network$to_lon>=xrange[1] & network$to_lon<=xrange[2] &
                        network$to_lat>=yrange[1] & network$to_lat<=yrange[2]
      )
    }
  }
  if(is.null(out)){
    return(network[inside,])
  }
  if(out=="outside"){
    return(network[-inside,])
  }
}
