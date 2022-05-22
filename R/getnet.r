#' Generate a Network
#'
#' This function generates a network according to the given location.
#'
#' @param location The location of a preferred network.
#' @param wt_for The specified profile for the network. The default setting is
#' "motorcar".
#'
#' @return The output is the network with the specified profile.
#'
#' @keywords getnet
#'
#' @examples
#' oxford <- getnet("oxford uk")
#' shownetwork(osmdata::getbb("oxford uk"), oxford)
#'
#' smallerNet <- getnet(c(-1.24, 51.71, -1.19, 51.79))
#' shownetwork(c(-1.24, 51.71, -1.19, 51.79), smallerNet)
#'
#' @import dodgr
#' @import osmdata
#'
#' @export

getnet <- function(location, wt_for = "motorcar"){
  # Checking and unifying the input "location"
  if(is.vector(location) & is.numeric(location)){
    if(length(location)!=4 | location[3]<=location[1] | location[4]<=location[2]){
      stop("The vector of a location should be in the form:
            c(longitude_min, latitude_min, longitude_max, latitude_max).")
    }
  }else if(is.character(location)){
    network.bb <- osmdata::getbb(location)
    location <- c(network.bb)
  }else{
    stop("The \"location\" should be a vector or a location name.")
  }

  # Extracting the network
  cat("Extracting the network...", "\n")
  network <- dodgr_streetnet(location, expand=0)

  # Weighting the network
  cat(paste("Weighting the network according to the provided \"",
            wt_for,"\"", "...", sep = ""), "\n")
  network.wt <- weight_streetnet(network, wt_profile = wt_for)

  cat(paste("The longitude is between ", location[1], " and ", location[3],
            ".", sep = ""), "\n")
  cat(paste("The latitude is between ", location[2], " and ", location[4],
            ".", sep = ""), "\n")
  cat("(You can tailor the network by re-setting the location boundary box.)", "\n")

  return(network.wt)
}
