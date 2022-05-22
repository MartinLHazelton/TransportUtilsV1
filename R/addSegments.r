#' Show Segments
#'
#' This function adds segments to the current plot.
#'
#' @param routes Segments with lon-lat coordinates.
#' @param col Specified Colour.
#' @param lwd line-width.
#' @param showEnds If TRUE, show end points.
#'
#' @return The out put is a series of segments on the current plot.
#'
#' @keywords addSegments
#'
#' @examples
#' bbox <- osmdata::getbb("oxford uk")
#' wholenet <- getnet("oxford uk")
#' oxford.od <- ODdata("oxford uk")
#' mainnet <- mainnet(wholenet)
#' cntrnet <- contractNet(mainnet)
#'
#' set.seed(1)
#' fromID <- cntrnet[sample(1:nrow(cntrnet), 1), "from_id"]
#' set.seed(12)
#' toID <- cntrnet[sample(1:nrow(cntrnet), 1), "to_id"]
#' shortPath <- accessPath(cntrnet, subbox = "NA", more=FALSE,
#'            fromID = fromID, toID = toID, status = "ShortPath")
#' shownetwork(bbox, wholenet, cntrnet)
#' addSegments(shortPath, "red", lwd = 2, showEnds = TRUE)
#'
#' @export

addSegments <- function(routes, col, lwd = 1, showEnds = TRUE){
  with(
    routes,
    segments(from_lon, from_lat, to_lon, to_lat, col = col, lwd = lwd)
  )
  if(showEnds){
    n <- nrow(routes)
    with(
      routes[1,],
      points(from_lon, from_lat, pch = 19)
    )
    with(
      routes[n,],
      points(to_lon, to_lat, pch = 19)
    )
  }
}
