#' Show a Boundary-Box
#'
#' This function adds a box to the current plot.
#'
#' @param location A vector contains coordinates of min-x, min-y, max-x and max-y.
#' @param col The specified colour.
#'
#' @return The output is a box added to the plot.
#'
#' @keywords addbox
#'
#' @examples
#' wholenet <- getnet("oxford uk")
#' bbox <- osmdata::getbb("oxford uk")
#' shownetwork(bbox, wholenet)
#' addbox(c(bbox), col = "blue")
#'
#' @export

addbox <- function(location, col = NULL){
  if(is.null(col)){
    col <-  "black"
  }
  segments(location[1], location[2], location[1], location[4], col = col)
  segments(location[1], location[2], location[3], location[2], col = col)
  segments(location[3], location[2], location[3], location[4], col = col)
  segments(location[3], location[4], location[1], location[4], col = col)
}
