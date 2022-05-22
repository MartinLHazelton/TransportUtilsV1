#' Vector to Boundary Box
#'
#' This function changes a vector to the boundary box frame.
#'
#' @param vec The vector of the range of longitudes and latitudes in the form
#' of c(min_lon, min_lat, max_lon, max_lat).
#'
#' @return The output is a boundary box.
#'
#' @keywords vec2bbox
#'
#' @examples
#' osmdata::getbb("oxford uk")
#' c(osmdata::getbb("oxford uk"))
#' vec2bbox(c(osmdata::getbb("oxford uk")))
#'
#' @export

vec2bbox <- function(vec){
  if(length(vec) !=4 ){
    stop("The input vector should be in the form of
         c(min_lon, min_lat, max_lon, max_lat).")
  }
  bbox <- matrix(vec, nrow = 2, byrow = FALSE)
  row.names(bbox) <- c("x", "y")
  colnames(bbox) <- c("min", "max")
  return(bbox)
}
