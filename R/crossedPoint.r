#' Crossed-Point of Two Segments
#'
#' This function gets the coordinates of the crossed-point of two segments.
#'
#' @param a1 The x-coordinate of the first point in segment-1.
#' @param b1 The y-coordinate of the first point in segment-1.
#' @param c1 The x-coordinate of the second point in segment-1.
#' @param d1 The y-coordinate of the second point in segment-1.
#' @param a2 The x-coordinate of the first point in segment-2.
#' @param b2 The y-coordinate of the first point in segment-2.
#' @param c2 The x-coordinate of the second point in segment-2.
#' @param d2 The y-coordinate of the second point in segment-2.
#'
#' @return The output is the coordinates of the crossed-point. It will be "NULL"
#' if the segments are parallel.
#'
#' @keywords crossedPoint
#'
#' @examples
#' a1=0; b1=0; c1=2; d1=2; a2=0; b2=2; c2=2; d2=0
#' plot(c(a1,c1), c(b1,d1), xlim=c(-1,3), ylim=c(-1,3), type="l")
#' segments(a2, b2, c2, d2)
#' xy <- crossedPoint(a1, b1, c1, d1, a2, b2, c2, d2)
#' xy
#' points(xy[1], xy[2], col="red")
#'
#' a1=0; b1=0; c1=0; d1=2; a2=1; b2=2; c2=1; d2=0
#' plot(c(a1,c1), c(b1,d1), xlim=c(-1,3), ylim=c(-1,3), type="l")
#' segments(a2, b2, c2, d2)
#' xy <- crossedPoint(a1, b1, c1, d1, a2, b2, c2, d2)
#' is.null(xy)
#'
#' @export

crossedPoint <- function(a1, b1, c1, d1, a2, b2, c2, d2){
  if(a1!=c1 & a2!=c2){
    if((d1-b1)/(c1-a1)!=(d2-b2)/(c2-a2)){
      x <- ((d1-b1)/(c1-a1)*a1-(d2-b2)/(c2-a2)*a2+b2-b1)/
        ((d1-b1)/(c1-a1)-(d2-b2)/(c2-a2))
      y <- (d1-b1)/(c1-a1)*(x-a1)+b1
    }else{
      return(NULL)
    }
  }
  if(a1==c1 & a2!=c2){
    if(a2!=a1){
      x <- a1
      y <- (d2-b2)/(c2-a2)*(x-a2)+b2
    }else{
      x <- a2
      y <- b2
    }
  }
  if(a1!=c1 & a2==c2){
    if(a1!=a2){
      x <- a2
      y <- (d1-b1)/(c1-a1)*(x-a1)+b1
    }else{
      x <- a1
      y <- b1
    }
  }
  if(a1==c1 & a2==c2){
    return(NULL)
  }

  return(c(x,y))
}
