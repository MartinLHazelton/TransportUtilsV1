#' Get Boundary-Crossed Points
#'
#' This function gets the coordinates of boundary-crossed points for the
#' provided boundary-crossed segments.
#'
#' @param crossSeg A data frame of boundary-crossed segments.
#' @param regn The boundary of a region.
#' @param more If TRUE, will return more info including the distance "d", the weighted
#' distance "d_weighted", the "time", and the weighted time "time_weighted".
#'
#' @return The output is a list of a new segments data frame with added boundary
#' nodes "crossedAdded" and a data frame of the coordinates of the created
#' boundary nodes "xyFrame".
#'
#' @keywords addCrossedPnts
#'
#' @examples
#' wholenet <- getnet("oxford uk")
#' crossSeg <- wholenet[1, ]
#'
#' x1 <- mean(c(crossSeg$from_lon, crossSeg$to_lon))
#' y1 <- mean(c(crossSeg$from_lat, crossSeg$to_lat))
#' x2 <- max(c(crossSeg$from_lon, crossSeg$to_lon)) + abs(x1)
#' y2 <- max(c(crossSeg$from_lat, crossSeg$to_lat)) + abs(y1)
#' regn <- c(x1, y1, x2, y2)
#' addCrossedPnts(crossSeg, regn, more = TRUE)$
#'   crossedAdded[, c("d", "d_weighted", "time", "time_weighted")]
#' addCrossedPnts(crossSeg, regn, more = FALSE)$
#'   crossedAdded[, c("d", "d_weighted", "time", "time_weighted")]
#' addCrossedPnts(crossSeg, regn, more = TRUE)$xyFrame
#'
#' @export

addCrossedPnts <- function(crossSeg, regn, more = FALSE){
  xyFrame <- data.frame(x=double(), y=double())
  n <- nrow(crossSeg)
  crossedAdded <- crossSeg[NULL, ]
  cnt <- 0
  for (i in 1:n) {
    cnt <- cnt + 1
    a1 <- crossSeg[i, "from_lon"]
    b1 <- crossSeg[i, "from_lat"]
    c1 <- crossSeg[i, "to_lon"]
    d1 <- crossSeg[i, "to_lat"]

    # Edge-1:
    a2 <- regn[1]
    b2 <- regn[2]
    c2 <- regn[1]
    d2 <- regn[4]

    xy <- crossedPoint(a1, b1, c1, d1, a2, b2, c2, d2)

    if((xy[1] >= min(a1, c1)) & (xy[1] <= max(a1, c1))){
      x <- xy[1]
      y <- xy[2]
      r <- (abs(a1-x))/(max(a1, c1)-min(a1, c1))
    }

    # Edge-2:
    a2 <- regn[1]
    b2 <- regn[4]
    c2 <- regn[3]
    d2 <- regn[4]

    xy <- crossedPoint(a1, b1, c1, d1, a2, b2, c2, d2)

    if((xy[2] >= min(b1, d1)) & (xy[2] <= max(b1, d1))){
      x <- xy[1]
      y <- xy[2]
      r <- (abs(b1-y))/(max(b1, d1)-min(b1, d1))
    }

    # Edge-3:
    a2 <- regn[3]
    b2 <- regn[4]
    c2 <- regn[3]
    d2 <- regn[2]

    xy <- crossedPoint(a1, b1, c1, d1, a2, b2, c2, d2)

    if((xy[1] >= min(a1, c1)) & (xy[1] <= max(a1, c1))){
      x <- xy[1]
      y <- xy[2]
      r <- (abs(a1-x))/(max(a1, c1)-min(a1, c1))
    }

    # Edge-4:
    a2 <- regn[3]
    b2 <- regn[2]
    c2 <- regn[1]
    d2 <- regn[2]

    xy <- crossedPoint(a1, b1, c1, d1, a2, b2, c2, d2)

    if((xy[2] >= min(b1, d1)) & (xy[2] <= max(b1, d1))){
      x <- xy[1]
      y <- xy[2]
      r <- (abs(b1-y))/(max(b1, d1)-min(b1, d1))
    }
    xyFrame[i, "x"] <- x
    xyFrame[i, "y"] <- y

    crossedAdded[cnt, ] <- crossSeg[i, ]
    crossedAdded[cnt, "to_lon"] <- x
    crossedAdded[cnt, "to_lat"] <- y
    if(more){
      crossedAdded[cnt, "d"] <- crossSeg[i, "d"]*r
      crossedAdded[cnt, "d_weighted"] <- crossSeg[i, "d_weighted"]*r
      crossedAdded[cnt, "time"] <- crossSeg[i, "time"]*r
      crossedAdded[cnt, "time_weighted"] <- crossSeg[i, "time_weighted"]*r
    }

    cnt <- cnt+1
    crossedAdded[cnt, ] <- crossSeg[i, ]
    crossedAdded[cnt, "from_lon"] <- x
    crossedAdded[cnt, "from_lat"] <- y
    if(more){
      crossedAdded[cnt, "d"] <- crossSeg[i, "d"]*(1-r)
      crossedAdded[cnt, "d_weighted"] <- crossSeg[i, "d_weighted"]*(1-r)
      crossedAdded[cnt, "time"] <- crossSeg[i, "time"]*(1-r)
      crossedAdded[cnt, "time_weighted"] <- crossSeg[i, "time_weighted"]*(1-r)
    }
  }
  return(list(crossedAdded=crossedAdded, xyFrame=xyFrame))
}
