% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/addSegments.r
\name{addSegments}
\alias{addSegments}
\title{Show Segments}
\usage{
addSegments(routes, col, lwd = 1, showEnds = TRUE)
}
\arguments{
\item{routes}{Segments with lon-lat coordinates.}

\item{col}{Specified Colour.}

\item{lwd}{line-width.}

\item{showEnds}{If TRUE, show end points.}
}
\value{
The out put is a series of segments on the current plot.
}
\description{
This function adds segments to the current plot.
}
\examples{
bbox <- osmdata::getbb("oxford uk")
wholenet <- getnet("oxford uk")
oxford.od <- ODdata("oxford uk")
mainnet <- mainnet(wholenet)
cntrnet <- contractNet(mainnet)

set.seed(1)
fromID <- cntrnet[sample(1:nrow(cntrnet), 1), "from_id"]
set.seed(12)
toID <- cntrnet[sample(1:nrow(cntrnet), 1), "to_id"]
shortPath <- accessPath(cntrnet, subbox = "NA", more=FALSE,
           fromID = fromID, toID = toID, status = "ShortPath")
shownetwork(bbox, wholenet, cntrnet)
addSegments(shortPath, "red", lwd = 2, showEnds = TRUE)

}
\keyword{addSegments}
