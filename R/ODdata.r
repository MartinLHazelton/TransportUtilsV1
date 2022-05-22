#' Get the OD Data
#'
#' This function generates the OD data for a given location.
#'
#' @param location The location where needs the OD data.
#'
#' @return The output is the OD data of the location.
#'
#' @keywords ODdata
#'
#' @examples
#' oxford.od <- ODdata("oxford uk")
#' head(oxford.od)
#'
#' @import pct
#' @import sf
#' @import stplanr
#'
#' @export

ODdata <- function(location){
  require("pct")
  require("sf")
  require("stplanr")
  location <- gsub("(\\w+).*", "\\1", location)
  oxford.desire.lines <- get_desire_lines(location)
  oxford.desire.lines <- st_transform(oxford.desire.lines,
                                      "+proj=longlat +datum=WGS84")
  oxford.od <- as.data.frame(line2df(oxford.desire.lines)[c("fx","fy","tx","ty")])
  oxford.od$geo_code1 <- oxford.desire.lines$geo_code1
  oxford.od$geo_code2 <- oxford.desire.lines$geo_code2
  oxford.od$car_driver <- oxford.desire.lines$car_driver
  cat("Connecting extra-links ...", "\n")
  iList <- unique(oxford.od$geo_code1)
  jList <- unique(oxford.od$geo_code2)
  for (i in iList){
    for (j in jList){
      if (i != j){
        if(sum(oxford.od$geo_code1==i & oxford.od$geo_code2==j)==0){
          temp.from <- min(which(oxford.od$geo_code1==i))
          temp.to <- min(which(oxford.od$geo_code2==j))
          # cat(i,j,"\n")
          extra.line <- data.frame(oxford.od$fx[temp.from],
                                   oxford.od$fy[temp.from],
                                   oxford.od$tx[temp.to],
                                   oxford.od$ty[temp.to],
                                   oxford.od$geo_code1[temp.from],
                                   oxford.od$geo_code2[temp.to],
                                   0)
          names(extra.line) <- names(oxford.od)
          oxford.od <- rbind(oxford.od, extra.line)
        }
      }
    }
  }
  cat("Extra-links connected.", "\n")
  oxford.od <- oxford.od[order(oxford.od$geo_code1, oxford.od$geo_code2), ]
  return(oxford.od)
}
