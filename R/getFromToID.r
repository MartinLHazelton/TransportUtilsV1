#' Get Specified Location IDs
#'
#' This function gets location IDs based on the given conditions on "from" or
#' "to".
#'
#' @param dataframe A data frame with "from_id" and "to_id".
#' @param status To specify the "from" or "to".
#'
#' @return The output is the specified IDs.
#'
#' @keywords getFromToID
#'
#' @examples
#' regn <- c(-1.20, 51.71, -1.19, 51.72)
#' network <- getnet(regn)
#' fromIDs <- getFromToID(network, "from")
#' head(fromIDs)
#'
#' @export

getFromToID <- function(dataframe, status){
  if(status=="from"){
    return(unique(dataframe$from_id))
  }
  if(status=="to"){
    return(unique(dataframe$to_id))
  }
}
