#----------------------------------------------------------------------------#

#' Pad elements of list with 0s to bring all elements to a fixed length.
#' @export
#' @param  topad List of elements to pad [list]
#' @param  num Final length [integer]
#' @param  method Whether to pad front or back [character]
#' @param  replace What to pad with [character]
#' @return TBC
#' @examples
#' TBC

pad <- function(topad, num, method="front", replace="0") {

  topad <- as.character(topad)
  fixed <-
    unlist(
    lapply(topad, function(x) {
     if(!is.na(x)) {
       lack <- num - nchar(x)
        if(lack > 0) {
          sub <- paste0(rep(replace, lack), collapse = "")
          if (method=="front") {
            x <- paste0(sub, x)
          }
          if (method=="back") {
            x <- paste0(x, sub)
          }
        } else {
          x <- x
        }
    } else {
      x <- x
    }
      return(x)
    }
  ))

  return(fixed)

}

#----------------------------------------------------------------------------#
