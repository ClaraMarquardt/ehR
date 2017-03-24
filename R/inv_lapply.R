#----------------------------------------------------------------------------#

#' Invisible lapply.
#' @export
#' @param  TBC
#' @return TBC
#' @examples
#' TBC

inv_lapply <- function(X, FUN,...) {

  invisible(lapply(X, FUN,...))

}

#----------------------------------------------------------------------------#
