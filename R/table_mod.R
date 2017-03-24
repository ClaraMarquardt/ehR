#----------------------------------------------------------------------------#

#' Ordered frequency table formatted as a data.table.
#' @export
#' @param  TBC
#' @return TBC
#' @examples
#' TBC

table_mod <- function(x) {

  table(x, useNA="always")[order(-table(x, useNA="always"))]
}

#----------------------------------------------------------------------------#
