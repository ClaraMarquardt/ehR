#----------------------------------------------------------------------------#

#' Replace (in place) zeros in in data.table with another value.
#' @export
#' @param name Name of data.table [character]
#' @param replace Value with which to replace 0s  [any]
#' @return Data.table modified in place
#' @examples
#' TBC

set_zero_na <- function(dt, replace=NA) {

  for (j in seq_len(ncol(dt)))
    set(dt, which(dt[[j]] %in% c(0)), j, replace)
}

#----------------------------------------------------------------------------#
