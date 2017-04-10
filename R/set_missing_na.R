#----------------------------------------------------------------------------#

#' Replace (in place) empty values ("[ ]*" or "") in data.table with another value.
#' @export
#' @param name Name of data.table [character]
#' @param replace Value with which to replace empty values  [any]
#' @return Data.table modified in place
#' @examples
#' TBC

set_missing_na <- function(dt, replace=NA, subset_col=names(dt)) {

  for (j in which(names(dt) %in% subset_col))
    set(dt, which(gsub("[ ]*", "", dt[[j]])==""), j, replace)
}

#----------------------------------------------------------------------------#

