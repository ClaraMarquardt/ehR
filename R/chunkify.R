#----------------------------------------------------------------------------#

#' Chunkify a data.table into a list of data.tables.
#' @export
#' @import data.table
#' @param  dt Name of data.table to be chunkfied [character]
#' @param  chunk_num Rows per chunk [integer]
#' @return TBC
#' @examples
#' dia_list <- chunkify(dia, 1000)

chunkify <- function(dt, chunksize) {

    rows <- nrow(dt)
    chunksize <- as.numeric(chunksize)
    chunks <- 1:ceiling(rows/chunksize)
    chunkid <- unlist(lapply(chunks, function(i) rep(chunks[[i]], chunksize)))
    chunkid <- chunkid[1:rows]
    dt2 <- dt[, chunkid := chunkid]
    out <- split(dt2, dt2$chunkid)
    dt[, chunkid:=NULL]
    return(out)
}

#----------------------------------------------------------------------------#
