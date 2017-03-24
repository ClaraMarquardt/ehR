#----------------------------------------------------------------------------#

#' Chunkify a data.table into a list of data.tables preserving group-level clusters.
#' @export
#' @import data.table
#' @param  dt Name of data.table to be chunkfied [character]
#' @param  group_var Name of clustering variable [character] 
#' @param  chunk_num Number of chunks to generate [integer]
#' @return TBC
#' @examples
#' dia_list <- chunkify_group(dia, group_var="empi", 10)

chunkify_group <- function(dt, group_var, chunk_num) {

    rows_group <- nrow(unique(dt, by=c(group_var)))
    chunk_num <- as.numeric(chunk_num)
    chunksize <- ceiling(rows_group/chunk_num)

    chunks <- 1:chunk_num

    chunkid <- unlist(lapply(chunks, function(i) rep(chunks[[i]], chunksize)))
    chunkid <- chunkid[1:rows_group]

    dt2 <- dt[dt[, .I[1], by=c(group_var)]$V1, chunk_id := chunkid]
    dt2[, chunk_id:=max(chunk_id, na.rm=T), by=c(group_var)]

    out <- split(dt2, dt2$chunk_id)

    return(out)
}

#----------------------------------------------------------------------------#
