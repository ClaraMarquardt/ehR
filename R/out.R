#----------------------------------------------------------------------------#

#' Save R objects to different formats.
#' 
#' @export
#' @param  TBC
#' @return TBC
#' @examples
#' TBC

out <- function(obj, path) {

  # purpose: 
  # save .csv,.pdf,.Rds files

  if (path %like% "csv") {

    write.csv(obj, path, row.names=F)

  } else if(path %like% "pdf|jpeg|png") {

    ggsave(obj, path)

  }  else if(path %like% "Rds") {

    saveRDS(obj, path)

  } else if (path %like% "txt") {

  	write.table(obj, path)

  }

}

#----------------------------------------------------------------------------#
