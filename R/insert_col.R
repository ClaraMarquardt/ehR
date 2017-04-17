#----------------------------------------------------------------------------#

#' Insert a column into a data.table at a specified position. 
#' @export
#' @import data.table
#' @param  TBC
#' @return TBC
#' @examples
#' TBC

insert_col <- function(dt,col_vector,pos,name) {

  dt[, c(name):=col_vector]
  setnames(dt, paste0(names(dt), "_", seq(1:ncol(dt))))
  setcolorder(dt, c(names(dt)[1:(pos-1)],
    grep(name,names(dt),value=T),names(dt)[pos:(ncol(dt)-1)]))

}

#----------------------------------------------------------------------------#
