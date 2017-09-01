#----------------------------------------------------------------------------#

#' Sequentially merge a list of data.tables on a given set of variables (inner-join).
#' @export
#' @import data.table
#' @param dt_list List of data.tables which are to be merged (list - data.table).
#' @param key_var Vector of variable names on the basis of which the data.tables are to be merged (vector - character).
#' @param allow.cartesian See ?data.table (logical - TRUE/FALSE) [default: FALSE].
#' @return Merged data.table.
#' @examples
#' dt_list   <- list(ed[1:100], dia[1:100], dem[1:100])
#' dt_merged <- multi_merge(dt_list=dt_list, key_var=c("empi"))
#' print(str(dt_merged))

multi_merge <- function(dt_list, key_var, allow.cartesian=FALSE) {

  # define Reduce-based function
  multi_merge_fun <- function(x, key_var) {Reduce(function(x, y) {x[y, on=c(key_var), 
  	nomatch=0, allow.cartesian=allow.cartesian]}, x, accumulate=FALSE)}

  # apply function
  dt_merged   <- multi_merge_fun(x=dt_list, key_var=key_var)

  # return
  return(dt_merged)

}



#----------------------------------------------------------------------------#
