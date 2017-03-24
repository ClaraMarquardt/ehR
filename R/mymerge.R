#----------------------------------------------------------------------------#

#' Merge list of data.tables on a given set of variables.
#' @export
#' @param x,y Names of DT [object]
#' @param by Names of variables on which to merge  [character vector]
#' @return Merged data.table
#' @examples
#' TBC

mymerge <- function(x,y, var_list=cohort_key_var) {

  merge(x,y, all = TRUE, by=var_list)

}

#----------------------------------------------------------------------------#
