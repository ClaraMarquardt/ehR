#----------------------------------------------------------------------------#

#' Classify a variable in a dt according to a given classification scheme. 
#' @export
#' @import data.table
#' @param  TBC
#' @return TBC
#' @examples
#' TBC

var_cat <- function(dt, var_name, class_scheme) {

  # class scheme
  class_scheme_var <- var_cat_gen(class_scheme)

  ## apply
  dt[, c(paste0(var_name, "_cat"), paste0(var_name, "_cat_name")):=NULL]

  dt[, paste0(var_name, "_cat"):=cut(get(var_name), breaks=c(class_scheme_var[[1]]), 
    labels=FALSE)]
  
  cat("\n\n\n\nselected classification scheme - note 1st/last cat -> NA/unclassified:\n\n")
  lapply(class_scheme_var[[4]], function(x) cat(paste0("\n", x)))

  dt[, paste0(var_name, "_cat_name"):=class_scheme_var[[4]][-c(1,
    length(class_scheme_var[[4]]))][get(paste0(var_name, "_cat"))]]

  ## stats
  cat(sprintf("\n\nnumber of invalid obs, i.e. not classifiable: %d\n\n", 
    nrow(dt[is.na(get(paste0(var_name, "_cat_name")))& !is.na(get(var_name))])))

  print(table_df(prop.table(table_mod(dt[, .(get( paste0(var_name, "_cat_name")))]))*100, "vert"))

}

#----------------------------------------------------------------------------#
