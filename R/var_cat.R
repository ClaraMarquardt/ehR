#----------------------------------------------------------------------------#

#' Classify a variable in a dt according to a given classification scheme. 
#' @export
#' @import data.table
#' @param  TBC
#' @return TBC
#' @examples
#' TBC

var_cat <- function(dt, var_name, class_scheme, round=FALSE) {

  # class scheme
  class_scheme_var <- var_cat_gen(class_scheme)

  ## temp
  dt[, c("var_temp"):=get(var_name)]

  ## round
  if (round==TRUE) {

    dt[get("var_temp")<class_scheme_var[[1]][1], c("var_temp"):=class_scheme_var[[1]][1]]
    dt[get("var_temp")>class_scheme_var[[1]][length(class_scheme_var[[1]])], 
      c("var_temp"):=class_scheme_var[[1]][length(class_scheme_var[[1]])]]
  }

  ## apply
  dt[, c(paste0("var_temp", "_cat"), paste0("var_temp", "_cat_name")):=NULL]

  dt[, paste0("var_temp", "_cat"):=cut(get("var_temp"), breaks=c(class_scheme_var[[1]]), 
    labels=FALSE, include.lowest=TRUE)]
  
  # cat("\n\n\n\nselected classification scheme - note 1st/last cat -> NA/unclassified:\n\n")
  # lapply(class_scheme_var[[4]], function(x) cat(paste0("\n", x)))

  dt[, paste0("var_temp", "_cat_name"):=class_scheme_var[[4]][-c(1,
    length(class_scheme_var[[4]]))][get(paste0("var_temp", "_cat"))]]

  ## ensure missingness is preserved
  dt[is.na(get("var_temp")), c(paste0("var_temp", "_cat")):=NA]
  dt[is.na(get("var_temp")), c(paste0("var_temp", "_cat_name")):=NA]

  ## stats
  cat(sprintf("\n\nnumber of invalid obs, i.e. not classifiable: %d\n\n", 
    nrow(dt[is.na(get(paste0("var_temp", "_cat_name")))& !is.na(get("var_temp"))])))

  print(table_df(prop.table(table_mod(dt[, .(get( paste0("var_temp", "_cat_name")))]))*100, "vert"))

  ## names
  setnames(dt, c(paste0("var_temp", "_cat"), paste0("var_temp", "_cat_name")), 
    c(paste0(var_name, "_cat"), paste0(var_name, "_cat_name")))

  ## drop temp
  dt[, c("var_temp"):=NULL]

}

#----------------------------------------------------------------------------#
