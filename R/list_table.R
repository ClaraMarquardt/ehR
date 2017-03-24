#----------------------------------------------------------------------------#

#' Convert list to table which, optionally, is appended to existing table. 
#' @import data.table
#' @export
#' @param list_name List name [character] 
#' @param var_name Name of variable which contains the header of the generated table (if merge==T 
#'    and var_name in names(old)) -> the system time is appended to var_name to ensure 
#'    column name uniqueness) [character] 
#' @param argument List elements which equal this string are converted into empty rows in the final 
#'    table (see \code{\link{list_space}})  [character] 
#' @param merge Whether or not to merge the generated table with an existing [logical]
#' @param old Path to existing csv file with which generated table is to be merged
#'    if merge==T [path]
#' @return Generated table [data.table]
#' @examples
#' sample_list <- list()
#' sample_list$nfold <- 3
#' sample_list$test_prop <- 0.3
#' sample_list_dt <- list_table(sample_list, var_name="sample_run", merge=F )


list_table <- function(list_name, var_name, argument="space", merge=FALSE, 
  old=NA) {
 
  temp <- as.data.table(t(rbindlist(list(list_name), fill=T)))
  temp$var_name <- names(list_name)
  temp[V1=="space", ':='(var_name="space", V1="")]
  
  if (!var_name %in% names(old)) {
    setnames(temp, c(var_name, "var_name"))
  } else {
    setnames(temp, c(paste(var_name, as.character(format(Sys.time(), 
      "%H:%M")), sep="_"), "var_name"))
  }

  if(merge==T) {

    col_name_old <- copy(names(temp))
    temp <- join(temp, old, by=c("var_name"), "full")
    setcolorder(temp, c(names(old), setdiff(col_name_old, "var_name")))
  } else {
    setcolorder(temp, c("var_name", var_name))
  }
  temp <- as.data.table(temp)

  temp[, drop:=0][var_name==shift(var_name, type="lead") & !(var_name=="space" & 
    shift(var_name, type="lead")=="space"), drop:=1]
  temp <- temp[!drop==1]
  temp[, drop:=NULL]
  temp[var_name=="space", var_name:=""]

  return(temp)

}

#----------------------------------------------------------------------------#
