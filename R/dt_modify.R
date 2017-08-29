#----------------------------------------------------------------------------#

#' Perform a set of common in-place modifications on a data.table. 
#' @export
#' @import data.table
#' @param dt Data.table which is to be modified in place (data.table). 
#' @param mode Modification which is to be performed ((i) set_na_zero (replace: Na, inf, -inf -> 0) (ii) set_zero_na (replace: 0 -> NA), (iii) set_missing_na (replace: " "* -> NA) (character).
#' @param replace Value used to replace the existing values (character) [Default: Mode-specific (see above)]
#' @param col_name Vector of column names which are to be modified (vector - character) [Default: All columns in the dt]. 
#' @return data.table modified in place
#' @examples
#' sample_dt <- copy(test_dt)
#' sample_dt[, ':='(test_col=prediction, test_col_1=control_cat_3)]
#' 
#' ## mode - set_na_zero
#' sample_dt[c(1,3,4), ':='(test_col=c(NA, -Inf), test_col_1=NA)]
#' print(sample_dt)
#' dt_modify(dt=sample_dt, mode="set_na_zero", col_name=c("test_col"))
#' print(sample_dt)

#' ## mode - set_zero_na
#' sample_dt[c(1,3,4), ':='(test_col=0, test_col_1=0)]
#' print(sample_dt)
#' dt_modify(dt=sample_dt, mode="set_zero_na", col_name=c("test_col_1"))
#' print(sample_dt)
#' 
#' ## mode - set_missing_na
#' sample_dt[, ':='(test_col=as.character(test_col), test_col_1=as.character(test_col_1))]
#' sample_dt[c(1,3,4), ':='(test_col=" ", test_col_1="     ")]
#' print(sample_dt)
#' dt_modify(dt=sample_dt, mode="set_missing_na")
#' print(sample_dt)

dt_modify <- function(dt, mode, replace=NA, col_name=names(dt)) {

	# Point Person: Clara

	# [1] set_na_zero
	if (mode=="set_na_zero") {

		# define function (set_na_zero)
		set_na_zero <- function(dt, replace, subset_col) {

  			for (j in which(names(dt) %in% subset_col))
    			set(dt, which(is.na(dt[[j]]) | dt[[j]] %in% c(-Inf, +Inf) ), j, replace)

		}

		# execute function
		if (is.na(replace)) replace <- 0
		set_na_zero(dt=dt, replace=replace, subset_col=col_name)

	# [2] set_zero_na
	} else if (mode=="set_zero_na") {

		# define function (set_zero_na)
		set_zero_na <- function(dt, replace, subset_col) {

  			for (j in which(names(dt) %in% subset_col))
    			set(dt, which(dt[[j]] %in% c(0)), j, replace)
		}

		# execute function
		if (is.na(replace)) replace <- NA
		set_zero_na(dt=dt, replace=replace, subset_col=col_name)

	# [3] set_missing_na
	} else if (mode=="set_missing_na") {
		
		# define function (set_missing_na)
		set_missing_na <- function(dt, replace=NA, subset_col=names(dt)) {

  			for (j in which(names(dt) %in% subset_col))
    			set(dt, which(gsub("[ ]*", "", dt[[j]])==""), j, replace)
		
		}

		# execute function
		if (is.na(replace)) replace <- NA
		set_missing_na(dt=dt, replace=replace, subset_col=col_name)

	}

}

#----------------------------------------------------------------------------#
