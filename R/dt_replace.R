#----------------------------------------------------------------------------#

#' Perform a set of common in-place modifications on a data.table. 
#'
#' \
#'
#' @details Maintained by: Clara Marquardata
#'
#' @export
#' @import data.table
#'
#' @param data Data.table which is to be modified in place (data.table). 
#' @param mode Modification which is to be performed ((i) set_na_zero (replace: Na, inf, -inf -> 0) (ii) set_zero_na (replace: 0 -> NA), (iii) set_missing_na (replace: " "* -> NA) (character).
#' @param replace Value used to replace the existing values (character) [Default: Mode-specific (see above)]
#' @param col Vector of column names which are to be modified (vector - character) [Default: All columns in the data]. 
#' 
#' @return data.table modified in place
#'
#' @examples
#' sample_data <- copy(test_data)
#' sample_data[, ':='(test_col=prediction, test_col_1=control_cat_3)]
#' 
#' ## mode - set_na_zero
#' sample_data[c(1,3,4), ':='(test_col=c(NA, -Inf), test_col_1=NA)]
#' print(sample_data)
#' dt_replace(data=sample_data, mode="set_na_zero", col=c("test_col"))
#' print(sample_data)

#' ## mode - set_zero_na
#' sample_data[c(1,3,4), ':='(test_col=0, test_col_1=0)]
#' print(sample_data)
#' dt_replace(data=sample_data, mode="set_zero_na", col=c("test_col_1"))
#' print(sample_data)
#' 
#' ## mode - set_missing_na
#' sample_data[, ':='(test_col=as.character(test_col), test_col_1=as.character(test_col_1))]
#' sample_data[c(1,3,4), ':='(test_col=" ", test_col_1="     ")]
#' print(sample_data)
#' dt_replace(data=sample_data, mode="set_missing_na")
#' print(sample_data)

dt_replace <- function(data, mode, replace="DEFAULT", col=names(data)) {

	# [1] set_na_zero
	if (mode=="set_na_zero") {

		# define function (set_na_zero)
		set_na_zero <- function(data, replace, subset_col) {

  			for (j in which(names(data) %in% subset_col))
    			set(data, which(is.na(data[[j]]) | data[[j]] %in% c(-Inf, +Inf) ), j, replace)

		}

		# execute function
		if (replace=="DEFAULT") replace <- 0
		set_na_zero(data=data, replace=replace, subset_col=col)

	# [2] set_zero_na
	} else if (mode=="set_zero_na") {

		# define function (set_zero_na)
		set_zero_na <- function(data, replace, subset_col) {

  			for (j in which(names(data) %in% subset_col))
    			set(data, which(data[[j]] %in% c(0)), j, replace)
		}

		# execute function
		if (replace=="DEFAULT") replace <- NA
		set_zero_na(data=data, replace=replace, subset_col=col)

	# [3] set_missing_na
	} else if (mode=="set_missing_na") {
		
		# define function (set_missing_na)
		set_missing_na <- function(data, replace=NA, subset_col=names(data)) {

  			for (j in which(names(data) %in% subset_col))
    			set(data, which(gsub("[ ]*", "", data[[j]])==""), j, replace)
		
		}

		# execute function
		if (replace=="DEFAULT") replace <- NA
		set_missing_na(data=data, replace=replace, subset_col=col)

	}

}

#----------------------------------------------------------------------------#
