#----------------------------------------------------------------------------#

#' Perform in-place zero/na/missing replacements on a data.table. 
#'
#' Perform an operation of choice on a data.table (in-place): (i) replace na/inf values (ii) replace zero values (iii) replace missing values (e.g. " ").
#'
#' @details Maintained by: Clara Marquardt
#'
#' @export
#' @import data.table
#'
#' @param data Data.table which is to be modified in place (data.table). 
#' @param mode Value which is to be replaced ((i) 'na_inf' (replace: Na, inf, -inf -> 0) (ii) 'zero' (replace: 0 -> NA), (iii) 'missing' (replace: " "* -> NA) (character).
#' @param replace Value used to replace the existing values (character) [Default: Mode-specific (see above)]
#' @param col Vector of column names which are to be modified (vector - character) [Default: All columns in the data]. 
#' 
#' @return data.table modified in place
#'
#' @examples
#' sample_data <- copy(ehR_cohort)
#' sample_data[, ':='(test_col=prediction, test_col_1=feature_categorical_3)]
#' 
#' ## replace: na_inf
#' sample_data[c(1,3,4), ':='(test_col=c(NA, -Inf, NA), test_col_1=NA)]
#' print(sample_data)
#' replace_na_zero_missing(data=sample_data, replace="na_inf", col=c("test_col"))
#' print(sample_data)
#' 
#' ## replace: zero
#' sample_data[c(1,3,4), ':='(test_col=0, test_col_1=0)]
#' print(sample_data)
#' replace_na_zero_missing(data=sample_data, replace="zero", col=c("test_col_1"))
#' print(sample_data)

#' ## mode - missing
#' sample_data[, ':='(test_col=as.character(test_col), test_col_1=as.character(test_col_1))]
#' sample_data[c(1,3,4), ':='(test_col=" ", test_col_1="     ")]
#' print(sample_data)
#' replace_na_zero_missing(data=sample_data, replace="missing")
#' print(sample_data)

replace_na_zero_missing <- function(data, replace, replace_with="DEFAULT", col=names(data)) {

	# [1] na_inf
	if (replace=="na_inf") {

		# define function (na_inf)
		set_na_zero <- function(data, replace, subset_col) {

  			for (j in which(names(data) %in% subset_col))
    			set(data, which(is.na(data[[j]]) | data[[j]] %in% c(-Inf, +Inf) ), j, replace)

		}

		# execute function
		if (replace_with=="DEFAULT") replace_with <- 0
		set_na_zero(data=data, replace=replace_with, subset_col=col)

	# [2] zero
	} else if (replace=="zero") {

		# define function (zero)
		set_zero_na <- function(data, replace, subset_col) {

  			for (j in which(names(data) %in% subset_col))
    			set(data, which(data[[j]] %in% c(0)), j, replace)
		}

		# execute function
		if (replace_with=="DEFAULT") replace_with <- NA
		set_zero_na(data=data, replace=replace_with, subset_col=col)

	# [3] set_missing_na
	} else if (replace=="missing") {
		
		# define function (set_missing_na)
		set_missing_na <- function(data, replace=NA, subset_col=names(data)) {

  			for (j in which(names(data) %in% subset_col))
    			set(data, which(gsub("[ ]*", "", data[[j]])==""), j, replace)
		
		}

		# execute function
		if (replace_with=="DEFAULT") replace_with <- NA
		set_missing_na(data=data, replace=replace_with, subset_col=col)

	}

}

#----------------------------------------------------------------------------#
