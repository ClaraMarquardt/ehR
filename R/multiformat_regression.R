#----------------------------------------------------------------------------#

#' @title: Format a given list of regression objects for easy presentation.
#' 
#' @description: Given a list of formulae (as strings), type of regression, and data, run listed regressions, return formatted results, & optionally  write to file. OLS, OLS with clustered SE, and logistic regressions currently supported.
#' 
#' @detail: Maintained by: Shreyas Lakhtakia
#' 
#' @export
#' @import plm
#' @import data.table
#' @import rowr
#' @param type character string of regression type, one of: "ols", "logistic" [all equations must be for the same class of regression] (character)
#' @param formula_list a list() of character strings specifying regression equations (list of character)
#' @param data a data.table (or data.frame) containing (among others,) ALL the regression variables specified in all formulae (data.table)
#' @param output_file the file path (with ".csv" extension) to write the results to, if no value provided, no output file will be created (character)
#' @param title_list optional list of titles for each regression in the list to write atop output file (blank by default) (list of character)
#' @param ndigit level of precision in output, 5 by default (integer)
#' @return matrix containing formatted results from the provided regression list (matrix)
#' @examples
#' dem_temp <- copy(dem)
#' dem_temp[, died:=0][!is.na(date_of_death), died:=1]
#' glm_reg_list <- list("died ~ age", "died ~ age + gender", "died ~ age + gender + race")
#' multiformat_regression(type = "logistic", formula_list = glm_reg_list, data = dem_temp)
#' 
#' dem_dia <- merge(x = dem, y = dia, by = "empi", all.y = TRUE)
#' dem_dia[, pt_dia_count := .N, by = "empi"]
#' pt_dem_dia <- unique(dem_dia, by = "empi")
#' lm_reg_list <- list("pt_dia_count ~ age", "pt_dia_count ~ age + gender", "pt_dia_count ~ age + gender + race")
#' multiformat_regression(type = "ols", formula_list = lm_reg_list, data=pt_dem_dia)

multiformat_regression <- function(type, formula_list, data, output_file = NA,title_list = NA, ndigit = 5, cluster_se_by = NA){
	switch(type,
		ols = {
			
			############### ORDINARY LEAST SQ REGRESSION ###############
			if(is.na(cluster_se_by)){
				# NOT PANEL DATA / NO CLUSTERING OF STANDARD ERRORS
				regression_list <- lapply(formula_list, function(equation) return(lm(formula(equation), data))) # run all regression
				multiformat_result <- multiformat_lm(lm_list = regression_list, 
								output_file = output_file, 
								formula_list = formula_list, 
								title_list = title_list, 
								ndigit = ndigit) # format
			} else{
				# STANDARD ERRORS CLUSTERED BY `cluster_se_by`
				regression_list <- lapply(formula_list, function(equation) return(plm(formula(equation), data, model = "pooling", index = cluster_se_by)))
				multiformat_result <- multiformat_plm(plm_list = regression_list, 
								output_file = output_file, 
								formula_list = formula_list, 
								title_list = title_list,
								ndigit = ndigit)
			}
		},
		logistic = {
			################### LOGISTIC REGRESSION ################
			if(is.na(cluster_se_by)){
				# NOT PANEL DATA / NO CLUSTERING OF STANDARD ERRORS
				regression_list <- lapply(formula_list, function(equation) return(glm(formula(equation), data, family = binomial()))) # run all regression
				multiformat_result <- multiformat_glm(glm_list = regression_list, 
								output_file = output_file, 
								formula_list = formula_list, 
								title_list = title_list, 
								ndigit = ndigit) # format
			} else{
				# NOT SUPPORTED AT PRESENT
				stop("Clustering of standard errors in logistic regression models is not currently supported.\nConsider contributing to this code.")
			}
		},
		{
			####### OTHER OPTIONS NOT SUPPORTED CURRENTLY #######
			stop("You seem to have entered an unsupported regression type. Try type = \"ols\" or type = \"logistic\" instead.")
		})
	
	return(multiformat_result)
}
#----------------------------------------------------------------------------#