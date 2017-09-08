#----------------------------------------------------------------------------#

#' @title: Format a given list of lm() regression objects for easy presentation.
#' 
#' @description: Given a list of OLS regressions, *performed using lm()*  format, return and optionally, write the results side-by-side in a csv. 
#' 
#' @detail: Maintained by: Shreyas Lakhtakia; Inspired by: https://www.r-bloggers.com/export-r-results-tables-to-excel-please-dont-kick-me-out-of-your-club/
#' 
#' @export
#' @import data.table
#' @param lm_list a list() of lm model objects (list of lm objects)
#' @param output_file the file path (with ".csv" extension) to write the results to, if no value provided, no output file will be created (character)
#' @param formula_list optional list of custom formulae to provide to print in results (code pulls regression formula call by default) (list of character)
#' @param title_list optional list of titles for each regression in the list to write atop output file (blank by default) (list of character)
#' @param ndigit level of precision in output, 5 by default (integer)
#' @return matrix containing formatted results from the provided regression list (matrix)
#' @examples \dontrun{
#' 	dem_dia <- merge(x = dem, y = dia, by = "empi", all.y = TRUE)
#'  dem_dia[, pt_dia_count := .N, by = "empi"]
#'  pt_dem_dia <- unique(dem_dia, by = "empi")
#'  lm_dia_count_age <- lm(pt_dia_count ~ age, data = pt_dem_dia)
#'  lm_dia_count_age_gender <- lm(pt_dia_count ~ age + gender, data = pt_dem_dia)
#'  lm_dia_count_age_gender_race <- lm(pt_dia_count ~ age + gender + race, data = pt_dem_dia)
#'  lm_list <- list(lm_dia_count_age, lm_dia_count_age_gender, lm_dia_count_age_gender_race)
#'  multiformat_lm(lm_list = lm_list, output_file = "dia_count_lm_variation.csv")
#' }

multiformat_lm <- function(lm_list, output_file = NA, formula_list = NA, title_list = NA, ndigit = 5) {

	formatter <- function(x) format(round(x,ndigit), nsmall=ndigit) # basic function to limit digits

	# default parameters for regression results
	nstats     <- 7	# num rows for header before the coefficients
	ncol_width <- 4 # number of columns per regression
	
	# bind each regression individually in a matrix
	composite_matrix <- matrix()
	regression_index <- 1

	# if no titles provided, use empty string instead
	if((length(title_list) == 1 & is.na(title_list))| length(title_list) < length(lm_list)){
		title_list <- rep(list(""), length(lm_list))
	}

	# if no formulae provided, extract from models
	if((length(formula_list) == 1 & is.na(formula_list))| length(formula_list) < length(lm_list)){
		formula_list <- lapply(lm_list, function(model) toString(model$call))
	}	

	# run all regressions in a loop
	for (regression_model in lm_list) {
			# If summary has not been run on the regression_model then run summary
			if (length(grep("summary", class(regression_model)))==0) {
				regression_model <- summary(regression_model)
			}

			# Store significant results from regression_model
			co          <- regression_model$coefficients
			f           <- regression_model$fstatistic
			deg_freedom <- regression_model$df
			nvar        <- nrow(co)
			
			# G matrix stores data for output
			G            <- matrix("", nrow = nvar + nstats, ncol = ncol_width + 1)
			
			# Save title and regression_model call
			G[1,1]       <- title_list[[regression_index]]
			G[2,1]       <- formula_list[[regression_index]]
			
			# Save sample size and F-statistic
			G[4,1]       <- "N (Number of observations)"
			G[5,1]       <- deg_freedom[1] + deg_freedom[2]  # num of obs = num of covariates + num of residual degrees of freedom 
			
			# Save rownames and colnames
			G[(nstats+1):(nvar+nstats),1] <- rownames(co)
			G[nstats, 2 :(ncol_width+1)]  <- colnames(co)
			
			# Save Coefficients
			G[(nstats+1):(nvar+nstats), 2:(ncol_width+1)] <- formatter(co)
			
			# Save F-stat
			G[4,2]       <- paste0("F(",f[2],",",f[3],")")
			G[5,2]       <- formatter(f[1])
			
			# Save F-p value
			G[4,3]       <- "Prob > P"
			G[5,3]       <- formatter(1-pf(f[1],f[2],f[3]))
			
			# Save R2
			G[4,4]       <- "R-Squared"
			G[5,4]       <- formatter(regression_model$r.squared)
			
			# Save Adj-R2
			G[4,5]       <- "Adj-Rsq"
			G[5,5]       <- formatter(regression_model$adj.r.squared)
			
			# update regression index
			regression_index <- regression_index + 1
			
			# bind into summary matrix along with a buffer column
			blank_space <- matrix("", nrow(G), 1)
			composite_matrix <- cbind.fill(composite_matrix, blank_space, fill = "")
			composite_matrix <- cbind.fill(composite_matrix, G, fill = "")
	}

	# drop first two columns which are blank (as a result of formatting in a loop with no exceptions for the first regression)
	composite_matrix <- composite_matrix[, -(1:2)]

	# write resuts if output file specified
	if(!is.na(output_file)) {
		write.table(composite_matrix, file=output_file, row.names=FALSE, col.names = FALSE, sep = ",")
		print(paste0("Output ", output_file, " created successfully."))
	}
	
	# return
	return(composite_matrix)
}

#----------------------------------------------------------------------------#