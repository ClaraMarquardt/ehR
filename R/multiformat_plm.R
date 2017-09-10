#----------------------------------------------------------------------------#

#' @title: Format a given list of plm() regression objects for easy presentation.
#' 
#' @description:Given a list of OLS regressions on panel data, *performed using plm()* (in order to obtain clustered SEs), format, return and optionally, write the results side-by-side in a csv. 
#' 
#' @detail: Maintained by: Shreyas Lakhtakia; Inspired by: https://www.r-bloggers.com/export-r-results-tables-to-excel-please-dont-kick-me-out-of-your-club/
#' 
#' @export
#' @import plm
#' @import data.table
#' @import rowr
#' @param plm_list a list() of plm model objects (list of plm objects)
#' @param output_file the file path (with ".csv" extension) to write the results to, if no value provided, no output file will be created (character)
#' @param formula_list optional list of custom formulae to provide to print in results (code pulls regression formula call by default) (list of character)
#' @param title_list optional list of titles for each regression in the list to write atop output file (blank by default) (list of character)
#' @param ndigit level of precision in output, 5 by default (integer)
#' @return matrix containing formatted results from the provided regression list (matrix)
#' @examples \dontrun{
#' plm_income_age_gender <- plm (monthly_income ~ age , index = "gender", data=dem, model="within") 
#' plm_income_dob_gender <- plm (monthly_income ~ date_of_birth, index = "gender", data=dem, model="within") 
#' plm_list <- list(plm_income_age_gender, plm_income_dob_gender)
#' multiformat_plm(plm_list = plm_list, output_file = "income_plm_variation.csv")
#' }

multiformat_plm <- function(plm_list, output_file = NA, formula_list = NA, title_list = "", ndigit = 5) {

	# dependencies
	formatter <- function(x) format(round(x,ndigit), nsmall=ndigit) # basic function to limit digits

	# default parameters for regression results
	nstats     <- 9	# num rows for header before the coefficients
	ncol_width <- 4 # number of columns per regression
	
	# bind each regression individually in a matrix
	composite_matrix <- matrix()
	regression_index <- 1

	# if no titles provided, use empty string instead
	if((length(title_list) == 1 & is.na(title_list))| length(title_list) < length(plm_list)){
		title_list <- rep(list(""), length(plm_list))
	}

	# if no formulae provided, extract from models
	if((length(formula_list) == 1 & is.na(formula_list))| length(formula_list) < length(plm_list)){
		formula_list <- lapply(plm_list, function(model) toString(model$call))
	}	

	# run all regressions in a loop
	for (regression_model in plm_list) {
		# If summary has not been run on the regression_model then run summary
		if (length(grep("summary", class(regression_model)))==0) {
			regression_model <- summary(regression_model, vcov = vcovHC(regression_model, type='HC1', sandwich=TRUE))
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

		# Save sample size: n obs, num "entities" (ID / length of "unique(cluster_by)"), "size of panel" timeframe
		G[4,1]       <- "N (Number of observations)"
		G[5,1]       <- deg_freedom[1] + deg_freedom[2] # num of obs = num of covariates + num of residual degrees of freedom
		G[4,2]       <- "n (number of unique IDs)"
		G[5,2]       <- length(levels(index(regression_model)[[1]]))
		G[4,3]       <- "Timeframe (max obs per ID)"
		G[5,3]       <- length(levels(index(regression_model)[[2]]))
			
		# Save rownames and colnames
		G[(nstats+1):(nvar+nstats),1] <- rownames(co)
		G[nstats, 2 :(ncol_width+1)]  <- colnames(co)
		
		# Save Coefficients
		G[(nstats+1):(nvar+nstats), 2:(ncol_width+1)] <- formatter(co)
		
		# Save F-stat
		G[7,1]       <- paste0("F(",f[[3]][1],",",f[[3]][2],")")
		G[8,1]       <- formatter(f[[2]])
		
		# Save F-p value
		G[7,2]       <- "Prob > P"
		G[8,2]       <- formatter(regression_model[["fstatistic"]]$p.value)
		
		# Save R2
		G[7,3]       <- "R-Squared"
		G[8,3]       <- formatter(regression_model$r.squared[[1]])
		
		# Save Adj-R2
		G[7,4]       <- "Adj-Rsq"
		G[8,4]       <- formatter(regression_model$r.squared[[2]])
		
		# update regression index
		regression_index <- regression_index + 1
		
		# bind into summary matrix along with a buffer column
		blank_space <- matrix("", nrow(G), 1)
		composite_matrix <- cbind.fill(composite_matrix, blank_space, fill = "")
		composite_matrix <- cbind.fill(composite_matrix, G, fill = "")
	}

	# drop first two columns which are blank (as a result of formatting in a loop with no exceptions for the first regression)
	composite_matrix <- composite_matrix[, -(1:2)]

	# write results if output file specified
	if(!is.na(output_file)) {
		write.table(composite_matrix, file=output_file, row.names=FALSE, col.names = FALSE, sep = ",")
		print(paste0("Output ", output_file, " created successfully."))
	}

	# return composite formatted matrix
	return(composite_matrix)
}

#----------------------------------------------------------------------------#