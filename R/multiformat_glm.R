#----------------------------------------------------------------------------#

#' @title: Format a given list of glm() regression objects for easy presentation.
#' 
#' @description: Given a list of logistic regressions, *performed using glm()* format, return and optionally, write the results side-by-side to csv. 
#' 
#' @detail: Maintained by: Shreyas Lakhtakia; Inspired by: https://www.r-bloggers.com/export-r-results-tables-to-excel-please-dont-kick-me-out-of-your-club/
#' 
#' @export
#' @import data.table
#' @import rowr
#' @param glm_list a list() of glm model objects (list of glm objects)
#' @param output_file the file path (with ".csv" extension) to write the results to, if no value provided, no output file will be created (character)
#' @param formula_list optional list of custom formulae to provide to print in results (code pulls regression formula call by default) (list of character)
#' @param title_list optional list of titles for each regression in the list to write atop output file (blank by default) (list of character)
#' @param ndigit level of precision in output, 5 by default (integer)
#' @return matrix containing formatted results from the provided regression list (matrix)
#' @examples
#' dem[, died := 0, ]
#' dem[tolower(vital_status) %like% "deceased", died := 1, ]
#' glm_age             <- glm(died ~ age, data = dem)
#' glm_age_gender      <- glm(died ~ age + gender, data = dem)
#' glm_age_gender_race <- glm(died ~ age + gender + race, data = dem)
#' glm_list            <- list(glm_age, glm_age_gender, glm_age_gender_race)
#' multiformat_glm(glm_list = glm_list)

multiformat_glm <- function(glm_list, output_file = NA, formula_list = NA, title_list = "", ndigit = 5) {

	# basic function to limit digits
	formatter <- function(x) format(round(x,ndigit), nsmall=ndigit)

	# default parameters for regression results
	nstats     <- 7	# num rows for header before the coefficients
	ncol_width <- 4 # number of columns per regression

	# bind each regression individually in a matrix
	composite_matrix <- matrix()
	regression_index <- 1

	# if no titles provided, use empty string instead
	if((length(title_list) == 1 & is.na(title_list))| length(title_list) < length(glm_list)){
		title_list <- rep(list(""), length(glm_list))
	}

	# if no formulae provided, extract from models
	if((length(formula_list) == 1 & is.na(formula_list))| length(formula_list) < length(glm_list)){
		formula_list <- lapply(glm_list, function(model) toString(model$call))
	}	

	for(regression_model in glm_list){
		# If summary has not been run on the regression_model then run summary
		if (length(grep("summary", class(regression_model)))==0) {
			summary_glm <- regression_model <- summary(regression_model)
		}

		# Store significant results from regression_model
		co          <- summary_glm$coefficients
		deg_freedom <- regression_model$df
		nvar        <- nrow(co)
		ncoll       <- ncol(co)	

	    # This sets the number of rows before we start recording the coefficients
	    nstats       <- 7	

	    # G matrix stores data for output
	    G            <- matrix("", nrow = nvar + nstats, ncol = ncoll + 1)
		G[4,1]       <- "N"
		G[5,1]       <- deg_freedom[1] + deg_freedom[2] # num of obs = num of covariates + num of residual degrees of freedom
		# Save rownames and colnames
		G[(nstats+1) :(nvar+nstats),1] <- rownames(co)
		G[nstats, 2  :(ncoll+1)] <- colnames(co)
		# Save Coefficients
		G[(nstats+1) :(nvar+nstats), 2:(ncoll+1)] <- formatter(co)
		# Save specificity
		G[4,2]       <- "Family"
		G[5,2]       <- regression_model$family$family#formatter(spec)
		# Save sensitivity
		G[4,3]       <- "Type"
		G[5,3]       <- regression_model$family$link #formatter(sens)
		# Save accuracy
		G[4,4]       <- "AIC" #"Accuracy"
		G[5,4]       <- regression_model$aic #formatter(acc)
		# # Save auc
		# G[1,5]       <- "AUC"
		# G[2,5]       <- percent(auc)

		# Save title and regression_model call
		G[1,1]       <- title_list[[regression_index]]
		G[2,1]       <- formula_list[[regression_index]]

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