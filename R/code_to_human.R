#----------------------------------------------------------------------------#

#' A function that turns code based variable names to human friendly names,
#' useful in particular for converting variable names with underscores to 
#' more easily interpretable words as axis labels in plots and in spreadsheets.
#' 
#' Author: Shreyas Lakhtakia, slakhtakia [at] bwh.harvard.edu 
#'
#' @export
#' @param code_string The string variable you would like to transform.
#' @param capitalization_length The length at or below which, substrings should be turned into all CAPS
#' @examples
#' code_to_human("age_years") \#"Age Years"
#' code_to_human("icu_patient_count", 3) \# "ICU Patient Count"

# Turn 
# 1. replace _ by " "
# 2. capitalize first letter

code_to_human <- function(code_string, capitalization_length = 1){
	# separate
	split_human_string               <- strsplit(code_string, "_")[[1]] 

	#capitalize first letters of all
	substr(split_human_string, 1, 1) <- toupper(substr(split_human_string, 1, 1)) 

	# capitalize all letters of segments shorter than "capitalization length" 
	split_human_string[nchar(split_human_string) <= capitalization_length] <- toupper(split_human_string[nchar(split_human_string) <= capitalization_length])

	# collapse
	human_string                     <- paste(split_human_string, collapse = " ") 
	return(human_string)
}

#----------------------------------------------------------------------------#