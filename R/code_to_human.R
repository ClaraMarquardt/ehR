#----------------------------------------------------------------------------#

#' @title: Format code variable names to proper names.
#' 
#' @description: A function that turns code based variable names to human friendly names, useful in particular for converting variable names with underscores to more easily interpretable words as axis labels in plots and in spreadsheets.
#' 
#' @detail: Maintained by: Shreyas Lakhtakia
#' 
#' @export
#' @param code_string The string variable you would like to transform. (character)
#' @param capitalization_length The length at or below which, substrings should be turned into all CAPS (integer)
#' @examples
#' code_to_human("age_years") \#"Age Years"
#' code_to_human("icu_patient_count", 3) \# "ICU Patient Count"

code_to_human <- function(code_string, capitalization_length = 1){
	# Turn 
	# 1. replace _ by " "
	# 2. capitalize first letter
	
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