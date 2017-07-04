#----------------------------------------------------------------------------#

#' summary function. 
#' @export
#' @param  TBC
#' @return TBC
#' @examples

summary_text <- function(..., digit=3) {

	summary_raw <- round(summary(...), digits=digit)

	summary_text <- sprintf(gsub("PRECISION", paste0(".", digit), 
		"Min: %PRECISIONf / Q1: %PRECISIONf / Median: %PRECISIONf / Mean: %PRECISIONf / Q3: %PRECISIONf / Max: %PRECISIONf"), 
		summary_raw[1], summary_raw[2], summary_raw[3], summary_raw[4], summary_raw[5], summary_raw[6])

	return(summary_text)
}

#----------------------------------------------------------------------------#
