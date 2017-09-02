#----------------------------------------------------------------------------#

#' Merge a list of PDFs into a multi-page PDF.
#' 
#' \
#' 
#' @details Maintainer: Clara Marquardt
#' 
#' @export
#' @import plotflow
#' 
#' @param input_pdf_list List of file paths to the individual PDF files which are to be merged (list - character).
#' @param output_file_name Path/filename at which the merged PDF file is to be saved (character).
#' 
#' @return Merged PDF saved to specified path. 
#' 
#' @examples \dontrun{
#' input_pdf_list <- list("[path.pdf]", "[path.pdf]") 
#' merge_pdf(input_pdf_list=input_pdf_list, output_file_name="merge_pdf_test.pdf")
#' }

merge_PDF <- function(input_pdf_list, output_file_name) {
 
 	# check if Ghostscript (gs) is installed
 	gs <- Sys.which("gs")
 	if (nchar(gs)==0) {stop("merge_pdf: Please install Ghostscript (e.g. 'brew install gs') (MacOSX)")}

 	# merge
    plotflow:::mergePDF(in.file=paste(input_pdf_list, collapse=" "),
    	file=output_file_name)

    # status & output location
    print(sprintf("Successfully merged PDFs: %s", output_file_name))

}

#----------------------------------------------------------------------------#
