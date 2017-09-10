#----------------------------------------------------------------------------#

#' Merge a list of PDFs into a multi-page PDF.
#' 
#' Given a list of paths to locally stored PDF files - merge the PDFs (in the given order) and save the newly created PDF to a specified location.
#' 
#' @details Maintained by: Clara Marquardt
#' 
#' @export
#' @import plotflow
#' 
#' @param pdf_list List of file paths to the individual PDF files which are to be merged (list - character).
#' @param output_file Path/filename at which the merged PDF file is to be saved (character).
#' 
#' @return Merged PDF saved to specified path. 
#' 
#' @examples \dontrun{
#' pdf_list <- list("[path.pdf]", "[path.pdf]") 
#' merge_PDF(pdf_list=pdf_list, output_file="merge_pdf_test.pdf")
#' }

merge_PDF <- function(pdf_list, output_file) {
 
 	# check if Ghostscript (gs) is installed
 	gs <- Sys.which("gs")
 	if (nchar(gs)==0) {stop("merge_pdf: Please install Ghostscript (e.g. 'brew install gs') (MacOSX)")}

 	# merge
    plotflow:::mergePDF(in.file=paste(pdf_list, collapse=" "),
    	file=output_file)

    # status & output location
    print(sprintf("Successfully merged PDFs: %s", output_file))

}

#----------------------------------------------------------------------------#
