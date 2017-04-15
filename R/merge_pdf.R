#----------------------------------------------------------------------------#

#' Merge a list of PDFs into a multi-paage PDF.
#' @export
#' @import plotflow
#' @param  TBC 
#' @return TBC
#' @examples
#' TBC

merge_pdf <- function(input_file_list, output_file_name, quiet=FALSE) {
 
    plotflow:::mergePDF(in.file=paste(input_file_list, 
        collapse=" "),file=output_file_name)

    if (quiet==FALSE) ps("succesfully merged pdf: %s", output_file_name)
}

#----------------------------------------------------------------------------#
