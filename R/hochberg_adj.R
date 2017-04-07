#----------------------------------------------------------------------------#

#' Perform a Hochberg correction of p-values to account for multiple hypothesis testing. 
#' 
#' @export
#' @param p_val Vector of raw p-values [numeric].
#' @return
#' @examples


hochberg_adj <- function(p_val) {

  adj_p_val <- round(p_val*(length(p_val)+1-rank(-p_val)), digits=3)

  return(adj_p_val)
}

#----------------------------------------------------------------------------#
