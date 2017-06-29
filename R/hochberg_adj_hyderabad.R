#----------------------------------------------------------------------------#

#' Perform a Hochberg correction of p-values to account for multiple hypothesis testing [Hochberg (1988) - Step Down (?) (Hyderabad Paper)]. 
#' 
#' @export
#' @param p Vector of raw p-values [numeric].
#' @return
#' @examples

hochberg_adj_hyderabad <- function(p) {

  p_mod        <- as.list(p)
  names(p_mod) <- as.character(1:length(p_mod))
  
  p_name <- names(p_mod)
  p_val  <- unname(c(unlist(p_mod)))

  # rank, in increasing order, the p-values
  p_val_name <- p_name[order(p_val)]
  p_val      <- p_val[order(p_val)]

  # determine the ranks
  k <- rank(-p_val)

  # determine the number of outcomes 
  m <- length(p_val)

  # adjust
  p_val  <- pmin(1, round(p_val *(m-k+1), digit=3))

  # reorder
  p_val_comb <- c(p_val)
  names(p_val_comb) <- c(p_val_name)

  p_val_comb <- p_val_comb[p_name]
  p_val_comb <- unname(p_val_comb)

  return(p_val_comb)

}

#----------------------------------------------------------------------------#
