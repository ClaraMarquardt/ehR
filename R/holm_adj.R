#----------------------------------------------------------------------------#

#' Perform a Holm correction of p-values to account for multiple hypothesis testing [Holm (1988) - Step Down]. 
#' 
#' @export
#' @param p Vector of raw p-values [numeric].
#' @return
#' @examples

holm_adj <- function(p) {

    ## Note: code taken from the p.adjust function ['stats' package] 
    
    method <- "holm"
    n      <- length(p)
    p      <- as.numeric(p)

    i   <- 1L:n
    o   <- order(p)
    ro  <- order(o)
    p_mod <- pmin(1, cummax((n - i + 1L) * p[o]))[ro]

    return(p_mod)
}

#----------------------------------------------------------------------------#



