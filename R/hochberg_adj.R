#----------------------------------------------------------------------------#

#' Perform a Hochberg correction of p-values to account for multiple hypothesis testing. 
#' 
#' @export
#' @param p_val Vector of raw p-values [numeric].
#' @return
#' @examples


hochberg_adj <- function(p) {

    ## Note: code taken from the p.adjust function ['stats' package]
    
    method <- "hochberg"
    n      <- length(p)

    nm <- names(p)
    p <- as.numeric(p)
    p0 <- setNames(p, nm)
    if (all(nna <- !is.na(p)))
        nna <- TRUE
    p <- p[nna]
    lp <- length(p)
    p0[nna] <- switch(method, hochberg = {
        i <- lp:1L
        o <- order(p, decreasing = TRUE)
        ro <- order(o)
        pmin(1, cummin((n - i + 1L) * p[o]))[ro]
    })

    return(p0)
}

#----------------------------------------------------------------------------#


