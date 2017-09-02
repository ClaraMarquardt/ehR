#----------------------------------------------------------------------------#

#' Calculate the confidence interval for a sample median. 
#' @export
#' @param TBC
#' @return Confidence interval [character vector]
#' @examples
#' TBC

median_ci <- function(var, ci=0.95, return_sd=TRUE, digit=2) {
  
  ci_comb <- 0000

  # calculate ci
  n <-nrow(as.matrix(var))
  if(qbinom((1-ci)/2,n,0.5)==0) {
    ci_comb <- NA
  }
  L <- qbinom((1-ci)/2,n,0.5)
  U <-n-L+1
 
  if(L>=U) {
    ci_comb <- NA
  }
  
  if (!is.na(ci_comb)) {
    order.var <-sort(var)
    res <-list()
    res$head <-paste(paste(as.character(ci*100),"%",sep=""), c("CI for population median"))
    res$ci   <-c(median=median(var),lower=order.var[L],upper=order.var[n-L+1])
    res$ends <-c("Estimate",paste(as.character(c((1-ci)/2,1-((1-ci)/2))*100),"%",sep=""))
    res$coverage  <-1-(2*pbinom(q=L-1,n,0.5))

    # combine
    ci_comb <- paste0(round(res$ci[["lower"]], digits=digit),  " - ", round(res$ci[["upper"]], digits=digit))

  }
  
  # return
  if (return_sd==TRUE) {
    return(c(NA, ci_comb))
  } else {
    return(ci_comb)
  }

}

#----------------------------------------------------------------------------#
