#----------------------------------------------------------------------------#

#' Calculate the confidence interval for a sample mean or proportion. 
#' @export
#' @param TBC
#' @return Confidence interval (and optionally standard deviation) [character vector]
#' @examples
#' TBC

mean_ci <- function(var, ci, return_sd=TRUE, digit=2, min_lim=NA, max_lim=NA, type="mean") {

  # determine critical values
  ci_stat_list <- list(
    sign_0.95=1.96, 
    sign_0.9=1.645
  )

  ci_stat <- ci_stat_list[[paste0("sign_", as.character(ci))]]

  if (type=="mean") {
    
    # determine CI
    var_sd   <- sd(var, na.rm=T)
    ci_low   <- round(mean(var, na.rm=T) - ci_stat * var_sd, digits=digit)
    ci_upper <- round(mean(var, na.rm=T) + ci_stat * var_sd, digits=digit)

    if (!is.na(min_lim)) {
      ci_low <- max(min_lim, ci_low)
    }

    if (!is.na(max_lim)) {
      ci_upper <- min(max_lim, ci_upper)
    }

    var_sd <- round(var_sd, digits=digit)

  } else if (type=="perc_prop") {

    # determine CI
    var_mod  <- var/100
    var_sd   <- sqrt(((mean(var_mod, na.rm=T)*(1-mean(var_mod, 
                    na.rm=T)))/length(var_mod[!is.na(var_mod)])))
    ci_low   <- mean(var_mod, na.rm=T) - ci_stat * var_sd
    ci_upper <- mean(var_mod, na.rm=T) + ci_stat * var_sd

    ci_low   <- max(0, ci_low)
    ci_upper <- min(1, ci_upper)

    ci_low   <- round(ci_low*100, digits=digit)
    ci_upper <- round(ci_upper*100, digits=digit)
    var_sd   <- round(var_sd*100, digits=digit)

  }

  # return
  ci_comb <- paste0(ci_low, " - ", ci_upper)

  if (return_sd==TRUE) {
    return(c(var_sd, ci_comb))
  } else {
    return(ci_comb)
  }

}

#----------------------------------------------------------------------------#
