#----------------------------------------------------------------------------#

#' Generate a varcov matrix for a system of truly unrelated equations - to be used as part of a linearHypothesis test. 
#' @export
#' @import data.table
#' @param  TBC
#' @return TBC
#' @examples
#' TBC

systemfit_var <- function(group_var, df, clust="ind", 
  cluster_var=NULL, systemfit_obj=NULL) {

  coef_count <- length(group_var[[1]])-1

  temp_vcov  <- matrix(rep(0, 
      (length(group_var)*coef_count)^2),
      ncol= (length(group_var)*coef_count))

  for (i in seq(from=0, to=length(group_var)-1)) {

    temp      <- lm(group_var[[i+1]], data=df)
    temp_coef <- setdiff(names(coef(temp)), "(Intercept)")

    if (clust=="ind") {
      temp_cov <- vcov(temp)
    } else if (clust=="clust") {
      temp_cov <- cluster.vcov(temp, df[, get(cluster_var)])
    } else if (clust=="robust") {
      temp_cov <- hccm(temp, "hc0")
    } else if (clust=="manual") {

      res <- systemfit_obj$eq[[i+1]]$residuals
      n   <- length(res)
      X   <- cbind(c(rep(1, nrow(systemfit_obj$eq[[i+1]]$model))), 
        systemfit_obj$eq[[i+1]]$model[, c(temp_coef)])
      k   <- ncol(X)

      temp_cov <- 1/(n-k) * as.numeric(t(res)%*%res) * solve(t(X)%*%X)

    }

    temp_vcov[seq(from=length(coef(temp))*i+1, 
        to=length(coef(temp))*i+(length(coef(temp)))), 
        seq(from=length(coef(temp))*i+1, 
        to=length(coef(temp))*i+
        (length(coef(temp))))]       <- temp_cov
  
  }

  return(temp_vcov)

}


#----------------------------------------------------------------------------#
