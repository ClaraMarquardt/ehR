#----------------------------------------------------------------------------#

#' Calculate the clustered confidence interval for a sample mean or proportion. 
#' @description (1) Clustered CI for proportions based on: 
#' www.fao.org/wairdocs/ILRI/x5436E/x5436e07.htm
#' (2) Clustered CI for means based on: diffuseprior.wordpress.com/2012/06/15/\
#' standard-robust-and-clustered-standard-errors-computed-in-r/
#' @export
#' @param TBC
#' @return Confidence interval (and optionally standard deviation) [character vector]
#' @examples
#' mean_ci_clust(as.integer(is.na(dem$date_of_death))*100,dem$empi, 
#' 0.95, type="perc_prop")  # clustered prop CI

mean_ci_clust <- function(var, cluster_var, ci, return_sd=TRUE, digit=2, min_lim=NA, 
  max_lim=NA, type="perc_prop") {

  # determine critical values
  ci_stat_list <- list(
    sign_0.95=1.96, 
    sign_0.9=1.645
  )

  ci_stat <- ci_stat_list[[paste0("sign_", as.character(ci))]]

  if (type=="mean") {
    
    cluster_dt <- data.table(cluster=cluster_var, outcome=var)

    # determine CI
    var_sd_clust   <- sapply(unique(cluster_dt$cluster), function(cluster_id) {
      cluster_sd <- sum(cluster_dt[cluster==cluster_id]$outcome - mean(cluster_dt$outcome))^2
      return(cluster_sd)
    })
    var_sd_clust <- sqrt(sum(var_sd_clust)/(nrow(cluster_dt)-1))

    ci_low   <- round(mean(var, na.rm=T) - ci_stat * var_sd_clust, digits=digit)
    ci_upper <- round(mean(var, na.rm=T) + ci_stat * var_sd_clust, digits=digit)

    if (!is.na(min_lim)) {
      ci_low <- max(min_lim, ci_low)
    }

    if (!is.na(max_lim)) {
      ci_upper <- min(max_lim, ci_upper)
    }

    var_sd <- round(var_sd_clust, digits=digit)


  } else if (type=="perc_prop") {

    # determine CI
    var_mod  <- var/100

    # vars
    cluster_dt <- data.table(cluster=cluster_var, outcome=var_mod)
    cluster_dt[, count:=.N ,by=c("cluster")]
    cluster_dt[, pos_count:=sum(outcome==1),by=c("cluster")]
    cluster_dt <- unique(cluster_dt, by=c("cluster"))[, outcome:=NULL]

    cluster_count    <- length(unique(cluster_var))
    cluster_size     <- cluster_dt$count
    cluster_pos      <- cluster_dt$pos_count
    obs_count        <- length(var)

    adj                   <- cluster_count/obs_count
    sos_cluster_size      <- sum(cluster_size^2)
    sos_cluster_pos       <- sum(cluster_pos^2)
    prod_cluster_size_pos <- sum(cluster_size*cluster_pos)

    p <- mean(var_mod, na.rm=T)

    comb   <- (p^2*sos_cluster_size) + (2*p*prod_cluster_size_pos) + sos_cluster_pos
    var_sd <- adj * sqrt(comb/(cluster_count*(cluster_count-1)))

    ci_low   <- p - ci_stat * var_sd
    ci_upper <- p + ci_stat * var_sd

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
