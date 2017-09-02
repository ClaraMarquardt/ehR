#----------------------------------------------------------------------------#

#' Count number of observations in a data.table falling into different bins of an identifier variable. 
#' @export
#' @import data.table
#' @param  TBC
#' @return TBC
#' @examples
#' TBC


cum_dt <- function(dt,id, var_, cutoff, external_count_dt=NA, split_var_list=NA, verbose=TRUE, 
  ci_sd=FALSE, ci_type="perc_prop", ci_clust=FALSE, ci_clust_var_name=NA, ci_level=0.95) {

  # prepare
  dt_copy <- copy(dt)
  dt_copy[, all_obs:=1]

  external_count_dt_copy <- copy(external_count_dt)
  external_count_dt_copy[, all_obs:=1]

  if (class(external_count_dt_copy)[1]=="data.table") {
    external_count_dt_copy[, all_obs:=1]
  } 

  if (is.na(split_var_list[1])) {
    split_var_list <- "all_obs"
  } else {
    split_var_list <- c(unlist(split_var_list), "all_obs")
  }

  # loop over split variables
  dt_count_list <- lapply(split_var_list, function(split_var) {

    dt_count_split_list <- lapply(unique(dt_copy[, get(split_var)]),function(split_var_value) {

      # dt_subset
      dt_subset <- dt_copy[get(split_var)==split_var_value]

      # generate cumulative obs count from a given dt
      count <- lapply(cutoff, function(x) nrow(unique(dt_subset[get(var_)<=x], by=c(id))))
        
      if (class(external_count_dt_copy)[1]==1) {
         overall_count <- nrow(unique(dt_subset, by=c(id)))
      } else {
         overall_count <- nrow(unique(external_count_dt_copy[get(split_var)==split_var_value], by=c(id)))
      }

      if (verbose==TRUE) {
        print(split_var)
        print(split_var_value)
        print(overall_count)
      }

      count_dt <- data.table(limit=cutoff, id_count=as.numeric(count), overall_count=overall_count)
      count_dt[, id_perc:=perc(id_count,overall_count)]

      if (ci_sd==TRUE) {
        
        ci_sd_list <- lapply(cutoff, function(x) {

          var_temp     <- dt_subset[, .(var_indic=ifelse((get(var_)>x | 
                            is.na(get(var_))), 0,100))]$var_indic
          cluster_temp <- dt_subset[, get(ci_clust_var_name)]


        if (ci_type=="perc_prop" & ci_clust==FALSE) {
    
          # var_temp <- c(rep(1, length=x), rep(0, length=(overall_count-x))) * 100

          var_ci_sd <- mean_ci(var=var_temp, ci=ci_level, 
            return_sd=TRUE, digit=2, type="perc_prop") 

          return(var_ci_sd)

        } else if (ci_type=="perc_prop" & ci_clust==TRUE) {

          # var_temp <- c(rep(1, length=x), rep(0, length=(overall_count-x))) * 100

          var_ci_sd <- mean_ci(var=var_temp, ci=ci_level, 
            return_sd=TRUE, digit=2, type="perc_prop") 

          var_ci_sd_clust <- mean_ci_clust(var=var_temp, 
            cluster_var=cluster_temp, 
            ci=ci_level, return_sd=TRUE, digit=2, type="perc_prop") 
        
          return(c(var_ci_sd, var_ci_sd_clust))

        } else  {

          stop("method not implemented")

        }

      })

        if (ci_clust==FALSE) {
          count_dt[, sd:=sapply(ci_sd_list, "[[", 1)]
          count_dt[, ci:=sapply(ci_sd_list, "[[", 2)]
          setnames(count_dt, c("sd", "ci"), c(paste0(id, "_unique_perc_sd"),
            paste0(id, "_unique_perc_", ci_level, "_ci")))

        } else if (ci_clust==TRUE) {
          count_dt[, sd:=sapply(ci_sd_list, "[[", 1)]
          count_dt[, ci:=sapply(ci_sd_list, "[[", 2)]
          count_dt[, sd_clust:=sapply(ci_sd_list, "[[", 3)]
          count_dt[, ci_clust:=sapply(ci_sd_list, "[[", 4)]
          setnames(count_dt, c("sd", "ci", "sd_clust", "ci_clust"), 
            c(paste0(id, "_unique_perc_sd"),
            paste0(id, "_unique_perc_", ci_level, "_ci"),
            paste0(id, "_unique_perc_sd_clust"),
            paste0(id, "_unique_perc_", ci_level, "_ci_clust")))
        }


        count_dt[get(paste0(id, "_unique_perc_sd"))==0, 
          c(paste0(id, "_unique_perc_", ci_level, "_ci"), 
            paste0(id, "_unique_perc_", ci_level, "_ci_clust")):=NA]
        count_dt[get(paste0(id, "_unique_perc_sd"))==0, 
          c(paste0(id, "_unique_perc_", "sd_clust")):=0]
        count_dt[get(paste0(id, "_unique_perc_sd"))=="NaN", 
          c(paste0(id, "_unique_perc_", ci_level, "_ci"), 
            paste0(id, "_unique_perc_", ci_level, "_ci_clust"), 
            paste0(id, "_unique_perc_", "sd"), 
            paste0(id, "_unique_perc_","sd_clust")):=NA]

      }

  
      count_dt[, subset:=split_var]
      count_dt[, c(split_var):=split_var_value]

      count_dt[, diff:=id_count-shift(id_count, type="lag")]
      count_dt[1, ':='(diff=id_count)]

      return(count_dt)
    
    })

    dt_count <- rbindlist(dt_count_split_list, use.names=T, fill=T)
    return(dt_count)

   })

  count_dt <- rbindlist(dt_count_list, use.names=T, fill=T)

  setnames(count_dt, 
    c("limit", "id_count", "id_perc", "overall_count"), 
    c(paste0(var_, "_max"),paste0(id, "_unique_count"), 
    paste0(id, "_unique_perc"), paste0(id, "_overall_count")))
  
  if (verbose==TRUE) print(head(count_dt))

  return(count_dt)

}

#----------------------------------------------------------------------------#
