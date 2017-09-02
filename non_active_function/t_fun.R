#----------------------------------------------------------------------------#

#' @title Perform a two-sided treatment-effect t-test (clustered/non-clustered).  
#' 
#' @description \
#' 
#' @export
#' @param TBC
#' @return p-value [numeric]
#' @examples


t_fun <- function(df, outcome_var, assignment_var, cluster_var=NULL) {

  # obtain control/treatment observations
  x1 <- df[get(assignment_var)==0, get(outcome_var)]
  x2 <- df[get(assignment_var)==1, get(outcome_var)]

  # obtain the t test statistic (assume: H0= mean(x1) - mean(x2)==0)
  t_mean <- mean(x1) - mean(x2)

  # unclustered t test - obtain se
  if (is.null(cluster_var)) {

    x1_var <- sum((x1-mean(x1))^2)/(length(x1)-1) 
    x2_var <- sum((x2-mean(x2))^2)/(length(x2)-1) 
    t_se   <- sqrt(x1_var/length(x1)+x2_var/length(x2))


  # clustered t test - obtain se
  } else {

    # obain the clustered standard errors
    x1_var <- sapply(unique(df[get(assignment_var)==0, get(cluster_var)]), function(cluster_id) {

      temp_sd <- sum(x1[df[get(assignment_var)==0, get(cluster_var)==cluster_id]] - 
                      mean(x1))^2
      return(temp_sd)

    })
    x1_var <- sum(x1_var)/(length(x1)-1) 


    x2_var <- sapply(unique(df[get(assignment_var)==1, get(cluster_var)]), function(cluster_id) {

      temp_sd <- sum(x2[df[get(assignment_var)==1, get(cluster_var)==cluster_id]] - 
                      mean(x2))^2
      return(temp_sd)

    })
    x2_var <- sum(x2_var)/(length(x2)-1) 
    
    t_se   <- sqrt(x1_var/length(x1)+x2_var/length(x2))

  }

  # obtain the p-val
  t <- t_mean/t_se

  # obtain the df
  degree_freedom <- t.test(x1, x2)$parameter

  # obtain the two sided p-value (p(t>t* | t<-t*)
  p_val=2*pt(abs(t), df=degree_freedom, lower=FALSE)

  # return the p-val
  return(p_val)


}

#----------------------------------------------------------------------------#
