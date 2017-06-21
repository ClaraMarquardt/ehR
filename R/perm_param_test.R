#----------------------------------------------------------------------------#

#' Permutation based F/t-test
#' @export
#' @param TBC
#' @return TBC
#' @examples TBC

perm_param_test <- function(df, assignment_var, group_var, permutation=ExpLearning.Default$permutation, 
                    cluster_var=ExpLearning.Default$cluster_var,
                    max_core=ExpLearning.Default$max_core, test=ExpLearning.Default$test) {

  # generate one master sur fit - if use  

  if (test=="wald_ind_independent") {

    temp_fit_sample <- systemfit(group_var, data=df)

  }

  # obtain the permutation stat (include model stat)
  perm_stat  <- mclapply_robust(X=1:(permutation+1), FUN=function(i) {

    # ensure that the actual treatment assignment is included as one of the permutations
    if(i==1) {

      perm_df <- copy(df)

    } else {

      perm_df <- perm(df, assignment_var, cluster_var=cluster_var, 
                    fold_var=fold_var, within_fold=FALSE, 
                    control_var=NULL, control_var_unbalanced=NULL, 
                    perm_strategy="entire_cluster", perm_test="baseline_signal")

    }

    if (test=="t_ind") {

      x1 <- perm_df[get(assignment_var) == 0, get(group_var)]
      x2 <- perm_df[get(assignment_var) == 1, get(group_var)]

      temp <- t.test(x1, x2)$statistic

     
    } else if (test=="t_clust") {

      temp <-  t.test.cluster(perm_df[, get(group_var)], perm_df[, get(cluster_var)], 
                  perm_df[, get(assignment_var)])[19]


    } else if (test=="wald_ind") {

      hypothesis_matrix            <- matrix(0, nrow=length(group_var), 
                                          ncol=(length(group_var)*2))
        
      for (i in 1:length(group_var)) {
            hypothesis_matrix[i,(i*2)]   <- 1
        }

      qvec                       <- rep(0,length(group_var))

      fit_sur                    <- systemfit(group_var, data=perm_df)
 
      temp                       <- linearHypothesis(fit_sur,hypothesis_matrix,qvec, 
                                        test = "F")$F[2]

    }  else if (test=="wald_clust") {

      hypothesis_matrix            <- matrix(0, nrow=length(group_var), 
                                          ncol=(length(group_var)*2))
        
      for (i in 1:length(group_var)) {
            hypothesis_matrix[i,(i*2)]   <- 1
        }

      qvec        <- rep(0,length(group_var))

      fit_sur     <- systemfit(group_var, data=perm_df)
      

      # robust cov matrix
      fit_sur_clust_robust_cov   <-  systemfit_var(group_var, perm_df, 
                                          clust="clust",
                                          cluster_var=cluster_var) 

      temp       <- linearHypothesis(systemfit(group_var, 
                     data=perm_df),hypothesis_matrix,qvec, test = "F", 
                      vcov=fit_sur_clust_robust_cov)$F[2]

    }  else if (test=="wald_ind_independent") {


     hypothesis_matrix            <- matrix(0, nrow=length(group_var), 
                                          ncol=(length(group_var)*2))
        
      for (i in 1:length(group_var)) {
            hypothesis_matrix[i,(i*2)]   <- 1
        }

      qvec                       <- rep(0,length(group_var))

      fit_sur                    <- systemfit_unrelated(formula=group_var, 
                                      df=perm_df, sample_systemfit=temp_fit_sample)
 
      temp                       <-  linearHypothesis(fit_sur,hypothesis_matrix,qvec, 
                                        test = "F")$F[2]

    }

    return(temp)


  }, max_core=max_core)

  # determine the p-values 
  model_stat <- perm_stat[[1]]
  perm_stat  <- c(unlist(perm_stat[2:length(perm_stat)]))

  if (test %like% "^t") {
    p    <- mean(perm_stat>=abs(model_stat) | perm_stat<=-abs(model_stat))
  } else if (test %like% "^wald") {
    p    <- mean(perm_stat>=model_stat)
  }

  # return the p values, permuted losses and model losses
  return(list(p=p, model_stat=model_stat, perm_stat=perm_stat))

}

#----------------------------------------------------------------------------#
