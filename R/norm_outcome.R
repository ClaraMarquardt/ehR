#----------------------------------------------------------------------------#

#' Generate a Kling Index from a list of outcomes.
#' @export
#' @param  TBC
#' @return TBC
#' @examples
#' TBC

norm_outcome <- function(df, outcome_list, include_missing=TRUE, 
  assignment_var="treatment") {

     df_temp      <- copy(df)
     df_temp_orig <- copy(df)  

     # using only complete observations


     if (include_missing==FALSE) {
      
      # drop observations with missing outcome
      df_temp <- df_temp[!is.na(get(assignment_var))]
      ps("number of observations with a missing assignment_var (dropped): %d", 
        nrow(df_temp_orig[is.na(get(assignment_var))]))

      # drop incomplete observations
      df_temp <- df_temp[complete.cases(df_temp[, mget(c(gsub("-", "", outcome_list)))])]
      ps("number of observations: %d, number of complete observations: %d", 
        nrow(df), nrow(df_temp))

     } else {

      # drop observations with missing outcome
      df_temp <- df_temp[!is.na(get(assignment_var))]
      ps("number of observations with a missing assignment_var (dropped): %d", 
        nrow(df_temp_orig[is.na(get(assignment_var))]))

      # keep observations with at least one non-missing index component
      df_temp[, non_missing_count:=rowSums(sapply(.SD, function(x) !is.na(x))), 
        .SDcols=gsub("-", "", outcome_list)]
      df_temp <- df_temp[non_missing_count>0]

      ps("number of observations: %d, number of observations with at least one non-missing index component: %d", 
        nrow(df), nrow(df_temp))

      # impute
      for (i in gsub("-", "", outcome_list)) {
       
       df_temp[is.na(get(i)) & get(assignment_var)==1, c(i):=mean(df_temp[!(is.na(get(i))) &
        get(assignment_var)==1, get(i)])]
       df_temp[is.na(get(i)) & get(assignment_var)==0, c(i):=mean(df_temp[!(is.na(get(i))) &
        get(assignment_var)==0, get(i)])]

      }

     }

     temp <- sapply(outcome_list, function(x) {

      # assume that higher values - indicate an improvement 
      if(x %like% "-") {
        df_temp[, c(paste0(gsub("-", "", x), "_stand")):=(-1)*get(gsub("-", "", x))]
        x <- paste0(gsub("-", "", x), "_stand")
      }

      # normalise outcomes by the control group mean/SD
      control_mean    <- mean(df_temp[get(assignment_var)==0, get(x)])
      control_sd      <- sd(df_temp[get(assignment_var)==0, get(x)])

      treatment_mean  <- mean(df_temp[get(assignment_var)==1, get(x)])
      treatment_sd    <- sd(df_temp[get(assignment_var)==1, get(x)])

      outcome_norm    <- (df_temp[, get(x)] - control_mean)/control_sd

      return(outcome_norm)

     })

     # generate index by summing up the normalised outcomes 
     temp       <- data.table(temp)
     names_orig <- copy(names(temp))
     temp[, index:=rowMeans(.SD), by=1:nrow(temp)]

     temp_index <- temp$index

     if(include_missing==FALSE) {

      df_temp_orig[complete.cases(df_temp_orig[, mget(c(gsub("-", "", 
        outcome_list)))]) & !is.na(get(assignment_var)),temp_index:=temp_index]

    } else {
      
      df_temp_orig[, non_missing_count:=rowSums(sapply(.SD, function(x) !is.na(x))), 
        .SDcols=gsub("-", "", outcome_list)]
      df_temp_orig[non_missing_count>0 & !is.na(get(assignment_var)),temp_index:=temp_index]
     
    }

     return(df_temp_orig$temp_index)

}

#----------------------------------------------------------------------------#
