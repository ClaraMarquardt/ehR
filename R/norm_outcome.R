#----------------------------------------------------------------------------#

#' Generate a Kling Index from a list of outcomes.
#' @export
#' @param  TBC
#' @return TBC
#' @examples
#' TBC

norm_outcome <- function(df, outcome_list) {

     df_temp <- copy(df)

     # using only complete observations
     df_temp <- df_temp[complete.cases(df_temp[, mget(outcome_list)])]
     ps("number of observations: %d, number of complete observations: %d", 
      nrow(df), nrow(df_temp))

     temp <- sapply(outcome_list, function(x) {

      # assume that higher values - indicate an improvement 
      if(x %like% "-") {
        df_temp[, c(paste0(gsub("-", "", x), "_stand")):=(-1)*get(gsub("-", "", x))]
        x <- paste0(gsub("-", "", x), "_stand")
      }

      # normalise outcomes by the control group mean/SD
      control_mean    <- mean(df_temp[treatment==0, get(x)])
      control_sd      <- sd(df_temp[treatment==0, get(x)])

      treatment_mean  <- mean(df_temp[treatment==1, get(x)])
      treatment_sd    <- sd(df_temp[treatment==1, get(x)])

      outcome_norm    <- (df_temp[, get(x)] - control_mean)/control_sd
 
      return(outcome_norm)

     })

     # generate index by summing up the normalised outcomes 
     temp       <- data.table(temp)
     names_orig <- copy(names(temp))
     temp[, index:=rowMeans(.SD), by=1:nrow(temp)]

     temp_index <- temp$index

     return(temp_index)

}

#----------------------------------------------------------------------------#
