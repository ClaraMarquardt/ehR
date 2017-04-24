#----------------------------------------------------------------------------#

#' @title Generate descriptive stats for the features in a feature set. 
#'
#' @description 
#'
#' @export
#' @import data.table
#' @param pred_set 
#' @param outcome_var 
#' @return
#' @examples

feature_ext_stat <- function(pred_set, outcome_var) {

    stat_dt <- data.table(var_name=names(pred_set))

    # type
    stat_dt[, class:=sapply(names(pred_set), function(var) class(pred_set[, get(var)]))]

    # numeric integer
    numeric_integer_var <- stat_dt[class %in% c("integer", "numeric")]$var_name

    # mean by outcome
    stat_dt[var_name %in% numeric_integer_var, treatment_mean:=sapply(numeric_integer_var, function(var) 
    	mean(pred_set[get(outcome_var)==1, get(var)], na.rm=T))]
    stat_dt[var_name %in% numeric_integer_var, control_mean:=sapply(numeric_integer_var, function(var) 
    	mean(pred_set[get(outcome_var)==0, get(var)], na.rm=T))]

    # sd by outcome
    stat_dt[var_name %in% numeric_integer_var, treatment_sd:=sapply(numeric_integer_var, function(var) 
    	sd(pred_set[get(outcome_var)==1, get(var)], na.rm=T))]
    stat_dt[var_name %in% numeric_integer_var, control_sd:=sapply(numeric_integer_var, function(var) 
    	sd(pred_set[get(outcome_var)==0, get(var)], na.rm=T))]

    # median by outcome
    stat_dt[var_name %in% numeric_integer_var, treatment_median:=sapply(numeric_integer_var, function(var) 
    	median(pred_set[get(outcome_var)==1, get(var)], na.rm=T))]
    stat_dt[var_name %in% numeric_integer_var, control_median:=sapply(numeric_integer_var, function(var) 
    	median(pred_set[get(outcome_var)==0, get(var)], na.rm=T))]

    # lowest quantile by outcome
    stat_dt[var_name %in% numeric_integer_var, treatment_mean_lowest_quantile:=sapply(numeric_integer_var, 
    	function(var) mean(pred_set[get(outcome_var)==1 & 
        get(var)<=quantile(pred_set[get(outcome_var)==1, get(var)], na.rm=T)[2], get(var)], na.rm=T))]
    stat_dt[var_name %in% numeric_integer_var, control_mean_lowest_quantile:=sapply(numeric_integer_var, 
    	function(var) mean(pred_set[get(outcome_var)==0 & 
        get(var)<=quantile(pred_set[get(outcome_var)==0, get(var)], na.rm=T)[2], get(var)], na.rm=T))]

    # highest quantile by outcome
    stat_dt[var_name %in% numeric_integer_var, treatment_mean_highest_quantile:=sapply(numeric_integer_var, 
    	function(var) mean(pred_set[get(outcome_var)==1 & 
        get(var)>=quantile(pred_set[get(outcome_var)==1, get(var)], na.rm=T)[4], get(var)], na.rm=T))]
    stat_dt[var_name %in% numeric_integer_var, control_mean_highest_quantile:=sapply(numeric_integer_var, 
    	function(var) mean(pred_set[get(outcome_var)==0 & 
        get(var)>=quantile(pred_set[get(outcome_var)==0, get(var)], na.rm=T)[4], get(var)], na.rm=T))]

    # lowest decile by outcome
    stat_dt[var_name %in% numeric_integer_var, treatment_mean_lowest_decile:=sapply(numeric_integer_var, 
    	function(var) mean(pred_set[get(outcome_var)==1 & 
        get(var)<=quantile(pred_set[get(outcome_var)==1, get(var)], prob=seq(0, 1, 0.1), na.rm=T)[2], 
        get(var)], na.rm=T))]
    stat_dt[var_name %in% numeric_integer_var, control_mean_lowest_decile:=sapply(numeric_integer_var, 
    	function(var) mean(pred_set[get(outcome_var)==0 & 
        get(var)<=quantile(pred_set[get(outcome_var)==0, get(var)], prob=seq(0, 1, 0.1), na.rm=T)[2], 
        get(var)], na.rm=T))]

    # highest decile by outcome
    stat_dt[var_name %in% numeric_integer_var, treatment_mean_highest_decile:=sapply(numeric_integer_var, 
    	function(var) mean(pred_set[get(outcome_var)==1 & 
        get(var)>=quantile(pred_set[get(outcome_var)==1, get(var)], prob=seq(0, 1, 0.1), na.rm=T)[10], 
        get(var)], na.rm=T))]
    stat_dt[var_name %in% numeric_integer_var, control_mean_highest_decile:=sapply(numeric_integer_var, 
    	function(var) mean(pred_set[get(outcome_var)==0 & 
        get(var)>=quantile(pred_set[get(outcome_var)==0, get(var)], prob=seq(0, 1, 0.1), na.rm=T)[10], 
        get(var)], na.rm=T))]

    # zero by outcome
    stat_dt[var_name %in% numeric_integer_var, treatment_zero_perc:=sapply(numeric_integer_var, 
    	function(var) perc(sum(pred_set[get(outcome_var)==1, 
        get(var)]==0, na.rm=T), nrow(pred_set[get(outcome_var)==1])))]
    stat_dt[var_name %in% numeric_integer_var, control_zero_perc:=sapply(numeric_integer_var, 
    	function(var) perc(sum(pred_set[get(outcome_var)==0, 
        get(var)]==0, na.rm=T), nrow(pred_set[get(outcome_var)==0])))]

    # missing by outcome
    stat_dt[, treatment_missing_perc:=sapply(names(pred_set), 
    	function(var) perc(sum(is.na(pred_set[get(outcome_var)==1, 
        get(var)])), nrow(pred_set[get(outcome_var)==1])))]
    stat_dt[, control_missing_perc:=sapply(names(pred_set), 
    	function(var) perc(sum(is.na(pred_set[get(outcome_var)==0, 
        get(var)])), nrow(pred_set[get(outcome_var)==0])))]

    stat_dt[var_name %in% numeric_integer_var, c(setdiff(names(stat_dt), c("var_name", "class"))):=lapply(.SD, 
    	function(x) round(x, 2)), .SDcols=setdiff(names(stat_dt), c("var_name", "class"))]

    # return
    return(stat_dt)

}


#----------------------------------------------------------------------------#
