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

	# mean by outcome
	stat_dt[, treatment_mean:=sapply(names(pred_set), function(var) mean(pred_set[get(outcome_var)==1, get(var)]))]
	stat_dt[, control_mean:=sapply(names(pred_set), function(var) mean(pred_set[get(outcome_var)==0, get(var)]))]

	# sd by outcome
	stat_dt[, treatment_sd:=sapply(names(pred_set), function(var) sd(pred_set[get(outcome_var)==1, get(var)]))]
	stat_dt[, control_sd:=sapply(names(pred_set), function(var) sd(pred_set[get(outcome_var)==0, get(var)]))]

	# median by outcome
	stat_dt[, treatment_median:=sapply(names(pred_set), function(var) median(pred_set[get(outcome_var)==1, get(var)]))]
	stat_dt[, control_median:=sapply(names(pred_set), function(var) median(pred_set[get(outcome_var)==0, get(var)]))]

	# lowest quantile by outcome
	stat_dt[, treatment_mean_lowest_quantile:=sapply(names(pred_set), function(var) mean(pred_set[get(outcome_var)==1 & 
		get(var)<=quantile(pred_set[get(outcome_var)==1, get(var)])[2], get(var)]))]
	stat_dt[, control_mean_lowest_quantile:=sapply(names(pred_set), function(var) mean(pred_set[get(outcome_var)==0 & 
		get(var)<=quantile(pred_set[get(outcome_var)==0, get(var)])[2], get(var)]))]

	# highest quantile by outcome
	stat_dt[, treatment_mean_highest_quantile:=sapply(names(pred_set), function(var) mean(pred_set[get(outcome_var)==1 & 
		get(var)>=quantile(pred_set[get(outcome_var)==1, get(var)])[4], get(var)]))]
	stat_dt[, control_mean_highest_quantile:=sapply(names(pred_set), function(var) mean(pred_set[get(outcome_var)==0 & 
		get(var)>=quantile(pred_set[get(outcome_var)==0, get(var)])[4], get(var)]))]

	# lowest decile by outcome
	stat_dt[, treatment_mean_lowest_decile:=sapply(names(pred_set), function(var) mean(pred_set[get(outcome_var)==1 & 
		get(var)<=quantile(pred_set[get(outcome_var)==1, get(var)], prob=seq(0, 1, 0.1))[2], get(var)]))]
	stat_dt[, control_mean_lowest_decile:=sapply(names(pred_set), function(var) mean(pred_set[get(outcome_var)==0 & 
		get(var)<=quantile(pred_set[get(outcome_var)==0, get(var)], prob=seq(0, 1, 0.1))[2], get(var)]))]

	# highest decile by outcome
	stat_dt[, treatment_mean_highest_decile:=sapply(names(pred_set), function(var) mean(pred_set[get(outcome_var)==1 & 
		get(var)>=quantile(pred_set[get(outcome_var)==1, get(var)], prob=seq(0, 1, 0.1))[10], get(var)]))]
	stat_dt[, control_mean_highest_decile:=sapply(names(pred_set), function(var) mean(pred_set[get(outcome_var)==0 & 
		get(var)>=quantile(pred_set[get(outcome_var)==0, get(var)], prob=seq(0, 1, 0.1))[10], get(var)]))]

	# zero by outcome
	stat_dt[, treatment_zero_perc:=sapply(names(pred_set), function(var) perc(sum(pred_set[get(outcome_var)==1, 
		get(var)]==0, na.rm=T), nrow(pred_set[get(outcome_var)==1])))]
	stat_dt[, control_zero_perc:=sapply(names(pred_set), function(var) perc(sum(pred_set[get(outcome_var)==0, 
		get(var)]==0, na.rm=T), nrow(pred_set[get(outcome_var)==0])))]

	# missing by outcome
	stat_dt[, treatment_missing_perc:=sapply(names(pred_set), function(var) perc(sum(is.na(pred_set[get(outcome_var)==1, 
		get(var)])), nrow(pred_set[get(outcome_var)==1])))]
	stat_dt[, control_missing_perc:=sapply(names(pred_set), function(var) perc(sum(is.na(pred_set[get(outcome_var)==0, 
		get(var)])), nrow(pred_set[get(outcome_var)==0])))]

	stat_dt[, c(setdiff(names(stat_dt), c("var_name", "class"))):=lapply(.SD, function(x) round(x, 2)), 
		.SDcols=setdiff(names(stat_dt), c("var_name", "class"))]

	# return
	return(stat_dt)

}

#----------------------------------------------------------------------------#
