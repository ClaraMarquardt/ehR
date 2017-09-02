#----------------------------------------------------------------------------#

#' Generate a basic overview of a dataset. 
#' 
#' \
#' 
#' @details Maintainer: Clara Marquardt
#' 
#' @export
#' @import data.table
#'
#' @param orig_dt data.table for which the overview is to be generated (data.table).
#' @param observation_unit Observation unit (character).
#' @param file_title file title (printed at the top of the overview) (character).
#' @param alt_id_name alternative observation unit, e.g. "patients" in a diagnosis file with multiple diagnoses per patient (character).
#' @param alt_id name of column corresponding to the alt_id_name (character).
#' 
#' @return Overview of dataset [data.table]
#' 
#' @examples
#' summary_dt <- var_overview(dia, alt_id="empi", alt_id_name="patients",observation_unit="diagnoses", 
#' file_title="dia.rda" )
#' print(str(summary_dt))

var_overview <- function(orig_dt, observation_unit=NA, alt_id=NA, alt_id_name="observations", 
  file_title=NA) {

  # helper functions
  # ---------------------------
  perc <- function(num, denom, digit=1) {

    round((num/denom)*100, digit)

  }

  # generate purely numeric version of the dataset & obtain var types
  # ---------------------------
  factor_col <- names(which(sapply(orig_dt, class)=="factor"))
  integer_col <- names(which(sapply(orig_dt, class)=="integer"))
  numeric_col <- names(which(sapply(orig_dt, class)=="numeric"))
  date_col <- names(orig_dt[,c((which(sapply(sapply(orig_dt, class), "[",1) %in% c("IDate", 
    "Date")))),with=F])
  character_col <- names(which(sapply(orig_dt, class)=="character"))
  if (length(factor_col)>0) orig_dt[, c(factor_col):=lapply(.SD, function(x) as.numeric(x)), 
    .SDcols=names(which(sapply(orig_dt, class)=="factor"))]

  # generate stats for integer
  # ---------------------------

  if (length(integer_col)>0) {

    integer_stat <- lapply(orig_dt[, mget(integer_col)], function(x) summary(x, digits=10))
    integer_stat <- lapply(integer_stat, function(x) data.table(t(as.data.table(list(unlist(x))))))
    integer_stat <- as.data.table(rbindlist(integer_stat, fill=T))
    integer_stat[, V7:=NULL]
    setnames(integer_stat, c("min", "Q1", "median", "mean", "Q3", "max")) 

    integer_stat[, sd:=sapply(orig_dt[, mget(integer_col)], function(x) sd(x, na.rm=T))]
    integer_stat[, missing_perc:=sapply(orig_dt[, mget(integer_col)], function(x) 
      perc(sum(is.na(x)), length(x), digit=2))]
    integer_stat[, zero_perc:=sapply(orig_dt[, mget(integer_col)], function(x) 
      perc(sum(x==0, na.rm=T), length(x), digit=2))]
    integer_stat[, unique_count:=sapply(orig_dt[, mget(integer_col)], function(x) 
      length(unique(x[!is.na(x)])))]
    integer_stat[, most_common_five_non_NA:=sapply(orig_dt[, mget(integer_col)], function(x) 
      gsub(",$", "", paste0(c(rbind(names(table(x, useNA="no")[order(-table(x, 
        useNA="no"))][1:min(5, length(unique(x[!is.na(x)])))]), 
        paste0("(prop: ", perc(table(x, useNA="no")[order(-table(x, useNA="no"))][
        1:min(5, length(unique(x[!is.na(x)])))], length(x), digit=2), ")"), rep(",", min(5, 
        length(unique(x[!is.na(x)])))))), collapse=" ")))]
 
    integer_stat[, var_name:=integer_col]
    integer_stat[, var_type:="integer"]

  }

  # generate stats for numeric vars
  # ---------------------------

  if (length(numeric_col)>0) {

    numeric_stat <- lapply(orig_dt[, mget(numeric_col)], function(x) summary(x, digits=10))
    numeric_stat <- lapply(numeric_stat, function(x) data.table(t(as.data.table(list(unlist(x))))))
    numeric_stat <- as.data.table(rbindlist(numeric_stat, fill=T))
    numeric_stat[, V7:=NULL]
    setnames(numeric_stat, c("min", "Q1", "median", "mean", "Q3", "max")) 

    numeric_stat[, sd:=sapply(orig_dt[, mget(numeric_col)], function(x) sd(x, na.rm=T))]
    numeric_stat[, missing_perc:=sapply(orig_dt[, mget(numeric_col)], function(x) 
      perc(sum(is.na(x)), length(x), digit=2))]
    numeric_stat[, zero_perc:=sapply(orig_dt[, mget(numeric_col)], function(x) 
      perc(sum(x==0, na.rm=T), length(x), digit=2))]
    numeric_stat[, unique_count:=sapply(orig_dt[, mget(numeric_col)], function(x) 
      length(unique(x[!is.na(x)])))]

    numeric_stat[, var_name:=numeric_col]
    numeric_stat[, var_type:="numeric"]

  }

  # generate stats for factor var
  # ---------------------------

  if (length(factor_col)>0) {
  
    factor_stat <- lapply(orig_dt[, mget(factor_col)], function(x) mean(x, na.rm=T))
    factor_stat <- lapply(factor_stat, function(x) data.table(t(as.data.table(list(unlist(x))))))
    factor_stat <- as.data.table(rbindlist(factor_stat, fill=T))
    setnames(factor_stat, c("mean")) 

    factor_stat[, missing_perc:=sapply(orig_dt[, mget(factor_col)], function(x) 
        perc(sum(is.na(x)), length(x), digit=2))]
    factor_stat[, zero_perc:=sapply(orig_dt[, mget(factor_col)], function(x) 
      perc(sum(x==0, na.rm=T), length(x), digit=2))]
    factor_stat[, unique_count:=sapply(orig_dt[, mget(factor_col)], function(x) 
      length(unique(x[!is.na(x)])))]
    factor_stat[, most_common_five_non_NA:=sapply(orig_dt[, mget(factor_col)], function(x) 
      gsub(",$", "", paste0(c(rbind(names(table(x, useNA="always")[order(-table(x, 
        useNA="no"))][1:min(5, length(unique(x[!is.na(x)])))]), 
        paste0("(prop: ", perc(table(x, useNA="no")[order(-table(x, useNA="no"))][
        1:min(5, length(unique(x[!is.na(x)])))], length(x), digit=2), ")"), rep(",", min(5, 
        length(unique(x[!is.na(x)])))))), collapse=" ")))]
 
    factor_stat[, var_name:=factor_col]
    factor_stat[, var_type:="factor"]

  }

 # generate stats for date var
 # ---------------------------

 if (length(date_col)>0) {

    date_stat <- do.call("c", lapply(orig_dt[, mget(date_col)], function(x) 
      as.IDate(min(x, na.rm=T), "%Y-%m-%d")))
    date_stat <- lapply(date_stat, function(x) data.table(t(as.data.table(list(unlist(x))))))
    date_stat <- as.data.table(rbindlist(date_stat, fill=T))
    setnames(date_stat, c("earliest_date"))  
    date_stat[, earliest_date:=as.IDate(earliest_date, "%Y-%m-%d")]
    date_stat[, latest_date:=do.call("c", lapply(orig_dt[, mget(date_col)], function(x) 
      as.IDate(max(x, na.rm=T), "%Y-%m-%d")))]

    date_stat[, missing_perc:=sapply(orig_dt[, mget(date_col)], function(x) 
      perc(sum(is.na(x)), length(x), digit=2))]
    date_stat[, unique_count:=sapply(orig_dt[, mget(date_col)], function(x) 
      length(unique(x[!is.na(x)])))]
    date_stat[, most_common_five_non_NA:=sapply(orig_dt[, mget(date_col)], function(x) 
      gsub(",$", "", paste0(c(rbind(names(table(x, useNA="always")[order(-table(x, 
        useNA="no"))][1:min(5, length(unique(x[!is.na(x)])))]), 
        paste0("(prop: ", perc(table(x, useNA="no")[order(-table(x, 
        useNA="no"))][1:min(5, length(unique(x[!is.na(x)])))], length(x), digit=2), ")"), 
        rep(",", min(5, length(unique(x[!is.na(x)])))))), collapse=" ")))]

    date_stat[, var_name:=date_col]
    date_stat[, var_type:="date"]

  }

 # generate stats for character var
 # ---------------------------

 if (length(character_col)>0) {
  
    character_stat <- sapply(orig_dt[, mget(character_col)], function(x) 
      perc(sum(is.na(x)), length(x), digit=2))
    character_stat <- lapply(character_stat, function(x) data.table(t(as.data.table(list(unlist(x))))))
    character_stat <- as.data.table(rbindlist(character_stat, fill=T))
    setnames(character_stat, c("missing_perc")) 

    character_stat[, most_common_five_non_NA:=sapply(orig_dt[, mget(character_col)], function(x) 
      gsub(",$", "", paste0(c(rbind(names(table(x, useNA="always")[order(-table(x, 
        useNA="no"))][1:min(5, length(unique(x[!is.na(x)])))]), 
        paste0("(prop: ", perc(table(x, useNA="no")[order(-table(x, useNA="no"))][
        1:min(5, length(unique(x[!is.na(x)])))], length(x), digit=2), ")"), rep(",", min(5, 
        length(unique(x[!is.na(x)])))))), collapse=" ")))]
    character_stat[, unique_count:=sapply(orig_dt[, mget(character_col)], function(x) 
      length(unique(x[!is.na(x)])))]

    character_stat[, var_name:=character_col]
    character_stat[, var_type:="character"]

  }

  # merge
  # ---------------------------
  feature_list <- list("date_stat","character_stat", "integer_stat", "numeric_stat", 
    "factor_stat")
  feature_list <- feature_list[which(feature_list %in% ls())]

  feature_vital_sign <- rbindlist(mget(unlist(feature_list)),
    use.names=TRUE, fill=TRUE)

  variable_order_list <- c("var_origin", "var_name", "var_type", "unique_count", "earliest_date",
    "latest_date", "earliest_date_restricted","latest_date_restricted",  "missing_perc","zero_perc",
    "most_common_five_non_NA", "mean", "min", "Q1","median", "Q3", "max","sd", "var_desc", 
    "rpdr_var_desc")
  variable_order_list <- variable_order_list[variable_order_list %in% names(feature_vital_sign)]

  setcolorder(feature_vital_sign, c(variable_order_list))


  orig_dt_temp <- copy(orig_dt)
  dt_replace(dt=orig_dt_temp, mode="set_missing_na") # to ensure that missingness is correctly calculated
  
  feature_vital_sign <- rbindlist(list(feature_vital_sign, data.table(
    var_name=c("", "unit of observation:", "number of observations:", paste0("number of ", 
      alt_id_name, ":")), var_type=c("", 
    ifelse(is.na(observation_unit), "", observation_unit), 
    nrow(orig_dt), 
    ifelse(!is.na(alt_id),nrow(unique(orig_dt, by=c(alt_id))), nrow(orig_dt))))), fill=T)

   feature_vital_sign <- rbindlist(list(feature_vital_sign, data.table(
    var_name=c("earliest_date:", "latest_date:"), var_type=c(ifelse("earliest_date" %in% 
      names(feature_vital_sign), as.character(min(feature_vital_sign$earliest_date, na.rm=T)), 
    NA), ifelse("latest_date" %in% names(feature_vital_sign),
    as.character(max(feature_vital_sign$latest_date, na.rm=T)), NA)))), 
    fill=T)

   feature_vital_sign <- rbindlist(list(feature_vital_sign, data.table(
    var_name=c("var_count:", "% of observations missing:"), var_type=c(ncol(orig_dt), 
      perc(sum(sapply(orig_dt_temp, function(x) sum(is.na(x)))),
      (nrow(orig_dt_temp)*ncol(orig_dt_temp)))))), fill=T)

 if ("earliest_date" %in% names(feature_vital_sign)){
  feature_vital_sign[, earliest_date:=as.character(earliest_date)]
  feature_vital_sign[, latest_date:=as.character(latest_date)]
 }

  if ("earliest_date_restricted" %in% names(feature_vital_sign)){
  feature_vital_sign[, earliest_date_restricted:=as.character(earliest_date_restricted)]
  feature_vital_sign[, latest_date_restricted:=as.character(latest_date_restricted)]
 }

 feature_vital_sign <- rbindlist(list(data.table(var_name=c("", "file name:", ""), 
    var_type=c("", ifelse(is.na(file_title), "", 
    file_title), "")), feature_vital_sign), fill=T, use.names=T)


  # final formatting 
  # ---------------------------

  ## round numeric cols
  num_col <- names(feature_vital_sign)[which(sapply(feature_vital_sign, function(x) class(x)) 
    %in% c("numeric", "integer"))]
  feature_vital_sign[, c(num_col):=lapply(.SD, function(x) round(x, 3)), .SDcols=num_col]

  ## deal with NAs
  feature_vital_sign[, names(feature_vital_sign):=lapply(.SD, function(x) as.character(x)), 
    .SDcols=names(feature_vital_sign)]
  dt_replace(dt=feature_vital_sign, mode="set_na_zero")

 return(feature_vital_sign)

}

#----------------------------------------------------------------------------#
