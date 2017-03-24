#----------------------------------------------------------------------------#

#' Generate an overview of a dataset. 
#' @export
#' @import data.table
#' @param DT Name of DT for which summary stats are to be generated 
#' @param alt_id,alt_id name Name of id variable on the basis of which to generate the 
#'  observation count (set to NA if no relevant identfier exists)
#' @param var_def_merge Whether to merge in csv file with variable definitions 
#' @param var_def_file Path of csv file with variable definitions merged in if var_def_merge==TRUE
#' @param observation_unit Unit of observation (e.g."diagnoses","encounters")
#' @param file_title Dataset title to printed at the top of the summary statistics
#' @return Overview of dataset [data.table]
#' @examples
#' summary_stat <- var_overview(dia, alt_id="empi", alt_id_name="patient",observation_unit="diagnoses", 
#' file_title="dia.rda" )

var_overview <- function(DT, alt_id=NA, alt_id_name="", var_def_merge=FALSE, 
  var_def_file=NA, observation_unit=NA, file_title=NA) {

  ######################################################################
  # generate purely numeric version of the dataset & obtain var types
  factor_col <- names(which(sapply(DT, class)=="factor"))
  integer_col <- names(which(sapply(DT, class)=="integer"))
  numeric_col <- names(which(sapply(DT, class)=="numeric"))
  date_col <- names(DT[,c((which(sapply(sapply(DT, class), "[",1) %in% c("IDate", "Date")))),with=F])
  character_col <- names(which(sapply(DT, class)=="character"))
  if (length(factor_col)>0) DT[, c(factor_col):=lapply(.SD, function(x) as.numeric(x)), 
    .SDcols=names(which(sapply(DT, class)=="factor"))]

  ######################################################################
  ## generate stats for integer
  if (length(integer_col)>0) {

    integer_stat <- lapply(DT[, mget(integer_col)], function(x) summary(x, digits=10))
    integer_stat <- lapply(integer_stat, function(x) data.table(t(as.data.table(list(unlist(x))))))
    integer_stat <- as.data.table(rbindlist(integer_stat, fill=T))
    integer_stat[, V7:=NULL]
    setnames(integer_stat, c("min", "Q1", "median", "mean", "Q3", "max")) 

    integer_stat[, sd:=sapply(DT[, mget(integer_col)], function(x) sd(x, na.rm=T))]
    integer_stat[, missing_perc:=sapply(DT[, mget(integer_col)], function(x) 
      perc(sum(is.na(x)), length(x), digit=2))]
    integer_stat[, zero_perc:=sapply(DT[, mget(integer_col)], function(x) 
      perc(sum(x==0, na.rm=T), length(x), digit=2))]
    integer_stat[, unique_count:=sapply(DT[, mget(integer_col)], function(x) 
      length(unique(x[!is.na(x)])))]
    integer_stat[, most_common_five_non_NA:=sapply(DT[, mget(integer_col)], function(x) 
      gsub(",$", "", paste0(c(rbind(names(table(x, useNA="no")[order(-table(x, 
        useNA="no"))][1:min(5, length(unique(x[!is.na(x)])))]), 
        paste0("(prop: ", perc(table(x, useNA="no")[order(-table(x, useNA="no"))][
        1:min(5, length(unique(x[!is.na(x)])))], length(x), digit=2), ")"), rep(",", min(5, 
        length(unique(x[!is.na(x)])))))), collapse=" ")))]
 
    integer_stat[, var_name:=integer_col]
    integer_stat[, var_type:="integer"]

  }

  ######################################################################
  ## generate stats for numeric vars
  if (length(numeric_col)>0) {

    numeric_stat <- lapply(DT[, mget(numeric_col)], function(x) summary(x, digits=10))
    numeric_stat <- lapply(numeric_stat, function(x) data.table(t(as.data.table(list(unlist(x))))))
    numeric_stat <- as.data.table(rbindlist(numeric_stat, fill=T))
    numeric_stat[, V7:=NULL]
    setnames(numeric_stat, c("min", "Q1", "median", "mean", "Q3", "max")) 

    numeric_stat[, sd:=sapply(DT[, mget(numeric_col)], function(x) sd(x, na.rm=T))]
    numeric_stat[, missing_perc:=sapply(DT[, mget(numeric_col)], function(x) 
      perc(sum(is.na(x)), length(x), digit=2))]
    numeric_stat[, zero_perc:=sapply(DT[, mget(numeric_col)], function(x) 
      perc(sum(x==0, na.rm=T), length(x), digit=2))]
    numeric_stat[, unique_count:=sapply(DT[, mget(numeric_col)], function(x) 
      length(unique(x[!is.na(x)])))]

    numeric_stat[, var_name:=numeric_col]
    numeric_stat[, var_type:="numeric"]

  }

  ######################################################################
  ## generate stats for factor var
  if (length(factor_col)>0) {
  
    factor_stat <- lapply(DT[, mget(factor_col)], function(x) mean(x, na.rm=T))
    factor_stat <- lapply(factor_stat, function(x) data.table(t(as.data.table(list(unlist(x))))))
    factor_stat <- as.data.table(rbindlist(factor_stat, fill=T))
    setnames(factor_stat, c("mean")) 

    factor_stat[, missing_perc:=sapply(DT[, mget(factor_col)], function(x) 
        perc(sum(is.na(x)), length(x), digit=2))]
    factor_stat[, zero_perc:=sapply(DT[, mget(factor_col)], function(x) 
      perc(sum(x==0, na.rm=T), length(x), digit=2))]
    factor_stat[, unique_count:=sapply(DT[, mget(factor_col)], function(x) 
      length(unique(x[!is.na(x)])))]
    factor_stat[, most_common_five_non_NA:=sapply(DT[, mget(factor_col)], function(x) 
      gsub(",$", "", paste0(c(rbind(names(table(x, useNA="always")[order(-table(x, 
        useNA="no"))][1:min(5, length(unique(x[!is.na(x)])))]), 
        paste0("(prop: ", perc(table(x, useNA="no")[order(-table(x, useNA="no"))][
        1:min(5, length(unique(x[!is.na(x)])))], length(x), digit=2), ")"), rep(",", min(5, 
        length(unique(x[!is.na(x)])))))), collapse=" ")))]
 
    factor_stat[, var_name:=factor_col]
    factor_stat[, var_type:="factor"]

  }

 ######################################################################
 ## generate stats for date var
 if (length(date_col)>0) {

    date_stat <- do.call("c", lapply(DT[, mget(date_col)], function(x) 
      as.IDate(min(x, na.rm=T), "%Y-%m-%d")))
    date_stat <- lapply(date_stat, function(x) data.table(t(as.data.table(list(unlist(x))))))
    date_stat <- as.data.table(rbindlist(date_stat, fill=T))
    setnames(date_stat, c("earliest_date"))  
    date_stat[, earliest_date:=as.IDate(earliest_date, "%Y-%m-%d")]
    date_stat[, latest_date:=do.call("c", lapply(DT[, mget(date_col)], function(x) 
      as.IDate(max(x, na.rm=T), "%Y-%m-%d")))]

    if(date_restriction==TRUE) {  

      DT_temp <- copy(DT)

      DT_temp[, paste0(date_col, "_restricted"):=lapply(.SD, function(x) x), .SDcols=date_col]

      check <- data.table(sapply(DT_temp[, mget(date_col)], function(x) sapply(x, function(y) ifelse(is.na(y), NA, in_between(year(y), date_restriction_range[[1]] , 
        date_restriction_range[[2]])))))
      setnames(check, paste0(date_col, "_check"))
      DT_temp <- data.table(DT_temp, check)

      for (col in date_col) {
        check <- c(which(DT_temp[,get(paste0(col, "_check"))]==FALSE))
        DT_temp[c(check), paste0(date_col, "_restricted"):=NA, with=F]
      }

     date_stat[, earliest_date_restricted:=do.call("c", lapply(DT_temp[, mget(paste0(date_col,"_restricted"))], function(x) 
      as.IDate(min(x, na.rm=T), "%Y-%m-%d")))]
     date_stat[, latest_date_restricted:=do.call("c", lapply(DT_temp[, mget(paste0(date_col,"_restricted"))], function(x) 
      as.IDate(max(x, na.rm=T), "%Y-%m-%d")))]

     DT_temp[, c(paste0(date_col, "_restricted")):=NULL]
     
     }

    date_stat[, missing_perc:=sapply(DT[, mget(date_col)], function(x) 
      perc(sum(is.na(x)), length(x), digit=2))]
    date_stat[, unique_count:=sapply(DT[, mget(date_col)], function(x) 
      length(unique(x[!is.na(x)])))]
    date_stat[, most_common_five_non_NA:=sapply(DT[, mget(date_col)], function(x) 
      gsub(",$", "", paste0(c(rbind(names(table(x, useNA="always")[order(-table(x, 
        useNA="no"))][1:min(5, length(unique(x[!is.na(x)])))]), 
        paste0("(prop: ", perc(table(x, useNA="no")[order(-table(x, 
        useNA="no"))][1:min(5, length(unique(x[!is.na(x)])))], length(x), digit=2), ")"), 
        rep(",", min(5, length(unique(x[!is.na(x)])))))), collapse=" ")))]

    date_stat[, var_name:=date_col]
    date_stat[, var_type:="date"]

  }


 ######################################################################
 ## generate stats for character var
 if (length(character_col)>0) {
  
    character_stat <- sapply(DT[, mget(character_col)], function(x) 
      perc(sum(is.na(x)), length(x), digit=2))
    character_stat <- lapply(character_stat, function(x) data.table(t(as.data.table(list(unlist(x))))))
    character_stat <- as.data.table(rbindlist(character_stat, fill=T))
    setnames(character_stat, c("missing_perc")) 

    character_stat[, most_common_five_non_NA:=sapply(DT[, mget(character_col)], function(x) 
      gsub(",$", "", paste0(c(rbind(names(table(x, useNA="always")[order(-table(x, 
        useNA="no"))][1:min(5, length(unique(x[!is.na(x)])))]), 
        paste0("(prop: ", perc(table(x, useNA="no")[order(-table(x, useNA="no"))][
        1:min(5, length(unique(x[!is.na(x)])))], length(x), digit=2), ")"), rep(",", min(5, 
        length(unique(x[!is.na(x)])))))), collapse=" ")))]
    character_stat[, unique_count:=sapply(DT[, mget(character_col)], function(x) 
      length(unique(x[!is.na(x)])))]

    character_stat[, var_name:=character_col]
    character_stat[, var_type:="character"]

  }

  ######################################################################
  #### merge

  feature_list <- list("date_stat","character_stat", "integer_stat", "numeric_stat", 
    "factor_stat")
  feature_list <- feature_list[which(feature_list %in% ls())]

  feature_vital_sign <- rbindlist(mget(unlist(feature_list)),
    use.names=TRUE, fill=TRUE)

  if(var_def_merge==TRUE) {
    var_def_file <- fread(var_def_file)
    feature_vital_sign <- var_def_file[feature_vital_sign, on=c("var_name"), nomatch=NA]
  }

  variable_order_list <- c("var_origin", "var_name", "var_type", "unique_count", "earliest_date","latest_date", 
    "earliest_date_restricted","latest_date_restricted",  "missing_perc","zero_perc",
    "most_common_five_non_NA", "mean", "min", "Q1","median", "Q3", "max","sd", "var_desc", "rpdr_var_desc")
  variable_order_list <- variable_order_list[variable_order_list %in% names(feature_vital_sign)]

  setcolorder(feature_vital_sign, c(variable_order_list))


  DT_temp <- copy(DT)
  set_missing_na(DT_temp) # to ensure that missingness is correctly calculated

  feature_vital_sign <- rbindlist(list(feature_vital_sign, data.table(
    var_name=c("", "unit of observation:", "number of observations:", paste0("number of ", alt_id_name, ":")), 
    var_type=c("", 
    ifelse(is.na(observation_unit), "", observation_unit), 
    nrow(DT), 
    ifelse(!is.na(alt_id),nrow(unique(DT, by=c(alt_id))), nrow(DT))))), fill=T)

   feature_vital_sign <- rbindlist(list(feature_vital_sign, data.table(
    var_name=c("earliest_date:", "latest_date:"), var_type=c(ifelse("earliest_date" %in% names(feature_vital_sign), 
    as.character(min(feature_vital_sign$earliest_date, na.rm=T)), 
    NA), ifelse("latest_date" %in% names(feature_vital_sign),as.character(max(feature_vital_sign$latest_date, na.rm=T)), NA)))), 
    fill=T)


   feature_vital_sign <- rbindlist(list(feature_vital_sign, data.table(
    var_name=c("var_count:", "% of observations missing:"), var_type=c(ncol(DT), perc(sum(sapply(DT_temp, 
    function(x) sum(is.na(x)))),(nrow(DT_temp)*ncol(DT_temp)))))), fill=T)

 if ("earliest_date" %in% names(feature_vital_sign)){
  feature_vital_sign[, earliest_date:=as.character(earliest_date)]
  feature_vital_sign[, latest_date:=as.character(latest_date)]
 }

  if ("earliest_date_restricted" %in% names(feature_vital_sign)){
  feature_vital_sign[, earliest_date_restricted:=as.character(earliest_date_restricted)]
  feature_vital_sign[, latest_date_restricted:=as.character(latest_date_restricted)]
 }

 feature_vital_sign <- rbindlist(list(data.table(var_name=c("", "file name:", ""), var_type=c("", ifelse(is.na(file_title), "", 
    file_title), "")), feature_vital_sign), fill=T, use.names=T)


# final formatting

## round numeric cols
num_col <- names(feature_vital_sign)[which(sapply(feature_vital_sign, function(x) class(x)) %in% c("numeric", "integer"))]
feature_vital_sign[, c(num_col):=lapply(.SD, function(x) round(x, 3)), .SDcols=num_col]

## deal with NAs
feature_vital_sign[, names(feature_vital_sign):=lapply(.SD, function(x) as.character(x)), 
  .SDcols=names(feature_vital_sign)]
 set_na_zero(feature_vital_sign, replace=" ")

 return(feature_vital_sign)

}


#----------------------------------------------------------------------------#
