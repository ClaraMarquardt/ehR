################################################################################
## Generic Function Specification

## Language: R

## Fuctions contained wihtin sections are self-contained, i.e. are not dependent  
## on other functions in this function repo

# ################################################################################
# ############################ LIBRARIES NEEDED ##################################
# ################################################################################

# Note: integrate into project code base - listed here only for reference purposes

# ################################################################################
# ### main packages
# package_list <- list( "plyr", "dplyr","data.table","tidyr","ggplot2", "stringr",
#   "scales",  "xtable", "gridExtra", "grid", "lubridate", "reshape", 
#   "reshape2", "reports", "validate", "RecordLinkage", "xlsx")

# load_or_install <- function(package_names) {  
#   lapply(package_names, function(x) if(!x %in% installed.packages()) 
#     install.packages(x,repos="http://cran.cnr.berkeley.edu/"))
#   lapply(package_names, function(x) library(x,character.only=TRUE, quietly=TRUE,
#     verbose=FALSE))
# } 

# load_or_install(package_list)

# ################################################################################
# ### other packages (devtools)
# # NOTE: Need to install as "trinker/plotflow" but load as "plotflow"

# install_github("trinker/plotflow")

################################################################################
################################ FUNCTIONS #####################################
################################################################################

################################################################################
## list_table
################################################################################

#' A Cat Function
#'
#' This function allows you to express your love of cats.
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
#' @examples
#' cat_function()

list_table <- function(list_name, var_name, argument="space", vital_signs_merge=F, 
  vital_signs_old=NA) {
 
  # purpose: 
  # (a) convert list into table and (b) [optional] append table to existing table in a manner 
  # consistent with the generation of 'vital signs'

  # arguments:
  #   list_name: name of named list (can contain elements which are converted into empty rows)
  #   var_name: name of variable which contains the header of the generated table (if vital_signs_merge==T 
  #             var_name %in% names(vital_signs_old) -> the system time is appended to var_name to ensure 
  #             the uniqueness of column names)
  #   argument: list elements == this phrase are converted into empty rows in the generated 
  #             table (default: "space")
  #   vital_signs_merge: whether or not to merge generated table with existing file in a 
  #                      manner consistent with the generation of 'vital signs' (default: F)
  #   vital_signs_old: path of existing csv file with which generated table is to be merged
  #                    if vital_signs_merge==T (default: NA)

  # returns:
  # generated table (merged with existing file if vital_signs_merge==T) [data.table format]


  temp <- as.data.table(t(rbindlist(list(list_name), fill=T)))
  temp$var_name <- names(list_name)
  temp[V1=="space", ':='(var_name="space", V1="")]
  
  if (!var_name %in% names(vital_signs_old)) {
    setnames(temp, c(var_name, "var_name"))
  } else {
    setnames(temp, c(paste(var_name, as.character(format(Sys.time(), "%H:%M")), sep="_"), 
    "var_name"))
  }

  if(vital_signs_merge==T) {

    col_name_old <- copy(names(temp))
    temp <- join(temp, vital_signs_old, by=c("var_name"), "full")
    setcolorder(temp, c(names(vital_signs_old), setdiff(col_name_old, "var_name")))
  } else {
    setcolorder(temp, c("var_name", var_name))
  }
  temp <- as.data.table(temp)

  temp[, drop:=0][var_name==shift(var_name, type="lead") & !(var_name=="space" & 
    shift(var_name, type="lead")=="space"), drop:=1]
  temp <- temp[!drop==1]
  temp[, drop:=NULL]
  temp[var_name=="space", var_name:=""]

  return(temp)
}


################################################################################
## list_space
################################################################################

list_space <- function(list_name, argument="space") {
 
  # purpose: 
  # insert element into list containing a specified phrase (useful when the list is to be 
  # converted into a table which is to contain 'spaces', i.e. empty rows)

  # arguments:
  #   list_name: name of list as characters ("...") 
  #   argument: phrase to be inserted into generated arguments
  
  # returns:
  # modified list in place - i.e. 'returns' modified list with inserted element

  # sample usage:
  #   test <- list()
  #   test$element1 <- "test element"
  #   list_space("test")
 
 count <- get(argument, sys.frame(sys.parent(n=1)))
 temp <- get(list_name, sys.frame(sys.parent(n=1)))
 
 temp[[paste0("space_", count)]] <- "space"
 
 assign(list_name, temp, sys.frame(sys.parent(n=1)))
 assign(argument, count+1, sys.frame(sys.parent(n=1)))

}

################################################################################
## mean_ci
################################################################################
mean_ci <- function(var, ci, return_sd=TRUE, digit=2, min_lim=NA, max_lim=NA, type="mean") {

  # determine critical values
  ci_stat_list <- list(
    sign_0.95=1.96, 
    sign_0.9=1.645
  )

  ci_stat <- ci_stat_list[[paste0("sign_", as.character(ci))]]

  if (type=="mean") {
    
    # determine CI
    var_sd   <- sd(var, na.rm=T)
    ci_low   <- round(mean(var, na.rm=T) - ci_stat * var_sd, digits=digit)
    ci_upper <- round(mean(var, na.rm=T) + ci_stat * var_sd, digits=digit)

    if (!is.na(min_lim)) {
      ci_low <- max(min_lim, ci_low)
    }

    if (!is.na(max_lim)) {
      ci_upper <- min(max_lim, ci_upper)
    }

    var_sd <- round(var_sd, digits=digit)

  } else if (type=="perc_prop") {

    # determine CI
    var_mod  <- var/100
    var_sd   <- sqrt(((mean(var_mod, na.rm=T)*(1-mean(var_mod, 
                    na.rm=T)))/length(var_mod[!is.na(var_mod)])))
    ci_low   <- mean(var_mod, na.rm=T) - ci_stat * var_sd
    ci_upper <- mean(var_mod, na.rm=T) + ci_stat * var_sd

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

################################################################################
## mean_ci_cluster
################################################################################

mean_ci_clust <- function(var, cluster_var, ci, return_sd=TRUE, digit=2, min_lim=NA, max_lim=NA, 
  type="perc_prop") {

  # determine critical values
  ci_stat_list <- list(
    sign_0.95=1.96, 
    sign_0.9=1.645
  )

  ci_stat <- ci_stat_list[[paste0("sign_", as.character(ci))]]

  if (type=="mean") {
    
    stop("method not implemented")

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

################################################################################
## median_ci
################################################################################

median_ci <- function(var, ci=0.95, return_sd=TRUE, digit=2) {
  
  ci_comb <- 0000

  # calculate ci
  n <-nrow(as.matrix(var))
  if(qbinom((1-ci)/2,n,0.5)==0) {
    ci_comb <- NA
  }
  L <- qbinom((1-ci)/2,n,0.5)
  U <-n-L+1
 
  if(L>=U) {
    ci_comb <- NA
  }
  
  if (!is.na(ci_comb)) {
    order.var <-sort(var)
    res <-list()
    res$head <-paste(paste(as.character(ci*100),"%",sep=""), c("CI for population median"))
    res$ci   <-c(median=median(var),lower=order.var[L],upper=order.var[n-L+1])
    res$ends <-c("Estimate",paste(as.character(c((1-ci)/2,1-((1-ci)/2))*100),"%",sep=""))
    res$coverage  <-1-(2*pbinom(q=L-1,n,0.5))

    # combine
    ci_comb <- paste0(round(res$ci[["lower"]], digits=digit),  " - ", round(res$ci[["upper"]], digits=digit))

  }
  
  # return
  if (return_sd==TRUE) {
    return(c(NA, ci_comb))
  } else {
    return(ci_comb)
  }

}

################################################################################
## var_overview
################################################################################


var_overview <- function(DT, patient_id="empi", var_def_merge=FALSE, var_def_file=NA,
  cohort_perc=FALSE, cohort_path=NA, cohort_name=NA, observation_unit=NA, file_title=NA, 
  date_excl=NA, date_excl_suffix=NA, date_restriction=FALSE, date_restriction_range=NA, 
  cohort_check=FALSE, ext_obs_count=NA, ext_patient_count=NA) {

  # purpose: 
  # generate table of variable level summary stats for a given DT based on the 
  # type of each variable (integer, numeric, factor, date or character)

  # arguments:
  #  DT: name of DT for which summary stats are to be generated 
  #  patient_id: name of patient_id on the basis of which to generate the patient count (default:"empi")
  #              set to NA to not generate a patient count/if no patient identfier existis
  #  var_def_merge: whether to merge in csv file with variable definitions (var_name and var_desc) (default:FALSE)
  #  var_def_file: path of csv file with variable definitions (var_name and var_desc) merged in if var_def_merge==TRUE (default:NA)
  #  cohort_perc: whether to report perc of cohort with obs (default:FALSE)
  #  cohort_path: path of RDs cohort file used if cohort_perc==TRUE (default:NA)
  #  cohort_name: name of cohort file used if cohort_perc==TRUE (default:NA)
  #  observation_unit: unit of observation (e.g."diagnosis","encounter") (default:NA)
  #  file_title: file title to be printed at the top of the table (default:NA)
  #  date_excl: date variables to be excluded in the generation of an additional earliest/latest date statistic (default:NA)
  #  date_excl_suffix: suffix used to name the date_excl based earliest/latest date summary statistic if date_excl!=NA (default:NA)

  # returns:
  # data.table with the generated summary stats

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

  if(cohort_perc==TRUE) {
    cohort_file <<- readRDS(cohort_path)
  }

  DT_temp <- copy(DT)
  set_missing_na(DT_temp) # to ensure that missingness is correctly calculated

  feature_vital_sign <- rbindlist(list(feature_vital_sign, data.table(
    var_name=c("", "unit of observation:", "number of observations:", "number of patients:", 
    ifelse(is.na(cohort_name), "perc (%) of cohort patients with observation:",
    paste0("perc (%) of ", cohort_name, " cohort patients with observation:"))), 
    var_type=c("", 
    ifelse(is.na(observation_unit), "", observation_unit), 
    ifelse(is.na(ext_obs_count),nrow(DT), ext_obs_count), 
    ifelse(is.na(ext_patient_count),nrow(unique(DT, by=c(patient_id))), ext_patient_count), 
    ifelse(cohort_perc==TRUE, as.character(perc(nrow(unique(DT[get(patient_id) %in% cohort_file[, get(patient_id)]], by=c(patient_id))), 
    nrow(cohort_file), digit=4)), NA)))), fill=T)

  if(cohort_check==TRUE) {
  feature_vital_sign <- rbindlist(list(feature_vital_sign, data.table(
    var_name=c(paste0("number of observations in ", cohort_name, " cohort:"),
    paste0("number of patients in ", cohort_name, " cohort:") ), 
    var_type=c(nrow(DT[get(patient_id) %in% cohort_file[, get(patient_id)]]), nrow(unique(DT[get(patient_id) 
    %in% cohort_file[, get(patient_id)]], by=patient_id))))),
    fill=T)
  } 

   feature_vital_sign <- rbindlist(list(feature_vital_sign, data.table(
    var_name=c("earliest_date:", "latest_date:"), var_type=c(ifelse("earliest_date" %in% names(feature_vital_sign), 
    as.character(min(feature_vital_sign$earliest_date, na.rm=T)), 
    NA), ifelse("latest_date" %in% names(feature_vital_sign),as.character(max(feature_vital_sign$latest_date, na.rm=T)), NA)))), 
    fill=T)

  if(!(is.na(date_excl))) {
  feature_vital_sign <- rbindlist(list(feature_vital_sign, data.table(
    var_name=c(paste0("earliest_date_", date_excl_suffix, ":"), paste0("latest_date_", 
      date_excl_suffix, ":")), 
    var_type=c(as.character(min(feature_vital_sign[!(var_name %in% date_excl)]$earliest_date, na.rm=T)), 
      as.character(max(feature_vital_sign[!(var_name %in% date_excl)]$latest_date, na.rm=T))))),
    fill=T)
  } 

   if(date_restriction==TRUE) {
   feature_vital_sign <- rbindlist(list(feature_vital_sign, data.table(
    var_name=c(paste0("earliest_date_restricted_",date_restriction_range[[1]], "_", date_restriction_range[[2]]), 
      paste0("latest_date_restricted_",date_restriction_range[[1]], "_", date_restriction_range[[2]])), 
      var_type=c(as.character(min(feature_vital_sign$earliest_date_restricted, na.rm=T)), 
     as.character(max(feature_vital_sign$latest_date_restricted, na.rm=T))))),fill=T)
   } 


   feature_vital_sign <- rbindlist(list(feature_vital_sign, data.table(
    var_name=c("var_count:", "% of observations missing:"), var_type=c(ncol(DT), perc(sum(sapply(DT_temp, 
    function(x) sum(is.na(x)))),(nrow(DT_temp)*ncol(DT_temp)))))), fill=T)

   feature_vital_sign <- rbindlist(list(feature_vital_sign, data.table(
    var_name=c("", "", ""), var_type=c("", "", ""))),fill=T)

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

 set_na_zero <- function(dt, replace=0) {
  for (j in seq_len(ncol(dt)))
    set(dt, which(is.na(dt[[j]]) | dt[[j]] %in% c(-Inf, +Inf) ), j, replace)
 }

 set_na_zero(feature_vital_sign, replace="")

 return(feature_vital_sign)

}



################################################################################
## set_zero_na & set_na_zero & set_missing_na & set_var_na
###############################################################################

################################################################################
###  set_zero_na

set_zero_na <- function(dt, replace=NA) {

  # purpose: 
  # replace (in place) zeros in DT with another value (e.g. NA)

  # arguments:
  #  dt: name of DT
  # replace:value with which to replace 0s (default=NA) 

  # returns:
  # modified data.table (modified in place)


  for (j in seq_len(ncol(dt)))
    set(dt, which(dt[[j]] %in% c(0)), j, replace)
}

################################################################################
###  set_na_zero

set_na_zero <- function(dt, replace=0, subset_col=names(dt)) {

  # purpose: 
  # replace (in place) NAs/+inf/-inf in DT with another value (e.g. 0s)

  # arguments:
  #  dt: name of DT
  #  replace:value with which to replace Nas/+inf/-inf (default=0) 

  # returns:
  # modified data.table (modified in place)

  for (j in which(names(dt) %in% subset_col))
    set(dt, which(is.na(dt[[j]]) | dt[[j]] %in% c(-Inf, +Inf) ), j, replace)

}



################################################################################
###  set_missing_na

set_missing_na <- function(dt, replace=NA, subset_col=names(dt)) {

  # purpose: 
  # replace (in place) empty values ("[ ]*" or "") in DT with another value (e.g. NA)

  # arguments:
  #  dt: name of DT
  #  replace: value with which to replace empty values (default=NA) 

  # returns:
  # modified data.table (modified in place)

  for (j in which(names(dt) %in% subset_col))
    set(dt, which(gsub("[ ]*", "", dt[[j]])==""), j, replace)
}

################################################################################
###  set_var_na

set_var_na <- function(dt, to_replace=NA, to_replace_partial=NA, replace=NA) {


  # purpose: 
  # replace values (complete and partial matches) in DT with another value

  # arguments:
  #  dt: name of DT
  #  to_replace: character vector of values - elements in the dt 
  #              are sequentially matched against each element in the vector and
  #              are replaced if there is a complete match (default: NA - no replacement)
  #  to_replace_partial: value - elements in the dt 
  #              are matched against this value and
  #              are replaced if they contain the value (partial match) (default: NA - no replacement)
  #  replace: value with which to replace matched/partially matched values (default=NA) 

  # returns:
  # modified data.table (modified in place)


  if (sum(!is.na(to_replace))>0) {
    for (j in seq_len(ncol(dt)))
      {
        set(dt, which(dt[[j]] %in% to_replace), j, replace)
      }
  }
  if (sum(!is.na(to_replace_partial))>0) {
    for (j in seq_len(ncol(dt)))
      {
        set(dt, which(dt[[j]] %like% to_replace_partial), j, replace)
      }
  }

}


################################################################################
## add_alpha
################################################################################


add_alpha <- function(col, alpha=1){

  # purpose: 
  # add an alpha specification to a hex/named color/color vectors

  # arguments:
  #  col: name/vector of hex/named col
  #  alpha: alpha value (default:1)

  # returns:
  # modified col/col vector

  # sample usage:
  # col_vect=add_alpha(colorRampPalette(brewer.pal(10, "Set3"))(15), 0.8)


  if(missing(col)) stop("Please provide a vector of colors.")
  
  apply(sapply(col, col2rgb)/255, 2, function(x) 
    rgb(x[1], x[2], x[3], alpha=alpha)) 

 }


################################################################################
## my.merge
################################################################################

mymerge <- function(x,y, var_list=cohort_key_var) {

  # purpose: 
  # merge list of data.tables on a given set of variables 

  # arguments:
  #  x,y: name of DT
  #  by: character vector with names of variables on which to merge 

  # returns:
  # merged DT

  # sample usage:
  #  Reduce(mymerge, list of data tables)

  merge(x,y, all = TRUE, by=var_list)

}

################################################################################
## parse_date and detect_date and auto_format and parse_time
################################################################################

################################################################################
### parse_date
parse_date <- function(dt_vector, column_name_list,column_name_list_new=column_name_list) {
  
  # purpose: 
  # automate detection and standardization of date variables - return verification output 
  # and standardized dates (IDate)

  # arguments:
  # dt_vector: DT or character vector

  #  below options only apply if the input is a DT
  #  column_name_list: list of names of date columns to be parsed
  #  column_name_list_new: list of names of new (parsed) date columns to be generated (default to 
  #                        names of original columns, i.e.replace existing date columns)

  # returns:
  # DT with formatted dates

  if (class(dt_vector)=="data.table") {

    DT <- dt_vector

    lapply(column_name_list, function(x) print(head(DT[!is.na(get(x)) & 
      as.character(get(x))!="" , get(x)],50)))
    lapply(column_name_list, function(x) DT[, c(x):=as.Date(parse_date_time(get(x), 
     c("mdy","ymd","dby","myd", "dmy", "bdy")))])
    lapply(column_name_list, function(x) print(head(DT[!is.na(get(x)), get(x)],50)))
      setnames(DT, column_name_list, column_name_list_new)
    non_date <- names(DT)[sapply(DT, function(x) sum(is.na(x))==nrow(DT))]

    if(length(non_date)>0){
      DT[, c(non_date):=lapply(.SD, function(x) as.character(x)), .SDcols=non_date]
    }
    
    print("-----------------")
    print("following columns not converted to a date:")
    print(non_date)
    print("-----------------")

  } 

    if (class(dt_vector)=="character") {

    string <- dt_vector

    print(string[1:50])
    string <- as.Date(parse_date_time(string, 
      c("mdy","ymd","dby","myd", "dmy", "bdy")))
    print(string[1:50])

  } 

}

# ################################################################################
# ### detect_date
# detect_date <- function(DT) {
  
#   # purpose: 
#   # detect which columns in a DT are date columns 

#   # arguments:
#   # DT: DT

#   # returns:
#   # name of date columns

#   ## note - 1st section - purpose: reduce number of columns to be considered based on the condition
#   ## that (1) start with integer and (2) contain non-numeric characters
#   DT_temp <- copy(DT)
#   non_all_numeric <- names(DT_temp)[sapply(DT_temp, function(x) {sum(x %like% "[-/ ]")>0})]
#   DT_temp[, names(DT_temp):=lapply(.SD, function(x) gsub("(.{1})(.*)", "\\1", x))]
#   all_zero <- names(DT_temp)[sapply(DT_temp, function(x) sum(is.na(x))<nrow(DT_temp))]
#   numeric_start <- names(DT_temp[, mget(all_zero)])[sapply(DT_temp[, mget(all_zero)], function(x) 
#     sum(is.na(as.integer(x[!is.na(x)])))==0)]
#   date_pot <- intersect(numeric_start,non_all_numeric )

#   ## for potential columns - see if date format can be detected
#   DT_temp <- copy(DT)
#   DT_temp <- DT_temp[, mget(date_pot)]
#   DT_temp[,c(date_pot ):=sapply(lapply(.SD, function(x) tstrsplit(x, " ")), "[[",1, simplify=F)]
#   parse_date(DT_temp, date_pot)
#   date_var <- names(DT_temp)[sapply(DT_temp, function(x) class(x)=="Date")]

#   ## sensibility check - mean(year) in range 1900-2020
#   date_var <-names(DT_temp[, mget(date_var)])[sapply(DT_temp[, mget(date_var)], function(x) 
#     (in_between(mean(year(x), na.rm=T), 1990, 2020)))]
  
#   return(date_var)

# }

# ################################################################################
# ## auto_format

# auto_format <- function(DT, date_var=c(), character_varc=c(), integer_var=c(), 
#   numeric_var=c()) {

#   # default format 
#   ## convert factors -> character
#   factor_col <- names(which(sapply(DT, class)=="factor"))
#   if (length(factor_col)>0) {
#     DT[, c(factor_col):=lapply(.SD, function(x) as.character(x)), .SDcols=c(factor_col)]
#   }

#   ## convert logical -> integer
#   logical_col <- names(which(sapply(DT, class)=="logical"))
#   if (length(logical_col)>0) {
#     DT[, c(logical_col):=lapply(.SD, function(x) as.integer(x)), .SDcols=c(logical_col)]
#   }

#   # date variables
#   # autorecognition of dates
#   date_var_auto <- detect_date(DT)
#   date_var <- c(date_var_auto,date_var )

#   ## given & auto dates
#   if (length(date_var)>0) {
#     for (i in date_var) {
#       print(i)
#       if(max(sapply(strsplit(DT[, get(i)], " "), length))>1) {
#         DT[, c(i, paste0(i, "_time_created")):=tstrsplit(get(i), " ")]
#       }
#     }
#     parse_date(DT, date_var)
#     DT[,c(date_var):=lapply(.SD, function(x) as.IDate(x, "%Y-%m-%d")), .SDcols=c(date_var)]
#   }

#   # character variables
#   if (length(character_var)>0) {
#     DT[,c(character_var):=lapply(.SD, function(x) as.character(x)), .SDcols=c(character_var)]
#   }

#   # integer variables
#   if (length(integer_var)>0) {
#    DT[,c(integer_var):=lapply(.SD, function(x) as.integer(x)), .SDcols=c(integer_var)]
#   }

#   # numeric variables
#   if (length(numeric_var)>0) {
#     DT[,c(numeric_var):=lapply(.SD, function(x) as.numeric(x)), .SDcols=c(numeric_var)]
#   }

# }

################################################################################
### parse_time

parse_time <- function(DT, column_name_list,column_name_list_new=column_name_list) {
  
  # purpose: 
  # automate detection and standardization of time variables - return verification output 
  # and standardized times(character)

  # arguments:
  #  DT: DT
  #  column_name_list: list of names of time columns to be parsed
  #  column_name_list_new: list of names of new (parsed) time columns to be generated (default to 
  #                        names of original columns, i.e.replace existing time columns)

  # returns:
  # DT with formatted time
  
  lapply(column_name_list, function(x) print(head(DT[!is.na(get(x)) & 
    as.character(get(x))!="" , get(x)],50)))
  lapply(column_name_list, function(x) DT[, c(x):=gsub("([^ ]*)( )(.*)", "\\3", 
    as.character(parse_date_time(get(x), c("hm", "hms", "%I:%M:%S %p"))))])
  lapply(column_name_list, function(x) print(head(DT[!is.na(get(x)), get(x)],50)))
  setnames(DT, column_name_list, column_name_list_new)

}
  
  
################################################################################
## shorten_label
################################################################################

shorten_label <- function(DT, colname_list=NA, max_length=20, beg_fraction=0.8, 
  in_place=TRUE, new_colname_list=NA) {

  # purpose: 
  # shorten a column of labels (e.g.medication names - may want to use as feature names/as graph labels)
  # by combining characters from the beginning and end of the label (separated by __) to generate
  # a label of a fixed max character length
  # note: function can also be used to shorten the elements of a character vector (in this case the vector is replaced/
  # a new vector with the name given by new_colname_list is created)

  # arguments:
  #  DT: DT (or character vector - cannot be defined in function, i.e. needs to be an existing vector object)
  #  colname_list: list of column names which are to be processed (NA if processing a vector)
  #  max_length: max character length of shortened labels (default: 20)
  #  beg_fraction: number of characters to to be kept from the label's beginning (as a % of 
  #                max_length characters)  (default: 0.8)
  #  in_place: modify in place,i.e. replace original labels (default: true)
  #  new_colname_list: list of new column names to be generated if in_place==False (name of new vector object 
  #                    to be created if parsing a vector) (default: NA)

  # returns:
  # DT with formatted time

  # sample usage:
  #  shorten_label(clinic_xwalk, list("clinic_name"))

  if(class(DT)=="character")  {
    vector_name <- deparse(substitute(DT))
    DT <- data.table(x=DT)
    colname_list <- list("x")
    data_table_check <- FALSE
  } else if(class(DT)=="data.table") {
    data_table_check <- TRUE
  }

  for (i in 1:length(colname_list)) {

    # define parameters based on specification
    colname <- colname_list[[i]]
    length <- nchar(DT[, get(colname)])
    cutoff <- round(beg_fraction*max_length, 0)
    end <- max_length-cutoff

    # shorten
    DT[, paste0(colname, "_1"):=ifelse(nchar(get(colname))>max_length, 
      substring(get(colname),1,cutoff), get(colname))]
    DT[, paste0(  colname, "_2"):=ifelse(nchar(get(colname))>max_length, 
      substring(get(colname),length - end,length), get(colname))] 
    DT[, c(paste0(colname, "_new")):=ifelse(nchar(get(colname))>max_length, 
      do.call(function(...) paste(...,sep="__"), .SD[,mget(c(paste0(colname, "_1"), 
      paste0(colname, "_2")))]), get(colname))]
    DT[, c(paste0(colname, "_new")):=gsub("[_]{3,}", "__", get(paste0(colname, "_new")))]

    DT[, c(paste0(colname, "_1"), paste0(colname, "_2")):=NULL]

    if(data_table_check==TRUE) {
      # update in place or return new variable
      if(in_place==TRUE) {
        DT[, c(colname):=NULL]
        setnames(DT, paste0(colname, "_new"), colname)
      } else {
        setnames(DT, paste0(colname, "_new"), new_colname_list[[i]])
      }
    } else {
     if(in_place==TRUE) {
        assign(vector_name, DT$x_new, sys.frame(sys.parent(n=1)))
      } else {
        assign(new_colname_list[[i]], DT$x_new,sys.frame(sys.parent(n=1)))
      }
    }

  }  
}

################################################################################
## chunkify and chunkify_group
################################################################################

################################################################################
### chunkify

chunkify <- function(dt, chunksize) {

  # purpose: 
  # chunkify a data.table into a list of data.tables (allows for the parallelization of
  # operations using commands from the apply family)

  # arguments:
  #  dt: DT
  #  chunksize: (max) rows per chunk
 
  # returns:
  # list of smaller dt

  # sample usage:
  #  test_list <- chunkify(test, 500)

    rows <- nrow(dt)
    chunksize <- as.numeric(chunksize)
    chunks <- 1:ceiling(rows/chunksize)
    chunkid <- unlist(lapply(chunks, function(i) rep(chunks[[i]], chunksize)))
    chunkid <- chunkid[1:rows]
    dt2 <- dt[, chunkid := chunkid]
    out <- split(dt2, dt2$chunkid)
    dt[, chunkid:=NULL]
    return(out)
}






################################################################################
### chunkify_group

chunkify_group <- function(dt, group_var, chunk_num) {

  # purpose: 
  # chunkify a data.table into a list of data.tables (allows for the parallelization of
  # operations using commands from the apply family) preserving the organization by a given grouping variable,e.g. 
  # ensure that all observations relating to a certain patient (empi) are contained within one chunk

  # arguments:
  #  dt: DT
  #  group_var: name of grouping variable
  #  chunk_num: number of` chunks to be generated
 
  # returns:
  # list of smaller dt

    rows_group <- nrow(unique(dt, by=c(group_var)))
    chunk_num <- as.numeric(chunk_num)
    chunksize <- ceiling(rows_group/chunk_num)

    chunks <- 1:chunk_num

    chunkid <- unlist(lapply(chunks, function(i) rep(chunks[[i]], chunksize)))
    chunkid <- chunkid[1:rows_group]

    dt2 <- dt[dt[, .I[1], by=c(group_var)]$V1, chunk_id := chunkid]
    dt2[, chunk_id:=max(chunk_id, na.rm=T), by=c(group_var)]

    out <- split(dt2, dt2$chunk_id)

    return(out)
}

################################################################################
## small helper functions
################################################################################

################################################################################
### perc

perc <- function(num, denom, digit=1) {

  # purpose: 
  # convert proportion (numerator, denominator) into formatted percentage value

  # arguments:
  #  num: numerator
  #  denom: denominator
  #  digit: number of digits to round to
 
  # returns:
  # percentage value

  round((num/denom)*100, digit)

}

################################################################################
### inv_lapply

inv_lapply <- function(X, FUN,...) {

  # purpose: 
  # quiet lapply,i.e.suppress output (to be used when modifying in place)

  # arguments:
  #  normal lapply arguments
 
  # sample usage
  #  inv_lapply(list("aa","ba"),function(x) gsub("a","",x))

  invisible(lapply(X, FUN,...))

}

################################################################################
### inv_mapply

inv_mapply <- function(FUN,...) {

  # purpose: 
  # quiet mapply,i.e.suppress output (to be used when modifying in place)

  # arguments:
  #  normal mapply arguments
 
  # sample usage
  #  inv_mapply(list("aa","ba"),function(x) gsub("a","",x))

  invisible(mapply(FUN,...))

}


################################################################################
### table_mod
table_mod <- function(x) {

  # purpose: 
  # ordered table command

  table(x, useNA="always")[order(-table(x, useNA="always"))]
}

################################################################################
### table_df

table_df <- function(x, orientation="vert", round_digit=1 ) {

  # purpose: 
  # convert table to df

  if(orientation=="hor") {

    if(ncol(as.data.frame(x)>1)) {

      temp <- setNames(data.table(t(as.data.frame(x)[,-1])), 
         as.character(as.data.frame(x)[,1]))
      return(temp)
    } 

  }

  if(orientation=="vert") {

    
  if(ncol(as.data.frame(x))>1) {

      temp <- data.table((as.data.frame(x)))
      setnames(temp, c("var_name","freq"))
      temp[, freq:=round(freq, digit=round_digit)]
      return(temp)
         
    } 

    if(ncol(as.data.frame(x))==1) {

      temp <- data.table((as.data.frame(x)))
      temp$var_name <- as.character(row.names(as.data.frame(x)))
      setnames(temp, "x","freq")
      setcolorder(temp, c("var_name", "freq"))
      temp[, freq:=round(freq, digit=round_digit)]
      return(temp)
         
    } 

  }


}

################################################################################
### in_between

ps <- function(char_string, ...) {

  # purpose: 
  # print(sprintf(.....))

     print(sprintf(char_string, ...))
}


catps <- function(char_string, ...) {

  # purpose: 
  # cat("\n") cat(...) cat("\n")

     cat("\n")
     cat(sprintf(char_string, ...))
      cat("\n")
}


catpss <- function(char_string, ...) {

  # purpose: 
  # cat("\n") cat(...) cat("\n")

     cat("\n")
     cat(sprintf(char_string, ...))
     cat("\n")
     cat("\n")
}

################################################################################
### in_between

in_between <- function(x, min, max, equal_to=TRUE) {

  # purpose: 
  # check if x lies between two values (integer or numeric)

  # arguments:
  # x, min, max 
  # equal_to: whether to include the end points of the range (default: TRUE)
 
  if (equal_to==TRUE) {
    if (x>=min & x<=max) {
      return(TRUE) 
    } else {
      return(FALSE)
    }
  } else if (equal_to==FALSE) {
    if (x>min & x<max) {
      return(TRUE) 
    } else {
      return(FALSE)
    }
  }
}


################################################################################
## pad
################################################################################

pad <- function(topad, num, method="front", replace="0") {

  # purpose: 
  # pad elements of list with 0s to bring all elements to a fixed length

  # arguments:
  #  topad: list of elements to pad
  #  num: final length (characters)
  #  method: pad - front or back (default="front")
 
  # return:
  #  padded list


  topad <- as.character(topad)
  fixed <-
    unlist(
    lapply(topad, function(x) {
     if(!is.na(x)) {
       lack <- num - nchar(x)
        if(lack > 0) {
          sub <- paste0(rep(replace, lack), collapse = "")
          if (method=="front") {
            x <- paste0(sub, x)
          }
          if (method=="back") {
            x <- paste0(x, sub)
          }
        } else {
          x <- x
        }
    } else {
      x <- x
    }
      return(x)
    }
  ))

  return(fixed)

}


################################################################################
## xtile and xtile_outlier
################################################################################

################################################################################
### xtile

xtile <- function(x, num) {

  # purpose: 
  # bin elements of list 

  # arguments:
  #  x: list of elements
  #  num: number of bins 
 
  # return:
  #  list of bin assignments


  .bincode(x, sort(quantile(x, (0:num)/num)), include.lowest = TRUE)

}

################################################################################
### xtile_outlier
 
xtile_outlier <- function(x, prob=c(0, 0.001,0.999,1)) {

  # purpose: 
  # bin elements of list into non-outliers and outliers based on distribution (top/bottom percentiles)

  # arguments:
  #  x: list of elements
  #  prob: cutoffs (0, lower_tail, upper_tail, 1) (default: c(0, 0.001,0.999,1))
 
  # return:
  #  list of outliers/non_outlier assignments

  .bincode(x, sort(quantile(x, prob)), include.lowest = TRUE)

}

################################################################################
## text_print
################################################################################

text_print <- function(file_path) {
  
  # purpose: read in txt file and print formatted output to the terminal 
  
  text <- suppressWarnings(readLines(file_path))
  text <- paste0(text, collapse="\n")

  cat(text)

}

################################################################################
## store_shorten_file
################################################################################

store_shorten_file <- function(DT, test_row) {

  # purpose: 
  # save copy of DT and replace the DT itself with a shortened version of itself
  # (useful when debugging scripts - run script with subset of data without changing file names)


  # arguments:
  #  DT: name of DT
  #  test_row: number of rows to keep in shortened table
 
  # return:
  #  original DT (name_copy) & shortened table (name) are stored in global environment

  # sample usage:
  #  store_shorten_file("test")

  assign(paste0(DT, "_copy"), copy(get(DT,sys.frame(sys.parent(n=1)))),sys.frame(sys.parent(n=1)))
  assign(DT, get(DT,sys.frame(sys.parent(n=1)))[1:test_row], sys.frame(sys.parent(n=1)))

}

################################################################################
## return_mult
################################################################################

return_mult <- structure(NA,class="result")
"[<-.result" <- function(x,...,value) {

  # purpose: 
  # store multiple arguments returned from function as separate objects

  # arguments:
 
  # return:
  #  multiple arguments returned from function

  # sample usage:
  #  within function_test: return(list(obj_1,obj_2, obj_3))
  #  outside of function: return_mult[obj_1, obj_2, obj_3] <- function_test()

   args <- as.list(match.call())
   args <- args[-c(1:2,length(args))]
   length(value) <- length(args)
   for(i in seq(along=args)) {
     a <- args[[i]]
     if(!missing(a)) eval.parent(substitute(a <- v,list(a=a,v=value[[i]])))
   }
   x
}

################################################################################
## replace_mult
################################################################################

replace_mult <- function(text_vec, pattern_list, remove_space=TRUE, reorder_length=TRUE, 
  replace=" ") {

  # purpose: 
  # replace multiple patterns at the same time, i.e. sequentially

  # arguments:
  #  text_vec: vector to modify
  #  pattern_list: list of patterns to replace 
  #  remove_space: if true - remove leading/trailing and repeated spaces post pattern replacement (default:true)
  #  reorder_length: if true - order patterns by length,i.e.1st find and replace longest 
  #                  pattern, then 2nd longest (default:true)
  #  replace: value to replace patterns with (default: true)

  # return:
  #  modified character field/character vector

  # sort by length - replace longest patterns 1st
  if(reorder_length==TRUE) {
    pattern_list <- pattern_list[order(-nchar(pattern_list))]
  }

  # replace
  for(i in pattern_list) {
    text_vec <- gsub(i, replace, text_vec)
  }

  # spaces
  if(remove_space==TRUE) {
    text_vec <- gsub("[ ]{2,}", " ", text_vec)
    text_vec <- gsub("[_]{2,}", " ", text_vec)

    text_vec <- gsub("(^[ _]*)(.*)", "\\2", text_vec)   
    text_vec <- gsub("(.*)([ _]*$)", "\\1", text_vec)
  }

  return(text_vec)
 
}


################################################################################
## basic_formatting
################################################################################

basic_formatting <- function(DT, tolower_col=NA, integer_col=NA) {

  # purpose: 
  # basic formatting of DT ((a) lowercase column names (b) lowercase selected columns
  # (c)turn into integers selected columns (d) remove leading/trailing white spaces and
  # (e) replace empty fields with NA (set.missing.na() function))

  # arguments:
  #  DT: DT
  #  tolower_col:columns to be turned into lowercase (list of names)
  #  integer_col:columns to be turned into integers(list of names)
 
  # return:
  #  modified DT


  # set_missing_na
  set_missing_na <- function(dt, replace=NA) {

    for (j in seq_len(ncol(dt)))
     set(dt, which(gsub("[ ]*", "", dt[[j]])==""), j, replace)

  }

  # column names
  setnames(DT, tolower(names(DT)))
  tolower_col=tolower(tolower_col)
  integer_col=tolower(integer_col)

  # remove leading and trailing spaces & "" fields -> NA
  DT[, names(DT):=lapply(.SD, function(x) gsub("^[ ]*|[ ]*$", "", x))]
  set.missing.na(DT)

  # lowercase  (tolower_case col)
  if(sum(!is.na(tolower_col))>0) {DT[, c(tolower_col):=lapply(.SD, function(x) tolower(x)), 
    .SDcols=tolower_col]}

  # integer col  (integer_col) - scientific notation to integer
  if(sum(!is.na(integer_col))>0) {DT[, c(integer_col):=lapply(.SD, function(x) as.integer(x)), 
    .SDcols=integer_col]}

}


################################################################################
## generate_pdf
################################################################################

generate_pdf <- function(plot_list, graph_cat=length(plot_list), ncol_plot, nrow_plot, 
  file_path, file_name, orientation="vertical", share_legend=FALSE) {

  # purpose: 
  # turn list of ggplots into a vertical/horizontal multi page PDF (multiple plots per page) with 
  # separate or shared legends

  # arguments:
  #  plot_list: list of ggplots
  #  graph_cat: number of ggplots (default:length of plot list)
  #  ncol_plot: number of graph columns on each page
  #  nrow_plot: number of graph rows on each page
  #  file_path: path to folder in which to store final PDF
  #  file_name: filename of final PDF
  #  orientation: horizontal vs.vertical PDF (default: vertical)
  #  share_legend: if true - share legend across all graphs on a given page

  # return:
  #  save generated PDF

  grid_arrange_shared_legend <- function(...) {
    plots <- list(...)
    g <- ggplotGrob(plots[[1]] + theme(legend.position="bottom"))$grobs
      legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
    lheight <- sum(legend$height)
    grid.arrange(arrangeGrob(grobs=lapply(plots, function(x)
      x + theme(legend.position="none")),ncol = ncol_plot, nrow=nrow_plot),
      legend,
      ncol = 1,
      heights = unit.c(unit(1, "npc") - lheight, lheight))
  
 }

  graph_count <- nrow_plot*ncol_plot
  graph_page <- ceiling(graph_cat/(graph_count))

  temp1 <- 1
  temp2 <- graph_count

  temp_folder_plot <- paste0("temp_plot_folder_", as.character(format(Sys.time(), "%H_%M_%S_%d_%m_%y")), 
    "_", toupper(sample(letters, 1)))
  # print(temp_folder_plot)

  dir.create(temp_folder_plot)

  for (k in 1:graph_page) {
   if(orientation=="vertical") pdf(paste0(temp_folder_plot, "/", file_name, "_", k, ".pdf")) 
     else pdf(paste0(temp_folder_plot, "/", file_name, "_", k, ".pdf"), height=7.6, width=11)

   temp2 <- min(temp2, length(plot_list))

   onepage <- plot_list[temp1:temp2]

   if(share_legend==FALSE) {
    height_value <- 7.6/(ncol_plot+0.5)
    do.call(grid.arrange,c(onepage, list(ncol=ncol_plot, heights=rep(height_value,ncol_plot))))
   } else {
     do.call(grid_arrange_shared_legend,c(onepage))
   }

   dev.off()
   temp1 <- temp1 + graph_count
   temp2 <- temp2 + graph_count
   print(paste0("successfully generated page ", k, " out of ", graph_page))
  
  }

  plotflow:::mergePDF(in.file=paste(file.path(temp_folder_plot, dir(temp_folder_plot)), 
    collapse=" "),file=paste0(file_path, file_name, "_combined.pdf"))

  print(unlink(temp_folder_plot, recursive=TRUE))

}


################################################################################
## merge_pdf
################################################################################

merge_pdf <- function(input_file_list, output_file_name) {
 
    plotflow:::mergePDF(in.file=paste(input_file_list, 
        collapse=" "),file=output_file_name)

    ps("succesfully merged pdf: %s", output_file_name)
}

################################################################################
## plot_marginal
################################################################################

plot_marginal <-  function(main_plot, hor_plot, vert_plot=NA, 
  layout_width=c(4, 0.5), 
  layout_height=c(0.5, 0.5, 2.5), plot_count=2, title_obj="", subtitle_obj="") {
  
  # purpose: 
  # combine one or two plots (e.g.histograms) with main plot (e.g.heatmap) to generate e.g. 
  # a heatmap with a horizontal and/or vertical marginal plot

  # arguments:
  #  main_plot: ggplot object - main graph
  #  hor_plot: ggplot object - horizontal,marginal plot -- will be added at top of graph
  #  vert_plot: ggplot object - vertical,marginal plot -- will be added on right hand side of graph
  #  layout_width: width allocation (i) main plot (ii) marginal plot
  #  layout_height:  height allocation (i) title (ii) top marginal plot (iii) main plot 
  #  plot_count: number of marginal plots (default:2)

  # return:
  #  save generated plot


  if (plot_count==2) {

  plot_1 <- ggplot_gtable(ggplot_build(main_plot))
  plot_2 <- ggplot_gtable(ggplot_build(hor_plot))
  plot_3 <- ggplot_gtable(ggplot_build(vert_plot))

  # Get maximum widths and heights
  maxWidth <- unit.pmax(plot_1$widths[2:3], plot_2$widths[2:3])
  maxHeight <- unit.pmax(plot_1$heights[4:5], plot_3$heights[4:5])

  # Set the maximums in the gtables for gt1, gt2 and gt3
  plot_1$widths[2:3] <- as.list(maxWidth)
  plot_2$widths[2:3] <- as.list(maxWidth)

  plot_1$heights[4:5] <- as.list(maxHeight)
  plot_3$heights[4:5] <- as.list(maxHeight)

  # Create a new gtable
  final_plot <- gtable(widths = unit(layout_width, "null"), 
    height = unit(layout_height, "null"))

  # Insert gt1, gt2 and gt3 into the new gtable
  final_plot <- gtable_add_grob(final_plot, plot_1, 3, 1)
  final_plot <- gtable_add_grob(final_plot, plot_2, 2, 1)
  final_plot <- gtable_add_grob(final_plot, plot_3, 3, 2)

  return(final_plot)

}

if (plot_count==1) {


  plot_1 <- ggplot_gtable(ggplot_build(main_plot))
  plot_2 <- ggplot_gtable(ggplot_build(hor_plot))

  # get max widths and heights
  maxWidth  <- unit.pmax(plot_1$widths[2:3], plot_2$widths[2:3])
  maxHeight <- unit.pmax(plot_1$heights[4:5], plot_1$heights[4:5])

  # Set the maximums in the gtables for gt1, gt2 and gt3
  plot_1$widths[2:3] <- as.list(maxWidth)
  plot_2$widths[2:3] <- as.list(maxWidth)

  plot_1$heights[4:5] <- as.list(maxHeight)
  
  # Create a new gtable

  title <- textGrob(title_obj, gp = gpar(fontsize = 10, fontfamily="URWHelvetica"), 
    just="left")
  subtitle <- textGrob(subtitle_obj, gp = gpar(fontsize = 5, fontfamily="URWHelvetica", 
    fontface="italic"), just="left")

  # final_plot <- gtable(widths=unit(rep(0.5,12), "null"),
  #              heights=unit(rep(0.5,15), "null"))
  
  # gtable_add_grobs <- gtable_add_grob # alias
  
  # final_plot <- gtable_add_grobs(final_plot, list(title, plot_2, plot_1), 
  #                        l=c(1,3,3),
  #                        r=c(8,12,12),
  #                        t=c(1,2,4),
  #                        b=c(1,3,15))
  
  final_plot <- gtable(widths = unit(layout_width, "null"), 
    height = unit(layout_height, "null"))

  # Insert gt1, gt2 and gt3 into the new gtable
  final_plot <- gtable_add_grob(final_plot, plot_1, 3, 1)
  final_plot <- gtable_add_grob(final_plot, plot_2, 2, 1)
  # final_plot <- gtable_add_grob(final_plot, subtitle, 2, 1)
  # final_plot <- gtable_add_grob(final_plot, title, 1, 1)

  return(final_plot)


 }
}


################################################################################
## parse_arg
################################################################################


parse_arg <- function(x) {


  # purpose: 
  # interactive mode - debugging a function -> wish to step into and execute line 
  # by line. Function to quickly evaluate the parameter assignment contained within 
  # the function call
 
  # arguments:
  #  x: quote([function call])

  # sample usage:
  #  parse_arg(quote(targex_chunk(arg1="xxxx", arg2=object_name, arg3="xxxx")))


  command_text <- gsub("[ ]{2,}"," ", paste0(deparse(x), collapse=""))


  setting <- as.data.table(strsplit(command_text, "="))

  ## deal with unnamed arguments
  setting[gsub("[^]\\[\\(),]","", V1) %like% "[^(\\(\\[)](,){2,}", V1:=gsub("([^,]*),.*,([^,]*)","\\1,\\2",V1)]

  setting[, ':='(arg1=gsub("(.*),([^,]*)", "\\1",V1), arg2=(gsub("(.*),([^,]*)",
   "\\2",V1)))]
  setting[, arg2:=shift(arg2, 1, "lead")]
  setting[2,arg2:=gsub(".*\\(", "", arg2)]
    setting[nrow(setting),arg1:=gsub("\\)", "", arg1)]
    setting[, command_arg:=paste0(arg2, "<-", arg1)]

  inv_lapply(setting$command_arg[-1], function(x) eval(parse(text=x), envir=sys.frame(sys.parent(n=4))))

}

################################################################################
## write_append_csv
################################################################################


write_append_csv <- function(element_list, file_path) {

  # purpose: 
  # paste multiple tables/character vectors (i.e. titles) into the same csv file 
  # (underneath each other - separated by an empty row)
  
  # arguments:
  #  element_list: list of tables/character strings to paste
  #  file_path: path of file in which to save output

  # return:
  #  save combined tables/character strings

  write.table(element_list[1], file_path, sep=",", row.names=F, col.names= rep(" ", length(element_list[1][[1]])))
  # write.table(" ", file_path, append=T,sep=",", row.names=F, col.names = "")

  for (i in 2:(length(element_list))) {
      if (class(element_list[i][[1]]) == "character") {
       write.table(element_list[i], file_path, append=T,sep=",", row.names=F, col.names = "")     
      } else {
       write.table(element_list[i], file_path, append=T,sep=",", row.names=F)
      }
      # write.table(" ", file_path, append=T,sep=",", row.names=F, col.names = "")
  }

}

################################################################################
## print_memory
################################################################################


print_memory <- function(namespace=ls(parent.env(environment()))) {
   
  # purpose: 
  # print table with memory usage of objects in namespace
  
   mem_table <- data.table(var_name=namespace, 
    mem_MB=sapply(namespace, function(x) {object.size(get(x))/1000000}))

   setorder(mem_table, -mem_MB)
   

   print(head(mem_table, 25))
}



################################################################################
## list_function
################################################################################

list_function <- function (filename, alphabetic = TRUE) {

  # adapted from NCmisc package

  # purpose: 
  # list (i.e. print) all functions used in an R  script (organised by package) 
  # note list only fucntions used in script itself, i.e does noe reflect functions used in sourced scripts

  # arguments:
  # filename: path to file


    if (!file.exists(filename)) {
        stop("couldn't find file ", filename)
    }
    if (!get.ext(filename) == "R") {
        warning("expecting *.R file, will try to proceed")
    }
    tmp <- getParseData(parse(filename, keep.source = TRUE))
    crit <- quote(token == "SYMBOL_FUNCTION_CALL")
    tmp <- dplyr::filter(tmp, crit)
    tmp <- unique(if (alphabetic) {
        sort(tmp$text)
    }
    else {
        tmp$text
    })
    src <- paste(as.vector(sapply(tmp, find)))
    outlist <- tapply(tmp, factor(src), c)
    outlist_custom <- unlist(outlist[grep("package", names(outlist), value=T,
     invert=T)])
    names(outlist_custom) <- NULL
    outlist <- outlist[grep("package", names(outlist), value=T)]
    outlist$custom <- outlist_custom
    outlist <- outlist[c("custom", setdiff(names(outlist), "custom"))]
    
    print(outlist)
}

################################################################################
## meta_data
################################################################################


################################################################################
## progress_bar
################################################################################
## loop.tracker

################################################################################
## write.xlsx_safe and removeSheet_save
################################################################################

################################################################################
### write.xlsx_safe

write.xlsx_safe <- function(dt, file, sheetName, lock=FALSE, password="", 
  unlock_col=NA, col_width=NA, alignment_var="left") {

  # purpose: 
  # write.xlsx (xlsx library) vs. (a) default to append=FALSE, i.e. requires workbook 
  # to be created prior to execution (minimise risk of overwriting) and (b) set default settings, e.g. 
  # row.names=F and (c) allows for the writing of write protected sheets

  # arguments:
  #  see the write.xlsx documentation

  # note
  # sample col width specification: list(list(c(1,2),30), list(2,20), list(3,2))

    if (!(file.exists(file))) {
      print("wb does not exist - cannot use write.xlsx_safe")
      break()
    }

    wb = loadWorkbook(file)
    sheet = createSheet(wb, sheetName)
    addDataFrame(dt,sheet, row.names=FALSE, col.names=TRUE)

    if (!is.na(col_width)) {

      lapply(col_width, function(x) {
        lapply(x[[1]], function(y) {
             if(unlist(x[2])=="auto") {
              autoSizeColumn(sheet, colIndex=y)
             } else {
              setColumnWidth(sheet, colIndex=y, colWidth=unlist(x[2]))
             }
          })
      })
    }

    if(lock==TRUE) {
       cs = CellStyle(wb, cellProtection = CellProtection(locked=T)) 
       if(!is.na(unlock_col)) {
        cs = CellStyle(wb, cellProtection = CellProtection(locked=F)) #setting style to unlock cells
        rows <- getRows(sheet, 1:1000)
        cells <- getCells(rows, colIndex = unlock_col) 
        lapply(names(cells), function(ii)setCellStyle(cells[[ii]],cs))
       }
      .jcall(sheet, "V", "protectSheet", password)
    
    }

    # if(alignment_var=="right") {
    #   align <- Alignment(horizontal="ALIGN_RIGHT")

    #    cs = CellStyle(wb, alignment= align) 
    # }

    saveWorkbook(wb, file)
}

################################################################################
### removeSheet_save 

removeSheet_save <- function(wb, file, sheetName) {

  # purpose: 
  # remove sheet from workbook and save the modified workbook
 
  # arguments:
  #  see the removeSheet and saveWorkbook documentation
     
     removeSheet(wb, sheetName)
     saveWorkbook(wb, file)
}

################################################################################
### read.xlsx_mod

read.xlsx_mod <- function(file, sheetName="Sheet1") {

  # purpose: 
  # read in sheet from xlsx workbook with most common settings
 
  # arguments:
  # see the read.xlsx documentation
     
  return(as.data.table(read.xlsx(file, sheetName, stringsAsFactors=F)))

}

################################################################################
### lapply utilities 

lapply2 <- function(one, FUN) {
  lapply(one, function(two) lapply(two, FUN))
}

lapply3 <- function(one, FUN) {
  lapply(one, function(two) lapply(two, function(three) lapply(three, FUN)))
}

lapply4 <- function(one, FUN) {
  lapply(one, function(two) lapply(two, function(three) lapply(three, function(four) 
    lapply(four, FUN))))
}



################################################################################
### text_print

text_print <- function(file_path) {
  text <- suppressWarnings(readLines(file_path))
  text <- paste0(text, collapse="\n")
  cat(text)
}


################################################################################
### jgc


jgc <- function() {

  # purpose: 
  # java garbage collection (helps resolve memory problems with xlsx package)

   gc()
  .jcall("java/lang/System", method = "gc")
}  


################################################################################
## sql_mod
################################################################################

sql_mod <- function(comment=sql_comment,connection=edw_connection){

  # purpose: 
  # execute sql query
 
  # arguments:
  # see the sqlQuery documentation

  return(as.data.table(sqlQuery(connection,gsub("\n", " ", comment), 
    stringsAsFactors=FALSE)))

}

################################################################################
## sql_mod_chunk
################################################################################

sql_mod_chunk <- function(sql_string, connection=edw_connection, pattern="%%[^%]*%%", chunk_row=5000) {

  # purpose: 
  # execute sql query & subset - chunkify/parallelize
 
  # arguments:
  # see the sqlQuery documentation


  selection_string <- gsub(paste0("(.*)(",pattern, ")(.*)"), "\\2",sql_string)

  if (!is.na(selection_string)) {

    selection_string <- gsub("^%%|%%$","", selection_string)
    selection_string <- eval(parse(text=selection_string))

    selection_string <- data.table(var=selection_string)
    selection_string <- chunkify(selection_string, chunk_row)

    temp_chunk <- mclapply(selection_string, function(x) {

      temp_selection_string <- paste0("('", paste0(x$var, collapse="','"), "')")
      
      temp_sql_string <- gsub(pattern, temp_selection_string, sql_string)

      temp <- sql_mod(temp_sql_string)

    })

    temp_comb <- rbindlist(temp_chunk, use.names=T)

    return(temp_comb)

} else {

     temp <- sql_mod(sql_string)

     return(temp)

}

}

################################################################################
## clean.icd.list
################################################################################

clean.icd.list <- function(list, icdcol, namescol, decimal=TRUE, out.name=namescol) {
### return list of all possible (real or not) icd9 codes for each concept from csv file
### csv can use dashes and decimal or short format 

  # sample usage
  # key_event_dx_codes <- fread("/data/zolab/methods_new/dia/key_event_dx.csv", 
  # stringsAsFactors=FALSE)
  # key_event_dx <<- clean.icd.list(key_event_dx_codes, "icd9_code", "dia_name", 
  # decimal=TRUE)
  
  # omit codes which are excluded/commented out (rows that start with "#")
  list <- list[!(list[[icdcol]] %like% "#" | list[[namescol]] %like% "#") ]
  
  # codes are separated by semicolon in csv file 
  x <- list[[icdcol]] %>% strsplit(";")
  names(x) <- list[[namescol]]

  expand_and_convert <- function(code) {
    # expand explicit ranges containing dash
    if(grepl("-",code)) {
      split <- strsplit(code, "-")[[1]]
      # convert decimals to short if needed
      if(decimal){
        split[1] <- icd9DecimalToShort(split[1])
        split[2] <- icd9DecimalToShort(split[2])
      }
      icd9ExpandRangeShort(split[1], split[2], onlyReal=FALSE)
    }
    # expand implicit ranges of 3- and 4-digit codes
    else {
      # convert decimals to short if needed
      if(decimal){
        code <- icd9DecimalToShort(code)
      }
      icd9ExpandRangeShort(code, code, onlyReal=FALSE)
    }
  }

  # apply expand_and_convert to each term & convert to list of data.tables
  clean <- lapply(x,
    function(xi) data.table("code"=unlist(lapply(xi,
      function(xij) expand_and_convert(xij)
      )))
    )

  # create concept flag and collapse list 
  lapply(seq_along(clean), function(i) clean[[i]][,paste(out.name):=names(clean)[[i]]])
  rbindlist(clean) %>% return

}

################################################################################
## bag_word_check
################################################################################

bag_word_check <- function(word, word_list) {

  # purpose: 
  # check how many words in a string are contained in a given other string (order is irrelevant)
 
  # return:
  # number of contained words

  sum(sapply(unlist(strsplit(word_list, " |-|,|\\.|:")), 
    function(x) {
      gsub("-|,|\\.|:", " ", word) %like% paste0("( |^)", x, "( |$)")
    }
  ))
}

################################################################################
## name_sort_match
################################################################################

name_sort_match <- function(dt_orig, name_col, title=TRUE, name_col_new=NA) {

  # purpose: 
  # standarise name col for merging - split, ommit titles/suffixes..., sort alphabetically

  # sample usage
  # unique_name <- data.table(prov_name=unique(enc_raw_icmp[!is.na(attending_md)]$attending_md))
  # unique_name <- name_sort_match(unique_name, "prov_name", title=TRUE, "prov_mod")



  dt <- copy(dt_orig)

  setnames(dt, name_col, "name_raw")

  dt[, name_final:=tolower(name_raw)]
  dt[, name_final:=gsub("^[ ]*|[ ]*$","",name_final)]
  
  # small fixes
  dt[, name_final:=gsub("r n$", "rn", name_final)]
  dt[, name_final:=gsub("pa-c", "pac", name_final)]
  dt[, name_final:=gsub("medical student", "medical_student", name_final)]
 
  dt[, name_final:=gsub("bwh", "", name_final)]
  dt[, name_final:=gsub("physician", "", name_final)]
  dt[, name_final:=gsub("surgery", "", name_final)]
  dt[, name_final:=gsub("dermatology", "", name_final)]
  dt[, name_final:=gsub("true", "", name_final)]
  dt[, name_final:=gsub("'|`", "", name_final)]
  dt[, name_final:=gsub("\\(.*\\)", "", name_final)]

  dt[, name_final:=gsub("nursing", "rn", name_final)]

  # deal with middle names
  provider_title_list <- c("bao","bcps","bwh nursing","dmd","do","dphil","jd","mba",
      "mbbs","md","medical_student","mhs","mpa","mph","mphil","mpp","ms","msc","np","pa","pac",
      "pharmd","phd","rn","rph", "lpn", "dpm", "aud", "licsw", "rd", "ldn")
  provider_title_list_pattern <- paste0( provider_title_list, collapse="|") 

  dt[gsub(provider_title_list_pattern, "", gsub("\\.|-", "", name_final)) %like% ",[ ]*[a-z]+", 
    name_final :=gsub("([^,]*),([ ]*)([^ ]+)( )([a-z])([^, ]+)( )*([^,]*)(,*.*)",
    "\\1,\\2\\3\\4\\5\\.\\7\\8\\9", name_final)]
  dt[!(gsub(provider_title_list_pattern, "", gsub("\\.|-", "", name_final)) %like% ",[ ]*[a-z]+"), 
    name_final :=gsub("(^[^ ]*) ([a-z])[a-z ]+ ([^ ]*),(.*)", 
    "\\1 \\2. \\3, \\4", name_final)]
   dt[gsub(provider_title_list_pattern, "", gsub("\\.|-", "", name_final)) %like% ",[ ]*[a-z]+" &
     gsub(provider_title_list_pattern, "", gsub("\\.|-", "", name_final)) %like% "[a-z]+ [a-z]+( )*," , name_final :=gsub("^([a-z])[a-z ]+( )([ ^ ,]*)", 
     "\\1.\\2\\3, \\4", name_final)]
   dt[!(name_final %like% ","), name_final :=gsub("^([^ ]*) ([a-z])([^ ]+).* ([^ ]+)$", 
     "\\1 \\2. \\4", name_final)]

  dt[, name_final:=gsub("-", " ", name_final)]

  dt[, c(paste0("name_", 1:(max(str_count(dt$name_final," |,"))+1))):=tstrsplit(name_final, 
    " |,")]

  dt[, c(grep("name_", names(dt), value=T)):=lapply(.SD, function(x) gsub("^[ ]*|[ ]*$","",x)), 
    by=1:nrow(dt), .SDcols=grep("name_", names(dt), value=T)]

  if(title==TRUE) {
    provider_title_list <- c("bao","bcps","bwh  nursing","dmd","do","dphil","jd","mba",
      "mbbs","md","medical_student","mhs","mpa","mph","mphil","mpp","ms","msc","np","pa","pac",
      "pharmd","phd","rn","rph", "lpn", "dpm", "aud", "licsw", "rd", "ldn")
    dt[, c("med_title"):=do.call(paste, c(lapply(.SD, function(x) ifelse(gsub("\\.|-", "", x) 
      %in% provider_title_list, x, "")))), by=1:nrow(dt), .SDcols=grep("name_", names(dt), value=T)]
    dt[, c(grep("name_[0-9]", names(dt), value=T)):=lapply(.SD, function(x) ifelse(gsub("\\.|-", "", x) 
      %in% provider_title_list, " ", x)), by=1:nrow(dt), .SDcols=grep("name_[0-9]", names(dt), value=T)]
 
    dt[, med_title:=gsub("\\.|-", "", med_title)]
    dt[, med_title:=gsub("^[ ]*|[ ]*$", "", med_title)]
    dt[, med_title:=gsub("[ ]{2,}", " ", med_title)]
    dt[, med_title:=do.call(paste, c(lapply(strsplit(med_title, " "), function(x) unique(x[order(x)])), 
      collapse=" ")), by=1:nrow(dt)]
    dt[, med_title:=gsub(" ", " / ", med_title)]
  }


  dt[, c(grep("name_[0-9]", names(dt), value=T)):=lapply(.SD, function(x) ifelse((x %like% "\\."), 
    " ", x)), by=1:nrow(dt), .SDcols=grep("name_[0-9]", names(dt), value=T)]
  set_na_zero(dt, " ")
  dt[, c(grep("name_[0-9]", names(dt), value=T)):=lapply(.SD, function(x) 
    ifelse(nchar(gsub("^[ ]*|[ ]*$", "",  as.character(x)))<=1, " ", x)), by=1:nrow(dt), 
    .SDcols=grep("name_[0-9]", names(dt), value=T)]
  set_na_zero(dt, " ")

 
  dt[, name_final:=do.call(paste, c(.SD, sep=" ")), .SDcols=grep("name_[0-9]", names(dt), value=T)]
  dt[, name_final:=gsub("[ ]{2,}", " ", name_final)]
  dt[, name_final_sort:=do.call(paste, c(lapply(strsplit(name_final, " "), function(x) x[order(x)]), 
    collapse=" ")), by=1:nrow(dt)]
  dt[, name_final:=gsub("^[ ]*|[ ]*$", "", name_final)]
  dt[, name_final_sort:=gsub("^[ ]*|[ ]*$", "", name_final_sort)]
  dt[, name_final:=gsub("`|'", "'", name_final)]

  setnames(dt,"name_raw", name_col)
  dt[, c(grep("name_[0-9]", names(dt), value=T)):=NULL]

  set_missing_na(dt)

  ## non providers
  dt[name_final_sort %like% "outside_ provider", ':=' (name_final_sort="outside_provider", 
    name_final="outside_provider")]
  dt[name_final_sort %like% "( |^)his( |$)", ':=' (name_final_sort="hosp_inf_sys", 
    name_final="hosp_inf_sys")]

  if(!is.na(name_col_new)) {
      setnames(dt,"name_final", name_col_new)
      setnames(dt,"name_final_sort", paste0(name_col_new, "_sort"))
  }

  return(dt)
}


################################################################################
## sparse_matrix_melt
################################################################################

sparse_matrix_melt <- function(dt, id_col, excl_col=NA) {

  # purpose: 
  # melt sparse matrix - retaining record when dummy!=0, i.e. delete all 0 records by performing a dt -> sparse 
  # matrix conversion (alternative to reshape when reshape is unable to handle size of the dt (crashes at the matrix -> dt stage))
 
  # return:
  # melted matrix

  temp_sparse <- Matrix(as.matrix(test_matrix[, mget(setdiff(names(dt), c(id_col, excl_col)))]), 
    sparse=TRUE)
  temp_sparse <- data.table(id=dt[, get(id_col)][summary(temp_sparse)$i],
      var_name=colnames(dt)[summary(temp_sparse)$j])
  
  print(names(temp_sparse))
  print(str(temp_sparse))
  print(nrow(temp_sparse))
  print(ncol(temp_sparse))

  return(temp_sparse)

}


################################################################################
## extract_note
################################################################################

extract_note <- function(note, patient_set, note_empi="empi", 
  note_date="lno_date", note_comment="comments", patient_empi="empi", 
  patient_date, note_type=NA, order=NA, other_var=NA) {


  # purpose: 
  # extract sample notes, e.g. lno/dis for subset of patients
  
  # set-up
  patient_set_temp <- copy(patient_set)
  patient_set_temp[, patient_set_id:=1:.N]
  note_temp <- copy(note)
  
  comment_var <- "comment"
 
  print(paste0("number of observations: ", nrow(patient_set_temp)))

  setnames(note_temp, c(note_empi, note_date), c(patient_empi, patient_date))
  setnames(note_temp, note_comment, comment_var)

  note_extract <- note_temp[patient_set_temp, on=c(patient_empi, patient_date)]

  if (!is.na(note_type)) {
    note_extract <- note_extract[comment %like% note_type]
  }

  print(paste0("number of notes: ", nrow(note_extract)))
  print(paste0("number of observations with at least one note: ", 
    length(unique(note_extract$patient_set_id))))

if(!is.na(other_var) & !is.na(order)) {
    temp <- note_extract[, mget(union(c("patient_set_id",patient_empi, patient_date, comment_var, 
    order),c(other_var)))]
  } else if (is.na(other_var) & is.na(order)) {
    temp <- note_extract[, mget(c("patient_set_id",patient_empi, patient_date, comment_var))]
  } else if(!is.na(other_var) & is.na(order)) {
    temp <- note_extract[, mget(union(c("patient_set_id",patient_empi, patient_date, comment_var),c(other_var)))]
  } else if (is.na(other_var) & !is.na(order)) {
    temp <- note_extract[, mget(c("patient_set_id",patient_empi, patient_date, comment_var, 
    order))]
}

  note_extract[, comment:=paste0(paste0(rep("#", 100), collapse=""), comment)]
  note_extract[, comment:=paste0(paste0(c(rbind((paste0("//",names(patient_set_temp)[!names(patient_set_temp) %like% "time"])), 
    unlist(str_split(do.call("paste", mget(names(patient_set_temp)[!names(patient_set_temp) %like% "time"])), " ")))), collapse=" : ")
    , comment), by=1:nrow(note_extract)]
  note_extract[, comment:=paste0(paste0(rep("#", 200), collapse=""), comment)]

  if (!is.na(order)) {
    
    note_extract <- split(note_extract, note_extract[, get(order)])
    note_extract <- lapply(note_extract, function(x) x[1, comment:=paste("number of notes: ", nrow(x), 
      "number of observations:", length(unique(x$patient_set_id)),
      "\n\n##############", comment)])
    note_extract <- lapply(note_extract, function(x) x[, get("comment")])
    note_extract <- list(note_extract, temp)
     } else {
      
    note_extract[1, comment:=paste("number of notes: ", nrow(note_extract), 
      "number of observations:", length(unique(note_extract$patient_set_id)), 
      "\n\n##############", comment)]
    note_extract[, setdiff(names(note_extract), "comment"):=NULL]
    note_extract <- list(note_extract, temp)
  }
  
  return(note_extract)

}


################################################################################
## gagne_score_calc
################################################################################

gagne_score_calc <- function(id_list, empi_list, date_list, dia_dt, timeframe_day=365, 
  buffer_day=0, gagne_count_var=FALSE) {

  # purpose: 
  # calculate the gagne score (i.e. gagne counts) for a given set of empis over a 
  # given time period leading up to an event

  # sample usage
  # gagne_score <- gagne_score_calc(
  #   dt$ed_enc_id,        # id column (*dt - data table - one row per event (date, patient, id (unique event identifier)))
  #   dt$empi,             # patient column
  #   dt$adm_date_ed,      # date column 
  #   oncdrs_rpdr_dia,     # dia dt containing a list of all diagnoses classified into gagne categories ('gagne column')
  #   timeframe_day=365, 
  #   buffer_day=1, 
  #   gagne_count_var=TRUE)
   
  #############################################################################
  ## gagne cat
  gagne_cat <- fread("/data/zolab/methods_new/dia/gagne_codes/Gagne_codes.csv")

  ##############################################################################
  ## id_dt
  id_dt <- data.table(empi=empi_list, event_date=date_list, id_id=id_list)
  id_dt[, event_date_end:=event_date-buffer_day]
  id_dt[, event_date_pre:=event_date_end-timeframe_day]

  ##############################################################################
  ## subset columns
  dia_dt <- dia_dt[, .(empi, dia_date, dia_date_1, gagne)]

  ##############################################################################
  ## subset to cohort 
  dia_dt  <- dia_dt [empi %in% id_dt$empi]

  ##############################################################################
  ## foverlaps 
  setkey(id_dt, empi, event_date_pre, event_date_end)
  dia_olap <-foverlaps(dia_dt, id_dt, by.x=c("empi","dia_date", "dia_date_1"), nomatch=0)

  print(min(dia_dt$dia_date))
  print(max(dia_dt$dia_date))

  print(min(id_dt$event_date))
  print(max(id_dt$event_date))

  print(min(dia_olap$dia_date))
  print(max(dia_olap$dia_date))


  ##############################################################################
  ### generating gagne scores

  # gagne_formula_exp <- quote(
  #   5 * metastatic_romano +
  #   2 * chf_romano +
  #   2 * dementia_romano +
  #   2 * renal_elixhauser +
  #   2 * wtloss_elixhauser +
  #   1 * hemiplegia_romano +
  #   1 * alcohol_elixhauser +
  #   1 * tumor_romano +
  #   1 * arrhythmia_elixhauser +
  #   1 * pulmonarydz_romano +
  #   1 * coagulopathy_elixhauser +
  #   1 * compdiabetes_elixhauser +
  #   1 * anemia_elixhauser +
  #   1 * electrolytes_elixhauser +
  #   1 * liver_elixhauser +
  #   1 * pvd_elixhauser +
  #   1 * psychosis_elixhauser +
  #   1 * pulmcirc_elixhauser +
  #  -1 * hivaids_romano +
  #  -1 * hypertension_elixhauser)

  # XXX NOTE: gagne_cat + gagne_weights + gagne_formula -> recreate gagne_formula_exp dynamically 
  gagne_name <- gagne_cat$gagne
  gagne_weight <- as.numeric(gsub("_", "-", gagne_cat$weight))
  gagne_formula_exp <- ""

  gagne_formula  <- function(cat, weight, ext) {
    for (i in 1:length(cat)) {
      gagne_formula_exp <- paste(gagne_formula_exp, weight[i], "*", paste0(ext, 
        cat[i]), "+", sep=" ")
    }
    gagne_formula_exp <- gsub("\\+$", "",gagne_formula_exp)
    return(gagne_formula_exp)
  }

  # (b) reshaping - create diagnosis code count vars (gagne)
  gagne_timeframe_comb <- dcast.data.table(dia_olap, id_id + empi + event_date ~  
    paste0("gagne_count_", gagne), length, value.var = "gagne", subset=.(!is.na(gagne)  & gagne!="" ))

  # expand to ensure that all the ids are represented 
  gagne_timeframe_comb <- gagne_timeframe_comb[id_dt[,.(id_id)], on=c("id_id"), nomatch=NA]
  gagne_timeframe_comb[,  grep("gagne_count", names(gagne_timeframe_comb), value=T) :=lapply(.SD, 
    function(x) ifelse(is.na(x), 0,x)), .SDcols=grep("gagne_count", names(gagne_timeframe_comb))]

  # (c) generate complete set of gagne category dummies (i.e. 0/1 if present 
  # or not at least once during time period) [impute 0]

  gagne_timeframe_comb[,  grep("gagne_count", names(gagne_timeframe_comb), value=T) :=lapply(.SD, 
    function(x) ifelse(x>=1, 1,0)), .SDcols=grep("gagne_count", names(gagne_timeframe_comb))]
  gagne_timeframe_comb[, setdiff(paste0("gagne_count_", gagne_name), names(gagne_timeframe_comb)):=0]

  # (d) determine the gagne score 
  gagne_timeframe_comb[,gagne_score:=eval(parse(text=gagne_formula(gagne_name[as.numeric(gsub("_", 
    "-", gagne_cat$weight))!=0], gagne_weight[as.numeric(gsub("_", "-", gagne_cat$weight))!=0], "gagne_count_")))]

  ##############################################################################
  ### returning the score

  setnames(gagne_timeframe_comb, "id_id", "id")
  
  if (gagne_count_var==FALSE) {
    return(gagne_timeframe_comb[, .(id, empi, event_date, gagne_score)])
  } else {
    return(gagne_timeframe_comb[, mget(c("id", "empi", "event_date", 
      "gagne_score", grep("gagne_count_*",  names(gagne_timeframe_comb), value=T)))])
  }

}

################################################################################
## obs_count
################################################################################

dt_stat <- function(id_list, empi_list, date_list, dt, dt_date_var, timeframe_day=365, 
  buffer_day=0, timeframe_hour=24,buffer_hour=0, exp_list=NA, exp_list_agg=NA, 
  mode="day", mode_direction="backward") {

  # purpose: 
  # count number of observations in given timeframe for a given list of empis 
  # in a given dt

  ## prepare id table
  id_dt <- data.table(empi=empi_list, event_date=date_list, id=id_list)

  if (mode=="day" & mode_direction=="backward") {

    id_dt[, event_date_end:=event_date-buffer_day]
    id_dt[, event_date_pre:=event_date_end-timeframe_day]

  } else if (mode=="hour" & mode_direction=="backward") {

    id_dt[, event_date_end:=as.POSIXct(event_date, 
      "%Y-%m-%d %H-%M-%s")-hours(buffer_hour)]
    id_dt[, event_date_pre:=as.POSIXct(event_date_end, 
      "%Y-%m-%d %H-%M-%s")-hours(timeframe_hour)]  

  } else if (mode=="day" & mode_direction=="forward") {

    id_dt[, event_date_pre:=event_date+buffer_day]
    id_dt[, event_date_end:=event_date_pre+timeframe_day]


  } else if (mode=="hour" & mode_direction=="forward") {

    id_dt[, event_date_pre:=as.POSIXct(event_date, 
      "%Y-%m-%d %H-%M-%s")+hours(buffer_hour)]  
    id_dt[, event_date_end:=as.POSIXct(event_date_pre, 
      "%Y-%m-%d %H-%M-%s")+hours(timeframe_hour)]
  
  }

  setkey(id_dt, empi, event_date_pre, event_date_end)

  ## prepare dt
  dt_copy <- copy(dt)
  dt_copy[,date_1:=mget(dt_date_var)]

  ## foverlaps
  olap <- foverlaps(dt_copy, id_dt, by.x=c("empi", dt_date_var, "date_1"), 
    nomatch=0)

 if (is.na(exp_list[1])) {

    olap_coll <- olap[, .(obs_count=as.numeric(.N)), by=c("empi", "event_date", "event_date_pre", 
      "event_date_end", "id")]

    return_var <- c("id","empi", "event_date", "event_date_pre", "event_date_end", "obs_count")


  } else {

    mapply(function(exp_name,exp) {
      olap[, c(exp_name):=as.numeric(eval(parse(text=exp))), by=c("empi", "event_date", "event_date_pre", 
      "event_date_end", "id")]

    }, exp_name=c("obs_count", names(exp_list)), exp=c(".N",exp_list))
     
     olap_coll <- unique(olap, by=c("empi", "event_date", "event_date_pre", 
      "event_date_end", "id"))

     return_var <- c("id","empi", "event_date", "event_date_pre", "event_date_end", "obs_count", names(exp_list))
  }


  if (is.na(exp_list_agg[1])) {
    
    ## return olap_coll
    return(olap_coll[,mget(return_var)])

  } else {
   
    stat <- lapply(exp_list_agg, function(exp) {

    return(unique(olap[, .(eval(parse(text=exp)))]))
    
    })

    names(stat) <- names(exp_list_agg)
  
     ## return olap_coll
    return(list(olap_coll[,mget(return_var)], unlist(stat)))
  }


}

################################################################################
## readRDS_merge
################################################################################

readRDS_merge <- function(file_name_list, nested=FALSE) {

     # purpose: 
     # read in (a) RDS file (standard) (b) list of RDS files - merge (assumed to be in 
     # master_file format,i.e. to contain a [file_name]_id)

      if (length(file_name_list)==1) {

        return(readRDS(file_name_list[[1]]))

      } else if ( length(file_name_list)>1 & nested==FALSE) {

        for(i in 1:length(file_name_list)) {
           assign(paste0("temp_dt_", i), readRDS(file_name_list[i][[1]]))
        }

        temp <- rbindlist(mget(paste0("temp_dt_", 1:length(file_name_list))),use.names=T, fill=T)
        temp <- unique(temp,by=c(setdiff(names(temp), grep("_id$", names(temp),value=T))))

        file_temp_list <- ls(pattern="temp_dt_[0-9]")
        lapply(file_temp_list, function(x) rm(x))
        return(temp)

  } else if ( length(file_name_list)>1 & nested==TRUE) {
        
        for(i in 1:length(file_name_list)) {
           assign(paste0("temp_dt_", i), readRDS(file_name_list[i][[1]]))
        }

        temp_name <- names(temp_dt_1)
        temp <- lapply(   temp_name, function(table_name) lapply(mget(paste0("temp_dt_", 1:length(file_name_list)), 
          sys.frame(sys.parent(n=2))), function(x) x[[table_name]]))
        temp <- lapply(temp, function(x) rbindlist(x, use.names=T, fill=T))
        temp <- lapply(temp, function(x) unique(x, by=c(setdiff(names(x), 
          grep("_id$", names(x),value=T)))))

        names(temp) <-  temp_name

        file_temp_list <- ls(pattern="temp_dt_[0-9]")
        lapply(file_temp_list, function(x) rm(x))
        return(temp)

  }

}

################################################################################
## corr_stat
################################################################################

corr_stat <- function(dt, var_list_list, save=FALSE, path=NA) {

  # purpose: 
  # generate correlation matrix, summary stats and distribution graph for a 
  # list of variables contained in a dt (iterate over a number of such variable lists)

    stat <- lapply(var_list_list, function(x) {

    cat("\n\n")
    cat(x)
    cat("\n\n")

    # generate corr matrix
    cat("\n\ngenerate corr matrix\n\n")
    corr_matrix <- cor(as.matrix(dt[, mget(c(x))][complete.cases(dt[, mget(c(x))])]))

    # generate stats
    cat("\n\ngenerate summ stats\n\n")
    summ_stat <- data.table(do.call(data.frame,lapply(x, function(y) data.table(stat=names(summary(dt[, get(y)])),
      var=(matrix(summary(dt[, get(y)])))))))
    setnames(summ_stat, "stat", "stat_name")
    summ_stat[, c(grep("stat.[0-9]", names(summ_stat), value=T)):=NULL]
    setnames(summ_stat, c("stat_name", x))

    # generate dist graphs
    cat("\n\ngenerate dist graphs\n\n")
    dt_graph <-copy(dt)
    dt_graph[,melt_id:=1:nrow(dt)]
    dt_graph <- melt(dt_graph, id.vars="melt_id", measure.vars=x)
    dist_graph <- ggplot(data=dt_graph, aes(x=value, colour=variable)) +
      geom_density() +
      labs(
        title="variable distribution"
      ) +
      theme_basic(axis_size=0.3, title_size=4,subtitle_size=3) +
        theme_legend_bottom(title_size=0.3, text_size=0.3, tick_size=0, legend_width=0.5, 
          legend_height=0.15)

    return(list(corr_matrix, summ_stat, dist_graph))

  })

  if (save==TRUE) {

    for(i in 1:length(var_list_list)) {
      cat(sprintf("begin saving output from %s", paste0(var_list_list[i], collapse="  ")))

      # print(head(sapply(stat, "[", 1)[i]))
      # print(paste0(path,"corr_matrix_", paste0(var_list_list[i][[1]][[1]],collapse="_") , ".csv"))
      write.csv(sapply(stat, "[", 1)[i], paste0(path,"corr_matrix_", paste0(var_list_list[i][[1]][[1]],collapse="_") ,"_", i,"_", ".csv"))
     
      # print(head(sapply(stat, "[", 2)[i]))
      # print(paste0(path,"corr_matrix_", paste0(path, "summ_stat_", paste0(var_list_list[i][[1]][[1]],collapse="_") , ".csv")))
      write.csv(sapply(stat, "[", 2)[i], paste0(path, "summ_stat_", paste0(var_list_list[i][[1]][[1]],collapse="_") ,"_", i,"_", ".csv"), row.names=F)
     
      # print(sapply(stat, "[", 3)[i][[1]])
      # print(paste0(path, "dist_graph_", paste0(var_list_list[i][[1]][[1]],collapse="_")))
      ggsave(paste0(path, "dist_graph_", paste0(var_list_list[i][[1]][[1]],collapse="_"),"_", i,"_", ".pdf"),sapply(stat, "[", 3)[i][[1]])

    }

    } else {

      return(stat)

    }
}

################################################################################
## cum_dt
################################################################################

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

################################################################################
## dt_paste
################################################################################

dt_paste <- function(list_dt, text, length=10, text_col=NA, number_col=NA, line_sep=F) {

  # purpose: 
  # paste elements of a freq dt into a string 

  if (is.na(text_col)) text_col <- names(list_dt)[which(sapply(list_dt,function(x) 
    class(x)[1]) %in% c("character", "factor"))]
  if (is.na(number_col)) number_col <- names(list_dt)[which(sapply(list_dt,function(x) 
    !(class(x)[1] %in% c("character","factor"))))]

   if (line_sep==FALSE)  { 
      comb <- paste0(list_dt[[text_col]][1:length], " - ", list_dt[[number_col]][1:length],  
          "  // ", collapse=" ")
      comb <- gsub(" // $", "",comb)
      comb <- paste(comb, text)

    } else if (line_sep==TRUE) {

     comb <- lapply(1:length, function(x) {

        comb <- paste0(list_dt[[text_col]][x], " - ", list_dt[[number_col]][x],
           "  // ", collapse=" ")
        comb <- gsub(" // $", "",comb)
        comb <- paste(comb, text)

     })

    }

    comb <- gsub("^[ ]*|[ ]*$", "", comb)

    return(comb)

}


################################################################################
## checksum
################################################################################

checksum <- function(dt_path_abb_name, raw_mod=NA, 
  dt_path_abb_sep_name=NA, md5_list=NA, md5_file=NA, date_comparison="most_recent" ) {

  md5 <- as.character(md5sum(get(dt_path_abb)))

  if (!is.na(dt_path_abb_sep)) dt_path_abb <- dt_path_abb_sep

  ps("md5 - %s - %s: %s", dt_path_abb_name, raw_mod, md5)


  if (!is.na(md5_file[1])) {

    # compare to previous 
    if (date_comparison=="most_recent") {

      setorder(file, date)
      file[, most_recent:=0][.GRP==1, most_recent:=1, by=c("date")]

    } else {

      file[, most_recent:=0][date==date_comparison, most_recent:=1, by=c("date")]

    }

    if (is.na(raw_mod)) {
      md5_comparison <- file[file==dt_name & most_recent==1 ]
    } else {
      md5_comparison <- file[file==dt_name & most_recent==1 & type=="raw_mod"]
    }

    ps("md5 comparison - %s - %s -- %s  (%s) vs. %s (%s): %s", dt_name, raw_mod, 
      current_date, md5,
      as.character(md5_comparison$date),
      as.character(md5_comparison$md5),
      as.character(md5==md5_comparison$md5))

  } 

  if  (!is.na(md5_list[1])) {

    # return  md5 appended to list
    list[[paste0(dt_path_abb, raw_mod)]] <<- md5

  }

}

################################################################################
## bigcor_mod
################################################################################

bigcor_mod <- function(x, nblocks = 10, verbose = TRUE, ...) {  

  suppressMessages(library(ff, quietly=TRUE,verbose=FALSE))

  dt <- copy(x)

  gap <- ceiling(ncol(dt)/10)*10-ncol(dt)
  # print(ncol(dt))
  # print(gap)

  if (gap>0) {
    dt[, paste0("temp_", 1:gap):=0]
  }
    
  # print(ncol(dt))
  # print(ncol(dt)/nblocks)

  MAT <- as.matrix(dt)

  # library(ff, quietly = TRUE) 
  NCOL <- ncol(dt)  
    
  ## test if ncol(x) %% nblocks gives remainder 0 
  if (NCOL %% nblocks != 0) stop("Choose different 'nblocks' so that ncol(x) %% nblocks = 0!")  
    
  ## preallocate square matrix of dimension 
  ## ncol(x) in 'ff' single format  
  corMAT <- ff(vmode = "single", dim = c(NCOL, NCOL)) 
    
  ## split column numbers into 'nblocks' groups 
  SPLIT <- split(1:NCOL, rep(1:nblocks, each = NCOL/nblocks)) 
    
  ## create all unique combinations of blocks 
  COMBS <- expand.grid(1:length(SPLIT), 1:length(SPLIT))  
  COMBS <- t(apply(COMBS, 1, sort)) 
  COMBS <- unique(COMBS)  
    
  ## iterate through each block combination, calculate correlation matrix between blocks and store    them in the preallocated matrix on both 
  ## symmetric sides of the diagonal  
  for (i in 1:nrow(COMBS)) {  
  COMB <- COMBS[i, ]  
  G1 <- SPLIT[[COMB[1]]]  
  G2 <- SPLIT[[COMB[2]]]  
  # if (verbose) cat("Block", COMB[1], "with Block", COMB[2], "\n") 
  flush.console() 
  COR <- cor(MAT[, G1], MAT[, G2], ...)
    
  corMAT[G1, G2] <- COR 
  corMAT[G2, G1] <- t(COR)  
  COR <- NULL 
  } 

  colnames(corMAT) <- colnames(dt)
  rownames(corMAT) <- colnames(dt)
  corMAT <- corMAT[c(setdiff(rownames(corMAT), grep("temp_", rownames(corMAT), value=T))), ]
  corMAT <- corMAT[, c(setdiff(colnames(corMAT), grep("temp_", colnames(corMAT), value=T))) ]
    
  gc()  
  return(corMAT)
}


################################################################################
## ingest_meta
################################################################################

ingest_meta <- function(start_var_end, file_name_var, meta_batch_doc_arg=meta_batch_doc, data_sample_var=data_sample, 
  date=as.character(format(Sys.time(), "%d/%m/%Y_%H:%M")), source_date_time_var=source_date_time) {

    # purpose: 
    # function to generate and update csv file with meta data regarding batch mode job execution

    # key arguments
    # file_name_var - name of the script being executed- can be set at beginning of script
    # data_sample - name of dataset (e.g. cohort) for which script is being executed - can be set at beginning of script
    # source-data_time - time (date_time) set in the first line of the script
    # meta_batch_doc - path to csv file where meta data is stored

    # sample usage
  #  code start: ingest_meta("start")
  #  code end: ingest_meta("end")

  jobid_var <- paste0(file_name_var, "_", source_date_time)
  date_date <- strptime(date, "%d/%m/%Y_%H:%M")
  source_date_time_var_date <- strptime(source_date_time, "%d/%m/%Y_%H:%M:%S")

  if (start_var_end=="start") {

    meta <- data.table(file_name=file_name_var, data_sample=data_sample_var, start_date=date, 
      var_list="/", end_date="/", run_time_min="/", job_id=jobid_var)

    if(file.exists(meta_batch_doc_arg)) {
      meta_master <- fread(meta_batch_doc_arg)
      meta <- rbindlist(list(meta_master, meta))
    }
    
    write.csv(meta, meta_batch_doc_arg, row.names=F)

  }


  if (start_var_end=="end") {

     meta <- fread(meta_batch_doc_arg)
     
     meta[job_id==jobid_var, end_date:=date]
     meta[job_id==jobid_var, run_time_min:=round(as.numeric(difftime(date_date, 
      source_date_time_var_date, units="mins")), digit=1)]

    write.csv(meta, meta_batch_doc_arg, row.names=F)

  }

}


################################################################################
##################################### END ######################################
################################################################################

