#----------------------------------------------------------------------------#

#' @title Automatically recognise and parse date variables. 
#'
#' @description /
#'
#' @export
#' @import data.table
#' @import lubridate
#' @param dt_vector 
#' @param column_name_list
#' @param column_name_list_new
#' @return
#' @examples


parse_date <- function(dt_vector, column_name_list,column_name_list_new=column_name_list) {
  
  if (class(dt_vector)[[1]]=="data.table") {

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
    
    # print("-----------------")
    # print("following columns not converted to a date:")
    # print(non_date)
    # print("-----------------")

  } 

    if (class(dt_vector)[[1]]=="character") {

    string <- dt_vector

    print(string[1:50])
    string <- as.Date(parse_date_time(string, 
      c("mdy","ymd","dby","myd", "dmy", "bdy")))
    print(string[1:50])

  } 

}

#----------------------------------------------------------------------------#
