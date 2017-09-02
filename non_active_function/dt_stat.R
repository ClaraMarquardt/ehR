#----------------------------------------------------------------------------#

#' Count number of observations in a data.table over a given timeframe/a given id list. 
#' @export
#' @import data.table
#' @param  TBC
#' @return TBC
#' @examples
#' TBC

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
  print(dt)
  dt_copy <- copy(dt)
  dt_copy[,date_1:=mget(dt_date_var)]

  ## foverlaps
  olap <- foverlaps(dt_copy, id_dt, by.x=c("empi", dt_date_var, "date_1"), 
    nomatch=0)
  
  if (mode=="hour") {
    olap[, time_to_event:=round(as.numeric(difftime(get(dt_date_var), 
      event_date, units="hour")), digits=2)]
  } else if (mode=="day") {
    olap[, time_to_event:=round(as.numeric(difftime(get(dt_date_var), 
      event_date, units="days")), digits=2)]

  }
  
  setorder(olap, "id", "time_to_event")

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
    return(list(olap_coll[,mget(return_var)], olap))

  } else {
   
    stat <- lapply(exp_list_agg, function(exp) {

    return(unique(olap[, .(eval(parse(text=exp)))]))
    
    })

    names(stat) <- names(exp_list_agg)
  
     ## return olap_coll
    return(list(olap_coll[,mget(return_var)], unlist(stat), olap))
  }


}

#----------------------------------------------------------------------------#
